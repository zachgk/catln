{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Python.Parser
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines a Python parser using TreeSitter, and
-- converts the parsed Python AST into a Catln 'RawPrgm'.
--------------------------------------------------------------------

module Syntax.Python.Parser where

import           Control.Monad
import           CtConstants
import qualified Data.ByteString          as BS
import qualified Data.HashMap.Strict      as H
import           Data.List                (intercalate, partition)
import           Data.Maybe               (listToMaybe, mapMaybe)
import           Foreign                  (Ptr)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Common.TreeSitter
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Syntax.Python.Prgm
import           Text.Read                (readMaybe)
import           TreeSitter.Node
import           TreeSitter.Parser
import           TreeSitter.Python        (tree_sitter_python)
import           TreeSitter.Tree

--------------------------------------------------------------------
-- TreeSitter helper utilities
--------------------------------------------------------------------

-- | Get named children mapped by their field name (as seen from parent).
-- Only includes named nodes (anonymous tokens like '+' are excluded).
withFieldChildMap :: Node -> (H.HashMap String [Ptr Node] -> IO a) -> IO a
withFieldChildMap node f = do
  withNamedNodeChildren node $ \children -> do
    labeledChildren <- forM children $ \childP -> do
      withReadNode childP $ \child -> do
        en <- easyNode child
        return (eFieldName en, childP)
    f $ H.fromListWith (flip (++)) [(fn, [p]) | (Just fn, p) <- labeledChildren]

-- | Like 'withFieldChildMap' but includes ALL children (named and anonymous),
-- so field-named anonymous nodes (e.g. the '+' operator) are also accessible.
withAllFieldChildMap :: Node -> (H.HashMap String [Ptr Node] -> IO a) -> IO a
withAllFieldChildMap node f = do
  withNodeChildren node $ \children -> do
    labeledChildren <- forM children $ \childP -> do
      withReadNode childP $ \child -> do
        en <- easyNode child
        return (eFieldName en, childP)
    f $ H.fromListWith (flip (++)) [(fn, [p]) | (Just fn, p) <- labeledChildren]

-- | Parse a dotted name node (e.g. "os.path") into a slash-separated path.
parseDottedName :: BS.ByteString -> Ptr Node -> IO String
parseDottedName fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    withNamedNodeChildren node $ \children -> do
      parts <- forM children $ \childP ->
        withReadNode childP $ \child -> nodeContents fileContents child
      if null parts
        then nodeContents fileContents node
        else return $ intercalate "/" parts

--------------------------------------------------------------------
-- Expression parsing
--------------------------------------------------------------------

-- | Parse a binary operator node using field-name-based child lookup.
-- Works for binary_operator, boolean_operator, and comparison_operator nodes.
-- The operator token is anonymous but has field name "operator".
parsePyBinOp :: BS.ByteString -> Node -> IO PyExpr
parsePyBinOp fileContents node = withAllFieldChildMap node $ \fields -> do
  left <- case H.lookup "left" fields of
    Just (lP:_) -> parsePyExpr fileContents lP
    _           -> return $ PyVar "<unknown>"
  op <- case H.lookup "operator" fields of
    Just (opP:_) -> withReadNode opP $ \opN -> nodeContents fileContents opN
    _            -> return "?"
  right <- case H.lookup "right" fields of
    Just (rP:_) -> parsePyExpr fileContents rP
    _           -> return $ PyVar "<unknown>"
  return $ PyBinOp op left right

-- | Parse a Python expression from a TreeSitter node.
parsePyExpr :: BS.ByteString -> Ptr Node -> IO PyExpr
parsePyExpr fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    en <- easyNode node
    case eType en of
      "identifier"          -> PyVar <$> nodeContents fileContents node
      "integer"             -> do
        s <- nodeContents fileContents node
        return $ case readMaybe s :: Maybe Integer of
          Just i  -> PyInt i
          Nothing -> PyLit s  -- fallback for hex (0xFF), binary (0b101), etc.
      "float"               -> do
        s <- nodeContents fileContents node
        return $ case readMaybe s :: Maybe Double of
          Just f  -> PyFloat f
          Nothing -> PyLit s  -- fallback for unusual float syntax
      "string"              -> PyLit <$> nodeContents fileContents node
      "concatenated_string" -> PyLit <$> nodeContents fileContents node
      "true"                -> return (PyBool True)
      "false"               -> return (PyBool False)
      "none"                -> return PyNone

      "call" -> withFieldChildMap node $ \fields -> do
        func <- case H.lookup "function" fields of
          Just (fP:_) -> parsePyExpr fileContents fP
          _           -> return $ PyVar "<unknown>"
        args <- case H.lookup "arguments" fields of
          Just (argsP:_) -> parsePyCallArgs fileContents argsP
          _              -> return []
        return $ PyCall func args

      -- Binary operators use field names: left, operator, right.
      -- The operator token is anonymous but has a field name, so we use
      -- withAllFieldChildMap which includes anonymous nodes.
      "binary_operator" -> parsePyBinOp fileContents node
      "boolean_operator" -> parsePyBinOp fileContents node
      "comparison_operator" -> parsePyBinOp fileContents node

      -- Unary operators: field names are 'operator' (anonymous) and 'operand'
      "unary_operator" -> withAllFieldChildMap node $ \fields -> do
        op <- case H.lookup "operator" fields of
          Just (opP:_) -> withReadNode opP $ \opN -> nodeContents fileContents opN
          _            -> return "-"
        e <- case H.lookup "operand" fields of
          Just (eP:_) -> parsePyExpr fileContents eP
          _           -> return $ PyVar "<unknown>"
        return $ PyUnOp op e

      -- 'not x': field 'argument' holds the operand
      "not_operator" -> withFieldChildMap node $ \fields -> do
        e <- case H.lookup "argument" fields of
          Just (eP:_) -> parsePyExpr fileContents eP
          _           -> return $ PyVar "<unknown>"
        return $ PyUnOp "not" e

      -- 'type' is a named wrapper for type annotations; look through it
      "type" -> withNamedNodeChildren node $ \children ->
        case children of
          (eP:_) -> parsePyExpr fileContents eP
          []     -> PyVar <$> nodeContents fileContents node

      -- expression_list (e.g. in return a, b) — use first element
      "expression_list" -> withNamedNodeChildren node $ \children ->
        case children of
          (eP:_) -> parsePyExpr fileContents eP
          []     -> return PyNone

      "attribute" -> withFieldChildMap node $ \fields -> do
        obj <- case H.lookup "object" fields of
          Just (oP:_) -> parsePyExpr fileContents oP
          _           -> return $ PyVar "<unknown>"
        attr <- case H.lookup "attribute" fields of
          Just (aP:_) -> withReadNode aP $ \aN -> nodeContents fileContents aN
          _           -> return "<unknown>"
        return $ PyAttr obj attr

      "list" -> withNamedNodeChildren node $ \children -> do
        elems <- forM children $ parsePyExpr fileContents
        return $ PyList elems

      "set" -> withNamedNodeChildren node $ \children -> do
        elems <- forM children $ parsePyExpr fileContents
        return $ PyList elems

      "tuple" -> withNamedNodeChildren node $ \children -> do
        elems <- forM children $ parsePyExpr fileContents
        return $ PyTuple elems

      "subscript" -> withFieldChildMap node $ \fields -> do
        base <- case H.lookup "value" fields of
          Just (bP:_) -> parsePyExpr fileContents bP
          _           -> return $ PyVar "<unknown>"
        idx <- case H.lookup "subscript" fields of
          Just (iP:_) -> parsePyExpr fileContents iP
          _           -> return PyNone
        return $ PySubscript base idx

      "parenthesized_expression" ->
        withNamedNodeChildren node $ \children ->
          case children of
            (eP:_) -> parsePyExpr fileContents eP
            []     -> return PyNone

      "dotted_name" -> do
        parts <- withNamedNodeChildren node $ \children ->
          forM children $ \childP ->
            withReadNode childP $ \child -> nodeContents fileContents child
        return $ PyVar (intercalate "." parts)

      "lambda" -> return $ PyVar "<lambda>"

      -- Fallback: treat as a literal with raw source text
      _ -> PyLit <$> nodeContents fileContents node

-- | Parse the argument list of a function call.
parsePyCallArgs :: BS.ByteString -> Ptr Node -> IO [PyCallArg]
parsePyCallArgs fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    withNamedNodeChildren node $ \children ->
      forM children $ \childP ->
        withReadNode childP $ \child -> do
          en <- easyNode child
          case eType en of
            "keyword_argument" -> withFieldChildMap child $ \fields -> do
              name <- case H.lookup "name" fields of
                Just (nP:_) -> withReadNode nP $ \nN -> nodeContents fileContents nN
                _           -> return "<unknown>"
              val <- case H.lookup "value" fields of
                Just (vP:_) -> parsePyExpr fileContents vP
                _           -> return PyNone
              return $ PyKwArg name val
            _ -> PyPosArg <$> parsePyExpr fileContents childP

--------------------------------------------------------------------
-- Parameter parsing
--------------------------------------------------------------------

-- | Parse a single function parameter.
parsePyParam :: BS.ByteString -> Ptr Node -> IO PyParam
parsePyParam fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    en <- easyNode node
    case eType en of
      "identifier" -> do
        name <- nodeContents fileContents node
        return $ PySimpleParam name Nothing

      -- In tree-sitter-python, typed_parameter's identifier has no field name;
      -- only 'type' has a field name. Use type-based lookup for the identifier.
      "typed_parameter" -> withNamedChildNodeMap node $ \children -> do
        name <- case H.lookupDefault [] "identifier" children of
          ((nP, _):_) -> withReadNode nP $ \nN -> nodeContents fileContents nN
          []          -> return "<unknown>"
        typ <- case H.lookupDefault [] "type" children of
          ((tP, _):_) -> Just <$> parsePyExpr fileContents tP
          []          -> return Nothing
        return $ PySimpleParam name typ

      "default_parameter" -> withFieldChildMap node $ \fields -> do
        name <- case H.lookup "name" fields of
          Just (nP:_) -> withReadNode nP $ \nN -> nodeContents fileContents nN
          _           -> return "<unknown>"
        val <- case H.lookup "value" fields of
          Just (vP:_) -> parsePyExpr fileContents vP
          _           -> return PyNone
        return $ PyDefaultParam name Nothing val

      "typed_default_parameter" -> withFieldChildMap node $ \fields -> do
        name <- case H.lookup "name" fields of
          Just (nP:_) -> withReadNode nP $ \nN -> nodeContents fileContents nN
          _           -> return "<unknown>"
        typ <- case H.lookup "type" fields of
          Just (tP:_) -> Just <$> parsePyExpr fileContents tP
          _           -> return Nothing
        val <- case H.lookup "value" fields of
          Just (vP:_) -> parsePyExpr fileContents vP
          _           -> return PyNone
        return $ PyDefaultParam name typ val

      "list_splat_pattern" -> withNamedNodeChildren node $ \children ->
        case children of
          (nP:_) -> do
            name <- withReadNode nP $ \nN -> nodeContents fileContents nN
            return $ PyStarParam (Just name)
          [] -> return $ PyStarParam Nothing

      "dictionary_splat_pattern" -> withNamedNodeChildren node $ \children ->
        case children of
          (nP:_) -> do
            name <- withReadNode nP $ \nN -> nodeContents fileContents nN
            return $ PyStarStarParam name
          [] -> return $ PyStarStarParam "<unknown>"

      -- '*' alone as keyword-only separator
      _ -> return $ PyStarParam Nothing

-- | Parse the parameters node of a function definition.
parsePyParams :: BS.ByteString -> Ptr Node -> IO [PyParam]
parsePyParams fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    withNamedNodeChildren node $ \children ->
      forM children $ parsePyParam fileContents

--------------------------------------------------------------------
-- Statement parsing
--------------------------------------------------------------------

-- | Parse a block of statements (the indented body).
parsePyBlock :: BS.ByteString -> Ptr Node -> IO [PyStatement]
parsePyBlock fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    withNamedNodeChildren node $ \children -> do
      stmts <- forM children $ parsePyStatement fileContents
      return $ concat stmts

-- | Parse a function definition node (already identified as function_definition).
parsePyFuncDef :: BS.ByteString -> Node -> IO PyStatement
parsePyFuncDef fileContents node = do
  withNamedChildNodeMap node $ \children -> do
    name <- case H.lookupDefault [] "identifier" children of
      ((nP, _):_) -> withReadNode nP $ \nN -> nodeContents fileContents nN
      []          -> return "<unknown>"
    params <- case H.lookupDefault [] "parameters" children of
      ((pP, _):_) -> parsePyParams fileContents pP
      []          -> return []
    retType <- case H.lookupDefault [] "type" children of
      ((tP, _):_) -> Just <$> parsePyExpr fileContents tP
      []          -> return Nothing
    body <- case H.lookupDefault [] "block" children of
      ((bP, _):_) -> parsePyBlock fileContents bP
      []          -> return []
    return $ PyFuncDef name params retType body

-- | Parse a class definition node.
parsePyClassDef :: BS.ByteString -> Node -> IO PyStatement
parsePyClassDef fileContents node = do
  withNamedChildNodeMap node $ \children -> do
    name <- case H.lookupDefault [] "identifier" children of
      ((nP, _):_) -> withReadNode nP $ \nN -> nodeContents fileContents nN
      []          -> return "<unknown>"
    bases <- case H.lookupDefault [] "argument_list" children of
      ((bP, _):_) -> withReadNode bP $ \bN ->
        withNamedNodeChildren bN $ \baseChildren ->
          forM baseChildren $ parsePyExpr fileContents
      []          -> return []
    body <- case H.lookupDefault [] "block" children of
      ((bodyP, _):_) -> parsePyBlock fileContents bodyP
      []             -> return []
    return $ PyClassDef name bases body

-- | Parse an import_statement node (import a.b [as c], ...).
parsePyImport :: BS.ByteString -> Node -> IO [PyStatement]
parsePyImport fileContents node = do
  withNamedNodeChildren node $ \children ->
    forM children $ \childP ->
      withReadNode childP $ \child -> do
        en <- easyNode child
        case eType en of
          "dotted_name" -> do
            modName <- parseDottedName fileContents childP
            return $ PyImport modName Nothing
          "aliased_import" -> withNamedChildNodeMap child $ \aliasChildren -> do
            modName <- case H.lookupDefault [] "dotted_name" aliasChildren of
              ((nP, _):_) -> parseDottedName fileContents nP
              []          -> return "<unknown>"
            alias <- case H.lookupDefault [] "identifier" aliasChildren of
              ((aP, _):_) -> withReadNode aP $ \aN -> Just <$> nodeContents fileContents aN
              []           -> return Nothing
            return $ PyImport modName alias
          _ -> return $ PyImport "<unknown>" Nothing

-- | Parse an import_from_statement node (from a.b import c, d).
parsePyImportFrom :: BS.ByteString -> Node -> IO PyStatement
parsePyImportFrom fileContents node = do
  withFieldChildMap node $ \fields -> do
    modName <- case H.lookup "module_name" fields of
      Just (nP:_) -> parseDottedName fileContents nP
      _           -> return "<unknown>"
    names <- case H.lookup "name" fields of
      Just namePtrs -> forM namePtrs $ \nP ->
        withReadNode nP $ \nN -> do
          en <- easyNode nN
          case eType en of
            "dotted_name"    -> parseDottedName fileContents nP
            "identifier"     -> nodeContents fileContents nN
            "aliased_import" -> withFieldChildMap nN $ \aliasFields ->
              case H.lookup "alias" aliasFields of
                Just (aP:_) -> withReadNode aP $ \aN -> nodeContents fileContents aN
                _ -> case H.lookup "name" aliasFields of
                  Just (oP:_) -> withReadNode oP $ \oN -> nodeContents fileContents oN
                  _           -> return "<unknown>"
            _ -> nodeContents fileContents nN
      _ -> return []
    -- Handle wildcard import
    let hasWildcard = H.member "wildcard_import" fields
    let names' = if hasWildcard then ["*"] else names
    return $ PyImportFrom modName names'

-- | Parse an assignment node (already identified as 'assignment').
parsePyAssignment :: BS.ByteString -> Node -> IO PyStatement
parsePyAssignment fileContents node = do
  withFieldChildMap node $ \fields -> do
    target <- case H.lookup "left" fields of
      Just (lP:_) -> parsePyExpr fileContents lP
      _           -> return $ PyVar "<unknown>"
    typ <- case H.lookup "type" fields of
      Just (tP:_) -> Just <$> parsePyExpr fileContents tP
      _           -> return Nothing
    val <- case H.lookup "right" fields of
      Just (rP:_) -> Just <$> parsePyExpr fileContents rP
      _           -> return Nothing
    return $ PyAssign target typ val

-- | Parse a single Python statement, returning a list (usually one element,
-- empty for skipped statements, multiple for expanded forms).
parsePyStatement :: BS.ByteString -> Ptr Node -> IO [PyStatement]
parsePyStatement fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    en <- easyNode node
    case eType en of
      "function_definition" ->
        (:[]) <$> parsePyFuncDef fileContents node

      "async_function_definition" ->
        (:[]) <$> parsePyFuncDef fileContents node

      "class_definition" ->
        (:[]) <$> parsePyClassDef fileContents node

      "import_statement" ->
        parsePyImport fileContents node

      "import_from_statement" ->
        (:[]) <$> parsePyImportFrom fileContents node

      "expression_statement" ->
        withNamedNodeChildren node $ \children ->
          case children of
            (childP:_) ->
              withReadNode childP $ \child -> do
                childEn <- easyNode child
                case eType childEn of
                  "assignment" -> (:[]) <$> parsePyAssignment fileContents child
                  "augmented_assignment" -> return []
                  _ -> (:[]) . PyExprStmt <$> parsePyExpr fileContents childP
            [] -> return []

      "decorated_definition" ->
        withNamedChildNodeMap node $ \children -> do
          decorators <- forM (H.lookupDefault [] "decorator" children) $ \(dP, _) ->
            withReadNode dP $ \dN ->
              withNamedNodeChildren dN $ \dChildren ->
                case dChildren of
                  (eP:_) -> parsePyExpr fileContents eP
                  []     -> return $ PyVar "<unknown>"
          let defLookup = concatMap (\t -> H.lookupDefault [] t children)
                            ["function_definition", "async_function_definition", "class_definition"]
          defStmts <- case defLookup of
            ((defP, _):_) -> parsePyStatement fileContents defP
            []            -> return []
          return $ case defStmts of
            [s] -> [PyDecorated decorators s]
            _   -> [PyDecorated decorators PyPass]

      "return_statement" ->
        withNamedNodeChildren node $ \children ->
          case children of
            (eP:_) -> (:[]) . PyReturn . Just <$> parsePyExpr fileContents eP
            []     -> return [PyReturn Nothing]

      "pass_statement"     -> return [PyPass]
      "comment"            -> return []
      "if_statement"       -> return []
      "for_statement"      -> return []
      "while_statement"    -> return []
      "with_statement"     -> return []
      "try_statement"      -> return []
      "raise_statement"    -> return []
      "assert_statement"   -> return []
      "delete_statement"   -> return []
      "global_statement"   -> return []
      "nonlocal_statement" -> return []
      "break_statement"    -> return []
      "continue_statement" -> return []
      _                    -> return []

-- | Parse the root module node.
parsePyModuleNode :: BS.ByteString -> Ptr Node -> IO PyModule
parsePyModuleNode fileContents nodeP = do
  withReadNode nodeP $ \node -> do
    withNamedNodeChildren node $ \children -> do
      stmts <- forM children $ parsePyStatement fileContents
      return $ PyModule (concat stmts)

-- | Parse a Python source file using TreeSitter.
parsePython :: BS.ByteString -> IO PyModule
parsePython fileContents = do
  parser <- ts_parser_new
  _ <- ts_parser_set_language parser tree_sitter_python
  withParseTree parser fileContents $ \tree -> do
    withRootNode tree $ \nodeP -> do
      parsePyModuleNode fileContents nodeP

--------------------------------------------------------------------
-- Conversion from PyModule to RawPrgm
--------------------------------------------------------------------

-- | Convert a Python expression to a Catln RawExpr.
-- | Map Python built-in type names to their Catln equivalents.
mapPyBuiltinType :: String -> String
mapPyBuiltinType "int"   = "Integer"
mapPyBuiltinType "float" = "Float"
mapPyBuiltinType "str"   = "String"
mapPyBuiltinType "bool"  = "Boolean"
mapPyBuiltinType "None"  = "IO"
mapPyBuiltinType "list"  = "List"
mapPyBuiltinType "dict"  = "Dict"
mapPyBuiltinType "set"   = "Set"
mapPyBuiltinType "tuple" = "Tuple"
mapPyBuiltinType name    = name

-- | Convert a Python expression that appears in type-annotation position.
-- Applies 'mapPyBuiltinType' so that e.g. @int@ becomes @Integer@.
convertPyTypeExpr :: PyExpr -> RawExpr ()
convertPyTypeExpr (PyVar name) = rawVal (mapPyBuiltinType name)
convertPyTypeExpr PyNone       = rawVal "IO"   -- None in annotation position maps to IO
convertPyTypeExpr e            = convertPyExpr e

convertPyExpr :: PyExpr -> RawExpr ()
convertPyExpr (PyVar name)        = rawVal name
convertPyExpr (PyInt i)           = RawCExpr emptyMetaN (CInt i)
convertPyExpr (PyFloat f)         = RawCExpr emptyMetaN (CFloat f)
convertPyExpr (PyLit s)           = rawStr s
convertPyExpr (PyBool True)       = rawVal "True"
convertPyExpr (PyBool False)      = rawVal "False"
convertPyExpr PyNone              = rawVal "None"
convertPyExpr (PyList elems)      = RawList emptyMetaN (map convertPyExpr elems)
convertPyExpr (PyTuple elems)     =
  rawAnon `applyRawArgs` map ((Nothing,) . convertPyExpr) elems
convertPyExpr (PyAttr obj attr)   =
  RawMethod emptyMetaN (convertPyExpr obj) (rawVal attr)
convertPyExpr (PySubscript b idx) =
  convertPyExpr b `applyRawArgs` [(Nothing, convertPyExpr idx)]
convertPyExpr (PyCall (PyVar name) args)
  | Just converted <- convertPyBuiltinCall name args = converted
convertPyExpr (PyCall func args)  =
  convertPyExpr func `applyRawArgs` map convertPyCallArg args
convertPyExpr (PyBinOp op l r)    =
  rawVal (operatorName op)
    `applyRawArgs` [ (Just $ partialKey operatorArgL, convertPyExpr l)
                   , (Just $ partialKey operatorArgR, convertPyExpr r)
                   ]
convertPyExpr (PyUnOp op e)       =
  rawVal (operatorName op)
    `applyRawArgs` [(Just $ partialKey operatorArgUnary, convertPyExpr e)]

-- | Translate calls to Python built-in functions to Catln equivalents.
-- Returns Nothing for unknown names so the caller falls through to generic
-- function-call conversion.
convertPyBuiltinCall :: String -> [PyCallArg] -> Maybe (RawExpr ())
-- abs(x) -> x.abs   [dispatches to :Integer.abs or :Float.abs]
convertPyBuiltinCall "abs" [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "abs")
-- pow(base, exp) -> base.pow(exp=exp)   [dispatches to :Integer.pow or :Float.pow]
convertPyBuiltinCall "pow" [PyPosArg base, PyPosArg expE] =
  Just $ RawMethod emptyMetaN (convertPyExpr base) (rawVal "pow")
    `applyRawArgs` [(Just $ partialKey "exp", convertPyExpr expE)]
-- round(x) -> x.round, floor(x) -> x.floor, ceil(x) -> x.ceil
convertPyBuiltinCall "round" [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "round")
convertPyBuiltinCall "floor" [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "floor")
convertPyBuiltinCall "ceil" [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "ceil")
-- len(x) -> x.length  (works for String; ConsList via :Nil.length / :Cons.length)
convertPyBuiltinCall "len" [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "length")
-- Type conversions: int(x)->x.toInt, float(x)->x.toFloat, str(x)->x.toString, bool(x)->x.toBool
convertPyBuiltinCall "int"   [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "toInt")
convertPyBuiltinCall "float" [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "toFloat")
convertPyBuiltinCall "str"   [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "toString")
convertPyBuiltinCall "bool"  [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "toBool")
-- chr(n) -> chr(n=n),  ord(c) -> c.ord
convertPyBuiltinCall "chr"   [PyPosArg x] =
  Just $ rawVal "chr"
    `applyRawArgs` [(Just $ partialKey "n", convertPyExpr x)]
convertPyBuiltinCall "ord"   [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "ord")
-- hex(n) -> n.hex, oct(n) -> n.oct, bin(n) -> n.bin
convertPyBuiltinCall "hex"   [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "hex")
convertPyBuiltinCall "oct"   [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "oct")
convertPyBuiltinCall "bin"   [PyPosArg x] =
  Just $ RawMethod emptyMetaN (convertPyExpr x) (rawVal "bin")
-- input() -> io.input  (reads a line from stdin via the IO context)
convertPyBuiltinCall "input" [] =
  Just $ RawMethod emptyMetaN (rawVal "io") (rawVal "input")
-- print(*args) -> io.println(msg=arg.toString) for single-arg case
convertPyBuiltinCall "print" [PyPosArg x] =
  Just $ RawMethod emptyMetaN (rawVal "io") (rawVal "println")
    `applyRawArgs` [(Just $ partialKey "msg",
                     RawMethod emptyMetaN (convertPyExpr x) (rawVal "toString"))]
-- sum(lst) -> sum(lst=lst)
convertPyBuiltinCall "sum" [PyPosArg x] =
  Just $ rawVal "sum"
    `applyRawArgs` [(Just $ partialKey "lst", convertPyExpr x)]
-- all(lst) -> all(lst=lst), any(lst) -> any(lst=lst)
convertPyBuiltinCall "all" [PyPosArg x] =
  Just $ rawVal "all"
    `applyRawArgs` [(Just $ partialKey "lst", convertPyExpr x)]
convertPyBuiltinCall "any" [PyPosArg x] =
  Just $ rawVal "any"
    `applyRawArgs` [(Just $ partialKey "lst", convertPyExpr x)]
-- range(n) -> range(stop=n)
convertPyBuiltinCall "range" [PyPosArg n] =
  Just $ rawVal "range"
    `applyRawArgs` [(Just $ partialKey "stop", convertPyExpr n)]
-- reversed(lst) -> reversed(lst=lst), sorted(lst) -> sorted(lst=lst)
convertPyBuiltinCall "reversed" [PyPosArg x] =
  Just $ rawVal "reversed"
    `applyRawArgs` [(Just $ partialKey "lst", convertPyExpr x)]
convertPyBuiltinCall "sorted" [PyPosArg x] =
  Just $ rawVal "sorted"
    `applyRawArgs` [(Just $ partialKey "lst", convertPyExpr x)]
-- max(a, b) -> max(l=a, r=b), min(a, b) -> min(l=a, r=b)
convertPyBuiltinCall "max" [PyPosArg a, PyPosArg b] =
  Just $ rawVal "max"
    `applyRawArgs` [ (Just $ partialKey operatorArgL, convertPyExpr a)
                   , (Just $ partialKey operatorArgR, convertPyExpr b)
                   ]
convertPyBuiltinCall "min" [PyPosArg a, PyPosArg b] =
  Just $ rawVal "min"
    `applyRawArgs` [ (Just $ partialKey operatorArgL, convertPyExpr a)
                   , (Just $ partialKey operatorArgR, convertPyExpr b)
                   ]
convertPyBuiltinCall _ _ = Nothing

convertPyCallArg :: PyCallArg -> (Maybe ArgName, RawExpr ())
convertPyCallArg (PyPosArg e)     = (Nothing, convertPyExpr e)
convertPyCallArg (PyKwArg name e) = (Just $ partialKey name, convertPyExpr e)

-- | Convert a Python parameter to a 'RawObjArr' for use in a function
-- declaration head.  Typed parameters produce a type-annotation ObjArr
-- (@roaArr = Just (Nothing, Just typeExpr)@); untyped ones carry no annotation.
convertPyParam :: PyParam -> RawObjArr RawExpr ()
convertPyParam (PySimpleParam name Nothing) = RawObjArr
  { roaObj = Just (rawVal name), roaBasis = ArgObj, roaDoc = Nothing
  , roaAnnots = [], roaArr = Nothing, roaDef = Nothing }
convertPyParam (PySimpleParam name (Just typ)) = RawObjArr
  { roaObj = Just (rawVal name), roaBasis = ArgObj, roaDoc = Nothing
  , roaAnnots = [], roaArr = Just (Nothing, Just (convertPyTypeExpr typ))
  , roaDef = Nothing }
convertPyParam (PyDefaultParam name Nothing _) = RawObjArr
  { roaObj = Just (rawVal name), roaBasis = ArgObj, roaDoc = Nothing
  , roaAnnots = [], roaArr = Nothing, roaDef = Nothing }
convertPyParam (PyDefaultParam name (Just typ) _) = RawObjArr
  { roaObj = Just (rawVal name), roaBasis = ArgObj, roaDoc = Nothing
  , roaAnnots = [], roaArr = Just (Nothing, Just (convertPyTypeExpr typ))
  , roaDef = Nothing }
convertPyParam (PyStarParam _) = RawObjArr
  { roaObj = Nothing, roaBasis = ArgObj, roaDoc = Nothing
  , roaAnnots = [], roaArr = Nothing, roaDef = Nothing }
convertPyParam (PyStarStarParam _) = RawObjArr
  { roaObj = Nothing, roaBasis = ArgObj, roaDoc = Nothing
  , roaAnnots = [], roaArr = Nothing, roaDef = Nothing }

-- | Apply a list of typed parameter ObjArrs to a base expression using
-- 'RawTupleApply', which correctly represents typed function parameters.
applyTypedParams :: RawExpr () -> [RawObjArr RawExpr ()] -> RawExpr ()
applyTypedParams base [] = base
applyTypedParams base oas =
  RawTupleApply emptyMetaN (emptyMetaN, base) (map (False,) oas)

-- | True for parameters whose type annotation maps to 'IO' — these become
-- Catln context parameters (passed implicitly via RawContextApply).
isContextParam :: PyParam -> Bool
isContextParam (PySimpleParam    _ (Just (PyVar t)))    = mapPyBuiltinType t == "IO"
isContextParam (PyDefaultParam   _ (Just (PyVar t)) _)  = mapPyBuiltinType t == "IO"
isContextParam _                                         = False

-- | Build sub-statements for a function body using the nestedDeclaration approach.
-- IO effect statements (print calls) become RawBindStatements threading 'io'.
-- Local variable assignments become RawDeclStatements.
-- The return expression (or final 'io' for IO functions) becomes a bare expression sub-statement.
buildFuncSubStmts :: Bool -> [PyStatement] -> [RawStatementTree RawExpr ()]
buildFuncSubStmts isIO stmts = localDecls ++ ioBinds ++ [finalStmt]
  where
    -- Local variable declarations (PyAssign with type annotation)
    localDecls = mapMaybe toLocalDecl stmts
    toLocalDecl s = case s of
      PyAssign _ (Just _) _ -> convertPyStatementToTree s
      PyAssign _ Nothing  _ -> convertPyStatementToTree s
      _                     -> Nothing
    -- IO-effect bind statements: print(x) -> io <- io.println(/msg= x.toString)
    ioBinds
      | isIO = mapMaybe toIOBind stmts
      | otherwise = []
    toIOBind (PyExprStmt (PyCall (PyVar "print") [PyPosArg x])) =
      let printExpr = RawMethod emptyMetaN (rawVal "io") (rawVal "println")
                        `applyRawArgs` [(Just $ partialKey "msg",
                                         RawMethod emptyMetaN (convertPyExpr x) (rawVal "toString"))]
          bindDecl = RawObjArr
            { roaObj    = Just (rawVal "io")
            , roaBasis  = FunctionObj
            , roaDoc    = Nothing
            , roaAnnots = []
            , roaArr    = Just (Just printExpr, Nothing)
            , roaDef    = Nothing
            }
      in Just $ RawStatementTree (RawBindStatement bindDecl) []
    toIOBind _ = Nothing
    -- Final sub-statement: return expression, or 'io' for IO functions, or anonymous
    returnExprs = [convertPyExpr e | PyReturn (Just e) <- stmts]
    finalExpr = case returnExprs of
      (e:_) -> e
      []    -> if isIO then rawVal "io" else rawAnon
    finalStmt = RawStatementTree
      (RawDeclStatement RawObjArr
        { roaObj    = Just finalExpr
        , roaBasis  = FunctionObj
        , roaDoc    = Nothing
        , roaAnnots = []
        , roaArr    = Nothing
        , roaDef    = Nothing
        }) []

-- | Convert a Python import to a RawFileImport for a "python" parser.
-- | Convert a Python statement to a RawStatementTree.
-- Returns Nothing for statements that produce no Catln output
-- (imports, pass, bare expressions, return outside functions, etc.).
convertPyStatementToTree :: PyStatement -> Maybe (RawStatementTree RawExpr ())
convertPyStatementToTree stmt = case stmt of

  PyFuncDef name params retType body ->
    let (ctxParams0, regParams) = partition isContextParam params
        -- If the return type maps to IO but there are no explicit IO context
        -- params, synthesise one: `def f() -> None: pass` becomes
        -- `f{io -> IO} = nestedDeclaration / io`.  This allows authentic
        -- Python signatures like `def main() -> None: pass` to round-trip
        -- through the full pipeline.
        retTypeIsIO = case retType of
          Just (PyVar t) -> mapPyBuiltinType t == "IO"
          Just PyNone    -> True
          _              -> False
        ctxParams
          | null ctxParams0 && retTypeIsIO = [PySimpleParam "io" (Just (PyVar "IO"))]
          | otherwise                      = ctxParams0
        isIO = not (null ctxParams)
        -- Build sub-statements for the body using nestedDeclaration
        subStmts = buildFuncSubStmts isIO body
        -- Use nestedDeclaration when the body has multiple statements, or when
        -- there is exactly one statement that is not a simple return (e.g. IO
        -- functions with only a final 'io').  For a single return with no local
        -- declarations, emit the expression directly to keep output readable.
        hasLocalOrIO = any isLocalOrIO body
        isLocalOrIO (PyAssign {})  = True
        isLocalOrIO (PyExprStmt _) = True
        isLocalOrIO _              = False
        hasReturn = any (\s -> case s of PyReturn (Just _) -> True; _ -> False) body
        hasLocalDecls = any (\s -> case s of PyAssign {} -> True; _ -> False) body
        -- IO effect statements for direct chaining (print calls only)
        ioEffects = [e | PyExprStmt e <- body]
        -- Chain a single IO-effect expression onto an accumulated IO value.
        chainIOEffect acc (PyCall (PyVar "print") [PyPosArg x]) =
          RawMethod emptyMetaN acc (rawVal "println")
            `applyRawArgs` [(Just $ partialKey "msg",
                             RawMethod emptyMetaN (convertPyExpr x) (rawVal "toString"))]
        chainIOEffect acc _ = acc
        (bodyExpr, bodySubStmts)
          -- IO functions with only print calls (no local variable declarations) use
          -- direct IO chaining to avoid intermediate lambdas that fail BoundedByObjs9.
          | isIO && not hasLocalDecls && not hasReturn =
              (Just (foldl chainIOEffect (rawVal "io") ioEffects), [])
          | hasLocalOrIO || (isIO && not hasReturn) =
              (Just (rawVal nestedDeclaration), subStmts)
          | hasReturn =
              (fmap convertPyExpr (listToMaybe [e | PyReturn (Just e) <- body]), [])
          | otherwise = (Nothing, [])
        baseHead   = applyTypedParams (rawVal name) (map convertPyParam regParams)
        funcHead   = case ctxParams of
          []  -> baseHead
          cps -> RawContextApply emptyMetaN (emptyMetaN, baseHead) (map convertPyParam cps)
        -- Context-parameterized functions (ContextIn) must not carry an
        -- explicit return-type annotation — the type is inferred from the
        -- context.  Plain functions keep the Python return-type annotation.
        retTypeExpr = if null ctxParams then fmap convertPyTypeExpr retType else Nothing
        decl = RawObjArr
          { roaObj    = Just funcHead
          , roaBasis  = FunctionObj
          , roaDoc    = Nothing
          , roaAnnots = []
          , roaArr    = Just (bodyExpr, retTypeExpr)
          , roaDef    = Nothing
          }
    in Just $ RawStatementTree (RawDeclStatement decl) bodySubStmts

  PyClassDef name bases body ->
    let baseExprs   = map convertPyExpr bases
        classDecl   = classDeclSt (rawVal name) baseExprs
        methodDecls = mapMaybe convertPyStatementToTree body
    in Just $ RawStatementTree classDecl methodDecls

  PyAssign target Nothing (Just val) ->
    let decl = RawObjArr
          { roaObj    = Just (convertPyExpr target)
          , roaBasis  = FunctionObj
          , roaDoc    = Nothing
          , roaAnnots = []
          , roaArr    = Just (Just (convertPyExpr val), Nothing)
          , roaDef    = Nothing
          }
    in Just $ RawStatementTree (RawDeclStatement decl) []

  PyAssign target (Just typ) mVal ->
    let decl = RawObjArr
          { roaObj    = Just (convertPyExpr target)
          , roaBasis  = FunctionObj
          , roaDoc    = Nothing
          , roaAnnots = []
          , roaArr    = Just (fmap convertPyExpr mVal, Just (convertPyTypeExpr typ))
          , roaDef    = Nothing
          }
    in Just $ RawStatementTree (RawDeclStatement decl) []

  PyDecorated _decorators innerStmt ->
    -- Decorators are stripped; only the inner definition is converted
    convertPyStatementToTree innerStmt

  -- These are handled separately as file imports
  PyImport _ _     -> Nothing
  PyImportFrom _ _ -> Nothing
  -- These produce no declarations
  PyReturn _       -> Nothing
  PyExprStmt _     -> Nothing
  PyPass           -> Nothing
  PyAssign _ Nothing Nothing -> Nothing

-- | Convert a full PyModule to a RawPrgm.
-- Python imports (import X, from X import Y) are dropped — they are Python
-- module-system constructs and do not correspond to Catln file imports.
convertPyModule :: PyModule -> RawPrgm ()
convertPyModule (PyModule stmts) =
  RawPrgm [mkRawFileImport (rawStr "python/builtins.ct")] statements
  where
    statements = mapMaybe convertPyStatementToTree (filter (not . isImport) stmts)

    isImport (PyImport _ _)     = True
    isImport (PyImportFrom _ _) = True
    isImport _                  = False

--------------------------------------------------------------------
-- Top-level ImportParser
--------------------------------------------------------------------

-- | The Python import parser, registered under the name "python".
pyParser :: ImportParser
pyParser imp = case [arg | arg@ObjArr{oaObj=Nothing} <- exprAppliedArgs imp] of
  (ObjArr{oaArr=Just (Just (CExpr _ (CStr filename)), _)}:_) -> do
    fileContents <- BS.readFile filename
    src <- readFile filename
    pyMod <- parsePython fileContents
    let prgm = convertPyModule pyMod
    return (prgm, [], Just src)
  _ -> undefined
