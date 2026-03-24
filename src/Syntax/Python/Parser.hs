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
import           Data.Maybe               (isNothing, listToMaybe, mapMaybe)
import           Foreign                  (Ptr)
import           Semantics.Prgm
import           Semantics.Types
import           Syntax.Common.TreeSitter
import           Syntax.Ct.Builder
import           Syntax.Ct.Prgm
import           Syntax.Python.Prgm
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
      "integer"             -> PyLit <$> nodeContents fileContents node
      "float"               -> PyLit <$> nodeContents fileContents node
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
mapPyBuiltinType name    = name

-- | Convert a Python expression that appears in type-annotation position.
-- Applies 'mapPyBuiltinType' so that e.g. @int@ becomes @Integer@.
convertPyTypeExpr :: PyExpr -> RawExpr ()
convertPyTypeExpr (PyVar name) = rawVal (mapPyBuiltinType name)
convertPyTypeExpr PyNone       = rawVal "IO"   -- None in annotation position maps to IO
convertPyTypeExpr e            = convertPyExpr e

convertPyExpr :: PyExpr -> RawExpr ()
convertPyExpr (PyVar name)        = rawVal name
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

-- | Separate the return expression and local declarations from a function body.
-- The last return statement becomes the function's result expression; all other
-- statements become nested declarations in the RawStatementTree.
convertPyBody :: [PyStatement] -> (Maybe (RawExpr ()), [RawStatementTree RawExpr ()])
convertPyBody stmts = (returnExpr, localDecls)
  where
    returnExpr = listToMaybe [convertPyExpr e | PyReturn (Just e) <- stmts]
    localDecls = mapMaybe convertPyStatementToTree (filter (not . isReturn) stmts)
    isReturn (PyReturn _) = True
    isReturn _            = False

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
        -- `f{io -> IO} = io`.  This allows authentic Python signatures like
        -- `def main() -> None: pass` to round-trip through the full pipeline.
        retTypeIsIO = case retType of
          Just (PyVar t) -> mapPyBuiltinType t == "IO"
          Just PyNone    -> True
          _              -> False
        (bodyExpr0, bodyDecls) = convertPyBody body
        (ctxParams, bodyExpr)
          | null ctxParams0 && retTypeIsIO && isNothing bodyExpr0 =
              ([PySimpleParam "io" (Just (PyVar "IO"))], Just (rawVal "io"))
          | otherwise = (ctxParams0, bodyExpr0)
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
    in Just $ RawStatementTree (RawDeclStatement decl) bodyDecls

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
  RawPrgm [] statements
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
