--------------------------------------------------------------------
-- |
-- Module    :  Syntax.Python.Prgm
-- Copyright :  (c) Zach Kimberg 2024
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines a Python program syntax as an intermediate
-- representation for the Python TreeSitter parser.
--------------------------------------------------------------------

module Syntax.Python.Prgm where

type PyName = String

-- | A Python expression
data PyExpr
  = PyLit    String               -- ^ A literal stored as raw source text
  | PyVar    PyName               -- ^ An identifier / variable reference
  | PyCall   PyExpr [PyCallArg]   -- ^ A function call
  | PyBinOp  String PyExpr PyExpr -- ^ A binary operator
  | PyUnOp   String PyExpr        -- ^ A unary operator
  | PyAttr   PyExpr PyName        -- ^ Attribute access (obj.attr)
  | PyList   [PyExpr]             -- ^ A list literal
  | PyTuple  [PyExpr]             -- ^ A tuple literal
  | PyNone                        -- ^ None literal
  | PyBool   Bool                 -- ^ True or False
  | PySubscript PyExpr PyExpr     -- ^ Subscript (a[b])
  deriving (Eq, Ord, Show)

-- | An argument in a function call
data PyCallArg
  = PyPosArg PyExpr              -- ^ Positional argument
  | PyKwArg  PyName PyExpr       -- ^ Keyword argument (name=value)
  deriving (Eq, Ord, Show)

-- | A parameter in a function definition
data PyParam
  = PySimpleParam   PyName (Maybe PyExpr)          -- ^ name or name: type
  | PyDefaultParam  PyName (Maybe PyExpr) PyExpr   -- ^ name[: type] = default
  | PyStarParam     (Maybe PyName)                 -- ^ * or *name
  | PyStarStarParam PyName                         -- ^ **name
  deriving (Eq, Ord, Show)

-- | A Python statement
data PyStatement
  = PyFuncDef  PyName [PyParam] (Maybe PyExpr) [PyStatement]
    -- ^ def name(params) [-> retType]: body
  | PyClassDef PyName [PyExpr] [PyStatement]
    -- ^ class name[(bases)]: body
  | PyAssign   PyExpr (Maybe PyExpr) (Maybe PyExpr)
    -- ^ target [: type] [= value]
  | PyReturn   (Maybe PyExpr)
    -- ^ return [expr]
  | PyImport    PyName (Maybe PyName)
    -- ^ import dotted.name [as alias]
  | PyImportFrom PyName [PyName]
    -- ^ from dotted.name import name1, name2, ...
  | PyExprStmt  PyExpr
    -- ^ Bare expression statement
  | PyDecorated [PyExpr] PyStatement
    -- ^ @decorator(s) def/class ...
  | PyPass
    -- ^ pass
  deriving (Eq, Ord, Show)

-- | A top-level Python module
newtype PyModule = PyModule [PyStatement]
  deriving (Eq, Ord, Show)
