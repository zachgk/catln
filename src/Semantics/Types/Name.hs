-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types.Name
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- The 'Name' type that names every type, class and argument, and the helpers that read
-- names as absolute or relative paths.
--------------------------------------------------------------------

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Semantics.Types.Name where

import           CtConstants
import           Data.Aeson
import           Data.Aeson.Types      (contramapToJSONKeyFunction)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.UTF8  as UTF8
import           Data.Hashable
import           Data.List             (intercalate, partition)
import qualified Data.List             as L
import           Data.List.Split       (splitOn)
import           Data.String           (IsString (..))
import           Data.Word             (Word8)
import           GHC.Generics
import           Semantics.CtType
import           Semantics.Val         (pattern AnyStrVal, pattern StrVal)
import           Text.Printf


-- |The name is the basic type used for various kinds of names.
-- It carries a precomputed hash so 'Hashable' is O(1): names key the 'PartialLeafs' maps the
-- set algebra rebuilds constantly, and hashing them character by character would dominate
-- compile time. 'nameHash' is exactly @hash nameStr@, so a name is placed in a 'Data.HashMap'
-- exactly where its string would be, and iteration order (hence printed output) is determined
-- by the string alone.
--
-- It also carries its UTF-8 bytes packed ('nameBytes'), which is what names are compared by. Names
-- key every map the type algebra reads and are compared on every lookup that finds its key — an
-- equal pair agrees on its hash, so comparing the 'String's walked both cons lists to the end. The
-- packed bytes compare in one memcmp, and order the same way, as UTF-8 preserves code point order.
data Name = Name { nameHash :: !Int, nameBytes :: !SBS.ShortByteString, nameStr :: String }

-- | The canonical way to build a 'Name': computes and caches the hash and the packed bytes once.
mkName :: String -> Name
mkName s = Name (hash s) (SBS.toShort (UTF8.fromString s)) s

-- | A name is its string, read as a Catln one.
--
-- Written by hand rather than read off the shape. A 'Name' is a string with a cache of its hash,
-- and 'nameHash' is exactly @hash nameStr@. Derived structurally, the page would declare the hash
-- as an argument, a program could be handed it, and 'fromCt' would rebuild a 'Name' whose hash and
-- string disagree. The derivation cannot see an invariant like that; 'mkName' is where it lives.
--
-- So the type a program is handed is @\/Data\/String@ and nothing else. Nothing is lost: the string
-- is the whole of what a name is, and the hash is recomputed on the way back.
instance CtType Name where
  ctPath _ = strPrim
  ctImports _ = [coreStrPage]
  ctPageInto = ctPageImport
  toCt = StrVal . nameStr
  fromCt (AnyStrVal s) = Right $ mkName s
  fromCt v             = Left $ printf "%s is not a %s" (show v) strPrim

instance IsString Name where
  fromString = mkName

instance Hashable Name where
  hash = nameHash
  hashWithSalt s n = hashWithSalt s (nameHash n)

instance Eq Name where
  a == b = nameHash a == nameHash b && nameBytes a == nameBytes b

instance Ord Name where
  compare a b = compare (nameBytes a) (nameBytes b)

instance Show Name where
  show = show . nameStr

instance ToJSON Name where
  toJSON = toJSON . nameStr
  toEncoding = toEncoding . nameStr

instance ToJSONKey Name where
  toJSONKey = contramapToJSONKeyFunction nameStr toJSONKey

-- | A NPath (name path) is used for names with namespaces, either absolutely referenced to with only part of it
data NPath = NPath {
  npathIsAbs :: Bool,
  npathPath  :: [String]
                   }
  deriving (Eq, Ord, Hashable, Generic, ToJSON)

type TypeName = Name

type ClassName = Name

type TypePropName = Name

type RelativeName = Name

type DocComment = String

makeAbsoluteName :: Name -> Name
makeAbsoluteName n
  | SBS.null (nameBytes n) || nameIsAbsolute n = n
  | otherwise = mkName ('/' : nameStr n)

-- | Whether a name is spelled from the root: it begins with the separator.
nameIsAbsolute :: Name -> Bool
nameIsAbsolute n = not (SBS.null b) && SBS.index b 0 == slashByte
  where b = nameBytes n

slashByte :: Word8
slashByte = 47

instance Show NPath where
  show (NPath a n) = (if a then "/" else "") ++ intercalate "/" n

getPath :: String -> NPath
getPath name = case splitOn "/" name of
  ("":n) -> NPath True n
  n      -> NPath False n

relPathAddPrefix :: String -> NPath -> NPath
relPathAddPrefix _ p@NPath{npathIsAbs=True}      = p
relPathAddPrefix "" n = n
relPathAddPrefix prefix (NPath False n) = NPath False (splitOn "/" prefix ++ n)

matchesNPath :: NPath -> NPath -> Bool
matchesNPath (NPath True rn) (NPath True n) = rn == n
matchesNPath (NPath False rn) (NPath _ n)   = rn `L.isSuffixOf` n
matchesNPath (NPath True _) (NPath False _) = False

-- | Whether a name written relative to some scope names this one: a relative name matches an
-- absolute name it is a path suffix of, broken at a path separator.
--
-- The anonymous type's name is the separator and nothing else, so it has no components to be
-- matched by and names only itself. Otherwise it would be a suffix of every name whose last
-- component is empty — integer division is written @//@, so 'CtConstants.operatorName' makes it
-- @/operator//@ — and the unit type would name both itself and integer division.
relativeNameMatches :: RelativeName -> Name -> Bool
relativeNameMatches rn n = rn == n || (not (SBS.null r) && r /= SBS.singleton slashByte && suffixAtSeparator)
  where
    r = nameBytes rn
    b = nameBytes n
    lr = SBS.length r
    lb = SBS.length b
    -- @'/' : rn@ is a suffix of @'/' : n@: @rn@ is @n@ itself, or a suffix of it preceded by a '/'.
    suffixAtSeparator
      | lr == lb  = r == b
      | lr < lb   = SBS.index b (lb - lr - 1) == slashByte && SBS.drop (lb - lr) b == r
      | otherwise = False

relativeNameFilter :: RelativeName -> [Name] -> [Name]
relativeNameFilter rn = filter (relativeNameMatches rn)

-- | Takes a list of relative and abs names, and prunes the relative names describing an absolute one
relAbsNamePrune :: [TypeName] -> [TypeName]
relAbsNamePrune names = map (mkName . show) (absNames ++ filter matchesNoAbsName relNames)
  where
    names' = map (getPath . nameStr) names
    (absNames, relNames) = partition npathIsAbs names'
    matchesNoAbsName n = not $ any (matchesNPath n) absNames
