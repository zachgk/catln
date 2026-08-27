-- | Tests for a Haskell type read as a Catln one.
--
-- A derived mapping is three answers that have to agree: what the builtins page declares, what an
-- evaluation is handed, and what is read back out of one. So the cases below are not three separate
-- checks of three functions — they are checks that the three say the same thing. What a value
-- converts to is asserted against the argument keys the DECLARATION would be read with, and what
-- the declaration says is asserted as the Catln it formats to, which is the text someone comparing
-- it against a @.ct@ file would read.
--
-- Over types written here rather than over 'Syntax.HsBuiltins.Types.BuiltFile' alone, because the
-- shapes worth covering — a sum, a nullary arm, a record inside a record, a type that reaches
-- itself — are not shapes the builtins page has any reason to carry. 'BuiltFile' is covered too,
-- since it is the one whose declaration the standard library actually typechecks and the one a
-- build is actually read back through.
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syntax.HsBuiltinsTests where

import qualified Data.HashMap.Strict      as H
import           Data.List                (isInfixOf)
import           Data.Proxy               (Proxy (..))
import           Eval.Common              (Val (..), valListOf)
import           GHC.Generics             (Generic)
import           Syntax.Ct.Formatter      (formatRootPrgm)
import           Syntax.Ct.Prgm           (RawPrgm (..))
import           Syntax.HsBuiltins.Derive
import           Syntax.HsBuiltins.Parser (hsBuiltinsImports)
import           Syntax.HsBuiltins.Types  (BuiltFile (..), hsBuiltinPage,
                                           oneFileResult, valResultFiles)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | A record inside a record, so that a walk reaching a type through a field is covered.
data Inner = Inner { innerLabel :: String }
  deriving (Eq, Show, Generic)
instance CtType Inner

data Outer = Outer { outerInner :: Inner, outerCount :: Integer }
  deriving (Eq, Show, Generic)
instance CtType Outer

-- | A sum, with one arm carrying fields and one carrying none.
data Shape
  = Circle { circleRadius :: Integer }
  | Point
  deriving (Eq, Show, Generic)
instance CtType Shape

-- | A type that reaches itself, which a walk marking what it has seen has to stop at.
data Chain = Chain { chainHead :: String, chainRest :: [Chain] }
  deriving (Eq, Show, Generic)
instance CtType Chain

-- | A field written with no prefix to drop, and one that is nothing but prefix.
data Plain = Plain { radius :: Integer, tags :: [String] }
  deriving (Eq, Show, Generic)
instance CtType Plain

-- | The Catln a type's declaration formats to, which is the text it would have been written as.
declText :: (CtType a) => Proxy a -> String
declText p = formatRootPrgm (RawPrgm [] (cpDecls $ ctPageOf p))

-- | A value converted and read back, which must be the value it started as.
roundTrip :: (CtType a, Eq a, Show a) => String -> a -> TestTree
roundTrip name x = testCase name $ fromCt (toCt x) @?= Right x

aFile :: BuiltFile
aFile = BuiltFile "site/index.html" "<h1>hi</h1>"

hsBuiltinsTests :: TestTree
hsBuiltinsTests = testGroup "HsBuiltinsTests" [
  testGroup "a Haskell type declared as a Catln one" [
      testCase "as one data statement, with each field under the name its prefix was dropped from" $
        head (lines (declText (Proxy :: Proxy BuiltFile)))
          @?= "data(/Catln/BuildFile(path -> /Data/String, contents -> /Data/String))"

    , testCase "at the Catln path the instance names, which is not the Haskell one" $
        ctPath (Proxy :: Proxy BuiltFile) @?= "/Catln/BuildFile"

    , testCase "with a field written without a prefix left as it was" $
        declText (Proxy :: Proxy Plain) @?=
          "data(/Catln/Plain(radius -> /Data/Primitive/Integer, tags -> /Data/List[$T -> /Data/String]))\n"

    , testCase "as one class statement where the type has several constructors" $
        declText (Proxy :: Proxy Shape) @?=
          "class(/Catln/Shape, [/Catln/Shape/Circle(radius -> /Data/Primitive/Integer), /Catln/Shape/Point])\n"

    , testCase "declaring the type a field is, beneath the type that holds it" $
        declText (Proxy :: Proxy Outer) @?= unlines [
          "data(/Catln/Outer(inner -> /Catln/Inner, count -> /Data/Primitive/Integer))",
          "data(/Catln/Inner(label -> /Data/String))"
                                                    ]

    , testCase "declaring a type which reaches itself once, rather than forever" $
        declText (Proxy :: Proxy Chain) @?=
          "data(/Catln/Chain(head -> /Data/String, rest -> /Data/List[$T -> /Catln/Chain]))\n"
                                                     ]

  , testGroup "a Haskell value as the value that declaration is read with" [
      testCase "keyed as an evaluated value keys its arguments" $
        toCt aFile @?= TupleVal "/Catln/BuildFile" (H.fromList [
          ("/path", StrVal "site/index.html"),
          ("/contents", StrVal "<h1>hi</h1>")
                                                               ])

    , testCase "a constructor of several named in the type's own namespace" $
        toCt (Circle 2) @?= TupleVal "/Catln/Shape/Circle" (H.singleton "/radius" (IntVal 2))

    , testCase "a constructor carrying nothing written as a bare name" $
        toCt Point @?= TupleVal "/Catln/Shape/Point" H.empty
                                                                          ]

  , testGroup "a Catln value read back as the Haskell one" [
      roundTrip "the one file of a build" aFile
    , roundTrip "a record held inside another" (Outer (Inner "in") 1)
    , roundTrip "the arm of a sum which carries fields" (Circle 7)
    , roundTrip "the arm of a sum which carries none" Point
    , roundTrip "a type which reaches itself" (Chain "a" [Chain "b" []])
    , roundTrip "a value which is absent" (Nothing :: Maybe String)
    , roundTrip "a value which is present" (Just "here" :: Maybe String)
    , roundTrip "an empty list" ([] :: [Integer])
    , roundTrip "a list of records" [Inner "a", Inner "b"]
    , roundTrip "a string" ("text" :: String)
    , roundTrip "a whole number" (3 :: Integer)
    , roundTrip "a machine word" (3 :: Int)
    , roundTrip "a fractional number" (1.5 :: Double)
    , roundTrip "truth" True
    , roundTrip "falsity" False
                                                           ]

  , testGroup "a value which is not one of the type" [
      testCase "reported by the constructors that would have been accepted" $
        assertLeftHolds "/Catln/Shape/Circle or /Catln/Shape/Point"
          (fromCt (TupleVal "/Catln/Shape/Square" H.empty) :: Either String Shape)

    , testCase "reported by the argument it has not got" $
        assertLeftHolds "/radius"
          (fromCt (TupleVal "/Catln/Shape/Circle" H.empty) :: Either String Shape)

    , testCase "reported by the type an argument is not" $
        assertLeftHolds "/Data/Primitive/Integer"
          (fromCt (TupleVal "/Catln/Shape/Circle" (H.singleton "/radius" (StrVal "two"))) :: Either String Shape)

    , testCase "reported where the value is not a tuple at all" $
        assertLeftHolds "/Catln/BuildFile" (fromCt (IntVal 1) :: Either String BuiltFile)

    , testCase "reported where a machine word would have to be truncated" $
        assertLeftHolds "does not fit"
          (fromCt (IntVal (toInteger (maxBound :: Int) + 1)) :: Either String Int)
                                                     ]

  , testGroup "the page the builtins are declared on" [
      testCase "importing what the types it declares are written in terms of" $
        cpImports hsBuiltinPage @?= ["core/str.ct"]

    , testCase "and the compiling types its own comments are written in, ahead of them" $
        -- The lines it writes, and not the blank one 'Syntax.Ct.Formatter.formatPrgm' puts after
        -- them: what is asked here is which pages the page imports, and in what order.
        filter (not . null) (lines (formatRootPrgm (RawPrgm hsBuiltinsImports [] :: RawPrgm ()))) @?= [
          "import \"core/compile.ct\"",
          "import \"core/str.ct\""
                                                                                                      ]

    , testCase "declaring each type once, however many fields reach it" $
        length (cpDecls $ ctPageOf (Proxy :: Proxy Chain)) @?= 1
                                                     ]

    -- The point of mapping 'BuiltFile' rather than a stand-in: what a build is written as and what
    -- it is read back as are the one derived reading, so the argument keys 'valResultFiles' got
    -- wrong before are not written down anywhere any more.
  , testGroup "a build result read back through the derived reading" [
      testCase "the file a one-file result was built from" $
        valResultFiles (oneFileResult "site/index.html" "<h1>hi</h1>") @?= Right [aFile]

    , testCase "no files where the result holds none" $
        valResultFiles (TupleVal "/Catln/CatlnResult" (H.singleton "/files" (toCt ([] :: [BuiltFile])))) @?= Right []

    , testCase "a value which is no result at all" $
        assertLeftHolds "/Catln/CatlnResult" (valResultFiles (IntVal 1))

    , testCase "a result holding something which is no file" $
        assertLeftHolds "/Catln/BuildFile"
          (valResultFiles (TupleVal "/Catln/CatlnResult" (H.singleton "/files" (valListOf [IntVal 1]))))
                                                                     ]
                                              ]

-- | A failure which says the thing a reader of it needs to see.
assertLeftHolds :: (Show a) => String -> Either String a -> Assertion
assertLeftHolds needle (Left err) = assertBool (concat ["\"", err, "\" does not say ", needle]) (needle `isInfixOf` err)
assertLeftHolds needle (Right v) = assertFailure $ concat ["expected a failure saying ", needle, ", got ", show v]
