module Common.TestCommon where
import           CtConstants      (errPageSuffix)
import           Data.List        (isSuffixOf)
import           System.Directory

filesWithExtension :: String -> String -> IO [String]
filesWithExtension ext testDir = do
  fileNames <- listDirectory testDir
  let fileNames' = filter (isSuffixOf ext) fileNames
  return $ map (testDir ++) fileNames'

-- | The pages in a directory, less those written to fail ('findErrCt').
findCt :: String -> IO [String]
findCt dir = filter (not . isSuffixOf errPageSuffix) <$> filesWithExtension ".ct" dir

-- | The pages in a directory that are written to fail, which a test requires to.
findErrCt :: String -> IO [String]
findErrCt = filesWithExtension errPageSuffix

findPy :: String -> IO [String]
findPy = filesWithExtension ".py"

findJson :: String -> IO [String]
findJson = filesWithExtension ".json"
