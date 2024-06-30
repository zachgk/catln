module Common.TestCommon where
import           Data.List        (isSuffixOf)
import           System.Directory

filesWithExtension :: String -> String -> IO [String]
filesWithExtension ext testDir = do
  fileNames <- listDirectory testDir
  let fileNames' = filter (isSuffixOf ext) fileNames
  return $ map (testDir ++) fileNames'

findCt :: String -> IO [String]
findCt = filesWithExtension ".ct"
