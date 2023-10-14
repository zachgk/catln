module TestCommon where
import           Data.List        (isSuffixOf)
import           System.Directory

findCt :: String -> IO [String]
findCt testDir = do
  fileNames <- listDirectory testDir
  let fileNames' = filter (isSuffixOf ".ct") fileNames
  return $ map (testDir ++) fileNames'
