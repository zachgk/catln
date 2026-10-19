module TestService where

import           CtService (CTSS, ctssBaseFiles, ctssBaseFilesFrom,
                            ctssBuildAll)
import           Semantics (testCtssConfig)

-- | How a test makes the service of its pages.
type MkService = [String] -> IO CTSS

-- | The standard library, read and built once for the whole suite.
--
-- A page which imports @core@ reaches all of it, and building it is most of what compiling any such
-- page costs. Built here once, every test's service starts from it ('seededService') rather than
-- building it again.
libraryService :: IO CTSS
libraryService = do
  lib <- ctssBaseFiles testCtssConfig ["stack"]
  ctssBuildAll lib
  return lib

-- | A service of a test's own pages, holding from the start whatever of the library they reach,
-- already built. Its own pages it reads and builds as a service made from nothing would.
seededService :: CTSS -> MkService
seededService lib = ctssBaseFilesFrom lib testCtssConfig
