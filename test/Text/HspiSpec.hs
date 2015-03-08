module Text.HspiSpec where
import Test.Hspec
import Text.Hspi

spec :: Spec
spec = do
  describe "getRelativePath Test" $ do
    getRelativePathTest "foo" "bar" "foo/bar"
    getRelativePathTest "foo" "/bar" "foo/bar"
    getRelativePathTest "foo" "//bar" "foo/bar"
    getRelativePathTest "foo/" "bar" "foo/bar"
    getRelativePathTest "foo/" "/bar" "foo/bar"

getRelativePathTest :: FilePath -> FilePath -> FilePath -> Spec
getRelativePathTest root path res = 
    it ("root = \"" ++ root ++ "\", and relative path = \"" ++ path ++ "\"") $
      getRelativePath root path `shouldBe` res
