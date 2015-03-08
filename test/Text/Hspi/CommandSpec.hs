module Text.Hspi.CommandSpec where
import Test.Hspec
import Text.Hspi
import Text.Hspi.Command

spec :: Spec
spec = do
  describe "preprocOpt Test" $ do
    preprocOptTest [] []
    preprocOptTest ["root"] ["root"]
    preprocOptTest ["root", "--add=script,src"] ["root", "-a", "script,src"]
    preprocOptTest ["root", "--add=script,src", "-i", "input"] ["root", "-a", "script,src", "-i", "input"]
    preprocOptTest ["root", "--add=script,src"] ["root", "-a", "script,src"]
    preprocOptTest ["root", "--no-script", "--add=script,src"] ["root", "--no-script", "-a", "script,src"]


preprocOptTest :: [String] -> [String] -> Spec
preprocOptTest opts res = 
    it ("option = \"" ++ (show opts) ++ "\"") $
      preprocOpt opts `shouldBe` res

--parseOptionsTest :: [String] -> Either String Options -> Spec
--parseOptionsTest opts res = 
--    it ("option = \"" ++ (show $ preprocOpt opts) ++ "\"") $
--      parseOptions (preprocOpt opts) `shouldBe` res

