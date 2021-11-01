import Test.Hspec
import Test.Telegram
import Test.Vkontakte

main :: IO ()
main =
  hspec $ do
    testVkontakte
    testTelegram
