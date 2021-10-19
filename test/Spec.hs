--import Test.Vkontakte
import Test.Telegram
import Test.Hspec

main :: IO ()
main = hspec $ do
--    testVkontakte
    testTelegram
