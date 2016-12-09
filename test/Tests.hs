import           Control.Lens
import qualified Parse        (updateEvent)
import           Test.HUnit
import           Types

main :: IO Counts
main = runTestTT parseTests

parseTests = TestList [testEventUpdate ]

testEventUpdate :: Test
testEventUpdate = TestCase $ do
  testHtml <- readFile "test/html/event.html"
  let evt = Parse.updateEvent defaultEvent testHtml
      in do 
          assertEqual "DateTime is updated" (Just "10.12.2016") (evt ^. date)
          assertEqual "Extra link is updated" (Just "https://vk.com/typical.hscafe") (evt ^. extraLink)
          assertEqual "Coordinates are updated" (Just Coords { _lat = 59.9388403, _lng = 30.31995059999999 }) (evt ^. coords) 


--test1 = TestCase (assertEqual "for (foo 3)," (1,2) (1, 3))
--test2 = TestCase (do (x,y) <- partA 3
--                     assertEqual "for the first result of partA," 5 x
--                     b <- partB y
--                     assertBool ("(partB " ++ show y ++ ") failed") b)

