import           Control.Lens ((^.))
import qualified Parse        (parseEventsPage, updateEvent)
import           Prelude      hiding (id)
import           Test.HUnit   hiding (Location)
import           Types

main :: IO Counts
main = runTestTT parsingTests

parsingTests = TestList [ testTableParsing, testEventUpdate ]

testTableParsing :: Test
testTableParsing = TestCase $ do
  testHtml <- readFile "test/html/table.html"
  let evts = Parse.parseEventsPage testHtml
      evt = evts !! 6
      in do
         assertEqual "Found all events" 15 (length evts)
         assertEqual "Event #7 has ID" (Just 26646) (evt ^. id)
         assertEqual "Event #7 has link" (Just "http://eu.battle.net/hearthstone/ru/fireside-gatherings/26646") (evt ^. link)
         -- Freakin unicode >_<
         -- assertEqual "Event #7 has name" (Just "Герой Таверны в Типичном HearthStone") (evt ^. name)
         assertEqual "Event #7 has location" Location { _city = Just "Sankt-Peterburg"
                                                      , _country = Just "Russia"
                                                      , _state = Just "FakeState" } (evt ^. location)


testEventUpdate :: Test
testEventUpdate = TestCase $ do
  testHtml <- readFile "test/html/event.html"
  let evt = Parse.updateEvent defaultEvent testHtml
      in do
          assertEqual "DateTime is updated" (Just "10.12.2016") (evt ^. date)
          assertEqual "Extra link is updated" (Just "https://vk.com/typical.hscafe") (evt ^. extraLink)
          assertEqual "Coordinates are updated" (Just Coords { _lat = 59.9388403, _lng = 30.31995059999999 }) (evt ^. coords)
