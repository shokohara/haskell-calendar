module LibSpec (spec) where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  it "1" $ run 160 30 7 "0001-01-01" `shouldBe` "A"
  it "2" $ run 160 30 7 "0001-01-07" `shouldBe` "G"
--  it "3" $ run 160 30 7 "0001-01-08" `shouldBe` "A"
--  it "4" $ run 160 30 7 "0001-01-30" `shouldBe` "B"
--  it "5" $ run 160 30 7 "0001-01-31" `shouldBe` "-1"
--  it "6" $ run 160 30 7 "0001-02-01" `shouldBe` "C"
--  it "8" $ run 160 30 7 "0001-05-01" `shouldBe` "B"
--  it "9" $ run 160 30 7 "0001-05-30" `shouldBe` "C"
--  it "10" $ run 160 30 7 "0001-06-01" `shouldBe` "-1"
--  it "11" $ run 160 30 7 "0002-01-01" `shouldBe` "D"
--  it "12" $ run 160 30 7 "0002-06-01" `shouldBe` "-1"
--  it "13" $ run 160 30 7 "0003-01-01" `shouldBe` "G"
--  it "14" $ run 160 30 7 "0003-05-30" `shouldBe` "B"
--  it "15" $ run 160 30 7 "0003-06-01" `shouldBe` "C"
--  it "16" $ run 160 30 7 "0003-06-30" `shouldBe` "D"
--  it "17" $ run 160 30 7 "0003-07-01" `shouldBe` "-1"
--  it "18" $ run 160 30 7 "0004-01-01" `shouldBe` "E"
--  it "19" $ run 160 30 7 "0004-06-01" `shouldBe` "-1"
--  it "20" $ run 160 30 7 "0005-06-01" `shouldBe` "-1"
--  it "21" $ run 160 30 7 "0006-06-01" `shouldBe` "G"
--  it "22" $ run 365 50 8 "0001-01-01" `shouldBe` "A"
--  it "23" $ run 365 50 8 "0001-02-01" `shouldBe` "C"
--  it "24" $ run 365 50 8 "0001-07-01" `shouldBe` "E"
--  it "25" $ run 365 50 8 "0002-01-01" `shouldBe` "G"
--  it "26" $ run 365 50 8 "0003-01-01" `shouldBe` "E"
--  it "27" $ run 365 50 8 "0004-01-01" `shouldBe` "C"
--  it "28" $ run 365 50 8 "0004-08-01" `shouldBe` "A"
--  it "29" $ run 365 50 8 "0005-01-01" `shouldBe` "C"
--  it "30" $ run 365 50 8 "0005-08-01" `shouldBe` "-1"
--  it "31" $ run 365 50 8 "0006-01-01" `shouldBe` "A"
--  it "32" $ run 365 50 8 "0006-08-01" `shouldBe` "-1"
--  it "33" $ run 365 50 8 "0007-01-01" `shouldBe` "G"
--  it "34" $ run 365 50 8 "0007-08-01" `shouldBe` "E"
--  it "35" $ run 365 50 8 "0008-01-01" `shouldBe` "G"
--  it "36" $ run 365 50 8 "0008-08-01" `shouldBe` "-1"
--  it "37" $ run 365 50 8 "0009-08-01" `shouldBe` "-1"
--  it "38" $ run 365 50 8 "0010-08-01" `shouldBe` "A"
--  it "39" $ run 365 50 8 "0011-01-01" `shouldBe` "C"
--  it "40" $ run 365 50 8 "0011-08-01" `shouldBe` "-1"
  describe "toWeekChar" $ do
    it "" $ toWeekChar 1 `shouldBe` Just 'A'
    it "" $ toWeekChar 2 `shouldBe` Just 'B'
  describe "parseDate" $ do
    it "" $ parseArgs 160 30 7 "0001-01-01" `shouldBe` Just (CalendarRule 160 30 7, Date 1 1 1)
    it "" $ parseArgs 160 30 7 "0001-05-30" `shouldBe` Just (CalendarRule 160 30 7, Date 1 5 30)
    it "" $ parseArgs 160 30 7 "0002-06-10" `shouldBe` Just (CalendarRule 160 30 7, Date 2 6 10)
    it "" $ parseArgs 160 30 7 "0002-06-11" `shouldBe` Just (CalendarRule 160 30 7, Date 2 6 11)
    it "" $ parseArgs 160 30 7 "0001-06-01" `shouldBe` Nothing
    it "" $ parseArgs 160 30 7 "0001-01-31" `shouldBe` Nothing
    it "" $ parseArgs 160 30 7 "0003-07-01" `shouldBe` Nothing
    it "" $ parseArgs 160 30 7 "0004-06-01" `shouldBe` Nothing
    it "" $ parseArgs 160 30 7 "0005-06-01" `shouldBe` Nothing
    it "" $ parseArgs 365 50 8 "0005-08-01" `shouldBe` Nothing
    it "" $ parseArgs 365 50 8 "0006-08-01" `shouldBe` Nothing
    it "" $ parseArgs 365 50 8 "0008-08-01" `shouldBe` Nothing
--    it "" $ parseArgs 365 50 8 "0009-08-01" `shouldBe` Nothing
    it "" $ parseArgs 365 50 8 "0011-08-01" `shouldBe` Nothing
  describe "dateToDays" $ do
    it "" $ fmap (\v -> dateToDays (fst v) (snd v)) (parseArgs 160 30 7 "0001-01-01") `shouldBe` Just 1
    it "" $ fmap (\v -> dateToDays (fst v) (snd v)) (parseArgs 160 30 7 "0001-01-07") `shouldBe` Just 7
    it "" $ fmap (\v -> dateToDays (fst v) (snd v)) (parseArgs 160 30 7 "0001-01-08") `shouldBe` Just 8
    it "" $ fmap (\v -> dateToDays (fst v) (snd v)) (parseArgs 160 30 7 "0001-02-01") `shouldBe` Just (1 * 30 + 1)
    it "" $ fmap (\v -> dateToDays (fst v) (snd v)) (parseArgs 160 30 7 "0001-05-01") `shouldBe` Just (4 * 30 + 1)
    it "" $ fmap (\v -> dateToDays (fst v) (snd v)) (parseArgs 160 30 7 "0003-07-01") `shouldBe` Nothing
  describe "dateToMonthOfYear" $ do
    it "" $ ruleToMonthOfYear (CalendarRule 160 30 7) 0 `shouldBe` 5
    it "" $ ruleToMonthOfYear (CalendarRule 160 30 7) 1 `shouldBe` 5
    it "" $ ruleToMonthOfYear (CalendarRule 160 30 7) 2 `shouldBe` 6
    it "" $ ruleToMonthOfYear (CalendarRule 160 30 7) 3 `shouldBe` 5
    it "" $ ruleToMonthOfYear (CalendarRule 160 30 7) 4 `shouldBe` 5
    it "" $ ruleToMonthOfYear (CalendarRule 160 30 7) 5 `shouldBe` 6
    it "" $ ruleToMonthOfYear (CalendarRule 160 30 7) 6 `shouldBe` 5
  describe "listExtraDayOfYear" $ do
    it "" $ listExtraDayOfYear (CalendarRule 160 30 7) !! 0 `shouldBe` 10
    it "" $ listExtraDayOfYear (CalendarRule 160 30 7) !! 1 `shouldBe` 20
    it "" $ listExtraDayOfYear (CalendarRule 160 30 7) !! 2 `shouldBe` 0
    it "" $ listExtraDayOfYear (CalendarRule 160 30 7) !! 3 `shouldBe` 10
    it "" $ listExtraDayOfYear (CalendarRule 160 30 7) !! 4 `shouldBe` 20
    it "" $ listExtraDayOfYear (CalendarRule 160 30 7) !! 5 `shouldBe` 0
    it "" $ listExtraDayOfYear (CalendarRule 160 30 7) !! 6 `shouldBe` 10

