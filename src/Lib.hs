module Lib where

import Data.Maybe
import Data.List
import Data.List.Split
import Text.Read
import Control.Monad

data CalendarRule = CalendarRule { daysInYear, daysInMonth, daysInWeek :: Int } deriving (Show, Eq)
data Date = Date { yearOfEra, monthOfYear, dayOfMonth :: Int } deriving (Show, Eq)

run :: Int -> Int -> Int -> String -> String
run y m w d = fromMaybe "-1" $ do
  (calendarRule, date) <- parseArgs y m w d
  fmap (\v -> [v]) $ toWeekChar $ dateToDays calendarRule date

toWeekChar :: (Num a, Eq a, Enum a) => a -> Maybe Char
toWeekChar n = fmap snd . find ((==) n . fst) $ zip [1..26] ['A'..'Z']

parseArgs :: Int -> Int -> Int -> String -> Maybe (CalendarRule, Date)
parseArgs dIY dIM dIW date = do
  es <- listToMaybe . take 1 . drop 0 $ splitOn "-" date
  ys <- listToMaybe . take 1 . drop 1 $ splitOn "-" date
  ms <- listToMaybe . take 1 . drop 2 $ splitOn "-" date
  yoe <- readMaybe es :: Maybe Int
  moy <- readMaybe ys :: Maybe Int
  dom <- readMaybe ms :: Maybe Int
  guard (dom <= dIM)
  guard (moy <= (dIY `div` dIM + if listExtraDayOfYear calendarRule !! yoe == 0 then 1 else 0))
  return $ (calendarRule, Date yoe moy dom)
    where
      calendarRule = CalendarRule dIY dIM dIW

listExtraDayOfYear :: CalendarRule -> [Int]
listExtraDayOfYear r = f 0 0
  where
    f :: Int -> Int -> [Int]
    f n d = b n d : f (n + 1) (b n d)
    a :: Int -> Int
    a n = (n + 1) * daysInYear r `mod` daysInMonth r
    b :: Int -> Int -> Int
    b n d = if d >= daysInMonth r then 0 else a n

dateToDays :: CalendarRule -> Date -> Int
dateToDays r d = daysInYear r * (yearOfEra d - 1) + daysInMonth r * (monthOfYear d - 1) + dayOfMonth d

ruleToMonthOfYear :: CalendarRule -> Int -> Int
ruleToMonthOfYear r y = daysInYear r `div` daysInMonth r + if listExtraDayOfYear r !! y == 0 then 1 else 0

extraMonthOfYear :: CalendarRule -> Int -> Int
extraMonthOfYear r n = n * daysInYear r `div` daysInMonth r `mod` n

listDayOfWeek :: CalendarRule -> [Char]
listDayOfWeek r = take (daysInWeek r) ['A'..'Z']

