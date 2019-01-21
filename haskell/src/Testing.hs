module Testing where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Coerce
import Control.Applicative

import Data.Time.Clock as Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime

testTime :: UTCTime
testTime = UTCTime { utctDay = ModifiedJulianDay 12000
                   , utctDayTime = 0
                   }

lunchTestTime :: UTCTime
lunchTestTime = testTime { utctDayTime = 44000 }

endLunchTestTime :: UTCTime
endLunchTestTime = testTime { utctDayTime = 52000 }

type Hour = Int

getHour :: UTCTime -> Hour
getHour = todHour . timeToTimeOfDay . utctDayTime

-- simplest version
isItLunchTime :: IO Bool
isItLunchTime = (\hr -> hr >= 12 && hr <= 14) <$> getHour <$> Clock.getCurrentTime

-- let's have the IO function passed in instead
injectableLunch :: IO UTCTime -> IO Bool
injectableLunch getTime = (\hr -> hr >= 12 && hr <= 14) <$> getHour <$> getTime

-- and then generalise the
-- let's break out most of the logic into this polymorphic version
testableLunch :: (Monad m) => m UTCTime -> m Bool
testableLunch getTime = (\hr -> hr >= 12 && hr <= 14) <$> getHour <$> getTime

-- now we can test this code with a much safer monad
testNotLunch :: Identity Bool
testNotLunch = testableLunch (pure testTime)
-- Identity False

-- now we can test this code with a much safer monad
testIsLunch :: Identity Bool
testIsLunch = testableLunch (pure lunchTestTime)
-- Identity True

-- now we have a new version of the function to use in code
-- which is covered in tests
isItLunchTime3 :: IO Bool
isItLunchTime3 = testableLunch Clock.getCurrentTime




-- another version
class Monad m => MonadTime m where
  getTheTimePlease :: m UTCTime

-- for REAL
instance MonadTime IO where
  getTheTimePlease = Clock.getCurrentTime

-- for testing
instance MonadTime Identity where
  getTheTimePlease = pure lunchTestTime
