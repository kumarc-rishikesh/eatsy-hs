
module ConvertTypes (
  convertDate 
) where 

import Data.Time.Format
import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T

convertDate :: Text -> Text
convertDate inputDate = do
    let parsedDate =  parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (T.unpack inputDate) :: UTCTime
    T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ" parsedDate
