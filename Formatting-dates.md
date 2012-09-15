# Formatting date strings

Add old-locale to .cabal.

    import Data.Time.Format (formatTime)
    import System.Locale (defaultTimeLocale)
    import Data.Time.Clock (UTCTime) 
    
    formatDate :: String -> String
    formatDate dateString = formatTime defaultTimeLocale "%e %B %Y %H:%M:%S" t
        where 
            t = read dateString
            t :: UTCTime
        
    main = print $ formatDate "2012-09-15 00:07:31.874712 UTC"


# Formatting day values
Useful when you use a Day field and jQuery date picker.

    import Data.Time
    import Data.Time.Format (formatTime)
    import System.Locale (defaultTimeLocale)
    import Data.Time.Clock (UTCTime, utctDay) 
    import Data.Time.Calendar (Day, toGregorian)
    
    dateTimeFormat = "%e %B %Y %H:%M:%S"
    dateFormat = "%e %B %Y"
    
    formatDay :: Day -> String
    formatDay day = formatTime defaultTimeLocale dateFormat t
        where 
            t = UTCTime day 0
            t :: UTCTime
        
    main = do
        now <- getCurrentTime
        let today = utctDay now
        print $ formatDay today
