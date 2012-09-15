    import Data.Time.Format (formatTime)
    import System.Locale (defaultTimeLocale)
    import Data.Time.Clock (UTCTime) 
    
    formatDate :: String -> String
    formatDate dateString = formatTime defaultTimeLocale "%e %B %Y %H:%M:%S" t
        where 
            t = read dateString
            t :: UTCTime
        
    main = print $ formatDate "2012-09-15 00:07:31.874712 UTC"