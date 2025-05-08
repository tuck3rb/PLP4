{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module DateTime where

-- IMPORTS
import Parsing2
import Data.Time (getCurrentTime, parseTimeM, defaultTimeLocale, UTCTime, 
                  addUTCTime, NominalDiffTime, LocalTime, timeZoneMinutes)
import Data.Time.Zones
import Control.Exception (catch, IOException)

-- DATA TYPES

-- Time units
data TimeUnit where
    Hours   :: TimeUnit
    Minutes :: TimeUnit
    Seconds :: TimeUnit
    deriving (Show, Eq)

-- A duration (ex: 3 hours)
data Duration where
    Duration :: Integer -> TimeUnit -> Duration
    deriving (Show, Eq)

-- Main expression type
data Expr where
    Convert :: String -> String -> String -> Expr  -- datetime from zone to zone
    NowIn   :: String -> Expr  -- timezone
    Add     :: Expr -> Duration -> Expr
    Sub     :: Expr -> Duration -> Expr
    ShowOffset :: String -> Expr
    Difference :: String -> String -> Expr
    deriving (Show, Eq) 

-- Description text shown at startup
description :: String
description = unlines
    [ "Welcome to the timezone machine!"
    , "Type an expression if you're familiar with this tool."
    , "Try `:help` (for features), `:zones` (for options)"
    , "and type `:quit` when you're ready to exit."
    ]

-- Help message to clarify features
helpMsg :: String
helpMsg = unlines
    ["The timezone machine supports:"
    , "• Requesting UTC offset for a time zone"
    , "• Finding difference between two time zones"
    , "• Converting from one time zone to another"
    , "• Getting the current time in any time zone"
    , "• Adding or subtracting to expressions using:"
    , "   - hours"
    , "   - minutes"
    , "   - seconds"
    , ""
    , "FORMATTING"
    , "-----------"
    , "• UTC offsets just require you to name the time zone:"
    , ""
    , "  `\"time zone\"`"
    , ""
    , "• Differences between time zones require the below syntax:"
    , ""
    , "  `difference \"time zone 1\" \"time zone 2\"`"
    , ""
    , "• Conversions must follow the below syntax:"
    , ""
    , "  `convert \"yyyy-mm-dd hh:mm\" from \"time zone 1\" to \"time zone 2\"`"
    , ""
    , "  IMPORTANT: For PM use 24 hour time (Ex: 5:00 PM would be 17:00)."
    , ""
    , "• Getting current time must follow the below syntax:"
    , ""
    , "  `now in \"time zone\"`"
    , ""
    , "• To add and subtract use + or - and type `hours`, `minutes`, or `seconds`."
    , ""
    , "EXAMPLES"
    , "---------"
    , "• convert \"2025-04-22 15:30\" from \"US/Central\" to \"Asia/Tokyo\""
    , "• now in \"UTC\""
    , "• now in \"US/Central\" + 20 minutes"
    , "• \"Europe/London\""
    , "• difference \"GMT\" \"Zulu\""
    ]

zonesList :: String
zonesList = unlines
    [ "For a full list of time zones visit:"
    , "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List"
    , "Reference the \'TZ Identifier\' column."
    , ""
    , "Examples: \"UTC\", \"US/Eastern\", \"Indian/Maldives\""
    ]

-- Main datetime function to execute calculation
datetime :: String -> IO String
datetime s =
    case parseStr s of
        Left err   -> return ("Parse error: " ++ err)
        Right expr -> eval expr

-- PARSING
lexer :: TokenParser ()
lexer = makeTokenParser $ emptyDef
    { reservedNames = ["convert", "from", "to", "now", "in"]
    , reservedOpNames = ["+", "-"]
    }

reserved :: String -> Parser ()
reserved = getReserved lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

reservedOp :: String -> Parser ()
reservedOp = getReservedOp lexer

parseStr :: String -> Either String Expr
parseStr s =
    case parse parseExpr s of
        Left err -> Left (show err)
        Right e  -> Right e

parseStrLit :: Parser String
parseStrLit = getStringLiteral lexer

parseExpr :: Parser Expr
parseExpr = parseExprAtom >>= parseAddSub

parseExprAtom :: Parser Expr
parseExprAtom = parseConvert <|> parseNowIn <|> parseDifference <|> parseShowOffset

-- For Addition and Subtraction of time (ex: +3 hours)
parseAddSub :: Expr -> Parser Expr
parseAddSub e =
    (reservedOp "+" >>
     parseDuration >>= \d ->
     parseAddSub (Add e d))
    <|>
    (reservedOp "-" >>
     parseDuration >>= \d ->
     parseAddSub (Sub e d))
    <|> return e

parseConvert :: Parser Expr
parseConvert =
    whiteSpace >>
    reserved "convert" >>
    parseStrLit >>= \dt ->
    reserved "from" >>
    parseStrLit >>= \fromZone ->
    reserved "to" >>
    parseStrLit >>= \toZone ->
    return (Convert dt fromZone toZone)

parseNowIn :: Parser Expr
parseNowIn =
    whiteSpace >>
    reserved "now" >>
    reserved "in" >>
    parseStrLit >>= \zone ->
    return (NowIn zone)

-- Amount of time for Add/Sub
parseDuration :: Parser Duration
parseDuration =
    getInteger lexer >>= \n ->
    parseUnit >>= \u ->
    return (Duration n u)

parseUnit :: Parser TimeUnit
parseUnit =
    (reserved "hours" >> return Hours)
    <|> (reserved "minutes" >> return Minutes)
    <|> (reserved "seconds" >> return Seconds)

parseShowOffset :: Parser Expr
parseShowOffset =
    whiteSpace >>
    parseStrLit >>= \zone ->
    return (ShowOffset zone)

parseDifference :: Parser Expr
parseDifference =
    whiteSpace >>
    reserved "difference" >>
    parseStrLit >>= \zone1 ->
    parseStrLit >>= \zone2 ->
    return (Difference zone1 zone2)

-- EVALUATOR
eval :: Expr -> IO String
eval expr = eval' expr `catch` handleBadTZ

eval' :: Expr -> IO String
eval' (Convert dt fromZone toZone) = evalConvert dt fromZone toZone
eval' (NowIn zone)                 = evalNowIn zone
eval' (Add e dur)                  = evalTimeOp e dur
eval' (Sub e dur) =
    let durNeg = case dur of
                    Duration n u -> Duration (negate n) u
    in evalTimeOp e durNeg  -- negating dur to allow subtraction in evalTimeOp
eval' (ShowOffset zone)            = evalShowOffset zone
eval' (Difference z1 z2)           = evalDifference z1 z2

evalConvert :: String -> String -> String -> IO String
evalConvert dt fromZone toZone =
    case safeParseTime dt of
        Right localTime ->
            loadSystemTZ fromZone >>= \fromTZ ->
            loadSystemTZ toZone   >>= \toTZ ->
            let utcTime = localTimeToUTCTZ fromTZ localTime
                newLocal = utcToLocalTimeTZ toTZ utcTime
            in return (show newLocal)
        Left err -> return ("Evaluation error: " ++ err)

evalNowIn :: String -> IO String
evalNowIn zone =
    getCurrentTime >>= \utcTime ->
    loadSystemTZ zone >>= \tz ->
    let localTime = utcToLocalTimeTZ tz utcTime
    in return (show localTime)

evalTimeOp :: Expr -> Duration -> IO String
evalTimeOp e dur =
    evalToUTC e >>= \case  -- Lambda case to avoid blue warning
        Left err -> return err
        Right baseTime ->
            let offset = durationToDiffTime dur
                newUTCTime = addUTCTime offset baseTime
            in case findTimeZone e of
                Just zone ->
                    loadSystemTZ zone >>= \tz ->
                    let newLocal = utcToLocalTimeTZ tz newUTCTime
                    in return (show newLocal)
                Nothing ->
                    return (show newUTCTime)

evalShowOffset :: String -> IO String
evalShowOffset zone =
    getCurrentTime >>= \utcNow ->
    loadSystemTZ zone >>= \tz ->
    let offsetMin = timeZoneMinutes (timeZoneForUTCTime tz utcNow)
        h = offsetMin `div` 60
        m = abs (offsetMin `mod` 60)
        sign = if offsetMin >= 0 then "+" else "-"
    in return ("UTC" ++ sign ++ show (abs h) ++ ":" ++ (if m < 10 then "0" else "") ++ show m)

evalDifference :: String -> String -> IO String
evalDifference z1 z2 =
    getCurrentTime >>= \utcNow ->
    loadSystemTZ z1 >>= \tz1 ->
    loadSystemTZ z2 >>= \tz2 ->
    let offset1 = timeZoneMinutes (timeZoneForUTCTime tz1 utcNow)
        offset2 = timeZoneMinutes (timeZoneForUTCTime tz2 utcNow)
        diffMin = abs (offset1 - offset2)
        h = diffMin `div` 60
        m = diffMin `mod` 60
    in return (z1 ++ ": UTC" ++ formatOffset offset1
        ++ "\n" ++ z2 ++ ": UTC" ++ formatOffset offset2
        ++ "\nDifference: " ++ show h ++ " hours " ++ show m ++ " minutes")

formatOffset :: Int -> String
formatOffset offsetMin =
    let h = offsetMin `div` 60
        m = abs (offsetMin `mod` 60)
        sign = if offsetMin >= 0 then "+" else "-"
    in sign ++ show (abs h) ++ ":" ++ (if m < 10 then "0" else "") ++ show m

-- Helper function for evalTimeOp to convert Duration to DiffTime
durationToDiffTime :: Duration -> NominalDiffTime
durationToDiffTime (Duration n unit) =
    case unit of
        Hours   -> fromInteger (n * 60 * 60)
        Minutes -> fromInteger (n * 60)
        Seconds -> fromInteger n

-- Helper function for converting any Expr to UTC for easy conversion
evalToUTC :: Expr -> IO (Either String UTCTime)
evalToUTC (Convert dt fromZone _) =
    case safeParseTime dt of
        Right localTime ->
            loadSystemTZ fromZone >>= \fromTZ ->
            return (Right (localTimeToUTCTZ fromTZ localTime))
        Left err -> return (Left ("Evaluation error: " ++ err))
evalToUTC (NowIn zone) =
    getCurrentTime >>= \utcNow ->
    loadSystemTZ zone >>= \tz ->
    let local = utcToLocalTimeTZ tz utcNow
    in return (Right (localTimeToUTCTZ tz local))
evalToUTC (Add e dur) =
    evalToUTC e >>= \case  -- Lambda case to avoid blue warning
        Left err -> return (Left err)
        Right baseTime ->
            let offset = durationToDiffTime dur
            in return (Right (addUTCTime offset baseTime))
evalToUTC (Sub e dur) =
    evalToUTC e >>= \case  -- Lambda case to avoid blue warning
        Left err -> return (Left err)
        Right baseTime ->
            let offset = durationToDiffTime dur
            in return (Right (addUTCTime (negate offset) baseTime))
evalToUTC (ShowOffset _)
    = return (Left "Evaluation error: Cannot convert offset query to a UTC time")
evalToUTC (Difference _ _)
    = return (Left "Evaluation error: Cannot convert difference query to a UTC time")

-- Helper function for finding time zone for nested adds or subs
findTimeZone :: Expr -> Maybe String
findTimeZone (Convert _ _ toZone) = Just toZone
findTimeZone (NowIn zone)         = Just zone
findTimeZone (Add e _)            = findTimeZone e
findTimeZone (Sub e _)            = findTimeZone e
findTimeZone (ShowOffset _)       = Nothing
findTimeZone (Difference _ _)     = Nothing

-- Safer functions to allow more assistive error handling
safeParseTime :: String -> Either String LocalTime
safeParseTime s =
    case parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M" s of
        Just t  -> Right t
        Nothing -> Left ("Invalid date/time format: " ++ s ++
                        "\nType `:help` for more information.")

handleBadTZ :: IOException -> IO String
handleBadTZ _ = return ("Invalid or unknown time zone." ++
                     "\nType `:zones` for a complete list.")
