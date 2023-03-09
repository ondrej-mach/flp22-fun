import System.Environment
-- import System.IO
import Data.List
import Data.Bits
import Data.Char
import Text.Printf
import Numeric (showHex, showIntAtBase)
import System.Random
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Data.Functor.Identity


---------------------------- CUSTOM DATA TYPES -------------------------------

data Point = Point {
    x :: Integer,
    y :: Integer
} | InfPoint deriving (Eq)

data Curve = Curve {
    p :: Integer,
    a :: Integer,
    b :: Integer,
    g :: Point,
    n :: Integer,
    h :: Integer
}

data Key = Key {
    d :: Integer,
    q_key :: Integer
} deriving (Read)

data Hash = Integer

data Signature = Signature {
    r :: Integer,
    s :: Integer
} deriving (Read)

data PublicKey = PublicKey {
    q_pubkey :: Integer
} deriving (Read)


----------------------------- SHOW FUNCTIONS -----------------------------------

instance Show Point where
    show (Point x y) = printf "Point {\nx: 0x%x\ny: 0x%x\n}" x y
    show InfPoint = "Infinity"

showAttribute :: (String, String) -> String
showAttribute (key, value)
    | length (lines value) > 1 =
        let alignedValue = intercalate "\n" offsetLines
            offsetLines = [head ls] ++ map (\x -> replicate offset ' ' ++ x) (init $ tail ls) ++ [last ls]
            offset = length key + 2
            ls = lines value
        in key ++ ": " ++ alignedValue
    | otherwise = key ++ ": " ++ value

instance Show Curve where
    show (Curve p a b g n h) = "Curve {\n" ++ attrString ++ "}"
            where attrString = unlines $ map showAttribute attrList
                    where
                        attrList = [("p", show0xHex p),
                                    ("a", show a),
                                    ("b", show b),
                                    ("g", show g),
                                    ("n", show0xHex n),
                                    ("h", show h)]

instance Show Key where
    show (Key d q_key) = printf "Key {\nd: 0x%x\nQ: 0x%0130x\n}" d q_key

show0xHex :: Integer -> String
show0xHex n = "0x" ++ showHex n ""



--------------------- PARSING ------------------------

type DataType = String
type Block = [(String, Value)]
data Value = SimpleValue String | CompoundValue DataType Block deriving (Show)

compoundValueParser :: Parser Value
compoundValueParser = do
    dt <- (many1 alphaNum)
    many $ oneOf " \t"
    block <- blockParser
    return $ CompoundValue dt block

simpleValueParser :: Parser Value
simpleValueParser = do
    str <- (many1 alphaNum)
    return $ SimpleValue str

keyValueParser :: Parser (String, Value)
keyValueParser = do
    key <- many1 alphaNum
    spaces
    char ':'
    spaces
    value <- try compoundValueParser <|> try simpleValueParser
    return (key, value)

blockParser :: Parser Block
blockParser = do
    string "{"
    spaces
    list <- many (keyValueParser <* spaces)
    string "}"
    return list

getIntAttr :: String -> Block -> Integer
getIntAttr name block =
    let
        (Just val) = lookup name block
        SimpleValue str = val
    in read str

pointParser :: Parser Point
pointParser = do
    struct <- compoundValueParser
    let CompoundValue dt block = struct
        -- TODO assert dt == "Point"
        x = getIntAttr "x" block
        y = getIntAttr "y" block
    return (Point x y)

curveParser :: Parser Curve
curveParser = do
    struct <- compoundValueParser
    let CompoundValue dt block = struct
        -- TODO assert dt == "Curve"
        p = getIntAttr "p" block
        a = getIntAttr "a" block
        b = getIntAttr "b" block
        n = getIntAttr "n" block
        h = getIntAttr "h" block
        (Just pointValue) = lookup "g" block
        (CompoundValue pointType pointBlock) =  pointValue
        x = getIntAttr "x" pointBlock
        y = getIntAttr "y" pointBlock
    return (Curve p a b (Point x y) n h)


examplePoint = Point {
    x=0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
    y=0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
}
exampleCurve = Curve {
    p=0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F,
    a=0,
    b=7,
    g=examplePoint,
    n=0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141,
    h=1
}

-------------------------- EC COMPUTATION -------------------------

-- egcd stands for extended greatest common divider
-- implemented using extended euclidean algorithm
-- returns (g, s, t), where a*s + b*t == g
-- https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm#Extended_2
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (b `mod` a) a
    in (g, t - (b `div` a) * s, s)

-- Multiplicative inverse of n mod q.
-- ret*n == 1 (mod q)
inverse n q = s `mod` q
    where (_, s, _) = egcd (n `mod` q) q

pointAdd :: Curve -> Point -> Point -> Point
pointAdd _ point InfPoint = point
pointAdd _ InfPoint point = point
pointAdd (Curve p a b g n h) (Point x1 y1) (Point x2 y2)
    -- Point doubling (ading the same point)
    | (x1 == x2) && (y1 == y2) = pointDouble (Curve p a b g n h) (Point x1 y1)
    -- Adding negative point, the result is point in infinity
    | (x1 == x2) && ((y1 /= y2) || (y1 == 0)) = InfPoint
    -- Adding any normal points
    | otherwise =
        let
            x3 = (lambda^2 - x1 - x2) `mod` p
            y3 = (lambda*(x1 - x3) - y1) `mod` p
            lambda = ((y2 - y1) * (inverse (x2 - x1) p)) `mod` p
        in Point x3 y3

pointDouble :: Curve -> Point -> Point
pointDouble (Curve p a b g n h) (Point x1 y1) =
    let lambda = ((3 * x1^2 + a) * (inverse (2 * y1) p)) `mod` p
        x3 = (lambda^2 - 2*x1) `mod` p
        y3 = (lambda*(x1 - x3) - y1) `mod` p
    in Point x3 y3

showBin :: Integer -> String -> String
showBin = showIntAtBase 2 intToDigit

pointMult :: Curve -> Integer -> Point -> Point
pointMult curve k point = result
    where
        bits = showBin k ""
        (result, _) = foldr helper (InfPoint, point) bits
        helper bit (prevRes, prevTemp) =
            let
                newTemp = pointDouble curve prevTemp
                newRes = if bit == '1'
                    then pointAdd curve prevRes prevTemp
                    else prevRes
            in (newRes, newTemp)


pointToPubKey :: Point -> Integer
pointToPubKey (Point x1 y1) = prefix .|. x_out .|. y_out
    where
        bits_per_coord = 256
        prefix = shift 0x40 (bits_per_coord * 2)
        x_out = shift x1 bits_per_coord
        y_out = y1

-- This function makes longer randowm numbers than standard random
-- It makes the private key the correct lenght, but the entropy is bad
-- It should be never used in practice
getLongerRandom :: StdGen -> Integer
getLongerRandom gen = foldl helper 0 randomList
    where
        randomList = take 4 $ randoms gen :: [Word]
        helper acc x = (shift acc 64) `xor` (fromIntegral x)



--------------------------- MAIN -----------------------------

actionInput :: String -> IO ()
actionInput input = do
    let (Right curve) = runIdentity $ runParserT curveParser () "" input
    putStrLn $ show curve

actionKeygen :: String -> IO ()
actionKeygen input = do
    gen <- getStdGen -- This random is total crap and should not be used for cryptography

    let (Right curve) = runIdentity $ runParserT curveParser () "" input
        Curve p a b g n h = curve
        d = getLongerRandom gen
        q = pointToPubKey $ pointMult curve d g
        key = Key d q

    putStrLn $ show key


dispatch :: [(String, String -> IO ())]
dispatch = [("-i", actionInput),
            ("-k", actionKeygen),
            ("-s", actionSignature),
            ("-v", actionVerify)]



actionSignature = actionInput
actionVerify = actionInput

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    case args of
        [] -> do
            input <- getContents
            action input
        [filename] -> do
            input <- readFile filename
            action input
        _ -> do
            putStrLn "Bad args"


