import System.Environment
import System.IO
import Data.List
import Text.Printf
import Numeric (showHex, showBin)
import System.Random

data Point = Point {
    x :: Integer,
    y :: Integer
} | InfPoint deriving (Read, Eq)

instance Show Point where
    show (Point x y) = printf "Point {\nx: 0x%x\ny: 0x%x\n}" x y
    show InfPoint = "Point in infinity"


data Curve = Curve {
    p :: Integer,
    a :: Integer,
    b :: Integer,
    g :: Point,
    n :: Integer,
    h :: Integer
} deriving (Read)

showAttribute :: (String, String) -> String
showAttribute (key, value) = key ++ ": " ++ value
    -- where alignedValue = unlines [x lines value]

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

data Key = Key {
    d :: Integer,
    q_key :: Integer
} deriving (Read)

instance Show Key where
    show (Key d q_key) = printf "Key {\nd: 0x%x\nQ: %x\n}" d q_key

data Hash = Integer

data Signature = Signature {
    r :: Integer,
    s :: Integer
} deriving (Read)

data PublicKey = PublicKey {
    q_pubkey :: Integer
} deriving (Read)

--show0xHex :: Integral -> String
show0xHex n = "0x" ++ showHex n ""

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
pointToPubKey (Point x1 y1) = 0x40

actionKeygen = do
    let curve = exampleCurve
        Curve p a b g n h = curve -- TODO input
    gen <- getStdGen
    let (d, _) = random gen
    let q = pointToPubKey $ pointMult curve d g
    let key = Key d q
    putStrLn $ show key



dispatch :: [(String, String -> String)]
dispatch = [("-i", actionInternal),
            ("-k", actionKey),
            ("-s", actionSignature),
            ("-v", actionVerify)]

actionInternal :: String -> String
--actionInternal inputString = show (read inputString :: Curve)
actionInternal inputString = show exampleCurve

actionKey = actionInternal
actionSignature = actionInternal
actionVerify = actionInternal

main = do
    (command:args) <- getArgs
    let (Just function) = lookup command dispatch
    case args of
        [] -> getContents >>= putStrLn . function
        [filename] -> readFile filename >>= putStrLn . function
        _ -> putStrLn "Bad args"

