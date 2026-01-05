import Data.Word(Word8)
import Data.Char(isSpace)
import System.Environment(getArgs)
import Data.List.Split(splitWhen)
import qualified Data.ByteString.Lazy as ByteString

data Bit = O | I deriving (Eq, Show)

integerToBits :: Integer -> Int -> [Bit]
integerToBits 0 0 = []
integerToBits _ 0 = error "internal: number out of range"
integerToBits 0 n = replicate n O
integerToBits m n = (if odd m then I else O) : integerToBits (m `div` 2) (n - 1)

getNumber :: String -> Int -> [Bit]
getNumber text size = let number = read text in
    reverse $ integerToBits number size

bitsToWord8 :: [Bit] -> Word8
bitsToWord8 [] = 0
bitsToWord8 (bit : bits) = 2 * bitsToWord8 bits + if bit == I then 1 else 0

-- TODO configurable endianness
bitsToWord8s :: [Bit] -> [Word8]
bitsToWord8s [] = []
bitsToWord8s bits = bitsToWord8 (reverse $ take 8 bits) : bitsToWord8s (drop 8 bits)

pack :: [Bit] -> [Word8]
pack bits = if length bits /= 32
    then error $ "internal: instruction of incorrect width " ++ show (length bits)
    else reverse $ bitsToWord8s bits

-- TODO replace this with a declarative rules-based engine
encode :: String -> [Word8] -- assembles a single instruction
encode text = let parts = splitWhen isSpace text in
    case parts of
        [] -> error "blank line of assembly"
        (operation : operands) -> case operation of
            "halt" -> case operands of
                ['r':p, 'r':l] -> pack $ 
                    replicate 22 O ++
                    getNumber p 5 ++
                    getNumber l 5
                _ -> error $ "improperly formatted arguments to " ++ operation
            "jmp" -> case operands of
                ['r':c, 'r':t] -> pack $
                    [O, O, I, I] ++
                    replicate 18 O ++
                    getNumber c 5 ++
                    getNumber t 5
                _ -> error $ "improperly formatted arguments to " ++ operation
            "ld" -> case operands of
                ['r':p, 'r':t] -> pack $
                    [O, I, O, O] ++
                    replicate 18 O ++
                    getNumber p 5 ++
                    getNumber t 5
                _ -> error $ "improperly formatted arguments to " ++ operation
            "st" -> case operands of
                ['r':p, 'r':t] -> pack $
                    [O, I, O, I] ++
                    replicate 18 O ++
                    getNumber p 5 ++
                    getNumber t 5
                _ -> error $ "improperly formatted arguments to " ++ operation
            "mov" -> case operands of
                ['@':w, '$':i, 'r':t] -> pack $
                    [O, I, I] ++
                    replicate 7 O ++
                    getNumber w 1 ++
                    getNumber t 5 ++
                    getNumber i 16
                _ -> error $ "improperly formatted arguments to " ++ operation
            "sub" -> case operands of
                ['r':a, 'r':b, 'r':t] -> pack $
                    [I] ++
                    replicate 10 O ++
                    getNumber t 5 ++
                    replicate 6 O ++
                    getNumber a 5 ++
                    getNumber b 5
                _ -> error $ "improperly formatted arguments to " ++ operation
            _ -> error $ "unknown assembly instruction type " ++ operation

assemble :: String -> [Word8]
assemble [] = []
assemble text = let (line, rest) = break (== '\n') text in
    case rest of
        ('\n':remainder) -> encode line ++ assemble remainder
        _ -> error "missing newline at end of assembly file"

main :: IO ()
main = do 
    args <- getArgs
    case args of 
        [assembly, machine] -> do
            text <- readFile (assembly ++ ".nasm")
            ByteString.writeFile (machine ++ ".nano") (ByteString.pack $ assemble text)
        _ -> error "improperly formatted arguments to assembler"
    return ()