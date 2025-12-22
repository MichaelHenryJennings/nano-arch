import Data.Word(Word8)
import Data.Char(isSpace)
import System.Environment(getArgs)
import Data.List.Split(splitWhen)
import qualified Data.ByteString.Lazy as ByteString

integerToBools :: Integer -> Int -> [Bool]
integerToBools 0 0 = []
integerToBools _ 0 = error "internal: number out of range"
integerToBools 0 n = replicate n False
integerToBools m n = odd m : integerToBools (m `div` 2) (n - 1)

getNumber :: String -> Int -> [Bool]
getNumber text size = let number = read text in
    if number < 0 || number > 255
        then error "number out of range"
        else reverse $ integerToBools number size

boolsToWord8 :: [Bool] -> Word8
boolsToWord8 [] = 0
boolsToWord8 (bool : bools) = 2 * boolsToWord8 bools + if bool then 1 else 0

boolsToWord8s :: [Bool] -> (Word8, Word8)
boolsToWord8s bools = if length bools /= 16 
    then error "internal: machine code of wrong length" 
    else (boolsToWord8 $ reverse $ drop 8 bools, boolsToWord8 $ reverse $ take 8 bools)
    -- TODO configurable endianness

-- TODO replace this with a declarative rules-based engine
encode :: String -> (Word8, Word8) -- assembles a single instruction
encode text = let parts = splitWhen isSpace text in
    case parts of
        [] -> error "blank line of assembly"
        (operation : operands) -> case operation of
            "halt" -> case operands of
                ['r':p, 'r':l] -> boolsToWord8s $ 
                    replicate 10 False ++
                    getNumber p 3 ++
                    getNumber l 3
                _ -> error $ "improperly formatted arguments to " ++ operation
            "jmp" -> case operands of
                ['r':c, 'r':t, '+':h] -> boolsToWord8s $
                    [False, False, True, True] ++
                    replicate 5 False ++
                    [h /= "0" && (h == "1" || error "bad boolean")] ++
                    getNumber c 3 ++
                    getNumber t 3
                _ -> error $ "improperly formatted arguments to " ++ operation
            "ld" -> case operands of
                ['r':p, 'r':t] -> boolsToWord8s $
                    [False, True, False, False] ++
                    replicate 6 False ++
                    getNumber p 3 ++
                    getNumber t 3
                _ -> error $ "improperly formatted arguments to " ++ operation
            "st" -> case operands of
                ['r':p, 'r':t] -> boolsToWord8s $
                    [False, True, False, True] ++
                    replicate 6 False ++
                    getNumber p 3 ++
                    getNumber t 3
                _ -> error $ "improperly formatted arguments to " ++ operation
            "mov" -> case operands of
                ['@':w, '$':i, 'r':t] -> boolsToWord8s $
                    [False, True, True] ++
                    getNumber w 2 ++
                    getNumber i 8 ++
                    getNumber t 3
                _ -> error $ "improperly formatted arguments to " ++ operation
            "sub" -> case operands of
                ['r':a, 'r':b, 'r':t] -> boolsToWord8s $
                    [True, False, False, False, False, False, False] ++
                    getNumber a 3 ++
                    getNumber b 3 ++
                    getNumber t 3
                _ -> error $ "improperly formatted arguments to " ++ operation
            _ -> error $ "unknown assembly instruction type " ++ operation

assemble :: String -> [Word8]
assemble [] = []
assemble text = let (line, rest) = break (== '\n') text in
    case rest of
        ('\n':remainder) -> let (front, back) = encode line in
            front : back : assemble remainder
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
    