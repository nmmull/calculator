import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import System.Exit
import System.Console.Haskeline
import Text.ParserCombinators.ReadP hiding (get)
import qualified Data.Map.Strict as Map
import Data.Char

type Message = String
type Variable = String
type Number = Double
type Bindings = Map.Map Variable Number
data Expression
    = Number Number
    | Variable Variable
    | UnOp (Number -> Number) Expression
    | BinOp (Number -> Number -> Number) Expression Expression

-- Calculator Monad

type Calculator = StateT Bindings (ExceptT Message (InputT IO))

runCalculator :: Calculator a -> IO ()
runCalculator calc = do
    runInputT defaultSettings $ runExceptT $ evalStateT calc Map.empty
    return ()

-- Error Handling

throwCalcE :: String -> Calculator a
throwCalcE = lift . throwE

catchCalcE :: Calculator a -> (Message -> Calculator a) -> Calculator a
catchCalcE = liftCatch catchE

displayCalcE :: Message -> Calculator a
displayCalcE msg = do
    lift . lift . outputStrLn $ "ERROR: " ++ msg
    mainLoop

try :: Calculator a -> Calculator a
try cx = catchCalcE cx displayCalcE

-- Bindings

lookupVariable :: Variable -> Calculator Number
lookupVariable name = do
    bindings <- get
    case Map.lookup name bindings of
        Nothing -> throwCalcE $ "no variable named \"" ++ name ++ "\""
        Just value -> pure value

assignVariable :: Variable -> Number -> Calculator ()
assignVariable name value = do
    bindings <- get
    put $ Map.insert name value bindings

-- IO

getCalcInput :: Calculator String
getCalcInput = do
    mInput <- lift . lift $ getInputLine ">> "
    case mInput of
        Nothing -> throwCalcE "error"
        Just input -> return input

displayCalcOutput :: Show a => a -> Calculator ()
displayCalcOutput = lift . lift . outputStrLn . show

exitCalc :: Calculator a
exitCalc = do
    lift . lift . outputStrLn $ "goodbye for now"
    lift . lift . lift $ exitSuccess

-- Evaluation

calculate :: Expression -> Calculator Number
calculate (Number value) = pure value
calculate (Variable name) = lookupVariable name
calculate (UnOp func exp) = do
    arg <- calculate exp
    return $ func arg
calculate (BinOp func exp1 exp2) = do
    arg1 <- calculate exp1
    arg2 <- calculate exp2
    return $ func arg1 arg2

-- Parsing

checkThenReturn :: String -> a -> ReadP a
checkThenReturn str out = do
    skipSpaces
    string str
    return out

check :: String -> ReadP ()
check = flip checkThenReturn ()

parseDouble :: ReadP Double
parseDouble = do
    skipSpaces
    beforeDecimal <- munch1 isDigit
    afterDecimalOrEmpty <- (do
        char '.'
        afterDecimal <- munch1 isDigit
        return $ '.':afterDecimal)
        <++ return ""
    return $ read $ beforeDecimal ++ afterDecimalOrEmpty

parseNumber :: ReadP Expression
parseNumber = Number <$> parseDouble

parseVariableName :: ReadP String
parseVariableName = do
    skipSpaces
    firstChar <- satisfy $ liftA2 (||) (== '_') isAlpha
    rest <- munch $ liftA2 (||) (== '_') isAlphaNum
    return $ firstChar:rest

parseVariable :: ReadP Expression
parseVariable = Variable <$> parseVariableName

parseUnOp :: String -> (Number -> Number) -> ReadP (Expression -> Expression)
parseUnOp name func = checkThenReturn name $ UnOp func

parseUnOps :: ReadP (Expression -> Expression)
parseUnOps = choice $
    [ parseUnOp "-" (0 -)
    , parseUnOp "sin" sin
    ]

parseBinOp :: String -> (Number -> Number -> Number) -> ReadP (Expression -> Expression -> Expression)
parseBinOp name func = checkThenReturn name $ BinOp func

parseExpression :: ReadP Expression
parseExpression = prec1 where
    prec1 = chainl1 prec2 $ parseBinOp "+" (+) +++ parseBinOp "-" (-)
    prec2 = chainl1 prec3 $ parseBinOp "*" (*) +++ parseBinOp "/" (/)
    prec3 = chainr1 prec4 $ parseBinOp "**" (**)
    prec4 = (parseUnOps <*> prec4) <++ prec5
    prec5 = between (check "(") (check ")") parseExpression <++ parseNumber +++ parseVariable

parseAssignment :: ReadP (Variable, Expression)
parseAssignment = do
    name <- parseVariableName
    check "="
    exp <- parseExpression
    return (name, exp)

parser :: ReadP (Either Expression (Variable, Expression))
parser = do 
    out <- (Left <$> parseExpression) +++ (Right <$> parseAssignment)
    skipSpaces
    return out

parse :: String -> Calculator (Either Expression (Variable, Expression))
parse str = case filter (null . snd) $ readP_to_S parser str of
    [(out, "")] -> pure out
    xs -> throwCalcE "invalid input"

-- Main

mainLoop :: Calculator a
mainLoop = try $ do
    input <- getCalcInput
    case input of
        "quit" -> exitCalc
        expStr -> do
            expOrAssign <- parse expStr
            case expOrAssign of
                Left exp -> do
                    value <- calculate exp
                    displayCalcOutput value
                Right (name, exp) -> do
                    value <- calculate exp
                    assignVariable name value
                    displayCalcOutput value
    mainLoop

main :: IO ()
main = runCalculator mainLoop
