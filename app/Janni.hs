{-# LANGUAGE LambdaCase #-}

module Janni where

import Control.Arrow
import Control.Exception hiding (try)
import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import qualified Debug.Trace as Debug
import Text.Parsec
import Text.Parsec.String

data JanniVal
  = JString String
  | JNumber Float
  | JChar Char
  | JBool Bool
  | JArray Int [JanniVal]
  | JanniAssignment (String, JanniVal)
  | JValue String
  | JOperation Operation
  deriving (Show)

type Env = IORef [(String, IORef JanniVal)]

data Operation = Operation Operation Operation Operator | Operand JanniVal deriving (Show)

data Operator = Sum | Subtraction | Mul deriving (Show)

main = do
  let expr1 = "var arr = {1, 2, 3, 4, 5};"
      expr2 = "\"test\""
      expr3 = "5.5"
      expr4 = "var test = \"teststring\""
  env <- newIORef [] :: IO (IORef [(String, IORef JanniVal)])
  let result = parse parseInstruction "janni" expr4
  case result of
    (Right r) -> do
      runExceptT (runReaderT (eval r) env) >>= print
      debug env
    (Left l) -> print l
  pure ()

janniRepl = do
  env <- newIORef [] :: IO (IORef [(String, IORef JanniVal)])
  loop env
  where
    loop env = do
      instruction <- getLine
      let result = parse parseInstruction "janni" instruction
      case result of
        (Right r) ->
          runExceptT (runReaderT (eval r) env)
            >>= print
              . ( \case
                    Right value -> show value
                    Left err -> err
                )
              >> debug env
        (Left l) -> print l
      loop env

operations =
  [("+", (+))]

debug env = liftIO $ do
  readEnv <- readIORef env
  values <- mapM readSecond readEnv
  print ("environment: " <> show values)
  where
    readSecond (a, b) = readIORef b >>= (\value -> pure (a, value))

-- Right found -> eval env found
-- Left err -> print err

parseExpr =
  parseArray
    <|> parseNumber
    <|> parseString
    <|> parseChar
    <|> parseValue

parseChar = do
  JChar <$> (char '\'' *> anyChar <* char '\'')

parseValue = JValue <$> many alphaNum

isJNumber (JNumber _) = True
isJNumber _ = False

parseInstruction :: Parser JanniVal
parseInstruction =
  do
    parseAssignment
    <|> try parseOperation
    <|> parseVariable

parseOperation = do
  operand1 <- parseString <|> parseNumber <|> parseValue
  spaces
  operator <- oneOf "+*-/"
  let op = case operator of
        '+' -> Sum
        '-' -> Subtraction
        '*' -> Mul
        _ -> undefined
  spaces
  operand2 <- try parseOperation <|> parseString <|> parseNumber <|> parseValue
  pure $ JOperation (Operation (Operand operand1) (Operand operand2) op)

-- var test = 50;
-- var test1 = 5;
-- test + test1;
-- 55;

jSum :: [JanniVal] -> ReaderT Env (ExceptT String IO) Float
jSum ((JNumber n) : xs) = do
  sum <- jSum xs
  pure $ n + sum
jSum ((JValue val) : xs) = do
  env <- ask
  readEnv <- liftIO $ readIORef env
  sum <- jSum xs
  case lookup val readEnv of
    (Just var) -> do
      variable <- liftIO $ readIORef var
      case variable of
        (JNumber n) -> pure $ n + sum
        _ -> throwError "variable is not a number"
    Nothing -> throwError "variable is undefined"
jSum _ = pure 0

jConcat ((JString str) : xs) = str <> jConcat xs
jConcat ((JNumber n) : xs) = show n <> jConcat xs
jConcat _ = show ""

parseVariable :: Parser JanniVal
parseVariable = do
  expr <- parseExpr
  spaces
  char ';'
  pure expr

eval :: JanniVal -> ReaderT Env (ExceptT String IO) JanniVal
eval = \case
  val@(JString _) -> pure val
  val@(JNumber _) -> pure val
  val@(JChar _) -> pure val
  val@(JBool _) -> pure val
  val@(JArray _ _) -> pure val
  (JValue value) -> do
    env <- ask
    lift (getVar env value)
  JanniAssignment (id, val) -> do
    env <- ask
    lift (defineVar env (id, val))
  JOperation op -> do
    env <- ask
    Debug.trace (show op) lift (evalOp env op)

-- TODO complete
evalOp :: Env -> Operation -> ExceptT String IO JanniVal
evalOp _ (Operation (Operand (JNumber op1)) (Operand (JNumber op2)) Sum) = pure $ JNumber (op1 + op2)
evalOp env (Operation (Operand (JNumber op1)) (Operand (JOperation op2@(Operation _ _ _))) Sum) = do
  evalOp env op2 >>= \case
    (JNumber result2) -> pure (JNumber (op1 + result2))
    (JString result2) -> pure (JString (show op1 <> result2))
evalOp _ (Operation (Operand (JString op1)) (Operand (JString op2)) Sum) = pure $ JString (op1 <> op2)
evalOp env (Operation (Operand (JString op1)) (Operand (JOperation op2@(Operation _ _ _))) Sum) = do
  evalOp env op2 >>= \case
    (JString result2) -> pure (JString (op1 <> result2))
    (JNumber result2) -> pure (JString (op1 <> show result2))
evalOp env (Operation (Operand (JValue op1)) (Operand (JValue op2)) Sum) = do
  liftM2 (,) (getVar env op2) (getVar env op1) >>= \case
    ((JNumber n1), (JNumber n2)) -> pure (JNumber (n1 + n2))
    ((JString n1), (JString n2)) -> pure (JString (n1 <> n2))
    ((JNumber n1), (JString n2)) -> pure (JString ((show n1) <> n2))
    ((JString n1), (JNumber n2)) -> pure (JString (n1 <> show n2))
evalOp env (Operation (Operand (JValue op1)) (Operand (JOperation op2@(Operation _ _ _))) Sum) = do
  liftM2 (,) (getVar env op1) (evalOp env op2) >>= \case
    ((JNumber n1), (JNumber n2)) -> pure (JNumber (n1 + n2))
    ((JString n1), (JString n2)) -> pure (JString (n1 <> n2))
    ((JNumber n1), (JString n2)) -> pure (JString ((show n1) <> n2))
    ((JString n1), (JNumber n2)) -> pure (JString (n1 <> show n2))
evalOp env (Operation (Operand (JValue op1)) (Operand number@(JNumber number1)) Sum) = do
  liftM2 (,) (getVar env op1) (pure number) >>= \case
    ((JNumber n1), (JNumber n2)) -> pure (JNumber (n1 + n2))
    ((JString n1), (JString n2)) -> pure (JString (n1 <> n2))
    ((JNumber n1), (JString n2)) -> pure (JString ((show n1) <> n2))
    ((JString n1), (JNumber n2)) -> pure (JString (n1 <> show n2))

-- evalOp (Operation (Operand (JValue op1)) (Operand (JOperation op2@(Operation _ _ _))) Sum) = do
--   result <- evalOp op2
--   case evalOp op2 of
--     (JNumber result2) -> pure $ JNumber (op1 + result2)
--     (JString result2) -> pure $ JString (show op1 <> result2)

getVar :: Env -> String -> ExceptT String IO JanniVal
getVar env var = do
  readEnv <- liftIO $ readIORef env
  case lookup var readEnv of
    (Just val) -> liftIO $ readIORef val
    Nothing -> throwError "Variable is undefined"

defineVar :: Env -> (String, JanniVal) -> ExceptT String IO JanniVal
defineVar env (identifier, value) = do
  readEnv <- liftIO $ readIORef env
  case lookup identifier readEnv of
    (Just var) -> throwError "Variable is already defined"
    Nothing -> do
      newValue <- liftIO $ newIORef value
      liftIO $ writeIORef env (readEnv <> [(identifier, newValue)])
      pure $ JanniAssignment (identifier, value)

setVar :: Env -> (String, JanniVal) -> ExceptT String IO JanniVal
setVar env (identifier, value) = do
  readEnv <- liftIO $ readIORef env
  newVal <- liftIO $ newIORef value
  let updated = (\(id, val) -> if id == identifier then (id, newVal) else (id, val)) <$> readEnv
  liftIO $ writeIORef env updated
  pure $ JanniAssignment (identifier, value)

parseAssignment :: Parser JanniVal
parseAssignment = do
  string "var"
  spaces
  id <- many1 alphaNum
  spaces
  char '='
  spaces
  value <- parseExpr
  pure $ JanniAssignment (id, value)

openingCurlyBrace :: Parser ()
openingCurlyBrace = char '{' >> spaces

closingCurlyBrace :: Parser Char
closingCurlyBrace = spaces >> char '}'

parseArray :: Parser JanniVal
parseArray = do
  between
    openingCurlyBrace
    (closingCurlyBrace >> spaces >> char ';')
    parseArr
  where
    parseArr = do
      values <- sepBy (parseString <|> parseNumber) (char ',' >> spaces)
      pure $ JArray (length values) values

parseString :: Parser JanniVal
parseString = char '\"' *> (JString <$> many alphaNum) <* char '\"'

parseNumber :: Parser JanniVal
parseNumber = do
  JNumber . read <$> many1 digit
