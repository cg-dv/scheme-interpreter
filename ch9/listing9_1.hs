import Control.Monad
import Control.Monad.Error
import Data.Array
import Data.Complex
import Data.IORef
import Data.Ratio
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import System.Environment
import System.IO

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol) 
                let atom = first : rest
                return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False 
                    _    -> Atom atom

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChars <|> noneOf "\"\\" 
                char '"'
                return $ String x

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal1 <|> parseDecimal2) 
                  char '+'
                  y <- (try parseFloat <|> parseDecimal1 <|> parseDecimal2)
                  char 'i' 
                  return $ Complex (toDouble x :+ toDouble y)

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseRational :: Parser LispVal
parseRational = do x <- many1 digit
                   char '/'
                   y <- many1 digit
                   return $ Ratio ((read x) % (read y))

parseNumber :: Parser LispVal
parseNumber = parseDecimal1
          <|> parseDecimal2
          <|> parseHex
          <|> parseOct
          <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x 

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10") 
              return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs 

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser LispVal
parseCharacter = do
 try $ string "#\\"
 value <- try (string "newline" <|> string "space")
         <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
 return $ Character $ case value of
   "space" -> ' '
   "newline" -> '\n'
   otherwise -> (value !! 0)

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\' -> x
                    '"' -> x
                    'n' -> '\n' 
                    'r' -> '\r' 
                    't' -> '\t' 

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail 

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`' 
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do arrayValues <- sepBy parseExpr spaces
                 return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRational
        <|> try parseNumber
        <|> try parseQuoted
        <|> try parseBool
        <|> try parseCharacter
        <|> try (do string "#("
                    x <- parseVector
                    char ')'
                    return x)
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x
        <|> parseQuasiQuoted
        <|> parseUnQuote

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form 
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                        ++ " args; found values " ++ unwordsList found 
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                        ++ ", found " ++ show found 
showError (Parser parseErr)             = "Parse error at " ++ show parseErr 

instance Show LispError where show = showError
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             Bool True -> eval env conseq 
             _ -> throwError $ TypeMismatch "boolean" pred 
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "No clause in cond expression: " form
  else case head clauses of
    List [Atom "else", expr] -> eval env expr
    List [test, expr]        -> eval env $ List [Atom "if",
                                            test,
                                            expr,
                                            List (Atom "cond" : tail clauses)]
    _ -> throwError $ BadSpecialForm "Unrecognized special form " form
{-eval env form@(List (Atom "case" : key : clauses)) =-}
  {-if null clauses-}
  {-then throwError $ BadSpecialForm "No clause in case expression: " form-}
  {-else case head clauses of-}
    {-List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last-}
    {-List ((List datums) : exprs) -> do-}
      {-result <- eval env key-}
      {-equality <- liftThrows $ mapM (\x -> eqv [result, x]) datums-}
      {-if Bool True `elem` equality-}
        {-then mapM (eval env) exprs >>= return . last-}
        {-else eval env $ List (Atom "case" : key : tail clauses)-}
    {-_ -> throwError $ BadSpecialForm "Unrecognized special form " form-}
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
      if num params /= num args && varargs == Nothing
         then throwError $ NumArgs (num params) args
         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length params) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) body
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
apply (IOFunc func) args = func args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol2string", unaryOp symbol2string),
              ("string2symbol", unaryOp string2symbol),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-length", stringLen),
              ("string-ref", stringRef),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort ),
                ("close-output-port", closePort ),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 [] 
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op 

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)    = Bool True 
symbolp _           = Bool False 
numberp (Number _)  = Bool True 
numberp _           = Bool False 
stringp (String _)  = Bool True 
stringp _           = Bool False 
boolp   (Bool _)    = Bool True 
boolp   _           = Bool False 
listp   (List _)    = Bool True 
listp   (DottedList _ _)    = Bool True 
listp   _           = Bool False 

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)      = String s
symbol2string _             = String "" 
string2symbol (String s)    = Atom s
string2symbol _             = Atom "" 

unpackNum :: LispVal -> ThrowsError Integer 
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n 
                             else return $ fst $ parsed !! 0 

unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

stringLen :: [LispVal] -> ThrowsError LispVal
stringLen [(String s)] = Right $ Number $ fromIntegral $ length s
stringLen [notString]  = throwError $ TypeMismatch "string" notString
stringLen badArgList   = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)]
    | length s < k' + 1 = throwError $ Default "Out of bounds" 
    | otherwise         = Right $ String $ [s !! k']
    where k' = fromIntegral k
stringRef [(String s), notNum] = throwError $ TypeMismatch "number" notNum
stringRef [notString, _]       = throwError $ TypeMismatch "string" notString 
stringRef badArgList           = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]                  = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]              = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]              = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]                  = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)]      = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [list1@(List arg1), list2@(List arg2)]                  = eqvList eqv [list1, list2] 
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList 

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (all eqvPair $ zip arg1 arg2)
      where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val 

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [list1@(List arg1), list2@(List arg2)] = eqvList equal [list1, list2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val 

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces) 

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env 

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives)
                                               ++ map (makeFunc PrimitiveFunc) primitives)
     where makeFunc constructor (var, func) = (var, constructor func)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

runOne :: [String] -> IO ()
runOne args = do
    env primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme>>> ") . evalAndPrint

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var 

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =   do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe  (throwError $ UnboundVar "Setting an unbound variable" var)
                                    (liftIO . (flip writeIORef value))
                                    (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

data LispVal = Atom String
             | Vector (Array Int LispVal) 
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Complex (Complex Double) 
             | Float Double 
             | Ratio Rational 
             | Number Integer
             | String String
             | Bool Bool 
             | Character Char 
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env } 
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle 
             {-deriving (Eq)-}

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError 
               | BadSpecialForm String LispVal 
               | NotFunction String String 
               | UnboundVar String String 
               | Default String 
