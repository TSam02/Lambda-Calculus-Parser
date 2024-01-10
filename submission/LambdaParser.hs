module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder


-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.

-- ###########################################
-- ##### IMPORTED/HELPER FUNCTIONS (ALL) #####
-- ###########################################

-- | parse at least one space
spaces1 :: Parser String
spaces1 = list1 space

-- | convert the variables into term
mapTerm :: [Char] -> [Builder]
-- mapTerm s = term <$> s
mapTerm = (<$>) term

-- | Imported from tutorial 11 and modified
-- | `chain p op` parses 1 or more instances of `p` separated by `op`
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
 where
  rest a =
    (do
        f <- op
        b <- p
        rest (f a b)
      )
      ||| pure a

-- | used to chain unary operator that only takes input on the right hand side
-- | and can keep calling themselves
chain2 :: Parser a -> Parser (a -> a) -> Parser a
chain2 p op = rest
 where
  rest =
    (do
        f' <- op 
        rtn <- rest 
        pure (f' rtn)
    ) ||| (do p)

-- | Parsing function that used for "if then else" statement
chainIf :: Parser Builder -> Parser (Builder -> Builder) -> Parser Builder -> Parser Builder
chainIf p op rp = rest
 where
  rest =
    (do
        f' <- op           -- ap "if" boolean construct 
        b <- p            -- condition for if
        _ <- thenP
        tb <- p            -- true return for if
        _ <- elseP
        fb <- p            -- false return for if 
        pure (ap2 (f' b) tb fb)  -- return for complete if then else statement
    ) ||| (do
        f' <- op            
        b <- rp                     
        _ <- thenP
        tb <- rp            
        _ <- elseP
        fb <- rp            
        pure (ap2 (f' b) tb fb)  
    ) ||| (do p)


-- | "combined ap" that ap 3 Builder together (from left to right)
ap2 :: Builder -> Builder -> Builder -> Builder
-- ap2 op l1 l2 = op `ap` l1 `ap` l2
ap2 = ((ap) .) . (ap)

-- | "combined ap" that ap 1 Builder with a ap"ed" 3 Builders
ap3 :: Builder -> Builder -> Builder -> Builder -> Builder
ap3 op1 op2 l2 l3 = op1 `ap` (ap2 op2 l2 l3)

-- | Function that use to evaluate less than result
-- | Made one for it as it is no a general function used by the other part of the code
lessTH :: Builder -> Builder -> Builder
lessTH l1 l2 = andCE `ap` (ap3 notCE equalCE l1 l2) `ap` (ap3 notCE lessEqualCE l2 l1)

-- | Function that use to evaluate greater or equal result
-- | Made one for it as it is no a general function used by the other part of the code
greatEQ :: Builder -> Builder -> Builder
greatEQ l1 l2 = notCE `ap` (lessTH l1 l2)

-- ###########################################
-- ############# CHURCH ENCODING #############
-- ###########################################

-- | church encoding of True
trueCE :: Builder
trueCE = boolToLam True

-- | church encoding of False
falseCE :: Builder
falseCE = boolToLam False

-- | church encoding of if
ifCE :: Builder
ifCE = 'b' `lam` 't' `lam` 'f' `lam` term 'b' `ap` term 't' `ap` term 'f'

-- | church encoding of not
notCE :: Builder
notCE = 'x' `lam` ifCE `ap` term 'x' `ap` falseCE `ap` trueCE

-- | church encoding of or
orCE :: Builder
orCE = 'x' `lam` 'y' `lam` ifCE `ap` term 'x' `ap` trueCE `ap` term 'y'

-- | church encoding of and
andCE :: Builder
andCE = 'x' `lam` 'y' `lam` ifCE `ap` term 'x' `ap` term 'y' `ap` falseCE

-- | church encoding of succ of number
succCE :: Builder
succCE = 'n' `lam` 'f' `lam` 'x' `lam` term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x')

-- | church encoding of pred of number
predCE :: Builder
predCE = 'n' `lam` 'f' `lam` 'x' `lam` (term 'n' `ap` ('g' `lam` 'h' `lam` (term 'h' `ap` (term 'g' `ap` term 'f'))) `ap` ('u' `lam` term 'x') `ap` ('u' `lam` term 'u'))

-- | church encoding of addition 
addCE :: Builder
addCE = 'x' `lam` 'y' `lam` (term 'y' `ap` succCE `ap` term 'x')

-- | church encoding of subtraction
minusCE :: Builder
minusCE = 'x' `lam` 'y' `lam` (term 'y' `ap` predCE `ap` term 'x')

-- | church encoding of multiplication
timesCE :: Builder
timesCE = 'x' `lam` 'y' `lam` 'f' `lam` term 'x' `ap` (term 'y' `ap` term 'f')

-- | church encoding of exponentiation
expCE :: Builder
expCE = 'x' `lam` 'y' `lam` term 'y' `ap` term 'x'

-- | church encoding of isZero
isZeroCE :: Builder
isZeroCE = 'n' `lam` (term 'n' `ap` ('x' `lam` falseCE) `ap` trueCE)

-- | church encoding of LEQ
lessEqualCE :: Builder
lessEqualCE = 'm' `lam` 'n' `lam` (isZeroCE `ap` (minusCE `ap` term 'm' `ap` term 'n'))

-- | church encoding of EQ
equalCE :: Builder
equalCE = 'm' `lam` 'n' `lam` (andCE `ap` (lessEqualCE `ap` term 'm' `ap` term 'n') `ap` (lessEqualCE `ap` term 'n' `ap` term 'm'))

-- | church encoding of null
nullCE :: Builder
nullCE = 'c' `lam` 'n' `lam` term 'n'

-- | church encoding of cons
consCE :: Builder
consCE = 'h' `lam` 't' `lam` 'c' `lam` 'n' `lam` term 'c' `ap` term 'h' `ap` (term 't' `ap` term 'c' `ap` term 'n')

-- | Church encoding of isNull
isNullCE :: Builder
isNullCE = 'l' `lam` (term 'l' `ap` ('h' `lam` 't' `lam` falseCE)) `ap` trueCE

-- | Church encoding of head
headCE ::  Builder
headCE = 'l' `lam` (term 'l' `ap`  ('h' `lam` 't' `lam` term 'h')) `ap` falseCE

-- | Church encoding of tail
tailCE :: Builder
tailCE = 'l' `lam` 'c' `lam` 'n' `lam` (term 'l' `ap` ('h' `lam` 't' `lam` 'g' `lam` term 'g' `ap` term 'h' `ap` (term 't' `ap` term 'c'))) `ap` ('t' `lam` term 'n') `ap` ('h' `lam` 't' `lam` term 't')

-- ###########################################
-- ######## CHURCH ENCODING FUNCTIONS ########
-- ###########################################

-- | or function
or' :: Builder -> Builder -> Builder
or' = ap2 orCE

-- | and function
and' :: Builder -> Builder -> Builder
and' = ap2 andCE

-- | not function
not' :: Builder -> Builder
not' = ap notCE

-- | if function
if' :: Builder -> Builder
if' = ap ifCE

-- | add function
add' :: Builder -> Builder -> Builder
add' = ap2 addCE

-- | subtraction function 
minus' :: Builder -> Builder -> Builder
minus' = ap2 minusCE

-- | multiplication function 
times' :: Builder -> Builder -> Builder
times' = ap2 timesCE

-- | exponential function
exp' :: Builder -> Builder -> Builder
exp' = ap2 expCE

-- | equal function
equal':: Builder -> Builder -> Builder
equal' = ap2 equalCE

-- | not equal function
notEqual' :: Builder -> Builder -> Builder
notEqual' = ap3 notCE equalCE

-- | less than or equal function
lessEqual' :: Builder -> Builder -> Builder
lessEqual' = ap2 lessEqualCE

-- | greater or equal function
greatEqual' :: Builder -> Builder -> Builder
greatEqual' = greatEQ

-- | less than function
lessThan' :: Builder -> Builder -> Builder
lessThan' = lessTH 

-- | greater than function
greaterThan' :: Builder -> Builder -> Builder
greaterThan' = ap3 notCE lessEqualCE

isNull' ::Builder -> Builder
isNull' = ap isNullCE

head' :: Builder -> Builder
head' = ap headCE

tail' :: Builder -> Builder
tail' = ap tailCE

cons' :: Builder -> Builder -> Builder
cons' = ap2 consCE

-- ###########################################
-- ######## COMPONENT PARSERS (PART 1) #######
-- ###########################################

-- | Parses the char/variable used in the lambda expressions
var :: Parser Char
var = oneof "abcdefghijklmnopqrstuvwxyz"

-- | Parses the all valid char/variable used in the lambda expressions
var' :: Parser [Char]
var' = list1 var

-- | Parses the lambda symbol 
lambdaSymbol :: Parser String
lambdaSymbol = string "λ" 

-- | Parses the dot symbol
dotSymbol :: Parser Char
dotSymbol = is '.'

-- | Parse single open bracket
openBracP :: Parser Char
openBracP = spaces >> is '('

-- | Parse single close bracket
closeBracP :: Parser Char
closeBracP = spaces >> is ')'

-- | Parses the short from argument (allows multiple variables connected)
shortArgumentList :: Parser [Char] 
shortArgumentList = (do  
        _ <- lambdaSymbol
        as <- var'
        _ <- dotSymbol
        pure as
        )

-- | Parses the long from argument (only 1 variable allowed)
longArgumentList :: Parser Char 
longArgumentList = do 
        _ <- lambdaSymbol
        a <- var
        _ <- dotSymbol
        pure a

-- | Parse the body of the lambda calculus
body :: Parser Builder  
body = (do 
        _ <- openBracP 
        fb <- var'                  -- parses all the alphabet
        _ <- closeBracP 
        let termLst = mapTerm fb
        pure $ foldl (ap) (head termLst) (tail termLst)     
    ) ||| (do 
        fb <- var
        let termLst = mapTerm [fb]
        pure $ foldl (ap) (head termLst) (tail termLst) 
        )  


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "((((λx.(λy.y)))))"                               
-- Result >< \xy.y
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '\955'

-- | parses the body of a lambda calculus (can be variable only or combination of variables and another lambda calculus)
-- | (long form)
longBodyExpr :: Parser Builder
longBodyExpr = (do 
    bs <- body                  
    pure bs
    ) ||| longLambdaPAux

-- | Parses a long form lambda calculus (wihout application of more than 1 long form)
longLambdaPAux :: Parser Builder
longLambdaPAux = (do
    _ <- openBracP
    as <- longArgumentList
    bs <- list1 longBodyExpr
    _ <- closeBracP
    pure $ foldr (lam) ((foldl (ap) (head bs) (tail bs))) ([as])
    ) ||| (do
    _ <- openBracP
    rtn <- longLambdaPAux
    _ <- closeBracP
    pure rtn
    )

longLambdaP :: Parser Lambda
longLambdaP = do
    lamExprs <- list1 longLambdaPAux    -- to hanlde application
    pure $ build $ foldl (ap) (head lamExprs) (tail lamExprs)

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "(λb.(λt.bt))(λxy.xx)"
-- Result >< (\bt.bt)\xy.xx
-- 
-- >>> parse shortLambdaP "λb.(λft.btf(bt)(λa.a)t)"
-- Result >< \bft.btf(bt)(\a.a)t
-- 
-- >>> parse shortLambdaP "((((λxy.xx))))"
-- Result >< \xy.xx
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

-- | parses the body of a lambda calculus (can be variable only or combination of variables and another lambda calculus) 
-- | (short form)
shortBodyExpr ::  Parser Builder
shortBodyExpr = (do 
    bs <- body
    pure bs
    ) ||| shortLambdaPAux

-- | Parses a short form lambda calculus (wihout application of more than 1 short form)
shortLambdaPAux :: Parser Builder
shortLambdaPAux =  (do   -- parse the bracket and recurse
    _ <- openBracP
    rtn <- shortLambdaPAux
    _ <- closeBracP
    pure rtn
    ) ||| (do            -- parses internal
    as <- shortArgumentList
    bs <- list1 shortBodyExpr
    pure $ foldr (lam) ((foldl (ap) (head bs) (tail bs))) (as)
    ) 

shortLambdaP :: Parser Lambda
shortLambdaP = (do
    lamExprs <- list1 shortLambdaPAux
    pure $ build $ foldl (ap) (head lamExprs) (tail lamExprs)
    ) 

-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--
lambdaP :: Parser Lambda
lambdaP = longLambdaP ||| shortLambdaP


{-|
    Part 2
-}
-- ###########################################
-- ######## COMPONENT PARSERS (PART 2) #######
-- ###########################################

-- | general purpose parser for operator that is longer than 1 char
strOp ::  String -> Parser String
strOp s = spaces >> string s >> pure s

-- | Parser for True 
trueP :: Parser Builder
trueP = strOp "True" >> pure trueCE    

-- | Parser for False
falseP :: Parser Builder
falseP = strOp "False" >> pure falseCE

-- | Parser for or
orP :: Parser (Builder -> Builder -> Builder)
orP = space >> strOp "or" >> space >> pure or'

-- | Parser for and
andP :: Parser (Builder -> Builder -> Builder)
andP = space >> strOp "and" >> space >> pure and'

-- | Parser for not
notP :: Parser (Builder -> Builder)
notP = strOp "not" >> space >> pure not'

-- | Parser for if
ifP :: Parser (Builder -> Builder)
ifP = strOp "if" >> space >> pure if'

-- | Parser for then
thenP :: Parser ()
thenP = space >> strOp "then" >> space >> pure ()

-- | Parser for else
elseP :: Parser ()
elseP = space >> strOp "else" >> space >> pure ()

-- | Parser for logical literals
logicLiteral :: Parser Builder
logicLiteral = trueP ||| falseP

-- | Parser for boolean expression that separated by "or" (for complex calc)
logicExpr :: Parser Builder
logicExpr = chain andExpr orP

-- | Parser for boolean expression that separated by "and" (for complex calc)
andExpr :: Parser Builder
andExpr = chain notExpr andP

-- | Parser for boolean expression that separated by "not" (for complex calc)
notExpr :: Parser Builder
notExpr = chain2 ifExpr notP

-- | Parser for boolean expression that separated by "if then else" (for complex calc)
ifExpr :: Parser Builder
ifExpr = chainIf compareExpr ifP logicExpr

-- | Parser for in/equality expression
compareExpr :: Parser Builder
compareExpr = chain arithExpr inequality

-- | Parser for add expression 
arithExpr :: Parser Builder
arithExpr = chain timesExpr addP 

-- | Parser for times expression
timesExpr :: Parser Builder
timesExpr = chain expExpr timesP 

-- | Parser for exponential expression and Bracket (precedence issue)
expExpr :: Parser Builder
expExpr = chain (naturalNumCE ||| group ||| logicLiteral) powerP 
  where group = openBracP *> logicExpr <* closeBracP

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
--
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "if True then (True or not False) and False else False"
-- Result >< Just False

-- logicP :: Parser Lambda
-- logicP = do
--     res <- logicExpr
--     pure $ build res

logicP :: Parser Lambda
logicP = do
    res <- logicExpr
    pure $ build res

-- ###########################################
-- ###### COMPONENT PARSERS (PART 2 EX2) #####
-- ###########################################

-- | parse a single char operator (Imported from Week 11 lecture notes)
charOp :: Char -> Parser Char
charOp c = spaces >> is c >> pure c

-- | Parsing single digit 
digit :: Parser Char
digit = oneof "0123456789"

-- | Parse natural number in Int form
naturalNum :: Parser Int
naturalNum = spaces >> (do
    sd <- oneof "123456789"             -- parse the start/first digit (not 0)
    rd <- list digit                    -- parse the remaining digits (if any)
    let n = read (sd : rd)              -- concatenate the digit and convert to Int
    pure n
    )

-- | Parsing natural number and convert to Church Encoding form
naturalNumCE :: Parser Builder
naturalNumCE = spaces >> (do
    num <- naturalNum
    pure (intToLam num)
    )

-- | parser for addition and substraction
addP :: Parser (Builder -> Builder -> Builder)
addP = (charOp '+' >> pure add') ||| (charOp '-' >> pure minus')

-- | parser for multiplication 
timesP :: Parser (Builder -> Builder -> Builder)
timesP = (charOp '*' >> pure times')

-- | parser for exponentiation (power)
powerP :: Parser (Builder -> Builder -> Builder)
powerP = (strOp "**" >> pure exp')

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minusCE= λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13
basicArithmeticP :: Parser Lambda
basicArithmeticP = do
    res <- chain naturalNumCE addP
    pure $ build res


-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68
-- 
-- lamToInt <$> parse arithmeticP "100 - (2 * 2)**(4-1)"
-- Result >< Just 36
-- 
-- lamToInt <$> parse arithmeticP "(100 - (3 * 4))**(3-1)"
-- Result >< Just 7744
arithmeticP :: Parser Lambda
arithmeticP = do
    res <- arithExpr
    pure (build $ res)


-- ###########################################
-- ###### COMPONENT PARSERS (PART 2 EX2) #####
-- ###########################################

-- | parser for 
inequality :: Parser (Builder -> Builder -> Builder)
inequality = (strOp "==" >> pure equal') ||| (strOp "!=" >> pure notEqual') ||| (strOp "<=" >> pure lessEqual') |||
             (strOp "<" >> pure lessThan') ||| (strOp ">=" >> pure greatEqual') ||| (strOp ">" >> pure greaterThan')

-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- |
-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
-- 
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False
-- 
-- >>> lamToInt <$> parse complexCalcP "1 + (if (1+1 == 2) then 3 else 2)"
-- Result >< Just 4

complexCalcP :: Parser Lambda
complexCalcP = do
    res <- logicExpr
    pure $ build res


{-|
    Part 3
-}

-- ###########################################
-- ###### COMPONENT PARSERS (PART 3 EX1) #####
-- ###########################################

-- | Parse open square bracket
openSquaBrac :: Parser Char
openSquaBrac = spaces >> is '['

-- | Parse close square bracket
closeSquaBrac :: Parser Char
closeSquaBrac = spaces >> is ']'

-- | Parse comma symbol
commaSymbol :: Parser Char
commaSymbol = spaces >> is ','

-- | Parse isNull
isNullP :: Parser Builder
isNullP = spaces >> strOp "isNull" >> space >> pure isNullCE

-- | Parse head
headP :: Parser Builder
headP = spaces >> strOp "head" >> space >> pure headCE

-- | Parse tail
tailP :: Parser Builder
tailP = spaces >> strOp "tail" >> space >> pure tailCE

consOpP :: Parser (Builder -> Builder -> Builder)
consOpP = spaces >> strOp "cons" >> space >> pure cons'

-- | Combination of parser of listOp (isNull/head/tail)
listOp :: Parser Builder
listOp = isNullP ||| headP ||| tailP

-- | Parsing the element of the list
-- | It can be any valid data type in part 2
elemTypeP :: Parser Builder
elemTypeP = logicExpr ||| zero
  where
    zero = do           -- handling 0 as the parsers in part 2 only meant for Natural numbers
        _ <- spaces
        d <- digit
        pure $ intToLam $ read [d]

-- | Parser that use to handle the process of parsing of element inside the list
elemP :: Parser Builder
elemP = (do
        element <- elemTypeP ||| listPAux
        _ <- commaSymbol
        rest <- elemP
        pure $ ap2 consCE element rest
    ) ||| (do
        element <- elemTypeP ||| listPAux
        pure $ ap2 consCE element nullCE
    )

-- | Auxilary Parser for list that return a Parser of Builder
listPAux :: Parser Builder
listPAux = (do
        _ <- openSquaBrac
        elements <- elemP
        _ <- closeSquaBrac
        pure elements
    ) ||| (do
        _ <- openSquaBrac
        _ <- closeSquaBrac
        pure nullCE
    )

-- | Handling the parsing of cons and its input
consP :: Parser Builder
consP = (do
    consOp <- consOpP
    e1 <- listOpPAux ||| elemTypeP
    e2 <- listOpPAux ||| consP ||| listPAux
    pure $ consOp e1 e2
    )

-- | Handling the parsing of other list operator other than cons
listOpPAux :: Parser Builder
listOpPAux = (do
        ops <- list1 listOp
        lst <- listPAux ||| consP 
        pure $ foldr (ap) (lst) (ops)
    ) 

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- | 
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\t_.t)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedChar '0'
-- 
-- >>> parse listP "[0, True, [], 1+1]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\t_.t)((\htcn.ch(tcn))(\cn.n)((\htcn.ch(tcn))((\xy.y(\nfa.f(nfa))x)(\f.f)(\f.f))\cn.n)))

listP :: Parser Lambda
listP = do
    res <- listPAux
    pure $ build res

-- | 
-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head tail [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull tail [1]"      
-- Result >< Just True
--
-- >>> lamToInt <$> parse listOpP "head [1 + (if 1*2 != 2 then 3 else 4*5), False]"
-- Result >< Just 21
--
-- >>> lamToInt <$> parse listOpP "head tail cons 1 cons 2 cons 5 [3,4]"
-- Result >< Just 2
--
-- >>> lamToInt <$> parse listOpP "head tail cons head [9,5,6] tail [2,3,4]"
-- Result >< Just 3
listOpP :: Parser Lambda
listOpP = (do
    res <- listOpPAux ||| consP
    pure $ build res
    )

-- | Exercise 2

-- | Compare the value and obtain the boolean result in Church encoding form
compareLamInt :: Builder -> Int -> ParseResult Bool
compareLamInt n t = do
    (case lamToBool $ build $ equal' n (intToLam t) of
        Just a -> Result "" a 
        Nothing -> Error UnexpectedEof)

-- | Parsing whole numbers
wholeNumber :: Parser Int
wholeNumber = naturalNum ||| zero
  where
    zero = do        
        _ <- spaces
        d <- is '0'
        pure $ read [d]

-- ###########################################
-- ############### FIBONACCI #################
-- ###########################################

-- | Parse fib operator
fibP :: Parser ()
fibP = spaces >> strOp "fib" >> space >> pure ()

-- | Calculation of fibonacci sequence
fibPAux :: Builder -> Parser Builder
fibPAux n 
    | Result _ True <- (compareLamInt n 0) = pure $ n              -- base case (Compare in lambda calculus form)
    | Result _ True <- (compareLamInt n 1) = pure $ n              -- base case (")
    | otherwise = do 
        f1 <- fibPAux (minus' n (intToLam 1))
        f2 <- fibPAux (minus' n (intToLam 2))                      -- Extract the add function
        pure $ add' f1 f2

-- | Parse fibonacci 
-- >>> lamToInt <$> parse fibonacciP "fib 8"
-- Result >< Just 21
--
-- >>> lamToInt <$> parse fibonacciP "fib 13"
-- Result >< Just 233
-- 
-- >>> lamToInt <$> parse fibonacciP "fib    0"
-- Result >< Just 0
-- 
-- >>> lamToInt <$> parse fibonacciP "fib5"    
-- UnexpectedChar '5'

fibonacciP :: Parser Lambda
fibonacciP = do
    _ <- fibP
    n <- wholeNumber 
    res <- fibPAux (intToLam n)
    pure $ build res

-- ###########################################
-- ############### FACTORIAL #################
-- ###########################################

-- | Parse factorial operator "!"
facP :: Parser ()
facP = spaces >> charOp '!' >> pure ()

-- | Calculation of factorial sequence
facPAux :: Builder -> Parser Builder
facPAux n 
    | Result _ True <- (compareLamInt n 0) = pure $ intToLam 1
    | otherwise = do 
        f1 <- facPAux(minus' n (intToLam 1))
        pure $ times' f1 n

-- | Parse factorial
-- >>> lamToInt <$> parse factorialP "5!"
-- Result >< Just 120
--
-- >>> lamToInt <$> parse factorialP "0!"
-- Result >< Just 1
--
-- >>> lamToInt <$> parse factorialP "7!"
-- Result >< Just 5040
-- 
-- >>> parse factorialP "3!"
-- Result >< (\xyf.x(yf))((\xyf.x(yf))((\xyf.x(yf))(\f.f)((\xy.y(\nfa.n(\gh.h(gf))(\u.a)\u.u)x)((\xy.y(\nfa.n(\gh.h(gf))(\u.a)\u.u)x)(\fx.f(f(fx)))(\f.f))(\f.f)))((\xy.y(\nfa.n(\gh.h(gf))(\u.a)\u.u)x)(\fx.f(f(fx)))(\f.f)))\fx.f(f(fx))
factorialP :: Parser Lambda
factorialP = do
    n <- wholeNumber
    _ <- facP
    res <- facPAux (intToLam n)
    pure $ build res

    
-- ###########################################
-- ################# FOLDR ###################
-- ###########################################
-- | Parse the foldr operator
foldrOpP :: Parser ()
foldrOpP =  spaces >> strOp "foldr" >> pure ()

-- | binary operators (inequality and arithmetic which does not need space before or after them)
arithEqualityOp :: Parser (Builder -> Builder -> Builder)
arithEqualityOp = do
    _ <- space
    op <- addP ||| timesP ||| powerP ||| inequality
    _ <- space
    pure op

-- | Check whether the list is null and return the boolean 
checkIsNull :: Builder -> Bool
checkIsNull ta = case lamToBool $ build $ isNull' ta of
    Just True -> True
    Just False -> False
    Nothing -> False

-- | Auxilary function of foldr to handle the folding process
foldrPAux :: (Builder -> Builder -> Builder) -> Builder -> Builder -> Parser Builder
foldrPAux op b ta 
    | checkIsNull ta = pure b
    | otherwise = do
        acc <- foldrPAux op b (tail' ta)
        pure $ op (head' ta) acc

-- | Parsing foldr
-- >>> lamToInt <$> parse foldrP "foldr + 1 [3,4,5]"
-- Result >< Just 13
-- 
-- >>> lamToInt <$> parse foldrP "foldr + 1+2 [3,4,5]"
-- Result >< Just 15
-- 
-- >>> lamToInt <$> parse foldrP "foldr + 1+2 tail [3,4,5]"
-- Result >< Just 12
--
-- >>> lamToBool <$> parse foldrP "foldr and True [True, False, True]"  
-- Result >< Just False
--
-- >>> lamToInt <$> parse foldrP "foldr + head [100,3]  tail [3,4,5]"
-- Result >< Just 109
foldrP :: Parser Lambda
foldrP = do
    _ <- foldrOpP
    op <- andP ||| orP ||| inequality ||| arithEqualityOp ||| (space >> consOpP)
    b <- elemTypeP  ||| listOpPAux                              -- any data type declared in part 2 and 3 except list
    ta <- listP ||| listOpP                                     -- can be list or opeartion on list that return a list
    res <- foldrPAux op b (lamToBuilder ta)
    pure $ build res













