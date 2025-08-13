module Parser
  ( tokenize,
    parse,
  )
where

import AST (Exp (..))
import Data.Char (isAlphaNum, isDigit)

keywords :: [String]
keywords =
  [ "if",
    "fi",
    "then",
    "else",
    "from",
    "do",
    "loop",
    "until",
    "skip",
    "call",
    "uncall"
  ]

op1 :: [String]
op1 =
  [ "+=",
    "-=",
    "^=",
    "*=",
    "/="
  ]

op2 :: [String]
op2 =
  [ "+",
    "-",
    "^",
    "*",
    "/",
    "%",
    "&",
    "|"
  ]

aop1 :: [String]
aop1 =
  [ "&&",
    "||"
  ]

aop2 :: [String]
aop2 =
  [ "==",
    "<",
    "<=",
    ">",
    ">=",
    "!="
  ]

whitespace :: [Char]
whitespace =
  [ ' ',
    '\n'
  ]

type Error = String
type Tokens = [String]
type ParseStep a = (Tokens, Either Error a)

empty :: Tokens
empty = [""]


-- | Fully tokenize an input stream.
-- Perhaps better to chunk tokens one at a time,
-- rather than tokenize the whole stream all at once,
-- to allow for interleaving steps of tokenization and parsing.
tokenize :: String -> Tokens
tokenize "" = []
tokenize (s:ss) =
  case s of
    -- ignore whitespaces
    x | x `elem` whitespace -> tokenize ss
    -- chunk coherent alphanumericals (constants, variables, keywords, etc.)
    x | isAlphaNum x ->
      let (token,remaining) = loop [x] ss
       in token : (tokenize remaining)
        where
          loop p s' =
            case s' of
              (h:t) | isAlphaNum h ->
                let p' = p ++ [h]
                 in loop p' t
              _ -> (p,s')
    -- chunk parentheses separately, even though this should rarely be necessary
    '(' -> "(" : (tokenize ss)
    ')' -> ")" : (tokenize ss)
    -- chunk operators and other gibberish (errors occur in evaluation)
    x -> let (token,remaining) = loop [x] ss
          in token : (tokenize remaining)
           where
             loop p s' =
               case s' of
                 (h:t) | not (h `elem` whitespace || isAlphaNum h) -> 
                    let p' = p ++ [h]
                     in loop p' t
                 _ -> (p,s')


-- Parsing
prog :: Tokens -> ParseStep Exp
prog t = sExp1 t -- TODO: implement


sExp1 :: Tokens -> ParseStep Exp
sExp1 t =
  let first = sExp2 t
   in case first of
        ([""],_)   -> (empty,Left "Tokenset is empty")
        (_,Left e) -> (empty,Left e)
        (t', Right x) -> 
          let second = sExp1 t'
           in case second of
                (_,Right y) -> (t, Right $ Seq x y)
                (_,Left _)  -> first


sExp2 :: Tokens -> ParseStep Exp
sExp2 t =
  case t of
    -- injective expression into variable
    (var:assop:t') | assop `elem` op1 ->
      let (t'',con) = aExp1 t'
       in case con of
            Right v -> 
              let assExp = 
                    case assop of
                      "+=" -> Right $ PlsAss (Var var) v 
                      "-=" -> Right $ SubAss (Var var) v
                      "^=" -> Right $ XorAss (Var var) v
                      "*=" -> Right $ TimAss (Var var) v
                      "/=" -> Right $ DivAss (Var var) v
                      _ -> Left $ "Operator " ++ assop ++ " invalid"
               in (t'',assExp)
            Left e  -> (empty,Left e)
    -- injective expression into variable array using expression index
    (var:"[":t') -> 
      let (t'',const1) = aExp1 t'
       in case t'' of
            ("]":assop:t''') | assop `elem` op1 ->
              let (t'''',const2) = aExp1 t'''
               in case (const1,const2) of
                    (Right v1, Right v2) -> 
                      let assExp = 
                            case assop of
                              "+=" -> Right $ PlsAss (ArrIdx var v1) v2 
                              "-=" -> Right $ SubAss (ArrIdx var v1) v2
                              "^=" -> Right $ XorAss (ArrIdx var v1) v2
                              "*=" -> Right $ TimAss (ArrIdx var v1) v2
                              "/=" -> Right $ DivAss (ArrIdx var v1) v2
                              _ -> Left $ "Operator " ++ assop ++ " invalid"
                       in (t'''',assExp)
                    (Left e,_) -> (empty,Left e)
                    (_,Left e) -> (empty,Left e)
            _ -> (empty,Left $ "Expected ] after " ++ var ++ "[" )
    -- TODO: Swap operator
    --       if-assertion
    --       from-do-until-loop
    --       skip
    --       Function calls
    _ -> (empty,Left "")


assert :: Tokens -> ParseStep Exp
assert t = undefined -- TODO: implement


-- | Exp1 := Exp2 Op2 Exp1
--         | Exp2
aExp1 :: Tokens -> ParseStep Exp
aExp1 t =
  let (t',first) = aExp2 t
   in case t' of
        (binop:t'') | binop `elem` op2 ->
          let (t''',second) = aExp1 t''
          in case (first,second) of
               (Right v1, Right v2) ->
                 let expr = case binop of
                       "+" -> Right $ Pls v1 v2
                       "-" -> Right $ Sub v1 v2
                       "^" -> Right $ Xor v1 v2
                       "*" -> Right $ Tim v1 v2
                       "/" -> Right $ Div v1 v2
                       "%" -> Right $ Mod v1 v2
                       "&" -> Right $ And v1 v2
                       "|" -> Right $ Or v1 v2
                       _ -> Left $ "Operator " ++ binop ++ " invalid"
                  in (t''',expr)
               (Left e,_) -> (empty,Left e)
               (_,Left e) -> (empty,Left e)
        _ -> (t',first)


-- | Exp2 := Int
--         | Var
--         | Var[Exp1]
--         | TODO: ...
aExp2 :: Tokens -> ParseStep Exp
aExp2 t = 
  case t of
    (con:t') | all isDigit con -> 
      (t',Right $ Const (read con :: Integer))
    (var:t') | all isAlphaNum var && not (var `elem` keywords) -> 
      (t',Right $ Var var)
    (var:"[":t') -> 
      let res = aExp1 t'
       in case res of
            (("]":t''),Right v) -> (t'',Right $ ArrIdx var v)
            (_,Left e) -> (empty,Left e)
            _ -> (empty,Left $ "Expected ] after " ++ var ++ " [")
    (x:_) -> (empty,Left ("Unknown token " ++ show x))


parse :: String -> Either Error Exp
parse "" = Left "Input program found to be empty"
parse s =
  let tokens = tokenize s
   in snd $ prog tokens
