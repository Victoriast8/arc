module Parser
  ( parse,
  )
where

import AST
import Data.Char (isAlpha, isAlphaNum, isDigit)

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
    "uncall",
    "fun"
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
empty = []


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
    '\\' -> "\\" : (tokenize ss) -- ... also special case for lambda backslash
    -- chunk operators and other gibberish (errors occur during evaluation)
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
prog t = funDef t


funDef :: Tokens -> ParseStep Exp
funDef t =
  let (t',first) = funDec t
   in case (t',first) of
        ([],Right f1) -> ([],Right f1)
        (t'',Right f1) ->
          let (t''',second) = funDef t''
           in case second of
                -- NOTE: Reuse of the AST Seq Exp Exp construct is justified,
                -- as its evaluation is semantically what is necessary.
                -- Do not view this as a necessary evil,
                -- rather view it as a more elegant, less heavy solution.
                Right f2 -> (t''',Right $ Seq f1 f2)
                Left e -> (empty,Left e)
        (_,Left e) -> (empty,Left e)



funDec :: Tokens -> ParseStep Exp
funDec t =
  case t of
    ("fun":fname:t') | (isAlpha $ head fname) && all isAlphaNum fname ->
      let (t'',fpar) = fPar t'
          (t''',expr) = sExp1 t''
       in case (fpar,expr) of
            (Right fp, Right e) ->
              (t''',Right $ Function fname fp e)
            (Left e,_) -> (empty,Left e)
            (_,Left e) -> (empty,Left e)
    ("fun":fname:_) -> (empty,Left $ "Invalid function id " ++ fname)
    _ -> (empty,Left "Declare functions using the \"fun\" token (initial token not found)")


fPar :: Tokens -> ParseStep Exp
fPar t = (fParFix . fPar') t


fPar' :: Tokens -> ParseStep Exp
fPar' t = 
  case t of
    (var:t') | all isAlphaNum var ->
      let (t'',fpar) = fPar t'
       in case fpar of
            Right (FPar vs) ->
              (t'',Right $ FPar (var:vs))
            _ -> (t',Right $ FPar [var])
    t' -> (empty,Left $ "Expected function parameters, got these tokens: " ++ show t')


-- | Fixes a necessary evil.
-- Tokenization skips whitespaces without having 
-- an effect on statements or sequencing,
-- meaning some statements may spill into others.
-- Parsing does not concern itself with deciding 
-- the amount of parameters a function takes,
-- so it may chunk too many; this results in
-- statements that lead with a variable or more
-- (at the time of writing this, only injective 
-- assignations have single leading variables)
-- gets chunked and become invalid.
-- To fix this, we manually backtrack when
-- creating parameters, which is the wrong
-- solution, but the best/only "quick fix"
-- for this type of error, that does not
-- require having to refactor the parser.
fParFix :: ParseStep Exp -> ParseStep Exp
fParFix (t,expr) =
  case (snd $ sExp2 t,expr) of
    -- risks redundant computations
    (Left _,Right (FPar vs)) -> fParFix ((last vs:t),Right $ FPar $ init vs)
    _ -> (t,expr)


sExp1 :: Tokens -> ParseStep Exp
sExp1 t =
  let (t',first) = sExp2 t
   in case first of
        Right x -> 
          let (t'',second) = sExp1 t'
           in case second of
                Right y -> (t'', Right $ Seq x y)
                Left _  -> (t',first)
        Left e -> (empty,Left e)


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
                      "+=" -> Right $ AddAss (Var var) v 
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
                              "+=" -> Right $ AddAss (ArrIdx var v1) v2 
                              "-=" -> Right $ SubAss (ArrIdx var v1) v2
                              "^=" -> Right $ XorAss (ArrIdx var v1) v2
                              "*=" -> Right $ TimAss (ArrIdx var v1) v2
                              "/=" -> Right $ DivAss (ArrIdx var v1) v2
                              _ -> Left $ "Operator " ++ assop ++ " invalid"
                       in (t'''',assExp)
                    (Left e,_) -> (empty,Left e)
                    (_,Left e) -> (empty,Left e)
            _ -> (empty,Left $ "Expected \"]\" after " ++ var ++ "\"[\"" )
    (var1:"<=>":var2:t') | all isAlphaNum var1 && all isAlphaNum var2 ->
      (t',Right $ Swap var1 var2)
    ("if":t') ->
      let (t'',cond1) = assert1 t'
       in case t'' of
            ("then":t''') ->
              let (t'''',expr1) = sExp1 t'''
               in case t'''' of
                    ("else":t5) ->
                      let (t6,expr2) = sExp1 t5
                       in case t6 of
                            ("fi":t7) ->
                              let (t8,cond2) = assert1 t7
                               in case (cond1,expr1,expr2,cond2) of
                                    (Right c1,Right e1,Right e2,Right c2) ->
                                      (t8,Right $ If c1 e1 e2 c2)
                                    (Left e,_,_,_) -> (empty,Left e)
                                    (_,Left e,_,_) -> (empty,Left e)
                                    (_,_,Left e,_) -> (empty,Left e)
                                    (_,_,_,Left e) -> (empty,Left e)
                                    _ -> (empty,Left "Unexpected if-statement error")
                            _ -> (empty,Left "Expected \"fi\" keyword in if-statement")
                    _ -> (empty,Left "Expected \"else\" keyword in if-statement")
            _ -> (empty,Left "Expected \"then\" keyword in if-statement")
    ("from":t') ->
      let (t'',cond1) = assert1 t'
       in case t'' of
            ("do":t''') ->
              let (t'''',expr1) = sExp1 t'''
               in case t'''' of
                    ("loop":t5) ->
                      let (t6,expr2) = sExp1 t5
                       in case t6 of
                            ("until":t7) ->
                              let (t8,cond2) = assert1 t7
                               in case (cond1,expr1,expr2,cond2) of
                                    (Right c1,Right e1,Right e2,Right c2) ->
                                      (t8,Right $ If c1 e1 e2 c2)
                                    (Left e,_,_,_) -> (empty,Left e)
                                    (_,Left e,_,_) -> (empty,Left e)
                                    (_,_,Left e,_) -> (empty,Left e)
                                    (_,_,_,Left e) -> (empty,Left e)
                                    _ -> (empty,Left "Unexpected until-loop parsing error")
                            _ -> (empty,Left "Expected \"until\" keyword in until-loop")
                    _ -> (empty,Left "Expected \"loop\" keyword in until-loop")
            _ -> (empty,Left "Expected \"do\" keyword in until-loop")
    ("skip":t') -> (t',Right $ Skip)
    _ -> callFun t


assert1 :: Tokens -> ParseStep Exp
assert1 t =
  let (t',first) = assert2 t
   in case t' of
        (aop:t'') | aop `elem` aop1 ->
          let (t''',second) = assert1 t''
           in case (first,second) of
                (Right v1, Right v2) ->
                  let expr = case aop of
                        "&&" -> Right $ And v1 v2
                        "||" -> Right $ Or v1 v2
                        _ -> Left $ "Operator " ++ aop ++ " invalid"
                   in (t''', expr)
                (Left e,_) -> (empty,Left e)
                (_,Left e) -> (empty,Left e)
        _ -> (t',first)


assert2 :: Tokens -> ParseStep Exp
assert2 t =
  let (t',first) = aExp1 t
   in case t' of
        (aop:t'') | aop `elem` aop2 ->
          let (t''',second) = aExp1 t''
           in case (first,second) of
                (Right v1, Right v2) ->
                  let expr = case aop of
                        "==" -> Right $ Eq v1 v2
                        ">=" -> Right $ Geq v1 v2
                        "<=" -> Right $ Leq v1 v2
                        ">" -> Right $ Gt v1 v2
                        "<" -> Right $ Lt v1 v2
                        "!=" -> Right $ Neq v1 v2
                        _ -> Left $ "Operator " ++ aop ++ " invalid"
                   in (t''',expr)
                (Left e,_) -> (empty,Left e)
                (_,Left e) -> (empty,Left e)
        _ -> (t',first)


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
                       "+" -> Right $ Add v1 v2
                       "-" -> Right $ Sub v1 v2
                       "^" -> Right $ Xor v1 v2
                       "*" -> Right $ Tim v1 v2
                       "/" -> Right $ Div v1 v2
                       "%" -> Right $ Mod v1 v2
                       "&" -> Right $ BAnd v1 v2
                       "|" -> Right $ BOr v1 v2
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
      (t',Right $ Const (read con :: Int))
    (var:t') | all isAlphaNum var && not (var `elem` keywords) -> 
      (t',Right $ Var var)
    (var:"[":t') -> 
      let res = aExp1 t'
       in case res of
            (("]":t''),Right v) -> (t'',Right $ ArrIdx var v)
            (_,Left e) -> (empty,Left e)
            _ -> (empty,Left $ "Expected \"]\" after " ++ var ++ " [")
    -- TODO: add primitive array functions
    (x:_) -> (empty,Left ("Unknown token " ++ show x))
    _ -> (empty,Left "Empty tokenset")


callFun :: Tokens -> ParseStep Exp
callFun t =
  case t of 
    ("call":fname:t') | all isAlphaNum fname ->
      let (t'',fpars) = fPar t'
       in case fpars of
            Right fpars' -> (t'',Right $ Call fname fpars')
            Left e       -> (empty,Left e)
    ("uncall":fname:t') | all isAlphaNum fname ->
      let (t'',fpars) = fPar t'
       in case fpars of
            Right fpars' -> (t'',Right $ Uncall fname fpars')
            Left e       -> (empty,Left e)
    ("(":"\\":t') ->
      let (t'',fpb) = fPar t'
       in case t'' of
            ("->":t''') ->
              let (t'''',expr) = sExp1 t'''
               in case t'''' of
                    (")":t5) ->
                      let (t6,fpi) = fPar t5
                       in case (fpb,expr,fpi) of
                            (Right p1, Right e, Right p2) ->
                              (t6,Right $ Lambda p1 e p2)
                            (Left e,_,_) -> (empty,Left e)
                            (_,Left e,_) -> (empty,Left e)
                            (_,_,Left e) -> (empty,Left e)
                    _ -> (empty,Left $ "Expected \")\" to enclose lambda expression")
            _ -> (empty,Left $ "Expected \"->\" token between parameters and expression")
    t' -> (empty,Left $ "Unexpected error in tokenset: " ++ show t')


parse :: String -> Either Error Exp
parse "" = Left "Input program found to be empty"
parse s =
  let tokens = tokenize s
   in snd $ prog tokens
