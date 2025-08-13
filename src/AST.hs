module AST
  ( Exp (..),
  )
where

type FName = String
type VName = String
type FPar  = [VName]


data Exp
  -- Constants
  = Var VName
  | Const Integer
  | ArrIdx VName Exp

  -- Function identities
  | Function FName FPar Exp
  | Lambda FPar Exp

  -- Trivial assignment operations
  | PlsAss Exp Exp
  | SubAss Exp Exp
  | XorAss Exp Exp
  | TimAss Exp Exp
  | DivAss Exp Exp

  -- Control-flow statements
  | From Exp Exp Exp Exp
  | If Exp Exp Exp Exp
  | Skip
  | Seq Exp Exp
  | Call FName FPar
  | Uncall FName FPar

  -- Binary operators
  | Pls Exp Exp
  | Sub Exp Exp
  | Tim Exp Exp
  | Div Exp Exp
  | Mod Exp Exp
  | Xor Exp Exp
  | And Exp Exp
  | Or Exp Exp

  -- Assertive expressions
  | Eq Exp Exp
  | Neq Exp Exp
  deriving (Eq, Ord, Show)
