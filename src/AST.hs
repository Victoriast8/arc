module AST
  (
    SVar (..),
    Stmt (..),
    Arith (..),
    Assert (..),
    Fun (..),
    FunCall (..),
  )
where


type FName = String
type VName = String
type FPar = [VName]


data SVar
  -- Store variables
  = Var VName
  | ArrIdx VName Arith
  deriving (Eq, Ord, Show)


data Stmt
  -- Trivial assignment operations
  = AddAss SVar Arith
  | SubAss SVar Arith
  | XorAss SVar Arith
  | TimAss SVar Arith
  | DivAss SVar Arith
  | Swap SVar SVar

  -- Control-flow statements
  | From Assert Stmt Stmt Assert
  | If Assert Stmt Stmt Assert
  | Skip
  | Seq Stmt Stmt
  deriving (Eq, Ord, Show)

data Arith
  -- Binary arithmetic operators
  = Const Int
  | Ref SVar
  | Add Arith Arith
  | Sub Arith Arith
  | Tim Arith Arith
  | Div Arith Arith
  | Mod Arith Arith
  | Xor Arith Arith
  | BAnd Arith Arith
  | BOr Arith Arith
  deriving (Eq, Ord, Show)

data Assert
  -- Assertive operators
  = Eq Arith Arith
  | Geq Arith Arith
  | Leq Arith Arith
  | Gt Arith Arith
  | Lt Arith Arith
  | Neq Arith Arith
  | And Assert Assert
  | Or Assert Assert
  deriving (Eq, Ord, Show)


data Fun
  -- Function definitions
  = FunDec FName FPar Stmt
  | FunDef Fun Fun
  deriving (Eq, Ord, Show)


data FunCall
  -- Function invocations
  = Call FName FPar
  | Uncall FName FPar
  | Lambda FPar FPar Stmt
  deriving (Eq, Ord, Show)
