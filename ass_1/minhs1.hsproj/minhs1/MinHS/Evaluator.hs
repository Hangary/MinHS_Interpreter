module MinHS.Evaluator where

import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP  

type VEnv = E.Env Value

data Value = I Integer
             | B Bool
             | Nil
             | Cons Integer Value
             -- Others as needed
             | FunV (E.Env Value) [String] Exp
             deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
-- Constants
evalE env (Num i) = I i 
evalE env (Con "True") = B True
evalE env (Con "False") = B False

-- List
evalE env (Con "Nil") = Nil
evalE env (App (App (Con "Cons") e1) e2) = 
  let I i1 = evalE env e1;
        v2 = evalE env e2 in
      Cons i1 v2

-- Primitive Operations
evalE env (App (Prim op) e1) = 
  let v = evalE env e1 in
    case op of
      Neg   -> let I i1 = v in I (- i1)
      -- listops
      Head  -> case v of 
                Nil -> error "Head of a null list"
                Cons int ls -> I int
      Tail  -> case v of 
                Nil -> error "Tail of a null list"
                Cons int ls -> ls
      Null  -> case v of Nil          -> B True
                         Cons int ls  -> B False

evalE env (App (App (Prim op) e1) e2) = 
  let v1 = evalE env e1
      v2 = evalE env e2 in
    case op of
      -- calculate
      Add ->  let I i1 = v1; I i2 = v2 in I (i1 + i2)
      Sub ->  let I i1 = v1; I i2 = v2 in I (i1 - i2)
      Mul ->  let I i1 = v1; I i2 = v2 in I (i1 * i2)
      Quot -> let I i1 = v1; I i2 = v2 in 
                case i2 of 
                  0 -> error "divide by zero"
                  _ -> I (div i1 i2)
      -- compare
      Gt -> let I i1 = v1; I i2 = v2 in B (i1 >  i2)
      Ge -> let I i1 = v1; I i2 = v2 in B (i1 >= i2)
      Lt -> let I i1 = v1; I i2 = v2 in B (i1 <  i2)
      Le -> let I i1 = v1; I i2 = v2 in B (i1 <= i2)
      Eq -> let I i1 = v1; I i2 = v2 in B (i1 == i2)
      Ne -> let I i1 = v1; I i2 = v2 in B (i1 /= i2)

-- Variable binding
evalE env (Let bs e) =
  let getNewEnv env [] = env
      getNewEnv env (b:bs) = getNewEnv newEnv bs
        where Bind id _ [] e = b
              v = evalE env e
              newEnv = E.add env (id, v)
  in evalE (getNewEnv env bs) e

evalE env (Var id) = 
  let Just v = E.lookup env id in v

-- IfThenElse
evalE env (If e1 e2 e3) =
  case evalE env e1 of 
    B True  -> evalE env e2
    B False -> evalE env e3
    
-- Function and Application
evalE env (Recfun bind) = 
  let Bind id _ args e = bind in
    case args of 
      []  -> let v = evalE newEnv e
                 newEnv = E.add env (id, v)
             in v
      _   -> let fun = FunV newEnv args e
                 newEnv = E.add env (id, fun)
             in fun

evalE env (App f arg) =
  let FunV closure args e = evalE env f
      argV = evalE env arg
      newEnv = E.add closure (head args, argV)
  in  evalE newEnv e