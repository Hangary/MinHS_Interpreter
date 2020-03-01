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
               | FunV (E.Env Value) String Exp
               | PrtlPrimV (Value -> Value)
               --deriving (Show)

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
  
  -- Variable binding
  evalE env (Let bs e) =
    case bs of 
      []    -> evalE env e
      x:xs  -> let Bind id _ args bindE = x
                   newEnv = case args of
                      [] -> E.add env (id, evalE env bindE) 
                      _  -> E.add env (id, evalE env (Recfun x))
               in evalE newEnv (Let xs e)
  
  evalE env (Var id) = 
    let Just v = E.lookup env id in v
  
  -- Letrec
  evalE env (Letrec bs e) = 
    let getPairs :: E.Env Value -> [Bind] -> [(String, Value)]
        getPairs env1 [] = []
        getPairs env1 (x:xs) =
          let Bind id _ args bindE = x
              pair = case args of
                            [] -> (id, evalE env1 bindE)
                            _  -> (id, evalE env1 (Recfun x))
          in  pair : getPairs env1 xs
        newEnv = E.addAll env (getPairs newEnv bs)
    in  evalE newEnv e
  
  
  -- Primitive Operations
  evalE env (Prim op) = 
    case op of
        Neg   -> PrtlPrimV (\(I i1) -> I (- i1))
        -- listops
        Head  -> PrtlPrimV (\l -> case l of
                    Nil -> error "Head of a null list"
                    Cons int ls -> I int)
        Tail  -> PrtlPrimV (\l -> case l of
                    Nil -> error "Tail of a null list"
                    Cons int ls -> ls)
        Null  -> PrtlPrimV (\l -> case l of
                    Nil -> B True
                    Cons int ls  -> B False)
        -- calculate
        Add ->  PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> I (i1 + i2)))
        Sub ->  PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> I (i1 - i2)))
        Mul ->  PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> I (i1 * i2)))
        Quot -> PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> case i2 of 
                    0 -> error "divide by zero"
                    _ -> I (div i1 i2)))
        -- compare
        Gt -> PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> B (i1 >  i2)))
        Ge -> PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> B (i1 >= i2)))
        Lt -> PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> B (i1 <  i2)))
        Le -> PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> B (i1 <= i2)))
        Eq -> PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> B (i1 == i2)))
        Ne -> PrtlPrimV (\(I i1) -> PrtlPrimV (\(I i2) -> B (i1 /= i2)))
  
  -- IfThenElse
  evalE env (If e1 e2 e3) =
    case evalE env e1 of 
      B True  -> evalE env e2
      B False -> evalE env e3
      
  -- Function
  evalE env (Recfun bind) = 
    let Bind id t args e = bind in
      case args of 
        []    ->  let v = evalE newEnv e
                      newEnv = E.add env (id, v)
                  in v
        [x]   ->  let fun = FunV newEnv x e
                      newEnv = E.add env (id, fun)
                  in  fun
        x:xs  ->  let Arrow t1 t2 = t
                      fun = FunV newEnv x e2
                      e2 = Recfun (Bind (id ++ " _") t2 xs e)
                      newEnv = E.add env (id, fun)
                  in  fun
  
  -- Application
  evalE env (App f arg) = 
    let argV = evalE env arg in
    case evalE env f of 
      FunV closure arg e -> let newEnv = E.add closure (arg, argV)
                             in  evalE newEnv e
      PrtlPrimV f      -> f argV
  
  
  