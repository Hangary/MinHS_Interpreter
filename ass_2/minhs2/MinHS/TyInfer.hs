module MinHS.TyInfer where

  import qualified MinHS.Env as E
  import MinHS.Syntax
  import MinHS.Subst
  import MinHS.TCMonad
  
  import Data.Monoid (Monoid (..), (<>))
  import Data.Foldable (foldMap)
  import Data.List (nub, union, (\\))
  
  primOpType :: Op -> QType
  primOpType Gt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Ge   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Lt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Le   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Eq   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Ne   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
  primOpType Neg  = Ty $ Base Int `Arrow` Base Int
  primOpType Fst  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "a"
  primOpType Snd  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "b"
  primOpType _    = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Int)
  
  constType :: Id -> Maybe QType
  constType "True"  = Just $ Ty $ Base Bool
  constType "False" = Just $ Ty $ Base Bool
  constType "()"    = Just $ Ty $ Base Unit
  constType "Pair"  = Just
                    $ Forall "a"
                    $ Forall "b"
                    $ Ty
                    $ TypeVar "a" `Arrow` (TypeVar "b" `Arrow` (TypeVar "a" `Prod` TypeVar "b"))
  constType "Inl"   = Just
                    $ Forall "a"
                    $ Forall "b"
                    $ Ty
                    $ TypeVar "a" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
  constType "Inr"   = Just
                    $ Forall "a"
                    $ Forall "b"
                    $ Ty
                    $ TypeVar "b" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
  constType _       = Nothing
  
  type Gamma = E.Env QType
  
  initialGamma :: Gamma
  initialGamma = E.empty
  
  
  -- tv is to get all type variables for a given type
  tv :: Type -> [Id]
  tv = tv'
   where
     tv' (TypeVar x) = [x]
     tv' (Prod  a b) = tv a `union` tv b
     tv' (Sum   a b) = tv a `union` tv b
     tv' (Arrow a b) = tv a `union` tv b
     tv' (Base c   ) = []
  
  -- similar to tv, but for a given qtype
  tvQ :: QType -> [Id]
  tvQ (Forall x t) = filter (/= x) $ tvQ t
  tvQ (Ty t) = tv t
  
  
  -- get all type variables from the gamma environment
  tvGamma :: Gamma -> [Id]
  tvGamma = nub . foldMap tvQ
  
  
  infer :: Program -> Either TypeError Program
  infer program = do (p',tau, s) <- runTC $ inferProgram initialGamma program
                     return p'
  
  
  unquantify :: QType -> TC Type
  {-
  Normally this implementation would be possible:
  
  unquantify (Ty t) = return t
  unquantify (Forall x t) = do x' <- fresh
                               unquantify (substQType (x =:x') t)
  
  However as our "fresh" names are not checked for collisions with names bound in the type
  we avoid capture entirely by first replacing each bound
  variable with a guaranteed non-colliding variable with a numeric name,
  and then substituting those numeric names for our normal fresh variables
  -}
  unquantify = unquantify' 0 emptySubst
  unquantify' :: Int -> Subst -> QType -> TC Type
  unquantify' i s (Ty t) = return $ substitute s t
  unquantify' i s (Forall x t) = do x' <- fresh
                                    unquantify' (i + 1)
                                                ((show i =: x') <> s)
                                                (substQType (x =:TypeVar (show i)) t)
  
                                                
  -- unify is to form a substition for two given types
  unify :: Type -> Type -> TC Subst
  -- base type
  unify (Base c1) (Base c2)
    | c1 == c2 = return emptySubst
    | c1 /= c2 = typeError $ TypeMismatch (Base c1) (Base c2)
  -- sum type
  unify (Sum t11 t12) (Sum t21 t22) = do
    s   <- unify t11 t21
    s'  <- unify t12 t22
    return $ s <> s'
  -- product type
  unify (Prod t11 t12) (Prod t21 t22) = do
    s   <- unify t11 t21
    s'  <- unify t12 t22
    return $ s <> s'
  -- function type
  unify (Arrow t11 t12) (Arrow t21 t22) = do
    s   <- unify t11 t21
    s'  <- unify t12 t22
    return $ s <> s'  
  -- with type var
  unify (TypeVar v1) (TypeVar v2)
    | v1 == v2 = return emptySubst
    | v1 /= v2 = return $ v2 =: TypeVar v1
  unify (TypeVar v) t
    | v `elem` tv t = return emptySubst -- recursive
    | otherwise = return $ v =: t
  unify t (TypeVar v) = unify (TypeVar v) t
  
  
  -- generalise is to quantify a polymorphic function
  generalise :: Gamma -> Type -> QType
  generalise g t = let all_tvs = tv t 
                       gamma_tvs = tvGamma g
                       free_tvs = all_tvs \\ gamma_tvs
                   in  foldr Forall (Ty t) free_tvs
  
  
  -- infer the whole program
  inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
  inferProgram env bs = case bs of
    [Bind "main" user_type [] e] -> do
      (e', t, tee) <- inferExp env e
      let t' = substitute tee t  
          e'' = allTypes (substQType tee) e' -- run the result substitution on the entire expression
          t'' = generalise env t
      return ([Bind "main" (Just t'') [] e''], t', tee)
  
  
  -- infer an expression
  inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)
  
  -- Constant
  inferExp g (Num n) = return (Num n, Base Int, emptySubst)
  -- Variable
  inferExp g (Var x) = case E.lookup g x of
    Nothing -> typeError $ NoSuchVariable x -- if cannot find this var
    Just t -> do
      t' <- unquantify t  -- remove all quantifiers
      return (Var x, t', emptySubst)
  
  -- Prim Operation
  inferExp g (Prim op) = do
    let t = primOpType op
    t' <- unquantify t  -- remove all quantifiers
    return (Prim op, t', emptySubst)
  -- Constructor
  inferExp g (Con id) = case constType id of
    Nothing -> typeError $ NoSuchConstructor id
    Just t -> do 
      t' <- unquantify t
      return (Con id, t', emptySubst)
  
  -- Function Application
  inferExp g (App e1 e2) = do 
    -- infer e1
    (e1', t1, tee) <- inferExp g e1
    -- infer e2
    (e2', t2, tee') <- inferExp (substGamma tee g) e2
    -- unify
    alpha <- fresh
    let lhs = substitute tee' t1
        rhs = Arrow t2 alpha
    u <- unify lhs rhs
    -- update the type of e1
    -- return
    return (App e1' e2', substitute u alpha, u <> tee' <> tee)
  
  -- If-Then-Else
  inferExp g (If e e1 e2) = do 
    -- infer e
    (e', t, tee) <- inferExp g e
    -- unify
    u <- unify t (Base Bool)
    -- infer e1
    let new_g = substGamma (u <> tee) g
    (e1', t1, tee1) <- inferExp new_g e1
    -- infer e2
    let new_g = substGamma (tee1 <> u <> tee) g
    (e2', t2, tee2) <- inferExp new_g e2
    u' <- unify (substitute tee2 t1) t2
    -- return
    return (If e' e1' e2', substitute u' t2, u' <> tee2 <> tee1 <> u <> tee)
  
  -- Case
  -- Note: this is the only case you need to handle for case expressions
  inferExp g (Case e cases) = case cases of 
    [Alt "Inl" [x] e1, Alt "Inr" [y] e2] -> do
      -- infer e
      (e', t, tee) <- inferExp g e
      -- infer e1
      alphaL <- fresh
      let new_g = substGamma tee (E.add g (x, Ty alphaL))
      (e1', tl, tee1) <- inferExp new_g e1
      -- infer e2
      alphaR <- fresh
      let new_g = substGamma (tee1 <> tee) (E.add g (y, Ty alphaR))
      (e2', tr, tee2) <- inferExp new_g e2
      -- unify
      u  <- unify (substitute (tee2 <> tee1 <> tee) (Sum alphaL alphaR)) (substitute (tee2 <> tee1) t)
      u' <- unify (substitute (u <> tee2) tl) (substitute u tr)
      -- return
      return (Case e' [Alt "Inl" [x] e1', Alt "Inr" [y] e2'], substitute (u' <> u) tr, u' <> u <> tee2 <> tee1 <> tee)
    _ -> typeError MalformedAlternatives
  
  -- Recursive Functions
  inferExp g (Recfun (Bind f user_type xs e)) = do
      -- infer e
      alpha1 <- fresh
      alpha2 <- fresh
      let new_g = E.add (E.add g (head xs, Ty alpha1)) (f, Ty alpha2)
      (e', t, tee) <- inferExp new_g e
      -- unify
      let lhs = substitute tee alpha2
          rhs = Arrow (substitute tee alpha1) t
      u  <- unify lhs rhs
      -- return
      let inferred_t = substitute u rhs
      return (Recfun (Bind f (Just (Ty inferred_t)) xs e'), inferred_t, u <> tee)
      
  -- Let Bindings
  -- generalise is neccessary here, for polymorphic types
  inferExp g (Let bs e2) = case bs of
    [Bind x user_type [] e1] -> do
      -- infer e1
      (e1', t, tee) <- inferExp g e1
      -- infer e2
      let qt_x = generalise (substGamma tee g) t
          new_g = E.add (substGamma tee g) (x, qt_x)
      (e2', t', tee') <- inferExp new_g e2
      -- return
      return (Let [Bind x (Just qt_x) [] e1'] e2', t', tee' <> tee)  