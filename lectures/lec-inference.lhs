---
title: Type Inference
---

**Adapted from the superb notes of [Martin Grabmueller][Grabmueller]**

> {-# LANGUAGE TupleSections, FlexibleContexts, TypeSynonymInstances, NoMonomorphismRestriction, OverlappingInstances, FlexibleInstances #-}

> import qualified Data.Map as Map
> import qualified Data.Set as Set
> import Data.Functor ((<$>))
> import Control.Monad.Error
> import Control.Monad.Reader
> import Control.Monad.State
> import qualified Text.PrettyPrint as PP

In this lecture, we will formalize the [Hindley-Milner][Damas82] 
[type inference][Milner78] algorithm, a function of whose type is 

~~~~~{.haskell}
typeInference :: TypeEnv -> Exp -> Either String Type
~~~~~

That is, the function takes as input a type-environment containing the
types for variables (think "symbol table"), an expression whose type is
to be inferred, and returns either an error string, if the expression has a
type error, or the type inferred for the expression.

Inference, Informally
=====================

Assume that you are given the library functions

~~~~~{.haskell}
gtI :: Int -> Int -> Bool
gtA :: forall a. a -> a -> Bool
~~~~~

Now, lets consider the following Haskell functions, and
informally figure out how we can infer their types *automatically*.

~~~~~{.haskell}
-- Example 1
pos = \x -> if ((gtI x) 0) then True else False
 
TPOS := TX -> TBODY = Int -> Bool 
Bool := TBODY
Bool := TBODY := Bool
TCOND := Bool
gtI   := Int -> (Int -> Bool)
TX    := Int 
~~~~~

~~~~~{.haskell}
-- Example 2
id  = \x -> x 

TID   := TX -> TBODY = forall a. a -> a
BODY := TX 

~~~~~

~~~~~{.haskell}
-- Example 3
goo = \x -> (\y -> x) 

TGOO := TX -> TBODY   = forall a, b. a -> b -> a  

TBODY := TY -> TBODY' = TY -> TX
TBODY' := TX

~~~~~

~~~~~{.haskell}
-- Example 4
compose = \f -> \g -> \x -> f (g x)) 

TCOMP := TF -> TG -> TX -> TBODY
       = forall a b c. (b -> c) -> (a -> b) -> a -> c 

TG    := TGi -> TGo = TX -> TGo 
TGi   := TX 
TF    := TFi -> TFo = TGo -> TBODY 
TFi   := TGo
TFo   := TBODY

~~~~~

~~~~~{.haskell}
-- Example 5
max = \x -> \y -> ite (gtA x y) x y


ite :: Bool -> BRANCH -> BRANCH -> BRANCH

ite :: forall a. Bool -> a -> a -> a
gtA :: forall a. a -> a -> Bool
gtA :: TTHING -> TTHING -> Bool

TMAX := forall TTHING. TTHING -> TTHING -> TTHING
  
TX   := TBRANCH
TY   := TBRANCH

TBODY := TTHING
~~~~~


Preliminaries
=============

We start by defining the datatypes (abstract syntax) for

- *expressions* (represented by `Exp`), 
- *types* (represented by `Type`), and *schemes* (`Scheme`), and
- *environments* (represented by `TypeEnv`).

Expressions
-----------

First, we define expressions, which is a direct translation
of the different cases of lambda-calculus expressions.

> data Exp     =  EVar EVar            -- x 
>              |  ELit Lit             -- 0,1,2,true,false
>              |  EApp Exp Exp         -- e1 e2
>              |  EAbs EVar Exp        -- \x -> e
>              |  ELet EVar Exp Exp    -- let x = e1 in e2
>              deriving (Eq, Ord)

We use a wrapped variant of `String` to represent *program* 
variables

> newtype EVar = EV String deriving (Eq, Ord)

and we use `Lit` to denote the primitive *constants* in the
language

> data Lit     =  LInt Integer
>              |  LBool Bool
>              deriving (Eq, Ord)

Types and Schemes
-----------------

Next, we define the different types for our language. 
In essence, a type is either a base type `TInt` or `TBool`, 
or function types denoted by `TArr` or a type variables `TVar`.

> data Type    =  TVar TVar        -- a  
>              |  TInt             -- Int
>              |  TBool            -- Bool
>              |  TArr Type Type   -- t1 -> t2
>              deriving (Eq, Ord)
>
> newtype TVar = TV String deriving (Eq, Ord)
>
> data Scheme  =  Forall [TVar] Type       -- forall a. a -> a -> Bool

In order to provide readable output and error messages, we define
several pretty-printing functions for the abstract syntax, that are
shown at the end of this file.

Environments
------------

The last datatype needed to formalize the behavior of the type inference
algorithm is an *environment* (think "symbol table") that contains the
type schemes of all the *free variables* that appear in the expression
to be analyzed. In other words, the environment maps each *externally*
defined variable to its scheme

> newtype TypeEnv = TypeEnv (Map.Map EVar Scheme)

We will use the helper function `env \\ (x, s)` to replace 
the binding for variable `x` in the environment `env` with `s`

> (\\) :: TypeEnv -> (EVar, Scheme) -> TypeEnv
> (TypeEnv env) \\ (x, s) =  TypeEnv $ Map.insert x s env



Unification
===========

As we saw in our informal overview, the algorithm proceeds by
generating fresh type variables for unknown types, and then 
traverses the expression to equate or *unify* the types of the
sub-expressions based on how they are used with each other. 
This unification, in turn, proceeds by *substituting* an 
occurrence of a type variable's occurrence in a type, with a 
whole type (eg substituting a type variable with some function 
type.) Next, we formalize the notion of substitution, and use
it to define the unification procedure.

Free Variables and Substitution
-------------------------------

First, we define substitutions, which are finite mappings from 
type variables to types.

> type Subst = Map.Map TVar Type

Next, we define the function `apply` that takes a substitution 
and applies it to a type by replacing variables in the type with 
the corresponding mapping in the substitution (if one exists.)

For example, if we have the substitution that maps type variable 
`a` to the type `c -> d`, then when we *apply* the substitution
to the type `a -> a` we should get the output `(c -> d) -> (c -> d)`.

*But lookout!* What if we were to apply the substitution to 
the *scheme* `Forall a. a -> a -> b` ? Now, due to the quantifier, 
the scheme is $\alpha$-equivalent (ie the same except for the
name of the quantified variable) to `Forall z. z -> z -> b`.

Thus, we should not substitute the `a` inside the scheme as 
they are *bound* by the quantifier. Instead, we should only 
substitute instances of *free* type variables, like `b` which
are *not bound* by any quantifier. And so, we need to be 
careful about the free type variables, when applying substitutions. 

It turns out that we will have to define the `apply` function 
for other kinds of values, like schemes, environments and so on, 
and so it is useful to create a typeclass

> class Substitutable a where 
>   apply     :: Subst -> a -> a
>   freeTvars :: a -> Set.Set TVar

that contains two operations, which respectively carry out the
substitution, and determine the set of free type variables.

Next, we formally define substitutions for types

> instance Substitutable Type where
>   apply _  TInt            = TInt
>   apply _  TBool           = TBool
>   apply su t@(TVar a)      = Map.findWithDefault t a su 
>   apply su (t1 `TArr` t2)  = apply su t1 `TArr` apply su t2

>   freeTvars TInt           =  Set.empty
>   freeTvars TBool          =  Set.empty
>   freeTvars (TVar a)       =  Set.singleton a
>   freeTvars (t1 `TArr` t2) =  freeTvars t1 `Set.union` freeTvars t2

and schemes 

> instance Substitutable Scheme where
>   apply s (Forall as t)   = Forall as $ apply s' t 
>                             where s' = foldr Map.delete s as 
>
>   freeTvars (Forall as t) = (freeTvars t) `Set.difference` (Set.fromList as)

Note that the free variables in a scheme are those in the underlying type
*minus* the variables quantified over by the scheme (since those are
*bound* inside the scheme.) Further, the `apply` function avoids
substituting the bound variables, by using a substitution `s'` from 
which the mappings for the scheme's quantified variables have 
been deleted.

Often, we will want to lift `Substitutable` to lists

> instance Substitutable a => Substitutable [a] where
>   apply     = map . apply
>   freeTvars = foldr Set.union Set.empty . map freeTvars

We use the above to make environments `Substitutable`

> instance Substitutable TypeEnv where
>   apply s   (TypeEnv env) =  TypeEnv   $ Map.map (apply s) env
>   freeTvars (TypeEnv env) =  freeTvars $ Map.elems env


It is useful to define an empty substitution (that leaves a type unchanged) 

> empSubst  ::  Subst
> empSubst  =   Map.empty

and we can *compose* two substitutions as `su1 after su2` 

> after         :: Subst -> Subst -> Subst
> su1 `after` su2 = (Map.map (apply su1) su2) `Map.union` su1

which yields a single substitution that carries out
the substitutions in `su2` *after which* it carries 
out those in `su1`.



Most General Unifier
--------------------

Armed with the above, we can now formally define the 
notion of type *unification*. Recall that we want 
unification to have the following example informal 
behavior:

**T1**     **T2**        **Unified**     **Substitution**
-------	   ---------     -----------     ------------------
`a`        `Int`         `Int`           `a := Int` 
`a`        `b`           `b`             `a := b`
`a->b`     `a->d`        `a->d`          `b := d`
`a->Int`   `Bool->b`     `Bool->Int`     `a := Bool, b:=Int`
`Int`      `Int`         `Int`           `empSubst`
`Int`      `Bool`        *Error*         *Error*
`Int`      `a->b`        *Error*         *Error*
`a`        `a->Int`      *Error*         *Error*

The first few cases are where unification is indeed possible,
and the last few cases are where it fails corresponding to a
*type error* in the source program. The very last case is an 
interesting one; the failure is because the type variable `a` 
in the first type *occurs* free inside the second type. Thus,
if we try substituting `a` with `a->Int` we will just keep 
spinning forever! Hence, this also throws a unification 
failure. 

**Exercise:** Write a Haskell program that is rejected by the 
typechecker because it fails the above *occurs check*. 
This is not difficult, the chances are you've done this any
number of times already!

Here is the unification function `mgu` that takes two types as 
input and either returns a successful unified output along with 
the substitution (as shown in the table above) or an error string
explaining the failure (hence, our use of an error monad to
describe output.)

> mgu :: Type -> Type -> HM Subst 

The `HM` monad is one that is like a `State + Error` monad. 

> mgu (l `TArr` r) (l' `TArr` r')  = do  s1 <- mgu l l'
>                                        s2 <- mgu (apply s1 r) (apply s1 r')
>                                        return (s1 `after` s2)
> mgu (TVar a) t                   = varAsgn a t
> mgu t (TVar a)                   = varAsgn a t
> mgu TInt TInt                    = return empSubst
> mgu TBool TBool                  = return empSubst
> mgu t1 t2                        = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

The function `varAsgn` attempts to assign a type variable to a type 
and return that assignment as a subsitution, but throws an error if
the variable occurs within the assigned type.

> varAsgn a t 
>   | t == TVar a                  =  return empSubst
>   | a `Set.member` (freeTvars t) =  throwError $ "occur check fails: " ++ show a ++ " in " ++ show t
>   | otherwise                    =  return $ Map.singleton a t

The name `mgu` stands for *Most-General-Unifier* ; the function is
guaranteed to find the most general unification possible (that is
not unify `a` and `b` to `Int` via the substitution `a := Int, b:= Int`.)
This property is crucial for showing that type inference returns the most
general type possible for any term (that is, `Forall a. a -> a` and not
`Int -> Int` for the identity function `\x -> x`)


The Type Inference Algorithm
============================

We are almost ready to put the pieces together and see 
how type inference works.


Generalization and Instantiation
--------------------------------

The two remaining parts of the puzzle are the operations 

- `generalize` which takes a type like `a -> a` and converts it 
   to the scheme `Forall a. a -> a` by determining that `a` 
   is not constrained in the environment, and its dual,

- `instantiate` which takes a general type scheme and replaces the
   quantified type variables with *fresh* type variables that correspond
   to the unknown type parameters at a particular usage site.

Here is the code for the `generalize` function which simply abstracts 
a type over all type variables which are free in the type but are not free
(and hence, unconstrained) in the given type environment.

> generalize :: TypeEnv -> Type -> Scheme
> generalize env t  = Forall as t
>   where
>     as = Set.toList $ (freeTvars t) `Set.difference` (freeTvars env)

The instantiation function replaces all bound type variables in a type
scheme with *fresh* (and hence, unconstrained) type variables. Now this 
slightly tricky; how do we generate a fresh variable that is distinct 
from all previously generated variables?

We need some *state* to know what type variables have already generated,
and so we will use a `MonadState` with the following type representing 
the number of previously generated variables.
(Aside: This is exactly like the `mlabel` function from [here](root/lectures/monads.html))

> data TIState = TIState { count :: Int }

Now, the following action yields a *fresh* `Int`

> fresh :: HM Int
> fresh = do
>   s     <- get
>   let n = count s
>   put   $ s { count = n + 1 }
>   return n

and we use it to build a function that generates 
a new type variable with a given `prefix`

> freshTVar :: String -> HM Type
> freshTVar prefix = do
>   i <- fresh
>   return $ intTVar prefix i
>
> intTVar   :: String -> Int -> Type 
> intTVar p = TVar . TV . (p ++) . show 
  
Now that we can generate fresh type variables, we can define the
`instantiate` function as

> instantiate :: Scheme -> HM Type 
> instantiate (Forall as t) = do
>   as' <- mapM (\ _ -> freshTVar "a") as 
>   let s = Map.fromList $ zip as as'
>   return $ apply s t


Several operations, for example type scheme instantiation, require
fresh names for newly introduced type variables.  This is implemented
by using an appropriate monad which takes care of generating fresh
names.  It is also capable of passing a dynamically scoped
environment, error handling and performing I/O, but we will not go
into details here.

Main Type Inference Function
----------------------------

Now, we are ready to pin down the type inference function `ti` which
infers the types for expressions.  

> ti :: TypeEnv -> Exp -> HM (Subst, Type)

The function expects the precondition that the type environment
*must* contain bindings for all free variables of the expressions.
The output action is a state-and-error monad `m` containing a pair
of a substitution which records the type constraints imposed on type
variables by the expression, and the inferred type of the expression.

First, the easiest cases are the literals whose *base* types are 
trivially inferred.

> ti _ (ELit (LInt _))  = return (empSubst, TInt)
> ti _ (ELit (LBool _)) = return (empSubst, TBool)

Next, consider the case of variables. Here we simply lookup the 
environment for the scheme for the variable, and if found, create
a fresh instantiation for this particular usage of the variable.

> ti (TypeEnv env) (EVar x) = 
>     case Map.lookup x env of
>        Nothing   ->  throwError $ "Unbound Variable: " ++ show x
>        Just s    ->  instantiate s >>= return . (empSubst,) 

For the `EAbs x e` case (ie function definition) we assign the 
formal `x` a fresh type variable and analyze the body `e` using 
that type variable. Note how the substitution from the body is 
applied to the type variable for the formal to obtain the 
inferred *function* type.

> ti env (EAbs x e) =
>     do  tv       <- freshTVar "a"
>         let env' = env \\ (x, Forall [] tv)
>         (s1, t1) <- ti env' e
>         return (s1, (apply s1 tv) `TArr` t1)

Next, notice how in the `EApp e1 e2` case the recorded substitutions
for `e1` are applied to the environment before `e2` is analyzed, and
the substitutions from `e2` are composed with those for `e1` to yield 
the substitution for the entire expression.

> ti env (EApp e1 e2) =
>     do  tv       <- freshTVar "a"
>         (s1, t1) <- ti env e1
>         (s2, t2) <- ti (apply s1 env) e2
>         s3       <- mgu (apply s2 t1) (TArr t2 tv)
>         return (s3 `after` s2 `after` s1, apply s3 tv)

Finally, for the case of a let-binding `ELet x e1 e2` we infer
the type for `e1` and then *generalize* it to obtain the scheme 
bound to `x` when analyzing `e2`.

> ti env (ELet x e1 e2) =
>     do  (s1, t1) <- ti env e1
>         let env'  = apply s1 env
>             t'    = generalize env' t1
>         (s2, t2) <- ti (env' \\ (x, t')) e2
>         return (s1 `after` s2, t2)

Finally, we use the `ti` function (which returns a type inference action)
to define the `typeInference` function promised at the beginning.

> type HM a = ErrorT String (State TIState) a

> typeInference :: TypeEnv -> Exp -> Either String Type
> typeInference env e = uncurry apply <$> res
>   where act = ti env e
>         res = evalState (runErrorT act) s0 
>         s0  = TIState { count = 0 }


Testing
-------

Here are some simple expressions that we can use to test `typeInference`

> e0  =  ELet (EV "id") (EAbs (EV "x") (EVar (EV "x")))
>         (EVar (EV "id"))
> 
> e1  =  ELet (EV "id") (EAbs (EV "x") (EVar (EV "x")))
>         (EApp (EVar (EV "id")) (EVar (EV "id")))
> 
> e2  =  ELet (EV "id") (EAbs (EV "x") (ELet (EV "y") (EVar (EV "x")) (EVar (EV "y"))))
>         (EApp (EVar (EV "id")) (EVar (EV "id")))
> 
> e3  =  ELet (EV "id") (EAbs (EV "x") (ELet (EV "y") (EVar (EV "x")) (EVar (EV "y"))))
>         (EApp (EApp (EVar (EV "id")) (EVar (EV "id"))) (ELit (LInt 2)))
> 
> e4  =  ELet (EV "id") (EAbs (EV "x") (EApp (EVar (EV "x")) (EVar (EV "x"))))
>         (EVar (EV "id"))
> 
> e5  =  EAbs (EV "m") (ELet (EV "y") (EVar (EV "m"))
>                      (ELet (EV "x") (EApp (EVar (EV "y")) (ELit (LBool True)))
>                           (EVar (EV "x"))))

This simple test function tries to infer the type for the given
expression.  If successful, it prints the expression together with its
type, otherwise, it prints the error message.

> test :: Exp -> IO ()
> test e = case typeInference (TypeEnv Map.empty) e of
>            Left err  ->  putStrLn $ "error: " ++ err
>            Right t   ->  putStrLn $ show e ++ " :: " ++ show t

The main program simply infers the types for all the example expressions
and prints them together with their inferred types, or prints an error
message if type inference fails.

> main :: IO ()
> main = mapM_ test [e0, e1, e2, e3, e4, e5]




Appendix: Pretty-printing
=========================

This appendix defines pretty-printing functions and instances for
`Show` for all interesting type definitions.

> instance Show TVar where
>   showsPrec _ x = shows (prTVar x)
>
> prTVar (TV a) = PP.text a
>
> instance Show Type where
>   showsPrec _ x = shows (prType x)
> 
> prType             ::  Type -> PP.Doc
> prType (TVar a)    =   prTVar a
> prType TInt        =   PP.text "Int"
> prType TBool       =   PP.text "Bool"
> prType (TArr t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s
> 
> prParenType     ::  Type -> PP.Doc
> prParenType  t  =   case t of
>                       TArr _ _  -> PP.parens (prType t)
>                       _         -> prType t
>

> instance Show EVar where
>   showsPrec _ x = shows (prEVar x)
>
> instance Show Exp where
>   showsPrec _ x = shows (prExp x)
>
> prEVar (EV x)          = PP.text x
>
> prExp                  ::  Exp -> PP.Doc
> prExp (EVar x)         =   prEVar x
> prExp (ELit lit)       =   prLit lit
> prExp (ELet x b body)  =   PP.text "let" PP.<+> 
>                            prEVar x PP.<+> PP.text "=" PP.<+>
>                            prExp b PP.<+> PP.text "in" PP.$$
>                            PP.nest 2 (prExp body)
> prExp (EApp e1 e2)     =   prExp e1 PP.<+> prParenExp e2
> prExp (EAbs x e)       =   PP.char '\\' PP.<+> prEVar x PP.<+>
>                            PP.text "->" PP.<+>
>                            prExp e
>                                                                    
> 
> prParenExp    ::  Exp -> PP.Doc
> prParenExp t  =   case t of
>                     ELet _ _ _  -> PP.parens (prExp t)
>                     EApp _ _    -> PP.parens (prExp t)
>                     EAbs _ _    -> PP.parens (prExp t)
>                     _           -> prExp t
> 
> instance Show Lit where
>     showsPrec _ x = shows (prLit x)
> 
> prLit            ::  Lit -> PP.Doc
> prLit (LInt i)   =   PP.integer i
> prLit (LBool b)  =   if b then PP.text "True" else PP.text "False"
> 
> instance Show Scheme where
>     showsPrec _ x = shows (prScheme x)
> 
> prScheme                ::  Scheme -> PP.Doc
> prScheme (Forall as t)  =   PP.text "All" PP.<+>
>                             PP.hcat (PP.punctuate PP.comma (map prTVar as))
>                             PP.<> PP.text "." PP.<+> prType t


[Grabmueller]: http://www.grabmueller.de/martin/www/pub/AlgorithmW.pdf
[Milner78]: http://citeseer.ist.psu.edu/viewdoc/download;jsessionid=3074270ADAA40FE5DD5958E8722A0C1D?doi=10.1.1.67.5276&rep=rep1&type=pdf 
[Damas82]: http://portal.acm.org/citation.cfm?id=582176
