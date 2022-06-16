{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Data types for the intermediate simplified-Erlang AST
--
module Language.PureScript.Erl.CodeGen.AST where

import Prelude.Compat

import Data.Text (Text)

import Control.Monad.Identity
import Control.Arrow (second)

import Language.PureScript.PSString (PSString)
import Language.PureScript.AST.SourcePos

-- |
-- Data type for simplified Erlang expressions
--
data Erl
  -- |
  -- A numeric literal
  --
  = ENumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | EStringLiteral PSString
  -- |
  -- A char literal
  --
  | ECharLiteral Char
  -- |
  -- An atom literal (possibly qualified a:b)
  --
  | EAtomLiteral Atom
  -- |
  -- A unary operator application
  --
  | EUnary UnaryOperator Erl
  -- |
  -- A binary operator application
  --
  | EBinary BinaryOperator Erl Erl
  -- |
  -- Top-level function definition (over-simplified)
  --
  | EFunctionDef (Maybe EType) (Maybe SourceSpan) Atom [Text] Erl
  -- TODO not really a separate form. and misused
  | EVarBind Text Erl
  -- |
  -- A variable
  --
  | EVar Text
  -- |
  -- A function reference f/1
  --
  | EFunRef Atom Int
  -- |
  -- A fun definition
  --
  | EFunFull (Maybe Text) [(EFunBinder, Erl)]
  -- |
  -- Function application
  --
  | EApp AppAnnotation Erl [Erl]
  -- |
  -- Block
  --
  | EBlock [Erl] -- Array should be nonempty
  -- |
  -- Tuple literal {a, 1, "C"}
  --
  | ETupleLiteral [Erl]

  | EComment Text

  | EMapLiteral [(Atom, Erl)]

  | EMapPattern [(Atom, Erl)]

  | EMapUpdate Erl [(Atom,Erl)]

  | EListLiteral [Erl]
  
  | EListCons [Erl] Erl

  | ECaseOf Erl [(EBinder, Erl)]
  -- |
  -- Attribute including raw text between the parens
  --
  | EAttribute PSString PSString
  -- Spec attribute
  | ESpec Atom EType
  | EType Atom [Text] EType

  deriving (Show, Eq)

data AppAnnotation 
  = RegularApp
  | SyntheticApp
  deriving (Show, Eq)

-- | Simple 0-arity version of EFun1
pattern EFun0 :: Maybe Text -> Erl -> Erl
pattern EFun0 name e = EFunFull name [(EFunBinder [] Nothing, e)]

-- | Simple fun definition fun f(X) -> e end (arity 1 with single head with simple variable pattern, name optional)
pattern EFun1 :: Maybe Text -> Text -> Erl -> Erl
pattern EFun1 name var e = EFunFull name [(EFunBinder [EVar var] Nothing, e)]

extractVars :: [Erl] -> Maybe [Text]
extractVars = traverse var
  where var (EVar x) = Just x
        var _ = Nothing

-- | Simple arity-N version of EFun1
pattern EFunN :: Maybe Text -> [Text] -> Erl -> Erl
pattern EFunN name vars e <- EFunFull name [(EFunBinder (extractVars -> Just vars) Nothing, e)] where
  EFunN name vars e = EFunFull name [(EFunBinder (map EVar vars) Nothing, e)]


curriedLambda :: Erl -> [Text] -> Erl
curriedLambda = foldr (EFun1 Nothing)

curriedApp :: [Erl] -> Erl -> Erl
curriedApp = flip (foldl (\fn a -> EApp RegularApp fn [a]))

litAtom :: Text -> Erl
litAtom = EAtomLiteral . Atom Nothing
qualFunCall :: Text -> Text -> [Erl] -> Erl
qualFunCall q t = EApp RegularApp (EAtomLiteral $ Atom (Just q) t)


data EFunBinder
 = EFunBinder [Erl] (Maybe Guard)

   deriving (Show, Eq)

data EBinder
  = EBinder Erl -- TODO split out literals?
  | EGuardedBinder Erl Guard

  deriving (Show, Eq)

data Guard
  = Guard Erl
  deriving (Show, Eq)

-- | Possibly qualified atom
-- TODO : This is not really an atom, each part is an atom.
data Atom
  = Atom (Maybe Text) Text
  | AtomPS (Maybe Text) PSString
  deriving (Show, Eq, Ord)
-- |
-- Built-in unary operators
--
data UnaryOperator
  -- |
  -- Numeric negation
  --
  = Negate
  -- |
  -- Boolean negation
  --
  | Not
  -- |
  -- Bitwise negation
  --
  | BitwiseNot
  -- |
  -- Numeric unary \'plus\'
  --
  | Positive
  deriving (Show, Eq)

-- |
-- Built-in binary operators
--
data BinaryOperator
  -- |
  -- Numeric addition
  --
  = Add
  -- |
  -- Numeric subtraction
  --
  | Subtract
  -- |
  -- Numeric multiplication
  --
  | Multiply
  -- |
  -- Numeric division (float)
  --
  | FDivide
  -- |
  -- Numeric division (integer)
  --
  | IDivide
  -- |
  -- Remainder
  --
  | Remainder
  -- |
  -- Generic equality test
  --
  | EqualTo
  -- |
  -- Generic inequality test
  --
  | NotEqualTo
  -- |
  -- Generic identical test
  --
  | IdenticalTo
  -- |
  -- Generic non-identical test
  --
  | NotIdenticalTo
  -- |
  -- Numeric less-than
  --
  | LessThan
  -- |
  -- Numeric less-than-or-equal
  --
  | LessThanOrEqualTo
  -- |
  -- Numeric greater-than
  --
  | GreaterThan
  -- |
  -- Numeric greater-than-or-equal
  --
  | GreaterThanOrEqualTo

  -- |
  -- Boolean and
  --
  | And
  -- |
  -- Boolean or
  --
  | Or
  -- |
  -- Boolean short-circuit and
  --
  | AndAlso
  -- |
  -- Boolean short-circuit or
  --
  | OrElse
  -- |
  -- Boolean xor
  --
  | XOr
  -- |
  -- Bitwise and
  --
  | BitwiseAnd
  -- |
  -- Bitwise or
  --
  | BitwiseOr
  -- |
  -- Bitwise xor
  --
  | BitwiseXor
  -- |
  -- Bitwise left shift
  --
  | ShiftLeft
  -- |
  -- Bitwise right shift
  --
  | ShiftRight
  
  -- |
  -- List concatenation (++)
  --
  | ListConcat
  -- |
  -- List subtraction (--)
  --
  | ListSubtract

  deriving (Show, Eq)

-- Simplified Erlang types
data EType
  = TAny
  
  | TNone
  | TPid
  | TPort
  | TReference
  | TNil
  | TAtom (Maybe Atom)
  -- bitstring
  | TFloat
  -- | TFunAny
  | TVar Text
  | TFun [EType] EType
  | TInteger -- no ranges
  | TList EType -- no improper lists
  -- maps
  | TMap (Maybe [(EType, EType)]) 
  | TTuple [EType]
  | TUnion [EType]
  | TRemote Text Text [EType]
  | TAlias Atom [EType]
  deriving (Show, Eq)

everywhereOnErl :: (Erl -> Erl) -> Erl -> Erl
everywhereOnErl f = go
  where
  go :: Erl -> Erl
  go (EUnary op e) = f $ EUnary op (go e)
  go (EBinary op e1 e2) = f $ EBinary op (go e1) (go e2)
  go (EFunctionDef t ssann a ss e) = f $ EFunctionDef t ssann a ss (go e)
  go (EVarBind x e) = f $ EVarBind x (go e)
  go (EFunFull fname args) = f $ EFunFull fname $ map (second go) args
  go (EApp meta e es) = f $ EApp meta (go e) (map go es)
  go (EBlock es) = f $ EBlock (map go es)
  go (ETupleLiteral es) = f $ ETupleLiteral (map go es)
  go (EMapLiteral binds) = f $ EMapLiteral $ map (second go) binds
  go (EMapPattern binds) = f $ EMapPattern $ map (second go) binds
  go (EMapUpdate e binds) = f $ EMapUpdate (go e) $ map (second go) binds
  go (ECaseOf e binds) = f $ ECaseOf (go e) $ map (second go) binds
  go (EListLiteral es) = f $ EListLiteral (map go es)
  go (EListCons es e) = f $ EListCons (map go es) (go e)

  go other = f other

everywhereOnErlTopDown :: (Erl -> Erl) -> Erl -> Erl
everywhereOnErlTopDown f = runIdentity . everywhereOnErlTopDownM (Identity . f)

everywhereOnErlTopDownM :: forall m. (Monad m) => (Erl -> m Erl) -> Erl -> m Erl
everywhereOnErlTopDownM f = f >=> go
  where
  f' = f >=> go

  fargs :: [(x, Erl)] -> m [(x, Erl)]
  fargs = traverse (sequence . second f')

  go (EUnary op e) = EUnary op <$> f' e
  go (EBinary op e1 e2) = EBinary op <$> f' e1 <*> f' e2
  go (EFunctionDef t ssann a ss e) = EFunctionDef t ssann a ss <$> f' e
  go (EVarBind x e) = EVarBind x <$> f' e
  go (EFunFull fname args) = EFunFull fname <$> fargs args
  go (EApp meta e es) = EApp meta <$> f' e <*> traverse f' es
  go (EBlock es) = EBlock <$> traverse f' es
  go (ETupleLiteral es) = ETupleLiteral <$> traverse f' es
  go (EMapLiteral binds) = EMapLiteral <$> fargs binds
  go (EMapPattern binds) = EMapPattern <$> fargs binds
  go (EMapUpdate e binds) = EMapUpdate <$> f' e <*> fargs binds
  go (ECaseOf e binds) = ECaseOf <$> f' e <*> fargs binds
  go (EListLiteral es) = EListLiteral <$> traverse f' es
  go (EListCons es e) = EListCons <$> traverse f' es <*> f' e
  go other = f other

-- Sorry. Really want a type that allows "child context" under binders etc
everywhereOnErlTopDownMThen :: forall m. (Monad m) => (Erl -> m (Erl, Erl -> m Erl)) -> Erl -> m Erl
everywhereOnErlTopDownMThen f = f'
  where
  f' e = do
    (x, f1) <- f e
    y <- go x
    f1 y

  fargs :: [(x, Erl)] -> m [(x, Erl)]
  fargs = traverse (sequence . second f')

  go (EUnary op e) = EUnary op <$> f' e
  go (EBinary op e1 e2) = EBinary op <$> f' e1 <*> f' e2
  go (EFunctionDef t ssann a ss e) = EFunctionDef t ssann a ss <$> f' e
  go (EVarBind x e) = EVarBind x <$> f' e
  go (EFunFull fname args) = EFunFull fname <$> fargs args
  go (EApp meta e es) = EApp meta <$> f' e <*> traverse f' es
  go (EBlock es) = EBlock <$> traverse f' es
  go (ETupleLiteral es) = ETupleLiteral <$> traverse f' es
  go (EMapLiteral binds) = EMapLiteral <$> fargs binds
  go (EMapPattern binds) = EMapPattern <$> fargs binds
  go (EMapUpdate e binds) = EMapUpdate <$> f' e <*> fargs binds
  go (ECaseOf e binds) = ECaseOf <$> f' e <*> fargs binds
  go (EListLiteral es) = EListLiteral <$> traverse f' es
  go (EListCons es e) = EListCons <$> traverse f' es <*> f' e
  go other = fst <$> f other
  
everything :: forall r. (r -> r -> r) -> (Erl -> r) -> Erl -> r
everything (<>.) f = go
  where
  go :: Erl -> r
  go e0@(EUnary _ e) = f e0 <>. go e
  go e0@(EBinary _ e1 e2) = f e0 <>. go e1 <>. go e2
  go e0@(EFunctionDef _ _ _ _ e) = f e0 <>. go e
  go e0@(EVarBind _ e) = f e0 <>. go e
  go e0@(EFunFull _ args) = foldl (<>.) (f e0) (map (go . snd) args)
  go e0@(EApp _ e es) = foldl (<>.) (f e0 <>. go e) (map go es)
  go e0@(EBlock es) = foldl (<>.) (f e0) (map go es)
  go e0@(ETupleLiteral es) = foldl (<>.) (f e0) (map go es)
  go e0@(EMapLiteral binds) = foldl (<>.) (f e0) (map (go . snd) binds)
  go e0@(EMapPattern binds) = foldl (<>.) (f e0) (map (go . snd) binds)
  go e0@(EMapUpdate e binds) = foldl (<>.) (f e0 <>. go e) (map (go . snd) binds)
  go e0@(ECaseOf e binds) = foldl (<>.) (f e0 <>. go e) (map (go . snd) binds)
  go e0@(EListLiteral es) = foldl (<>.) (f e0) (map go es)
  go e0@(EListCons es e) = foldl (<>.) (f e0) (map go $ es <> [e])
  go other = f other
