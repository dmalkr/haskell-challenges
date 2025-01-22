{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import           Data.Proxy
import           Data.Typeable

data Scheme a where
    Res :: Typeable a => Proxy a -> Scheme a
    Arg :: Typeable a => Proxy a -> Scheme b -> Scheme (a -> b)

data Function = forall a. Function (Scheme a) a

newtype Wrap a = Wrap
    { unWrap :: a
    }

wrapFunction :: Function -> Function
wrapFunction (Function (Res v) f) =
    Function (Res $ Wrap <$> v) (Wrap f)
wrapFunction (Function (Arg arg (Res v)) f) =
    Function (Arg Proxy (Res $ Wrap <$> v))
             (\(Wrap v) -> Wrap $ f v)
wrapFunction (Function (Arg arg1 (Arg arg2 (Res v))) f) =
    Function (Arg Proxy (Arg Proxy (Res $ Wrap <$> v)))
             (\(Wrap x) (Wrap y) -> Wrap $ f x y)
wrapFunction (Function (Arg arg1 (Arg arg2 (Arg arg3 (Res v)))) f) =
    Function (Arg Proxy (Arg Proxy (Arg Proxy (Res $ Wrap <$> v))))
             (\(Wrap x) (Wrap y) (Wrap t) -> Wrap $ f x y t)
wrapFunction (Function (Arg arg1 (Arg arg2 (Arg arg3 (Arg arg4 (Res v))))) f) =
    Function (Arg Proxy (Arg Proxy (Arg Proxy (Arg Proxy (Res $ Wrap <$> v)))))
             (\(Wrap x) (Wrap y) (Wrap t) (Wrap u) -> Wrap $ f x y t u)
wrapFunction _ = undefined
