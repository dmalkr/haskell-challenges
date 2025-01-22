{-# LANGUAGE GADTs #-}
module Lib where


import Data.Typeable


data Scheme a where
    Res :: Typeable a => Proxy a -> Scheme a
    Arg :: Typeable a => Proxy a -> Scheme b -> Scheme (a -> b)

data Function = forall a. Function (Scheme a) a


newtype Wrap a = Wrap
    { unWrap :: a
    }


data Result a = forall b . Result (Scheme b) (a -> b)


dive :: Scheme a -> Result a
dive (Res _)     = Result (Res Proxy) Wrap
dive (Arg _ sch) =
    case dive sch of
        Result scheme' wr -> Result (Arg Proxy scheme') (\f -> wr . f . unWrap)


wrapFunction :: Function -> Function
wrapFunction (Function scheme f) =
    case dive scheme of
        Result scheme' wr -> Function scheme' (wr f)

