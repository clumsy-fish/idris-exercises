module BDevice
{-
   B-device is defined over some monad m as follows: 
     1) it consists of 
       a) monadic 'value' of monoidal type s
       b) monadic 'function' over s
     2) it supports such operations as
       a) applying 'function' to the 'value'
       b) monoidal addition of some external value to the device 'value'
       c) extracting monadic 'value'
       d) changing 'function' via composition with external function
     3) two other operations are supported in case if Alternative instance 
        for monad m is defined:
       a) creating default device
       b) combining devices
     4) several most useful devices are predefined for their prospective users
-}

data BDevice : (Type -> Type) -> Type -> Type  where
  MkBDevice : (Monad m, Monoid s) => (fun : m (s->s)) -> (val : m s) -> BDevice m s

-- ??? apply

-- ??? add

-- ??? extract

-- ??? compose

defaultBDevice : (Monad m, Alternative m, Monoid s) => BDevice m s
defaultBDevice = MkBDevice empty empty

-- ??? combine

-- ??? bdevice1 over Maybe monad

-- ??? bdevice2 over Either monad

-- ??? bdevice3 over List monad

{-
  Don't you think that B-device itself can be turned into 
  a) Functor
  b) Applicative
  c) Monad
  d) Alternative
  e) Semigroup
  f) Monoid
  
  Should you agree implement respective instances.
-}
