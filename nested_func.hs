import Text.Show.Functions

data NestedFunc = F (NestedFunc -> NestedFunc) deriving Show

class NestedFuncable t where
  box :: t -> NestedFunc
  unbox :: NestedFunc -> t

instance NestedFuncable NestedFunc where
  box f = f
  unbox f = f

instance (NestedFuncable a, NestedFuncable b) => NestedFuncable (a -> b) where
  box f = F ((box::(b -> NestedFunc)) . f . (unbox::(NestedFunc -> a)))
  unbox (F f) = (unbox::(NestedFunc -> b)) . f . (box::(a -> NestedFunc))

identity :: NestedFunc -> NestedFunc
identity = id

identity' :: NestedFunc
identity' = box identity

compose :: (NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc)
compose f g = f . g

compose' :: NestedFunc
compose' = box compose

fix :: (NestedFunc -> NestedFunc) -> NestedFunc
fix f = f (fix f)

fix' :: NestedFunc
fix' = box fix

self_fix' :: NestedFunc
self_fix' = (unbox fix') fix'

self_apply :: NestedFunc -> NestedFunc
self_apply f = (unbox f) f

self_apply' :: NestedFunc
self_apply' = box self_apply

apply_twice :: NestedFunc -> NestedFunc -> NestedFunc
apply_twice f x = (unbox f) (((unbox f) x)::NestedFunc)

apply_twice' :: NestedFunc
apply_twice' = box apply_twice

swap :: (NestedFunc -> NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc -> NestedFunc)
swap f x y = f y x

swap' :: NestedFunc
swap' = box swap

constant :: NestedFunc -> NestedFunc -> NestedFunc
constant x y = x

constant' :: NestedFunc
constant' = box constant

ap :: (NestedFunc -> NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc) -> NestedFunc -> NestedFunc
ap f g x = (f x) (g x)

ap' :: NestedFunc
ap' = box ap

const_self :: NestedFunc -> NestedFunc
const_self f = box const_self

const_self' :: NestedFunc
const_self' = box const_self

const_self'' :: NestedFunc
const_self'' = (unbox fix') constant'

zero' :: NestedFunc
zero' = identity'

plus_one :: (NestedFunc -> NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc) -> NestedFunc -> NestedFunc
plus_one n f x = f (n (box f) x)

plus_one' :: NestedFunc
plus_one' = box plus_one

infinity' :: NestedFunc
infinity' = (unbox fix') plus_one