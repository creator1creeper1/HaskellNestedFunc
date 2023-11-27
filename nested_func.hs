import Text.Show.Functions

data NestedFunc = F (NestedFunc -> NestedFunc) deriving Show

box :: (NestedFunc -> NestedFunc) -> NestedFunc
box f = F f

unbox :: NestedFunc -> (NestedFunc -> NestedFunc)
unbox (F f) = f

box' :: NestedFunc -> NestedFunc
box' = box . unbox

box'' :: NestedFunc
box'' = box box'

unbox' :: NestedFunc -> NestedFunc
unbox' = box . unbox

unbox'' :: NestedFunc
unbox'' = box unbox'

identity :: NestedFunc -> NestedFunc
identity = id

identity' :: NestedFunc
identity' = box identity

compose :: (NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc)
compose f g = f . g

compose' :: NestedFunc -> NestedFunc -> NestedFunc
compose' f g = box (compose (unbox f) (unbox g))

compose'' :: NestedFunc -> NestedFunc
compose'' = box . compose'

compose''' :: NestedFunc
compose''' = box compose''

fix :: (NestedFunc -> NestedFunc) -> NestedFunc
fix f = f (fix f)

fix' :: NestedFunc -> NestedFunc
fix' = fix . unbox

fix'' :: NestedFunc
fix'' = box fix'

self_fix :: NestedFunc
self_fix = fix' fix''

deep_box :: ((NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc)) -> NestedFunc
deep_box f = box (box . f . unbox)

deep_unbox :: NestedFunc -> ((NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc))
deep_unbox f = unbox . (unbox f) . box

deep_box' :: (NestedFunc -> NestedFunc) -> NestedFunc
deep_box' f = deep_box (unbox . f . box)

deep_box'' :: NestedFunc -> NestedFunc
deep_box'' = deep_box' . unbox

deep_box''' :: NestedFunc
deep_box''' = box deep_box''

deep_unbox' :: NestedFunc -> (NestedFunc -> NestedFunc)
deep_unbox' f = box . (deep_unbox f) . unbox

deep_unbox'' :: NestedFunc -> NestedFunc
deep_unbox'' = box . deep_unbox'

deep_unbox''' :: NestedFunc
deep_unbox''' = box deep_unbox''

self_apply :: NestedFunc -> NestedFunc
self_apply f = (unbox f) f

self_apply' :: NestedFunc
self_apply' = box self_apply

apply_twice :: NestedFunc -> NestedFunc -> NestedFunc
apply_twice f x = (unbox f) ((unbox f) x)

apply_twice' :: NestedFunc
apply_twice' = box (box . apply_twice)

swap :: (NestedFunc -> NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc -> NestedFunc)
swap f x y = f y x

swap' :: (NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc)
swap' f x = box (swap (unbox . f) x)

swap'' :: NestedFunc
swap'' = deep_box swap'

constant :: NestedFunc -> NestedFunc -> NestedFunc
constant x y = x

constant' :: NestedFunc
constant' = box (box . constant)

ap :: (NestedFunc -> NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc) -> NestedFunc -> NestedFunc
ap f g x = (f x) (g x)

ap' :: (NestedFunc -> NestedFunc) -> (NestedFunc -> NestedFunc) -> NestedFunc -> NestedFunc
ap' f g x = ap (unbox . f) g x

ap'' :: NestedFunc -> NestedFunc -> NestedFunc
ap'' f g = box (ap' (unbox f) (unbox g))

ap''' :: NestedFunc
ap''' = box (box . ap'')