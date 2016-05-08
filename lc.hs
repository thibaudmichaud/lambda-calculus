import Data.Set

data Node = Abstr Char Node | App Node Node | Var Char | Error

alphabet = fromList "abcdefghijklmnopqrstuvwxyz"

pretty_print:: Node -> String
pretty_print (Abstr x n) = "(\\" ++ [x] ++ "." ++ (pretty_print n) ++ ")"
pretty_print (App m n)   = "(" ++ (pretty_print m) ++ "" ++ (pretty_print n) ++ ")"
pretty_print (Var x)     = [x]
pretty_print Error       = "Syntax Error."

parse:: String -> (Node, String)
parse str = let (term, rest) = parse_one str in parse_app term rest

parse_app:: Node -> String -> (Node, String)
parse_app res prog = case prog of
  "" -> (res, "")
  (')':tail) -> (res, ')':tail)
  str -> let (term, rest) = parse_one str in parse_app (App res term) rest

parse_one:: String -> (Node, String)
parse_one []          = (Error, "")
parse_one ('(':tail)  = let (term, rest) = parse tail in case rest of
  (')':tail) -> (term, tail)
  _ -> (Error, "")
parse_one ('\\':x:'.':tail) =
  let (body, rest) = parse tail in (Abstr x body, rest)
parse_one (x:tail)    = ((Var x), tail)

fv:: Node -> Set Char
fv (Var x) = singleton x
fv (App m n) = fv m `union` fv n
fv (Abstr x m) = delete x (fv m)
fv Error = empty

subst:: Node -> Char -> Node -> Node
subst m x (Var y)     = if y == x then m else (Var y)
subst m x (App n l) = App (subst m x n) (subst m x l)
subst _ x (Abstr y n) | x == y        = Abstr y n
subst m x (Abstr y n) | elem y (fv m) =
  let z = elemAt 0 (alphabet `difference` (fv m `union` fv n)) in
    subst m x (Abstr z (subst (Var z) y n))
subst m x (Abstr y n)                 = Abstr y (subst m x n)
subst _ _ Error       = Error

eval:: Node -> Node
eval (App m n)   = case (eval m) of
  Abstr c e -> eval (subst (eval n) c e)
  e         -> App e (eval n)
eval (Abstr x e) = Abstr x (eval e)
eval e           = e

main = do
  prog <- getLine
  putStrLn $ pretty_print $ eval $ fst $ parse prog
  main
