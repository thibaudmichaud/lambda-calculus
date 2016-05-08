data Node = Abstr Char Node | App Node Node | Var Char | Error

alphabet = "abcdefghijklmnopqrstuvwxyz"

pretty_print:: Node -> String
pretty_print (Abstr c n) = "(\\" ++ [c] ++ "." ++ (pretty_print n) ++ ")"
pretty_print (App m n)   = "(" ++ (pretty_print m) ++ "" ++ (pretty_print n) ++ ")"
pretty_print (Var c)     = [c]
pretty_print Error       = "Syntax Error."

parse_app:: Node -> String -> (Node, String)
parse_app m ""         = (m, "")
parse_app m (')':tail) = (m, tail)
parse_app m str        = let (arg, rest) = parse_term str in ((App m arg), rest)

parse_abstr:: String -> (Node, String)
parse_abstr (c:'.':tail) = let (body, rest) = parse_term tail in (Abstr c body, rest)
parse_abstr _            = (Error, "")

parse_term:: String -> (Node, String)
parse_term []          = (Error, "")
parse_term ('\\':tail) = parse_abstr tail
parse_term ('(':tail) = let (term, rest) = parse_term tail in parse_app term rest
parse_term (c:tail) = parse_app (Var c) tail

-- free variables
fv:: Node -> [Char]
fv (Var x) = [x]
fv (App m n) = fv m ++ fv n
fv (Abstr x m) = [y | y <- (fv m), y /= x]
fv Error = []

-- substition
subst:: Node -> Char -> Node -> Node
-- [M/x]y = M
-- [M/x]y = y (x != y)
subst m x (Var y)     = if y == x then m else (Var y)
-- [M/x](NL) = ([M/x]N)([M/x]L)
subst m x (App n l) = App (subst m x n) (subst m x l)
-- [M/x](\x.N) = \x.N
subst _ x (Abstr y n) | x == y        = Abstr y n
-- [M/x](\y.N) = \z.[M/x][z/y]N with x != y and y in fv(m)
subst m x (Abstr y n) | elem y (fv m) =
  -- find a variable name that will avoid collisions
  let z = head [v | v <- alphabet, not (elem v (fv m)),
                                   not (elem v (fv n))] in
    subst m x (Abstr z (subst (Var z) y n))
-- [M/x](\y.N) = \y.[M/x]N with x != y and y not in fv(m)
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
  putStrLn $ pretty_print $ eval $ fst $ parse_term prog
  main
