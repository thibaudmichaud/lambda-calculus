import Data.Set

data Node = Abstr Char Node | App Node Node | Var Char

alphabet = fromList "abcdefghijklmnopqrstuvwxyz"

pretty_print:: Node -> String
pretty_print (Abstr x n) = "(\\" ++ [x] ++ "." ++ pretty_print n ++ ")"
pretty_print (App m n)   = "(" ++ pretty_print m ++ "" ++ pretty_print n ++ ")"
pretty_print (Var x)     = [x]

-- Parse a lambda-term.
-- Return value: the AST and the rest of the string if it is unfinished.
parse:: String -> Maybe (Node, String)
parse str = do
  (term, rest) <- parse_one str
  parse_app term rest

-- Parse an application (MN), or a single term M if it has no argument.
-- To handle left-associativity, the result is stored in an accumulator.
parse_app:: Node -> String -> Maybe (Node, String)
parse_app res prog = case prog of
  "" -> Just (res, "")
  (')':tail) -> Just (res, ')':tail)
  str -> do
    (term, rest) <- parse_one str
    parse_app (App res term) rest

-- Parse an abstraction, a variable or a parenthesized expression.
parse_one:: String -> Maybe (Node, String)
parse_one []          = Nothing
parse_one ('(':tail)  = do
  (term, rest) <- parse tail
  case rest of
    (')':tail) -> Just (term, tail)
    _ -> Nothing
parse_one ('\\':x:'.':tail) = do
  (body, rest) <- parse tail
  return (Abstr x body, rest)
parse_one (x:tail)    = Just ((Var x), tail)

-- Free variables of a term.
fv:: Node -> Set Char
fv (Var x) = singleton x
fv (App m n) = fv m `union` fv n
fv (Abstr x m) = delete x (fv m)

-- Substitution:
subst:: Node -> Char -> Node -> Node
-- [M/x]y = y    [M/x]x = M
subst m x (Var y)                     = if y == x then m else (Var y)
-- [M/x](NL) = ([M/x]N)([M/x]L)
subst m x (App n l)                   = App (subst m x n) (subst m x l)
-- [M/x](\y.N) = \y.N if x == y
subst _ x (Abstr y n) | x == y        = Abstr y n
--             = \z.[M/x][z/y]N if x /= y and y is a free variable of M
subst m x (Abstr y n) | elem y (fv m) =
  -- Name collision, substitute y for an unused letter (z)
  let z = elemAt 0 (alphabet `difference` (fv m `union` fv n)) in
    subst m x (Abstr z (subst (Var z) y n))
--             = \y.[M/x]N if x /= y and y is not a free variable of M
subst m x (Abstr y n)                 = Abstr y (subst m x n)

eval:: Node -> Node
eval (App m n)   = case (eval m) of
  -- (\x.M)N = [N/x]M
  Abstr x m -> eval (subst (eval n) x m)
  e         -> App e (eval n)
eval (Abstr x e) = Abstr x (eval e)
eval e           = e

eval_print_maybe (Just (ast, _)) = pretty_print (eval ast)
eval_print_maybe Nothing = ""

main = do
  prog <- getLine
  if prog == "exit" || prog == "quit" then return ()
  else do
    putStrLn $ eval_print_maybe $ parse $ prog
    main
