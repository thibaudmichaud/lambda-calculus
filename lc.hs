data Node = Abstr Char Node | App Node Node | Var Char | Error

pretty_print:: Node -> String
pretty_print (Abstr c n) = "(\\" ++ [c] ++ "." ++ (pretty_print n) ++ ")"
pretty_print (App m n)   = "(" ++ (pretty_print m) ++ "" ++ (pretty_print n) ++ ")"
pretty_print (Var c)     = [c]
pretty_print Error       = "Syntax Error."

parse_app:: Node -> String -> (Node, String)
parse_app fun ""         = (fun, "")
parse_app fun (')':tail) = (fun, tail)
parse_app fun str        = let (arg, rest) = parse_term str in ((App fun arg), rest)

parse_abstr:: String -> (Node, String)
parse_abstr (c:'.':tail) = let (body, rest) = parse_term tail in (Abstr c body, rest)
parse_abstr _            = (Error, "")

parse_term:: String -> (Node, String)
parse_term []          = (Error, "")
parse_term ('\\':tail) = parse_abstr tail
parse_term ('(':tail) = let (term, rest) = parse_term tail in parse_app term rest
parse_term (c:tail) = parse_app (Var c) tail

replace:: Node -> Char -> Node -> Node
replace n x (Abstr c e) = if c == x then Abstr c e else Abstr c (replace n x e)
replace n x (App e1 e2) = App (replace n x e1) (replace n x e2)
replace n x (Var v)     = if v == x then n else (Var v)
replace _ _ Error       = Error

eval:: Node -> Node
eval (App m n)   = case (eval m) of
  Abstr c e -> eval (replace (eval n) c e)
  e         -> App e (eval n)
eval (Abstr x e) = Abstr x (eval e)
eval e           = e

main = do
  prog <- getLine
  putStrLn $ pretty_print $ eval $ fst $ parse_term prog
  main
