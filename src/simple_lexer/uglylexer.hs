import           Data.List   (elemIndex)
import           Data.Monoid ((<>))

ops2 :: [(String, String)]
ops2 = [(">=", "token_mayor_igual"), ("<=", "token_menor_igual"), ("==", "token_igual_num"),
        ("!=", "token_diff_num"), ("&&", "token_and"), ("||", "token_or")]

ops :: [(String, String)]
ops = [("{", "token_llave_izq"), ("}", "token_llave_der"), ("[", "token_cor_izq"), ("]", "token_cor_der"), ("(", "token_par_izq"),
       (")", "token_par_der"), (">", "token_mayor"), ("<", "token_menor"), ("!", "token_not"), ("+", "token_mas"), ("-", "token_menos"),
       ("*", "token_mul"), ("/", "token_div"), ("%", "token_mod"), ("^", "token_pot"), ("=", "token_assign"), (",", "token_coma"),
       (".", "token_point"), (":", "token_dosp")]

reservedwords :: [String]
reservedwords = ["while", "for", "if", "log", "funcion"]

uglylexer :: String -> Integer -> Integer -> [String]
uglylexer (' ':xs)  l c = uglylexer xs l (c+1)
uglylexer ('\n':xs) l _ = uglylexer xs (l+1) 0
uglylexer xs'@(x:y:xs) l c = case lookup [x,y] ops2 of
                               Just tokenname -> ('<':tokenname<>","<>show l<>","<>show c<>">") : uglylexer xs l (c+2)
                               Nothing        -> oneop xs' l (c+2)
uglylexer xs l c = oneop xs l c

oneop :: String -> Integer -> Integer -> [String]
oneop xs'@(x:xs) l c = case lookup [x] ops of
                         Just op -> ('<':op<>","<>show l<>","<>show c<>">") : uglylexer xs l (c+1)
                         Nothing -> word xs' l c
oneop [] _ _ = []

word :: String -> Integer -> Integer -> [String]
word xs l c = case getword xs of
                Just (aword, n, xs') -> checkreserved aword : uglylexer xs' l (c+n)
                Nothing              -> number xs l c
  where
    checkreserved :: String -> String
    checkreserved w = case w `elemIndex` reservedwords of
                        Just _  -> ('<':w<>","<>show l<>","<>show c<>">")
                        Nothing -> ("<token_string,"<>w<>","<>show l<>","<>show c<>">")
    getword :: String -> Maybe (String, Integer, String)
    getword xs = case help [] 0 xs of
                ([], 0, _)  -> Nothing
                (w, l, xs') -> Just (w, l, xs')
      where
        help w l xs'@(x:xs)
          | ('A' < x && x < 'Z') || ('a' < x && x < 'z') = help (w<>[x]) (l+1) xs
          | otherwise = (w, l, xs')
        help w l [] = (w, l, [])

number :: String -> Integer -> Integer -> [String]
number = undefined

main :: IO ()
main = print $ uglylexer " == << -+!==log\n( averylongname 2 \n )" 1 1
