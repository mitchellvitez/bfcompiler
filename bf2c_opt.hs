import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (die)

data Node = Move Int | Increment Int | Put | Get | While [Node] | Clear | Copy
  deriving Show

parse :: String -> [Node]
parse [] = []
parse (token:rest) =
  case token of
    '>' -> Move 1 : r
    '<' -> Move (-1) : r
    '+' -> Increment 1 : r
    '-' -> Increment (-1) : r
    '.' -> Put : r
    ',' -> Get : r
    '[' -> While (parse (fst (splitOnClose "" rest 1))) : parse (snd (splitOnClose "" rest 1))
    ']' -> r
    _ -> r
  where r = parse rest

splitOnClose :: String -> String -> Int -> (String, String)
splitOnClose acc rest 0 = (acc, rest)
splitOnClose acc ('[':xs) n = splitOnClose (acc ++ "[") xs (n+1)
splitOnClose acc (']':xs) n = splitOnClose (acc ++ "]") xs (n-1)
splitOnClose acc (x:xs) n = splitOnClose (acc ++ [x]) xs n

optimize :: [Node] -> [Node]
optimize [] = []
optimize (Move x : Move y : rest) = optimize $ Move (x + y) : rest
optimize (Increment x : Increment y : rest) = optimize $ Increment (x + y) : rest
optimize (While [Increment (-1), Move 1, Increment 1, Move (-1)] : rest) = Copy : optimize rest
optimize (While [Increment (-1)] : rest) = Clear : optimize rest
optimize (While nodes : rest) = While (optimize nodes) : optimize rest
optimize (x:xs) = x : optimize xs

cCode :: [Node] -> Int -> String
cCode [] _ = ""
cCode (node:rest) indentLevel =
  indent ++
    case node of
      Clear -> "*ptr = 0;"
      Copy -> "tmp = *ptr;\n*ptr = 0;\n++ptr;\n*ptr += tmp;\n--ptr;"
      Move 1 -> "++ptr;"
      Move (-1) -> "--ptr;"
      Move n -> if n >= 0 then "ptr += " ++ show n ++ ";" else "ptr -= " ++ show (abs n) ++ ";"
      Increment 1 -> "++*ptr;"
      Increment (-1) -> "--*ptr;"
      Increment n -> if n >= 0 then "*ptr += " ++ show n ++ ";" else "*ptr -= " ++ show (abs n) ++ ";"
      Put -> "putchar(*ptr);"
      Get -> "*ptr = getchar();"
      While nodes -> "while (*ptr) {\n" ++ cCode nodes (indentLevel + 1) ++ indent ++ "}" 
  ++ "\n" ++ cCode rest indentLevel
  where indent = take (indentLevel * 2) (cycle " ")

main = do
  args <- getArgs
  when (length args /= 1) $ die "Please provide a .b filename argument"
  code <- readFile $ head args
  let ast = parse code 
  {- print ast -}
  let betterAst = optimize ast
  {- print betterAst -}
  putStrLn "#include <stdio.h>\n\nint main() {\n  char array[100000] = {0};\n  char *ptr = array;\nchar tmp;"
  putStr $ cCode betterAst 1
  putStrLn "}"

