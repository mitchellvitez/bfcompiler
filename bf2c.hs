import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (die)

translate [] _ = return ()
translate (token:rest) indentLevel = do
  putStrLn $ indentation token indentLevel ++ 
    case token of
      '>' -> "++ptr;"
      '<' -> "--ptr;"
      '+' -> "++*ptr;"
      '-' -> "--*ptr;"
      '.' -> "putchar(*ptr);"
      ',' -> "*ptr = getchar();"
      '[' -> "while (*ptr) {"
      ']' -> "}"
      tok -> "// unknown token: " ++ [tok]
  translate rest $ newIndentLevel token indentLevel

indentation token indentLevel =
  let indntLvl = if token == ']' then indentLevel - 1 else indentLevel in
  take (indntLvl * 2) (cycle " ")

newIndentLevel token indentLevel =
  case token of
    '[' -> indentLevel + 1
    ']' -> indentLevel - 1
    _ -> indentLevel

main = do
  args <- getArgs
  when (length args /= 1) $ die "Please provide a .b filename argument"
  code <- readFile $ head args
  putStrLn "#include <stdio.h>\n\nint main() {\n  char array[100000] = {0};\n  char *ptr = array;"
  translate code 1
  putStrLn "}"

