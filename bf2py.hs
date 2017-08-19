import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (die)

translate [] _ = return ()
translate (token:rest) indentLevel = do
  putStrLn $ indentation token indentLevel ++ 
    case token of
      '>' -> "ptr += 1"
      '<' -> "ptr -= 1"
      '+' -> "array[ptr] += 1"
      '-' -> "array[ptr] -= 1"
      '.' -> "sys.stdout.write(chr(array[ptr]))"
      ',' -> "array[ptr] = sys.stdin.read(1)"
      '[' -> "while array[ptr]:"
      ']' -> ""
      tok -> "# unknown token: " ++ [tok]
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
  putStrLn "import sys\narray = [0] * 100000\nptr = 0"
  translate code 0
  putStrLn ""

