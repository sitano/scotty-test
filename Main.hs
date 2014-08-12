import System.Environment

main :: IO ()
main = getArgs >>= print . test . head

test s = "Hi! " ++ s
