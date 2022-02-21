import Data.Char

capitalize :: [String] -> [String]
capitalize = map(\x -> capitalizeWord x)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord x = toUpper (head x) : tail x

main :: IO ()
main = print(capitalize ["hello", "darkness", "my", "old", "friend", "i", "have", "come", "to", "talk"])
