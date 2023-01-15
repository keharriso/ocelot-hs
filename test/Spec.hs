import Ocelot (parse)

main :: IO ()
main = do
    symbols <- parse "lib/ocelot/src/ocelot.h" []
    text <- readFile "test/ocelot.h.txt"
    putStrLn $ if show symbols == text then "PASS" else "FAIL"
