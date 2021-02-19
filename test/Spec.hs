import Test.Hspec
import Lib

minChoices :: Grid -> Matrix Choices
minChoices = map $ map chcs
    where chcs digit = if blank digit then "." else [digit]

main :: IO ()
main = hspec $ do
    
    describe "singleton" $ do
        it "should return False in case of an empty list" $ do
            singleton [] `shouldBe` False
        it "should return False in case of a list containing more than one entry" $ do
            singleton [1, 2] `shouldBe` False
        it "should return True in case it contains just one entry" $ do
            singleton ['a'] `shouldBe` True
    
    describe "blank" $ do
        it "should return True if it is '.'" $ do
            blank '.' `shouldBe` True
        it "should return False else" $ do
            blank 'a' `shouldBe` False

    describe "choices" $ do
        it "should point out possibilites" $ do
            choices ["ab", "cd"] `shouldBe` [["a", "b"], ["c", "d"]]
            choices ["a.", "cd"] `shouldBe` [["a", "123456789"], ["c", "d"]]

    describe "transpose" $ do
        it "should transpose matrixes" $ do
            transpose ["ab", "cd"] `shouldBe` ["ac", "bd"]
            transpose ["a"] `shouldBe` ["a"]

    describe "group" $ do
        it "should group into boxSize" $ do
            group "123456789" `shouldBe` ["123", "456", "789"]

    describe "ungroup" $ do
        it "should concat" $ do
            ungroup ["123", "456", "789"] `shouldBe` "123456789"

    describe "rows" $ do
        it "should return a list of rows" $ do
            let puzzle = (minChoices . parse)"..4..57.......94..36......872..6.......4.2.......8..934......56..53.......61..9.." 
            rows puzzle `shouldBe` puzzle

    {-describe "cols" $ do
        it "should return a list of cols" $ do
            let puzzle = (minChoices . parse)"..4..57.......94..36......872..6.......4.2.......8..934......56..53.......61..9.." 
            cols puzzle `shouldBe` puzzle-}

    describe "boxs" $ do
        it "should return a list of boxs" $ do
            boxs [["123", "456"], ["789", "abc"]] `shouldBe` [["123", "456", "789", "abc"]]

    describe "nodups" $ do
        it "checks for duplicated entries" $ do
            nodups "" `shouldBe` True
            nodups "2" `shouldBe` True
            nodups "123" `shouldBe` True
            nodups "1223" `shouldBe` False
            nodups "1234521" `shouldBe` False

    describe "ok" $ do
        it "should check rows being free of duplicates" $ do
            ok ["1", "123", "2", "12345"] `shouldBe` True
            ok ["1", "123", "1", "12345"] `shouldBe` False
            ok [] `shouldBe` True

    describe "parse" $ do
        it "should parse plane strings to grids" $ do
            parse "..4..57.......94..36......872..6.......4.2.......8..934......56..53.......61..9.." `shouldBe` [
                "..4..57..", 
                ".....94..", 
                "36......8", 
                "72..6....", 
                "...4.2...", 
                "....8..93",
                "4......56", 
                "..53.....", 
                "..61..9.."]

    describe "solve" $ do
        it "should handle a trivial sodoku" $ do
            (concat . solve . parse)
                "123456789456789123789123456234567891567891234891234567345678912678912345912345678" 
            `shouldBe` 
                "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
        it "should handle an easy sodoku" $ do
            (concat . solve . parse)
                "12345..89456789123789123..62345678915678.123489123.5673456789.2678912.459......78" 
            `shouldBe` 
                "123456789456789123789123456234567891567891234891234567345678912678912345912345678"
        it "should handle a fair sodoku" $ do
            (concat . solve . parse)
                "..4..57.......94..36......872..6.......4.2.......8..934......56..53.......61..9.." 
            `shouldBe` 
                "184625739572839461369741528728963145953412687641587293417298356295376814836154972"
        it "should handle a hard sodoku" $ do
            (concat . solve . parse)
                "....6..8..2.........1.......7....1.25...3..........4....42.1...3..7..6.........5." 
            `shouldBe` 
                "435162987726895314891374526678549132549636768213987495964251873352789641187443259"