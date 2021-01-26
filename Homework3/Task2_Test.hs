import Task2
import Test.HUnit

testEmpty :: Test
testEmpty = TestCase $ do
    assertBool "Empty Image 1" (null (content $ grayscale Image{height=0,width=0,content=[]}))
    assertBool "Empty Image 2" (null (content $ grayscale Image{height=1,width=0,content=[]}))
    assertBool "Empty Image 3" (null (content $ grayscale Image{height=0,width=1,content=[]}))

testGrayScale :: Test
testGrayScale = TestCase $ do
    assertEqual "Grayscale 1" [[Rgb{red=255, green=255, blue=255}]]
        (content $ grayscale Image{height=1,width=1,content=[[Rgb{red=255, green=255, blue=255}]]})
    assertEqual "Grayscale 2" [[Rgb{red=107, green=107, blue=107}]]
        (content $ grayscale Image{height=1,width=1,content=[[Rgb{red=111, green=105, blue=111}]]})
    assertEqual "Grayscale 3" [
        [Rgb{red=107, green=107, blue=107}],
        [Rgb{red=107, green=107, blue=107}],
        [Rgb{red=107, green=107, blue=107}]
        ]
        (content $ grayscale Image{height=3,width=1,content=[
            [Rgb{red=111, green=105, blue=111}],
            [Rgb{red=111, green=105, blue=111}],
            [Rgb{red=111, green=105, blue=111}]]})


tl = TestList [testEmpty, testGrayScale]
main = runTestTT tl
