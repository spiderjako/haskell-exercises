module Task2 where

import Data.Char
import Data.Word

data Rgb = Rgb {
  red :: Word8,
  green :: Word8,
  blue :: Word8 } deriving (Show, Read)

instance Eq Rgb where
  Rgb{red=r,blue=b,green=g} == Rgb{red=x, blue=y, green=z} = (r==x) && (b==y) && (g==z)

data Image = Image {
  width :: Int,
  height :: Int,
  content :: [[Rgb]] } deriving (Show, Read)


grayscale :: Image -> Image
grayscale img
  | null (content img) = Image {height=0,width=0, content=[]}
  | width img /= length (head (content img)) = Image {height=0,width=0, content=[]}
  | height img /= length (content img) = Image {height=0,width=0, content=[]}
  | otherwise = Image {
    width=width img,
    height=height img,
    content = [
      map (\a ->
        let pixelColour = (fromIntegral (red a)) * 0.3 + (fromIntegral (green a)) * 0.59 + (fromIntegral(blue a)) * 0.11 :: Float
        in Rgb{
          red=(round pixelColour),
          green=(round pixelColour),
          blue=(round pixelColour)
        }
      )
      x | x <-(content img)
    ]
 }


chunk :: Int -> [a] -> [[a]]
chunk n lst
  | length lst < n = []
  | otherwise =
    (take n lst) : chunk n (tail lst)


chunkEvery :: Int -> [a] -> [[a]]
chunkEvery n lst
  | n == 0 = [lst]
  | length lst < n = []
  | otherwise = (take n lst) : chunk n (drop n lst)


transpose :: [[[[Rgb]]]] -> [[[Rgb]]]
transpose mtx
  | null (head mtx) = []
  | otherwise = (map (\a -> head a) mtx) ++ (transpose (map tail mtx))


reverseMatrix :: [[Float]] -> [[Float]]
reverseMatrix mtx =
  map reverse (reverse mtx)


-- The idea is that the matrix will be grayscale
makeOneColourMatrix :: [[[Rgb]]] -> [[[Float]]]
makeOneColourMatrix mtx
  | null mtx = []
  | otherwise =
    map (\row -> map (\rgb -> fromIntegral (red rgb)) row) (head mtx) : makeOneColourMatrix (tail mtx)


matrixMultiplication :: [[Float]] -> [[Float]] -> [[Float]]
matrixMultiplication a b
  | null (head b) = []
  | otherwise =
    let colB = map head b
    in (map (\row -> sum (zipWith (*) row colB)) a) : matrixMultiplication a (map tail b)


getNonEdgePixelsNeighbours :: [[Rgb]] -> [[[[Rgb]]]]
getNonEdgePixelsNeighbours colours
  | length (head colours) < 3 = []
  | otherwise =
    [(chunk 3 $ map (\a  -> take 3 a) colours)] ++ getNonEdgePixelsNeighbours (map tail colours)


clamp :: Float -> Word8
clamp x
  | x > 255 = 255
  | x < 0 = 0
  | otherwise = round x :: Word8


edgeDetect :: Image -> Image
edgeDetect img =
  let colours = content img
      w = width img
      h = height img
      neighbours = transpose (getNonEdgePixelsNeighbours colours)
      neighboursFloat = makeOneColourMatrix neighbours
      scalarProductMatrices = map reverseMatrix neighboursFloat
      gx = [[1,0,-1],[2,0,-2], [1,0,-1]]
      gy = [[1,2,1],[0,0,0],[-1,-2,-1]]
      scalarProductsX = (map (\a -> zipWith (\x y -> zipWith (*) x y) gx a) neighboursFloat)
      scalarProductsY = (map (\a -> zipWith (\x y -> zipWith (*) x y) gy a) neighboursFloat)
      summedX = (map (\a -> sum (map sum a)) scalarProductsX)
      summedY = (map (\a -> sum (map sum a)) scalarProductsY)
      normed = zipWith (\a b -> sqrt(a*a + b*b)) summedX summedY
      clampedRows =[(take w [0,0..])] ++ (map (\row -> [0] ++ map clamp row ++ [0]) (chunkEvery (h - 2) normed)) ++ [(take w [0,0..])] :: [[Word8]]
  in Image {width=(width img), height=(height img), content=[
    map (\a -> Rgb {red=a, green=a, blue=a}) x | x<-clampedRows
  ]}


-- floodFill :: Rgb -> Int -> Int -> Image -> Image
-- floodFill color x y img = undefined3


saveImage :: FilePath -> Image -> IO()
saveImage path img =
  let header = "P3\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n255\n"
      mappedColours =
        [map (\a -> show (red a) ++ " " ++ show (green a) ++ " " ++ show (blue a) ++ "\n") x | x <- content img]
      colourRows = map (\x -> foldr (\acc2 y -> acc2 ++ y) [] x) mappedColours
      colourString = foldr (\acc x -> acc ++ x) [] colourRows
      combinedStr = header ++ colourString
  in writeFile path combinedStr


trimNewline :: [Char] -> [Char]
trimNewline str
  | str == [] = []
  | str == ['\n'] = []
  | head str == '\n' = ' ':(trimNewline (tail str))
  | otherwise = (head str) : (trimNewline (tail str))


takeWhileEnd :: [Char] -> [Word8] -> [Word8]
takeWhileEnd [] content = content
takeWhileEnd lst content =
  let numberValue = takeWhile (\a -> a/=' ') lst
      numberLength = length numberValue
  in takeWhileEnd (drop (numberLength + 1) lst) (content ++ [read numberValue :: Word8])


colourRow :: Int -> [Word8] -> [Rgb]
colourRow width content
  | width > 0 && length content >= 3 =
    let pixel = take 3 content
        in Rgb{red=pixel !! 0, green=pixel !! 1, blue=pixel !! 2}:colourRow (width-1) (drop 3 content)
  | otherwise = []


colourArray :: Int -> [Word8] -> [[Rgb]]
colourArray width content
  | not (null content) =
    let row = colourRow width content
        rowLength = (length row) * 3
    in row:(colourArray width (drop rowLength content))
  | otherwise = []


getInt :: [Char] -> Int
getInt str = read str :: Int


loadImage :: String -> IO Image
loadImage path = do
  trimmedString <- trimNewline <$> (readFile path)
  let format = takeWhile (\a -> a /= ' ') trimmedString
      trimmedString2 = drop ((length format) + 1) trimmedString
      width = takeWhile (\a -> a /= ' ') trimmedString2
      intWidth = getInt width
      trimmedString3 = drop ((length width) + 1)  trimmedString2
      height = takeWhile (\a -> a /= ' ') trimmedString3
      intHeight = getInt height
      trimmedString4 = drop ((length height) + 5)  trimmedString3
      colourValues = takeWhileEnd trimmedString4 []
      colours = colourArray 3 colourValues
  return Image{
    height=intHeight,
    width=intWidth,
    content=colours
  }
