import Data.List.Split

loop render = animationFrame 0
  where
    w = 80
    h = 40
    animationFrame t = do
      putStr $ clear (round w + 1) (round h + 1)
      putStr $ "T:" ++ show (round t) ++ render w h t
      animationFrame (t + 1)

    clear w h = "\ESC[" ++ show h ++ "A\ESC[" ++ show w ++ "D"

scale = 2
lineWidth = 3

render width height t =
  let f x = x * sin (x / 4 + t / 4) * 0.5 * cos (0.5 * x + t / 3) + sin (t / 5)
      values = [ (round x, round $ f x) | x <- [-width..width] ]
      plot = [ if snd m == y then "▇" else "░" | m <- values, y <- [-height..height], snd m == y]
      chunkedPlot = chunksOf height plot
  in [ foldl (++) "" x | x <- chunkedPlot ]
