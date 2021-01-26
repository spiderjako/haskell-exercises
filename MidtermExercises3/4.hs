type ItemName = String
type MenuItem = (ItemName, Int, Float)
type Menu = [MenuItem]
type Budget = Float

getName (name, _, _) = name
getPrice (_,_, price) = price

getGrams (gram, _) = gram
maxMenuItem :: (Int, ItemName) -> (Int, ItemName) -> (Int, ItemName)
maxMenuItem item1 item2 = if getGrams item1 > getGrams item2 then item1 else item2

starving :: Menu -> Budget -> (Int, ItemName)
starving menu budget =
  let moneySpent = map (\a -> floor (budget / (getPrice a))) menu
      uppedGrams = map (\((n,g,_), m) -> (m, n) ) (zip menu moneySpent)
  in foldl maxMenuItem (head uppedGrams) uppedGrams


getSecond (_,second,_) = second a b = if (getSecond a) > (getSecond b) then a else b

helper [] _ res = res
helper (h:t) budget res
  | p < budget = res
  | otherwise = helper t (budget - p) (res ++ [n])
    where n = getName h
          p = getPrice h


qsort (h:t) = qsort smaller ++ (h : qsort larger)
  where smaller = [y | y <- xs, sorty]

--yolo :: Menu -> Budget -> (Int, ItemName)
yolo menu budget =
  let realisedGrams = map (\(n,g,p) -> (n, (fromIntegral g)/p, p)) menu
      sortedItems = foldr (\a acc -> )
  --in helper sortedItems budget []
  in map sorty realisedGrams
