{-
If you cut a white cable you can't cut white or black cable.
If you cut a red cable you have to cut a green one
If you cut a black cable it is not allowed to cut a white, green or orange one
If you cut a orange cable you should cut a red or black one
If you cut a green one you have to cut a orange or white one
If you cut a purple cable you can't cut a purple, green, orange or white cable
-}

--Import for toUpper
import Data.Char

data Wire = White 
          | Red
          | Black
          | Orange
          | Green
          | Purple
        deriving Read

upperCase :: String -> String
upperCase (x:xs) = toUpper x:xs

cutWire :: Maybe Wire -> Wire -> Maybe Wire
cutWire (Just White)  Red    = Just Red
cutWire (Just White)  Green  = Just Green
cutWire (Just White)  Orange = Just Orange
cutWire (Just White)  Purple = Just Purple
cutWire (Just Red)    Green  = Just Green
cutWire (Just Black)  Red    = Just Red
cutWire (Just Black)  Black  = Just Black
cutWire (Just Black)  Purple = Just Purple
cutWire (Just Orange) Red    = Just Red
cutWire (Just Orange) Black  = Just Black
cutWire (Just Green)  Orange = Just Orange
cutWire (Just Green)  White  = Just White
cutWire (Just Purple) Red    = Just Red
cutWire (Just Purple) Black  = Just Black
cutWire _             _      = Nothing

out :: Maybe Wire -> String
out Nothing = "BOOM"
out _       = "Bomb defused"

--Takes list of wire inputs from user and determines if the bomb was successfully defused
solve :: [Wire] -> String
solve (x:xs) = out $ foldl cutWire (Just x) xs
 
main :: IO()
main = interact $ solve . map (read . upperCase) . words
