import Data.List

lst = foldr ( (++) . show ) "" [1..]
