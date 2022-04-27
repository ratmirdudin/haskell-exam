module Exam_8_1 where

-- Возвращает разницу a - k*b и k
foo :: (Num a, Num b, Ord a) => a -> a -> b -> (a, b)
foo a b k
  | a >= b = foo (a - b) b (k + 1)
  | otherwise = (a, k)

-- Подсчитываем общее число квадратов
boo :: (Num t, Num a, Ord t) => t -> t -> a -> a
boo 0 _ n = 0
boo _ 0 n = 0
boo a b n =
  let (bb, kk) = foo a b 0
   in if bb /= 0
        then boo b bb n + kk
        else n + kk

task_one :: (Num t, Num a, Ord t) => t -> t -> a
task_one a b = boo a b 0

main :: IO ()
main = print (task_one 425 131)
