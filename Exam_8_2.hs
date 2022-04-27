module Exam_8_2 where

-- Вычисляем длину листа(количество чиселок)
my_length :: Num a1 => [a2] -> a1
my_length [] = 0
my_length (_ : xs) = 1 + my_length xs

-- Фильтруем лист
my_filter :: (a -> Bool) -> [a] -> [a]
my_filter pred [] = []
my_filter pred (x : xs)
  | pred x = x : my_filter pred xs
  | otherwise = my_filter pred xs

-- Подсчитываем количество неотрицательных и отрицательных
task_two :: (Num b, Ord a1, Num a2, Num a1) => [a1] -> (a2, b)
task_two x =
  let n = my_length (my_filter (>= 0) x)
      m = my_length (my_filter (< 0) x)
   in (n, m)

main :: IO ()
main = do
  print (task_two [-1, 2, -5, 6, -2, 7, 8])
  print (task_two [1, -1337, 3.1, 5.53, -228.228, 7, 9, -9, 9, -1011111, 0])
  print (task_two [41.5, 15.1, -2.7, 50.15, -60.22, -70.666, 85.10, 0])
  print (task_two [])
