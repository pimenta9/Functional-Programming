    -- 1

check_rows :: [[Int]] -> Int -> Bool
check_rows [] _ = True
check_rows (row:square) k
    | sum row /= k = False
    | otherwise = check_rows square k
-----------------------------------------
first_col :: [[Int]] -> [Int]
first_col [] = []
first_col (row:square) = head row : first_col square

erase_first_col :: [[Int]] -> [[Int]]
erase_first_col [] = []
erase_first_col (row:square) = tail row : erase_first_col square

check_cols :: [[Int]] -> Int -> Bool
check_cols square k
    | head square == [] = True
    | sum (first_col square) /= k = False
    | otherwise = check_cols (erase_first_col square) k
-----------------------------------------
build_diagonal1 :: [[Int]] -> [Int]
build_diagonal1 [] = []
build_diagonal1 (row:square) = 
    head row : build_diagonal1 (erase_first_col square)

build_diagonal2 :: [[Int]] -> [Int]
build_diagonal2 [] = []
build_diagonal2 (row:square) = last row : build_diagonal2 (map init square)

check_diagonals :: [[Int]] -> Int -> Bool
check_diagonals square k = 
    sum (build_diagonal1 square) == k &&
    sum (build_diagonal2 square) == k
-----------------------------------------
magic_ :: [[Int]] -> Int -> Bool
magic_ square k = 
    (check_rows square k) &&
    (check_cols square k) &&
    (check_diagonals square k)

magic :: [[Int]] -> Bool
magic square = magic_ square (sum (head square))
