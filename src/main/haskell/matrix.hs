
middle :: [a] -> [a]
middle = init . tail

obtainOuterLayer :: [[a]] -> [a]
obtainOuterLayer []     = []
obtainOuterLayer [h]    = h
obtainOuterLayer matrix = firstRow ++ lastColumn ++ lastRow ++ firstColumn
                           where
                              firstRow    = matrix !! 0
                              lastColumn  = tail $ map (\row -> last row) $ matrix
                              lastRow     = reverse $ init $ last $ matrix
                              firstColumn = reverse $ middle $ map (\row -> head row) $ matrix

recurseIntoMatrix :: [[a]] -> [[a]]
recurseIntoMatrix []     = []
recurseIntoMatrix [h]    = [[]]
recurseIntoMatrix matrix = map middle submatrix
                           where
                                submatrix = init $ tail $ matrix

snakeRepresentation :: [[a]] -> [a]
snakeRepresentation []     = []
snakeRepresentation [h]    = h
snakeRepresentation matrix = (obtainOuterLayer matrix) ++ (snakeRepresentation $ recurseIntoMatrix $ matrix)

main = do print $ snakeRepresentation $ [[1,2,3,4,5], [6,7,8, 9, 10], [11, 12, 13, 14, 15], [16,17,18,19, 20]]