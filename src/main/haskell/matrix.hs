
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

main = do print "123"