
module Main
      where
main = do 
  let graph = [[ 0, 5, 0, 0, 0, 0, 0, 0, 10 ],[ 5, 0, 6, 0, 0, 0, 0, 7, 0 ],[ 0, 6, 0, 10, 0, 0, 5, 9, 7 ],[ 0, 0, 10, 0, 3, 7, 8, 9, 0 ],[ 0, 0, 0, 3, 0, 1, 0, 0, 0 ],[ 0, 0, 0, 7, 1, 11, 0, 0, 0 ],[ 0, 0, 5, 8, 0, 11, 0, 3, 0 ],[ 0, 7, 9, 9, 0, 0, 3, 0, 9 ],[ 10, 0, 7, 0, 0, 0, 0, 9, 0 ]]
  let dist = [0, 9223372036854775807, 9223372036854775807, 9223372036854775807, 9223372036854775807, 9223372036854775807, 9223372036854775807, 9223372036854775807, 9223372036854775807]
  let pset = [False,False,False,False,False,False,False,False,False]
  putStrLn "Dijkstra Algorithm"
  putStr "Distances:"
  print $ dijkstra graph dist pset (0) (0)
  putStr "Graph: "
  print graph

psetOlusturma :: [int]->[Bool]
psetOlusturma []=[]
psetOlusturma (x:xs) = False : psetOlusturma xs

pset1 = take 9 (repeat False)

maxInt = maxBound :: Int
f2 :: [Int] -> [Int]
f2 [] = []
f2 (x:xs) = maxInt : f2 xs

distOlusturma :: [Int] -> [Int]
distOlusturma [] = []
distOlusturma (x:xs) = 0 : f2 xs

recMin [] [] a b v = b
recMin dist pset index mini v =
            if v <9
              then if ((pset !! v) == False) && ((dist !! v) <= mini)
                      then recMin dist pset v (dist !! v) (v+1)
                      else recMin dist pset index mini (v+1)
              else index

dijkstra :: [[Int]] ->[Int] -> [Bool] -> Int->Int ->[Int]
dijkstra [[]] [] [] u i = []
dijkstra graph dist pset u i = do
            let u = recMin dist pset (1) maxInt (0)
            if i<9
              then dongu1 graph dist (f8 pset u) (recMin dist pset (1) maxInt (0)) (0) i
              else dist --0 4 12 19 21 11 9 8 14

dongu1 :: [[Int]] ->[Int] -> [Bool] -> Int -> Int -> Int->[Int]
dongu1 [[]] [] [] (0) (0) (0) = []
dongu1 graph dist pset u v i  = do
            if v<9
              then dongu1 graph (f7 graph dist pset u v) pset u (v+1) i
              else dijkstra graph dist pset u (i+1)

f7 :: [[Int]] ->[Int] -> [Bool] -> Int -> Int -> [Int]
f7 [[]] [] [] (0) (0) = []
f7 graph dist pset u v = do
            if pset !! v == False && graph !! u !! v /= 0 && dist !! u /= maxInt && ((dist !! u) + (graph !! u !! v)) < dist !! v
              then updateDist dist v (dist !! u + graph !! u !! v) --dist[v] = dist[u] + graph[u, v];
              else dist
--pset için indisi True yapıyor
f8 pset i= do
      let a0 = pset !! 0
      let a1 = pset !! 1
      let a2 = pset !! 2
      let a3 = pset !! 3
      let a4 = pset !! 4
      let a5 = pset !! 5
      let a6 = pset !! 6
      let a7 = pset !! 7
      let a8 = pset !! 8
      f9 a0 a1 a2 a3 a4 a5 a6 a7 a8 i

f9::Bool->Bool->Bool->Bool->Bool->Bool->Bool->Bool->Bool->Int->[Bool]
f9 a0 a1 a2 a3 a4 a5 a6 a7 a8 i =
    if i ==0 
      then [True,a1,a2,a3,a4,a5,a6,a7,a8]
      else if i ==1 
              then [a0,True,a2,a3,a4,a5,a6,a7,a8]
              else if i==2
                      then [a0,a1,True,a3,a4,a5,a6,a7,a8]
                      else if i==3
                              then [a0,a1,a2,True,a4,a5,a6,a7,a8]
                              else if i==4
                                      then [a0,a1,a2,a3,True,a5,a6,a7,a8]
                                      else if i==5
                                              then [a0,a1,a2,a3,a4,True,a6,a7,a8]
                                              else if i==6
                                                      then [a0,a1,a2,a3,a4,a5,True,a7,a8]
                                                      else if i==7
                                                              then [a0,a1,a2,a3,a4,a5,a6,True,a8]
                                                              else if i==8
                                                                      then [a0,a1,a2,a3,a4,a5,a6,a7,True]
      else [a0,a1,a2,a3,a4,a5,a6,a7,a8]
--girilen indisi toplam ile değiştiriyor
updateDist dist v toplam = do
      let a0 = dist !! 0
      let a1 = dist !! 1
      let a2 = dist !! 2
      let a3 = dist !! 3
      let a4 = dist !! 4
      let a5 = dist !! 5
      let a6 = dist !! 6
      let a7 = dist !! 7
      let a8 = dist !! 8
      f11 a0 a1 a2 a3 a4 a5 a6 a7 a8 v toplam 
f11::Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->[Int]
f11 a0 a1 a2 a3 a4 a5 a6 a7 a8 i toplam =
      if i==0
        then [toplam,a1,a2,a3,a4,a5,a6,a7,a8]
        else if i==1
                then [a0,toplam,a2,a3,a4,a5,a6,a7,a8]
                else if i==2
                        then [a0,a1,toplam,a3,a4,a5,a6,a7,a8]
                        else if i==3
                                then [a0,a1,a2,toplam,a4,a5,a6,a7,a8]
                                else if i==4
                                        then [a0,a1,a2,a3,toplam,a5,a6,a7,a8]
                                        else if i==5
                                                then [a0,a1,a2,a3,a4,toplam,a6,a7,a8]
                                                else if i==6 
                                                        then [a0,a1,a2,a3,a4,a5,toplam,a7,a8]
                                                        else if i==7
                                                                then [a0,a1,a2,a3,a4,a5,a6,toplam,a8]
                                                                else if i==8
                                                                        then [a0,a1,a2,a3,a4,a5,a6,a7,toplam]
        else [a0,a1,a2,a3,a4,a5,a6,a7,a8]