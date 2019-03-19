module Poker where

 winner hand1 hand2 = do
  let handn1 = quicksort hand1
  let handn2 = quicksort hand2
  let h11 = head handn1
  let t1 = tail handn1
  let h12 = head t1
  let t2 = tail t1
  let h13 = head t2
  let t3 = tail t2
  let h14 = head t3
  let t4 = tail t3
  let h15 = head t4
  let h21 = head handn2
  let t5 = tail handn2
  let h22 = head t5
  let t6 = tail t5
  let h23 = head t6
  let t7 = tail t6
  let h24 = head t7
  let t8 = tail t7
  let h25 = head t8
  let cval = checkv h11 h12 h13 h14 h15 h21 h22 h23 h24 h25
  let score1 = scorecheck h11 h12 h13 h14 h15
  let score2 = scorecheck h21 h22 h23 h24 h25 
  if (cval==0) then "Cards are invalid"
  else if (score1>score2) then "hand 1 is the winner" 
  else if (score2>score1) then "hand 2 is the winner"
  else if (score2==score1) && (h15>h25) then "hand 1 is the winner"
  else if (score2==score1) && (h15<h25) then "hand 2 is the winner"
  else if (score2==score1) && (h15==h25) then "Invalid"
  else "0"
 
 
 quicksort :: (Ord a) => [a] -> [a]  
 quicksort [] = []  
 quicksort (x:xs) =   
     let smallerSorted = quicksort [a | a <- xs, a <= x]  
         biggerSorted = quicksort [a | a <- xs, a > x]  
     in  smallerSorted ++ [x] ++ biggerSorted  
  
 
 scorecheck h1 h2 h3 h4 h5 = do
  let t1 = fst h1
  let t2 = t1 -1
  let t3 = t1 -2
  let t4 = t1 -3
  let t5 = t1 -4
  if (fst h1 == 14 )&&(fst h2 == 13 )&&(fst h3 == 12 )&&(fst h4 == 11 )&&(fst h5 == 10 )&&(snd h2 == snd h1 )&&(snd h3 == snd h1 )&&(snd h4 == snd h1 )&&(snd h5 == snd h1 ) then 9
  else if (fst h2 == t2)&&(fst h3 == t3)&&(fst h4 == t4)&&(fst h5 == t5)&&(snd h2 == snd h1 )&&(snd h3 == snd h1 )&&(snd h4 == snd h1 )&&(snd h5 == snd h1 )then 8
  else if (fst h1 == fst h2)&&(fst h1 == fst h3)&&(fst h1 == fst h4) then 7 
  else if (fst h2 == fst h5)&&(fst h2 == fst h3)&&(fst h2 == fst h4) then 7 
  else if (fst h1 == fst h2)&&(fst h2 == fst h3)&&(fst h4 == fst h5) then 6 
  else if (fst h1 == fst h2)&&(fst h3 == fst h4)&&(fst h4 == fst h5) then 6
  else if (snd h2 == snd h1 )&&(snd h3 == snd h1 )&&(snd h4 == snd h1 )&&(snd h5 == snd h1 ) then 5
  else if (fst h2 == t2)&&(fst h3 == t3)&&(fst h4 == t4)&&(fst h5 == t5) then 4
  else if (fst h1 == fst h2)&&(fst h1 == fst h3) then 3 
  else if (fst h2 == fst h3)&&(fst h2 == fst h4) then 3 
  else if (fst h3 == fst h4)&&(fst h3 == fst h5) then 3 
  else if (fst h1 == fst h2) && (fst h3 == fst h4) then 2
  else if (fst h1 == fst h2) && (fst h4 == fst h5) then 2 
  else if (fst h2 == fst h3) && (fst h4 == fst h5) then 2 
  else if (fst h1 == fst h2) || (fst h2 == fst h3) ||(fst h3 == fst h4) ||(fst h4 == fst h5) then 1
  else 0
  
  
 checkv i1 i2 i3 i4 i5 j1 j2 j3 j4 j5 =
  if(i1 == i2)||(i1 == i3)|| (i1 == i4)|| (i1 == i5)|| (i2 == i3)|| (i2 == i4)|| (i2 == i5)then 0
  else if (i3 == i4)||(i3 == i5)|| (i4 == i5)|| (j1 == j2)|| (j1 == j3 )|| (j1 == j4)|| (j1 == j5)then 0
  else if (j2 == j3)||(j2== j4)|| (j2 == j5)|| (j3 == j4)|| (j3 == j5 )|| (j4 == j5)|| (i1 == j1)then 0
  else if (i1 == j2)||(i1 == j3)|| (i1 == j4)|| (i1 == j5)|| (i2 == j1 )|| (i2 == j2)|| (i2 == j3)then 0
  else if (i2 == j4)||(i2 == j5)|| (i3 == j1)|| (i3 == j2)|| (i3 == j3 )|| (i3 == j4)|| (i3 == j5)then 0
  else if (i4 == j1)||(i4 == j2)|| (i4 == j3)|| (i4 == j4)|| (i4 == j5 )|| (i5 == j1)|| (i5 == j2)then 0
  else if (i5 == j3)||(i5 == j4) then 0
  else 1