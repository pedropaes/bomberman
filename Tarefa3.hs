{-|
Module : Main
Description : Tarefa 3 - Comprimir o estado do jogo
Copyright : Martinha Ribeiro <a68258@alunos.uminho.pt>; 
            Pedro Machado <a33524@alunos.uminho.pt>
Este módulo pertence à Tarefa 3 da 1ª fase do Projecto de LI1 - Bomberman.
Aos programas pré-definidos foram acrescentadas funções respeitando o tipo de dados em questão.

Nesta tarefa em específico foi necessário implementar duas funções principais:
*__Encode__  
*__Decode__ 
Recorrendo a diversas funções auxiliares.
-} 
module Main where

import System.Environment
import Data.Char

{- | Definimos um tipo de dados para ser usado nos /powerups/, que são respectivamente as coordenadas(x,y) e o caracter.
 -}
type PowerUp = (Int,Int,Char)


{- | Função principal que encoda uma lista de strings 
 -}
encode :: [String] -> String
encode (h:t) = (show size) ++ (encodaStringL4(encodaStringL3((encodaStringL2(encodaStringL1(miolo (lines mapaCompleto) 0 size)))++listabactivas++listajogadores)))
              where mapastring= (filter (/= '\n') (unlines (take (length h) (h:t))))
                    listabactivas= separa (unlines bombasactivas)
                    mapacbombs = filter (/= '\n') (criamapa mapastring listabombs 0 0 size)
                    listabombs = powerupsLista (filter (\x -> head x == '+') (drop (length h) (h:t)))
                    listaflames = powerupsLista (filter (\x -> head x == '!') (drop (length h) (h:t)))
                    mapaCompleto = criamapa mapacbombs listaflames 0 0 size
                    bombasactivas= filter (\x -> head x == '*') (drop (length h) (h:t)) 
                    jogadores= filter (\x -> isDigit (head x) == True  ) (drop (length h) (h:t)) 
                    listajogadores = separa (unlines jogadores)
                    size = length h


criamapa :: String -> [PowerUp] -> Int -> Int -> Int -> String
criamapa [] l x y s = "\n"
criamapa (h:t) [] x y s | (x==n) && (y==n) = h:""
                        | (y==n) = h:'\n':criamapa t [] (x+1) 0 s
                        | otherwise = h: criamapa t [] x (y+1) s
                         where n=s-1
criamapa (h:t) ((x1,y1,c):xs) x y s | (x==n) && (y==n) = h:"\n"
                                    | (y==n) = h:'\n':criamapa t ((x1,y1,c):xs) (x+1) 0 s
                                    | (y==x1) && (x==y1) && (c=='+') = if (h=='?') then '$': criamapa t xs x (y+1) s else '&' : criamapa t xs x (y+1) s
                                    | (y==x1) && (x==y1) && (c=='!') = if (h=='?') then '@': criamapa t xs x (y+1) s else '%' : criamapa t xs x (y+1) s
                                    | otherwise = h: criamapa t ((x1,y1,c):xs) x (y+1) s
                                     where n=s-1

             

powerupsLista :: [String] -> [PowerUp]
powerupsLista [] = []
powerupsLista (h:t) = createPowerup h: powerupsLista t


createPowerup :: String -> PowerUp
createPowerup [] = (-1,-1,'p')
createPowerup (h:t) | (h == '+') = (read n :: Int, read (getXNumbers (drop (v+1) t) 1) ::Int,h)
                    | (h == '!') = (read n :: Int, read (getXNumbers (drop (v+1) t) 1) ::Int,h)
                    | otherwise = createPowerup t
                        where n = getXNumbers t 1
                              v = length n

miolo :: [String] -> Int -> Int-> String
miolo [] x s  = ""
miolo (h:t) x s |  (x==0) = miolo t (x+1) s
                |  (x==n) = "|"
                |  (x==1) || (x==l1) = (drop 3 (init(init(init h)))) ++ (miolo t (x+1) s)
                |  (x==2) || (x==l2) = (filter (/= '#') (drop 2 (init(init (h))))) ++ (miolo t (x+1) s)
                |  otherwise = string ++ miolo t (x+1) s
                where n= s-1
                      l1= s-2
                      l2= s-3
                      string= filter (/= '#') h

esqueleto :: Int -> Int -> String -> [String]
esqueleto x t [] = replicate t '#':[]
esqueleto x t l | (x==0) = (replicate t '#'): esqueleto (x+1) t l
                | (x==s) = (replicate t '#'):[]
                | (x==1) || (x==s-1) = ("#  "++(take (t-6) l)++"  #" ): esqueleto (x+1) t (drop(t-6) l)
                | (x==2) || (x==s-2) = ("# " ++(merge (replicate (n+1) '#') (take n l))++" #" ): esqueleto (x+1) t (drop n l)
                | (odd x) = ('#':(take (t-2) l)++"#"): esqueleto (x+1) t (drop (t-2) l)
                | (even x) = (merge (replicate (r+1) '#') (take r l)) : esqueleto (x+1) t (drop r l)
                 where n = (div (t-4) 2)
                       r = (div t 2)
                       s = t-1



separa :: String -> String
separa [] = ""
separa (h:t) | (h=='\n') = '|':separa t
                      | otherwise = h:separa t





{- | Level 1 do Encoding
 -}
encodaStringL1 :: String -> String
encodaStringL1 [] = []
encodaStringL1 [x] = x:""
encodaStringL1 (h:t) | (h ==' ') && (i<=8) && (i>=2) =(int2Char i (h:t)):encodaStringL1 (drop (i-1) t)
                     | (h =='?') && (i<=5) && (i>=2) =(int2Char (i+10) (h:t)):encodaStringL1 (drop (i-1) t)
                     | otherwise = h:encodaStringL1 t
                     where i= contaSeguidos h (h:t)

{- | Level 1 do Encoding - função auxiliar
 -}
int2Char :: Int -> String -> Char
int2Char x l    | (x==2) = 'a'
                | (x==3) = 'b'
                | (x==4) = 'c'
                | (x==5) = 'd'
                | (x==6) = 'e'
                | (x==7) = 'f'
                | (x==8) = 'g'
                | (x==12) = 'h'
                | (x==13) = 'k'
                | (x==14) = 'l'
                | (x==15) = 'm'

{- | Level 2 do Encoding
 -}
encodaStringL2 :: String -> String
encodaStringL2 [] = []
encodaStringL2 [h] = h:""
encodaStringL2 (h:t) | (h=='?') = if (isLetter c) then c:encodaStringL2 (drop s t) else h:encodaStringL2 t
                     | (h==' ') = if (isLetter c) then c:encodaStringL2 (drop s t) else h:encodaStringL2 t
                     | otherwise = h:encodaStringL2 t
                     where  c=fst (aux_L2 (h:t))  
                            s=snd (aux_L2 (h:t))

{- | Level 2 do Encoding - Função auxiliar que conta o número consecutivo de caracteres numa dada String
 -}
contaSeguidos :: Char -> String -> Int
contaSeguidos c [] = 0
contaSeguidos c (x:t) | (c==x) && (c==head t) = 1+contaSeguidos c t
                      | (c==x) && (c/=head t) = 1
                      | otherwise = 0

{- | Level 2 do Encoding - função auxiliar
 -}
aux_L2 :: String -> (Char,Int)
aux_L2 [] = ('9',0)
aux_L2 l | ((take 7 l)=="? ? ? ?") = ('n',6)
         | ((take 5 l)=="? ? ?") = ('o',4)
         | ((take 3 l)=="? ?") = ('p',2)
         | ((take 7 l) == "       ") = ('q',6)
         | ((take 6 l) == "      ") = ('r',5)
         | ((take 5 l) == "     ") = ('s',4)
         | ((take 4 l) == "    ") = ('t',3)
         | ((take 3 l) == "   ") = ('u',2)
         | ((take 2 l) == "  ") = ('v',1)
         | otherwise = ('9',0)



{- | Level 3 do Encoding
 -}

encodaStringL3 :: String -> String
encodaStringL3 [] = []
encodaStringL3 [h] = h:""
encodaStringL3 (h:t) | (h==' ') = if (isLetter c) then c:encodaStringL3 (drop s t) else h:encodaStringL3 t
                     | (h=='+') = if (isLetter c) then c:encodaStringL3 (drop s t) else h:encodaStringL3 t
                     | otherwise = h:encodaStringL3 t
                     where  c=fst (aux_L3 (h:t))  
                            s=snd (aux_L3 (h:t))


{- | Level 3 do Encoding - Função auxiliar
 -}
aux_L3 :: String -> (Char,Int)
aux_L3 [] = ('9',0)
aux_L3 l | ((take 2 l) == " 0") = ('A',1)
         | ((take 2 l) == " 1") = ('B',1)
         | ((take 2 l) == " 2") = ('C',1)
         | ((take 2 l) == " 3") = ('D',1)
         | ((take 2 l) == " 4") = ('E',1)
         | ((take 2 l) == " 5") = ('F',1)
         | ((take 2 l) == " 6") = ('G',1)
         | ((take 2 l) == " 7") = ('H',1)
         | ((take 2 l) == " 8") = ('I',1)
         | ((take 2 l) == " 9") = ('J',1)
         | ((take 2 l) == " ?") = ('K',1)
         | ((take 2 l) == " h") = ('L',1)
         | ((take 2 l) == " @") = ('M',1)
         | ((take 2 l) == " k") = ('N',1)
         | ((take 2 l) == " m") = ('O',1)
         | ((take 2 l) == " l") = ('P',1)
         | ((take 3 l) == " h ") = ('Q',2)
         | ((take 3 l) == " ? ") = ('R',2)
         | ((take 3 l) == " k ") = ('S',2)
         | ((take 3 l) == " $ ") = ('T',2)
         | ((take 3 l) == " l ") = ('U',2)
         | ((take 2 l) == " $") = ('V',1)
         | ((take 2 l) == " p") = ('W',1)
         | ((take 2 l) == " g") = ('X',1)
         | ((take 4 l) == "++++") = ('Y',3)
         | ((take 3 l) == "+++") = ('Z',2)
         | otherwise = ('9',0)


{- | Level 4 do Encoding 
 -}
encodaStringL4 :: String -> String
encodaStringL4 [] = []
encodaStringL4 [h] = h:""
encodaStringL4 (h:t) | (h==' ') = if (not (isLetter c)) then c:encodaStringL4 (drop s t) else h:encodaStringL4 t
                     | (h=='?') = if (not (isLetter c)) then c:encodaStringL4 (drop s t) else h:encodaStringL4 t
                     | otherwise = h:encodaStringL4 t
                     where  c=fst (aux_L4 (h:t))  
                            s=snd (aux_L4 (h:t))
           
{- | Level 4 do Encoding - Função auxiliar
 -}           
aux_L4 :: String -> (Char,Int)
aux_L4 [] = ('A',0)
aux_L4 l | ((take 2 l) == " o") = ('.',1)
         | ((take 2 l) == " |*") = (':',2)
         | ((take 2 l) == "?a?a?") = ('-',4)
         | ((take 2 l) == "?a?") = ('_',2)
         | ((take 2 l) == "?c?") = ('«',2)
         | ((take 2 l) == "?d?") = ('»',2)
         | ((take 2 l) == "?e?") = ('=',2)
         | ((take 2 l) == "?a") = (';',1)
         | ((take 2 l) == "?b") = ('^',1)
         | ((take 2 l) == "?c") = ('~',1)
         | otherwise = ('A',0)

{- | Level 1 do Decoding 
 -}
decodaStringL1 :: String -> String
decodaStringL1 [] = ""
decodaStringL1 (h:t) | (h=='a') = (char2String h)++decodaStringL1 t
                     | (h=='b') = (char2String h)++decodaStringL1 t
                     | (h=='c') = (char2String h)++decodaStringL1 t
                     | (h=='d') = (char2String h)++decodaStringL1 t
                     | (h=='e') = (char2String h)++decodaStringL1 t
                     | (h=='f') = (char2String h)++decodaStringL1 t
                     | (h=='g') = (char2String h)++decodaStringL1 t
                     | (h=='h') = (char2String h)++decodaStringL1 t
                     | (h=='k') = (char2String h)++decodaStringL1 t
                     | (h=='l') = (char2String h)++decodaStringL1 t
                     | (h=='m') = (char2String h)++decodaStringL1 t
                     | otherwise = h:decodaStringL1 t

{- | Level 1 do Decoding - Função auxiliar que transforma um caracter numa string
 -}
char2String :: Char -> String
char2String c | (c=='a') = "  "
              | (c=='b') = "   "
              | (c=='c') = "    "
              | (c=='d') = "     "
              | (c=='e') = "      "
              | (c=='f') = "       "
              | (c=='g') = "        "
              | (c=='h') = "??"
              | (c=='k') = "???"
              | (c=='l') = "????"
              | (c=='m') = "?????"
              | otherwise = ""

{- | Level 2 do Decoding 
 -}
decodaStringL2 :: String -> String
decodaStringL2 [] = ""
decodaStringL2 (h:t) | (h=='n') = (char2String2 h)++decodaStringL2 t
                     | (h=='o') = (char2String2 h)++decodaStringL2 t
                     | (h=='p') = (char2String2 h)++decodaStringL2 t
                     | (h=='q') = (char2String2 h)++decodaStringL2 t
                     | (h=='r') = (char2String2 h)++decodaStringL2 t
                     | (h=='s') = (char2String2 h)++decodaStringL2 t
                     | (h=='t') = (char2String2 h)++decodaStringL2 t
                     | (h=='u') = (char2String2 h)++decodaStringL2 t
                     | (h=='v') = (char2String2 h)++decodaStringL2 t
                     | otherwise = h:decodaStringL2 t

{- | Level 2 do Decoding - Função auxiliar que transforma um caracter numa string
 -}
char2String2 :: Char -> String
char2String2 c | (c=='n') = "? ? ? ?"
               | (c=='o') = "? ? ?"
               | (c=='p') = "? ?"
               | (c=='q') = "       "
               | (c=='r') = "      "
               | (c=='s') = "     "
               | (c=='t') = "    "
               | (c=='u') = "   "
               | (c=='v') = "  "
               | otherwise = ""


{- | Level 3 do Decoding 
 -}
decodaStringL3 :: String -> String
decodaStringL3 [] = ""
decodaStringL3 (h:t) | (h=='A') = (char2String3 h)++decodaStringL3 t
                     | (h=='B') = (char2String3 h)++decodaStringL3 t
                     | (h=='C') = (char2String3 h)++decodaStringL3 t
                     | (h=='D') = (char2String3 h)++decodaStringL3 t
                     | (h=='E') = (char2String3 h)++decodaStringL3 t
                     | (h=='F') = (char2String3 h)++decodaStringL3 t
                     | (h=='G') = (char2String3 h)++decodaStringL3 t
                     | (h=='H') = (char2String3 h)++decodaStringL3 t
                     | (h=='I') = (char2String3 h)++decodaStringL3 t
                     | (h=='J') = (char2String3 h)++decodaStringL3 t
                     | (h=='K') = (char2String3 h)++decodaStringL3 t
                     | (h=='L') = (char2String3 h)++decodaStringL3 t
                     | (h=='M') = (char2String3 h)++decodaStringL3 t
                     | (h=='N') = (char2String3 h)++decodaStringL3 t
                     | (h=='O') = (char2String3 h)++decodaStringL3 t
                     | (h=='P') = (char2String3 h)++decodaStringL3 t
                     | (h=='Q') = (char2String3 h)++decodaStringL3 t
                     | (h=='R') = (char2String3 h)++decodaStringL3 t
                     | (h=='S') = (char2String3 h)++decodaStringL3 t
                     | (h=='T') = (char2String3 h)++decodaStringL3 t
                     | (h=='U') = (char2String3 h)++decodaStringL3 t
                     | (h=='V') = (char2String3 h)++decodaStringL3 t
                     | (h=='W') = (char2String3 h)++decodaStringL3 t
                     | (h=='X') = (char2String3 h)++decodaStringL3 t
                     | (h=='Y') = (char2String3 h)++decodaStringL3 t
                     | (h=='Z') = (char2String3 h)++decodaStringL3 t
                     | otherwise = h:decodaStringL3 t


char2String3 :: Char -> String
char2String3 c | (c=='A') = " 0"
               | (c=='B') = " 1"
               | (c=='C') = " 2"
               | (c=='D') = " 3"
               | (c=='E') = " 4"
               | (c=='F') = " 5"
               | (c=='G') = " 6"
               | (c=='H') = " 7"
               | (c=='I') = " 8"
               | (c=='J') = " 9"
               | (c=='K') = " ?"
               | (c=='L') = " h"
               | (c=='M') = " @"
               | (c=='N') = " k"
               | (c=='O') = " m"
               | (c=='P') = " l"
               | (c=='Q') = " h "
               | (c=='R') = " ? "
               | (c=='S') = " k "
               | (c=='T') = " $ "
               | (c=='U') = " l "
               | (c=='V') = " $"
               | (c=='W') = " p"
               | (c=='X') = " g"
               | (c=='Y') = "++++"
               | (c=='Z') = "+++"
               | otherwise = ""

{- | Level 4 do Decoding 
 -}
decodaStringL4 :: String -> String
decodaStringL4 [] = ""
decodaStringL4 (h:t) | (h=='.') = (char2String4 h)++decodaStringL4 t
                     | (h==':') = (char2String4 h)++decodaStringL4 t
                     | (h=='-') = (char2String4 h)++decodaStringL4 t
                     | (h=='_') = (char2String4 h)++decodaStringL4 t              
                     | (h=='«') = (char2String4 h)++decodaStringL4 t   
                     | (h=='»') = (char2String4 h)++decodaStringL4 t   
                     | (h==',') = (char2String4 h)++decodaStringL4 t
                     | (h==';') = (char2String4 h)++decodaStringL4 t 
                     | (h=='^') = (char2String4 h)++decodaStringL4 t  
                     | (h=='~') = (char2String4 h)++decodaStringL4 t 
                     | otherwise = h:decodaStringL4 t

{- | Level 4 do Decoding - Função auxiliar que transforma um caracter numa string
 -}
char2String4 :: Char -> String
char2String4 c | (c=='.') = " o"
               | (c==':') = " |*"
               | (c=='-') = "?a?a?"
               | (c=='_') = "?a?"
               | (c=='«') = "?c?"
               | (c=='»') = "?d?"
               | (c=='=') = "?e?"
               | (c==';') = "?a"
               | (c=='^') = "?b"
               | (c=='~') = "?c"
               | otherwise = ""
             


                        
sacapowerups :: String -> Char -> Int -> Int -> Int-> [String]
sacapowerups [] _ _ _ _ = []
sacapowerups (h:t) c s x y | (x==(s-1)) = sacapowerups t c s 0 (y+1) 
                           | (y==(s-1)) = sacapowerups [] c s 0 (y+1) 
                           | (h=='$') || (h=='&') = if (c=='+') then ("+ "++(show x)++" "++(show y)):sacapowerups t c s (x+1) y else sacapowerups t c s (x+1) y
                           | (h=='@') || (h=='%') = if (c=='!') then ("! "++(show x)++" "++(show y)):sacapowerups t c s (x+1) y else sacapowerups t c s (x+1) y
                           | otherwise = sacapowerups t c s (x+1) y 
                            where l=s-1 
                       
{-| Faz decode do que está a seguir ao mapa
-}
decodaResto :: String -> [String]
decodaResto []  = []
decodaResto l = lines (decodaString l) 

decodaString :: String -> String
decodaString [] = ""
decodaString (h:t) | (h=='|') = '\n':decodaString t 
                   | otherwise = h:decodaString t


contaPowerUps :: String -> Int
contaPowerUps [] = 0
contaPowerUps (h:t) | (h=='+') && (head t=='+') = 1+contaPowerUps t
                    | (h=='+') && (head t/='+') = 1
                    | otherwise = 0+ contaPowerUps t 

getPowerups :: String -> String
getPowerups [] = ""
getPowerups [h] = if (h=='+') || (h=='!') then h:"" else ""
getPowerups (h:t) | (h=='|') || (isDigit h) = ""
                  | ((h=='+') || (h=='!')) && ((head t)=='+' || (head t) =='!') = h:getPowerups t
                  | ((h=='+') || (h=='!')) && ((head t)/='+' || (head t) /='!') = h:""
                  | (h/='+')  || (h/='!') = getPowerups t
         
{-| A função ’getXNumbers’ obtém de uma string x números sob a forma de uma string também  

 == Exemplos de utilização:
 >>> getXNumbers " 12 13 30 2 qualquer coisa" ^
 "12 13 30 2"
-}
getXNumbers :: String -> Int -> String
getXNumbers [] x = ""
getXNumbers [h] x = if (isDigit h) then h:"" else ""
getXNumbers (h:t) 0 = []
getXNumbers (h:t) x  | (h=='|') || (h=='*') = ""
                     | (isDigit h) && (s=='|') = h:""
                     | (x==1) && (isDigit h) && (not (isDigit s)) = h:""
                     | (isDigit h) && (not (isDigit s)) = h:' ':getXNumbers t (x-1)
                     | (isDigit h) = h:getXNumbers t x
                     | (isSpace h) = getXNumbers t x
                     | otherwise = getXNumbers t x
                      where s= head t
{-| A função ’merge’ intercala duas listas, e acaba quando uma delas acabar

 == Exemplos de utilização:
 >>> merge [1,2,3,4,5] [a,b,c,d,e]
 [1,a,2,b,3,c,4,d,5,e]
-}
merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys
                      

{- | Função principal que encoda uma lista de strings 
 -}
decode :: String -> [String]
decode [] = []
decode l = mapareal ++ bombs ++ flames ++ decodaResto resto
               where n = length (readNumber l)
                     size= numbertoInt (readNumber l)
                     mapa = esqueleto 0 size mapaseparado
                     mapastring = filter (/= '\n') (unlines (mapa))
                     mapaquasereal = map (map (\y -> if y == '$' || y == '@' then '?' else y)) mapa
                     mapareal = map (map (\y -> if y == '&' || y == '%' then ' ' else y)) mapaquasereal
                     bombs = sacapowerups mapastring '+' size 0 0
                     flames = sacapowerups mapastring '!' size 0 0
                     mapaseparado = drop n (getmapa lista)
                     lista = decodaStringL1 (decodaStringL2 (decodaStringL3  (decodaStringL4 l)))
                     resto = getresto lista


-- | ’main’: função que controla as funções ’encode’ e ’decode’
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"




--Funções auxiliares

readNumber :: String -> String
readNumber [] = ""
readNumber (h:t) | (isDigit h) = h:readNumber t
                 | otherwise = ""

numbertoInt :: String -> Int
numbertoInt l = read l :: Int



getresto :: String -> String
getresto [] = ""
getresto (h:t) | (h == '|') = t
               | otherwise = getresto t
                 
getmapa :: String -> String
getmapa [] = ""
getmapa (h:t) | (h == '|') = ""
              | otherwise = h:getmapa t

