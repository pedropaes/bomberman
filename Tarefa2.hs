{- 
Module : Trabalho LI 1ª fase
Description : Tarefa2
Copyright : Martinha Ribeiro <a68258@alunos.uminho.pt>; 
            Pedro <outro@algures.com>
 Modulo Haskell contendo as funções necessárias para o funcionamento da tarefa2.
 -} 

import Data.Char 
import System.Environment

{- | Definimos um tipo de dados para ser usado nos Jogadores e Bomba's
 -}
type Jogador = (Int,Int,Int,String) -- Tipo do Jogador (Id,cord x,cordy,powerups)
type Bomba = [Int] -- Tipo das Bombas (Caracter, cord x, cord y, jogador, raio, tempo)

-- | ’main’: função que controla a função move
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"

{- | Função implementada que reage a um comando específico de um jogador usando funções auxiliares: __move_possivel__ e __faz_move__
 -}
move :: [String] -> Int -> Char -> [String]
move l j c = if (move_possivel l c j  coordJ == True) then mapa++faz_move l j c coordJ else l
             where mapa =  filter (\x -> head x == '#') l
                   coordJ = coordenadas l (intToDigit j)

{- | Função que testa se o movimento do jogador é possível
 -}
move_possivel :: [String] -> Char -> Int -> (Int,Int) -> Bool
move_possivel l c j (x,y) | (c=='B') = if ((deixouBomba j listabombas) == True)  then False else True -- se o player já pos bombas ou não
                          | (c=='U') = destino_valido (x,y-1) mapa
                          | (c=='D') = destino_valido (x,y+1) mapa
                          | (c=='L') = destino_valido (x-1,y) mapa
                          | (c=='R') = destino_valido (x+1,y) mapa
                          | otherwise = False
                          where  mapa = filter (\x -> head x == '#') l
                                 listabombas = filter (\x -> head x == '*') l
                                

{- | Função auxiliar da __move_possivel__ que vê se o destino do jogador é válido
 -}
destino_valido :: (Int,Int) -> [String] -> Bool
destino_valido (x,y) l | (destino == '#') || (destino == '?') = False
                       | (destino == ' ') = True
                       | otherwise = True
                                   where destino = (l!!y)!!x -- vê qual é o destino baseado na posição na lista de strings e depois na posição dentro da string
                                   
{- | Efectua o movimento do jogador e gera as alterações no jogo
 -}
faz_move :: [String] -> Int -> Char -> (Int,Int) -> [String]
faz_move l j c (x,y)| (c=='B') = if (existebomba) then listapowerups++listabombas++(move_jogador l (listajogadores l) j (0,0)) else listapowerups++listadeBombas++(move_jogador l (listajogadores l) j (0,0))
                    | (c=='U') = (removepowerup listapowerups (x,y-1))++listabombas++(move_jogador l (listajogadores l) j (0,-1))
                    | (c=='D') = (removepowerup listapowerups (x,y+1))++listabombas++(move_jogador l (listajogadores l) j (0,1)) 
                    | (c=='L') = (removepowerup listapowerups (x-1,y))++listabombas++(move_jogador l (listajogadores l) j (-1,0))
                    | (c=='R') = (removepowerup listapowerups (x+1,y))++listabombas++(move_jogador l (listajogadores l) j (1,0)) 
                    where listabombas = filter (\x -> head x == '*') l
                          listapowerups = filter (\x -> (head x=='!') || (head x =='+')) l
                          bombanova = criabombas2 (x,y) j 1 10
                          lbombasInt = bombasInt listabombas
                          listadebombasInt = poeBomba bombanova lbombasInt
                          listadeBombas= bombas (listadebombasInt)
                          existebomba= bombaExiste bombanova lbombasInt

                        

{- | Função auxiliar da __faz_move__ que retorna as coordenadas actualizadas dos jogadores
 -}
move_jogador :: [String] -> [Jogador] ->  Int -> (Int,Int) -> [String]
move_jogador l [] j (x1,y1) = []
move_jogador l ((i,x,y,p):t) j (x1,y1) | ((i == j) && (power /=' ')) = if (power=='+') then ((show i)++" "++(show x2)++" "++(show y2)++" "++(power:p)): move_jogador l t j (x,y) else ((show i)++" "++(show x2)++" "++(show y2)++(p++(power:""))): move_jogador l t j (x,y) 
                                       | (i ==  j) = ((show i)++" "++(show x2)++" "++(show y2)++" "++p): move_jogador l t j (x,y) 
                                       | otherwise = ((show i)++" "++(show x)++" "++(show y)++" "++p):move_jogador l t j (x1,y1)
                                       where x2= x+x1
                                             y2= y+y1 
                                             powerups = filter (\x -> (head x=='!') || (head x =='+')) l                       
                                             power= tempowerup powerups (x2,y2)



{- | Função auxiliar que verifica as coordenadas de um jogador ou de um powerup dado um caracter
 -}
coordenadas :: [String] -> Char -> (Int,Int)
coordenadas [] c = (0,0)
coordenadas (h:t) c | (c == (head h)) = (read x :: Int,read y :: Int)
                    | otherwise = coordenadas t c
                     where x = getXNumbers (drop 1 h) 1
                           n = length x
                           y = getXNumbers (drop (n+1) h) 1

{-| A função ’getXNumbers’ obtém de uma string x números sob a forma de uma string também  

 == Exemplos de utilização:
 >>> getXNumbers " 12 13 30 2 qualquer coisa" ^
 "12 13 30 2"
-}                                              
getXNumbers :: String -> Int -> String
getXNumbers [] x = ""
getXNumbers [h] x = if (isDigit h) then h:"" else ""
getXNumbers (h:t) 0 = []
getXNumbers (h:t) x  | (isSpace h) =' ':getXNumbers t x
                     | (isDigit h) && (isDigit(head t))= h:getXNumbers t x
                     | (isDigit h) && (not (isDigit (head t)))=h:getXNumbers t (x-1)
                     | (h=='|') = ""
                     | otherwise = getXNumbers t x
           
-- A função que lista os jogadores ...
listajogadores :: [String] -> [Jogador]
listajogadores [] = []
listajogadores (h:t) | (head h =='0') || (head h =='1') || (head h =='2') || (head h =='3') = (digitToInt (h!!0),digitToInt (h!!2),digitToInt (h!!4),drop 6 h): listajogadores t
                     | otherwise = listajogadores t

-- A função criabombas ...

criabombas2 :: (Int,Int) -> Int -> Int -> Int -> Bomba
criabombas2 (x,y) j r t = 4:x:y:j:r:t:[]

-- A função removepowerup ...
removepowerup:: [String]->(Int,Int) -> [String]
removepowerup [] (_,_) = []
removepowerup (h:t) (x,y) | ((h!!2) == intToDigit x) && ((h!!4) == intToDigit y) = removepowerup t (x,y)
                          | otherwise = h:removepowerup t (x,y)

-- A função ganhapowerup ...
tempowerup :: [String] -> (Int,Int) -> Char
tempowerup [] (_,_) = ' '
tempowerup (h:t) (x,y) | (x1 ==  x) && (y1 ==  y) = c
                       | otherwise =  tempowerup t (x,y)
                        where n= getXNumbers h 1
                              l= length (getXNumbers h 1)
                              x1= read n :: Int
                              t2= drop (l+1) h
                              m= getXNumbers t2 1
                              y1= read m :: Int
                              c= head h


-- A função deixouBomba recebe o número do Jogador e verifica se tem alguma bomba posta no mapa
deixouBomba :: Int -> [String] -> Bool
deixouBomba x [] = False
deixouBomba x (h:t) | (x == j ) = True
                    | otherwise = deixouBomba x t
                         where j = digitToInt  (head((words h) !! 3))

                       


{- | Função que acrescenta uma bomba à lista
 -}
poeBomba :: Bomba -> [Bomba] -> [Bomba]
poeBomba b [] = []
poeBomba b l = b:l 


-- A função bombas lista as bombas
bombas :: [[Int]] -> [String]
bombas [] = []
bombas (h:t) | (head h ==4) = ("*"++" "++t1++" "++t2++" "++t3++" "++t4++" "++t5):bombas t
             | otherwise = bombas t
                  where t1 = show (h!!1) 
                        t2 = show (h!!2) 
                        t3 = show (h!!3) 
                        t4 = show (h!!4) 
                        t5 = show (h!!5) 

bombasInt :: [String] -> [[Int]]
bombasInt [] = []
bombasInt (h:t) = (4:aux_bombas h):bombasInt t
                 where aux_bombas :: String -> [Int]
                       aux_bombas [] = []
                       aux_bombas l = i:aux_bombas (drop (s+1) l)
                                         where n= getXNumbers l 1
                                               s= length n
                                               i = read n :: Int


bombaExiste :: Bomba -> [Bomba] -> Bool
bombaExiste b [] = False
bombaExiste (b:x1:y1:t) (x:xs) | (x1 == (x!!1)) && (y1 == (x!!2)) = True
                               | otherwise = bombaExiste (b:x1:y1:t) xs

d=["#########","#       #","# #?#?# #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 1 3","+ 3 3","! 5 5","! 5 7","* 7 7 1 1 10","0 4 3 +","1 7 7","2 2 3","3 7 7"]