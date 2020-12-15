{-|
Module : Main
Description : Tarefa 4 - Passagem do tempo
Copyright : Martinha Ribeiro <a68258@alunos.uminho.pt>; 
            Pedro Machado <a33524@alunos.uminho.pt>

Este módulo pertence à Tarefa 4 da 2ª fase do Projecto de LI1 - Bomberman.
Tendo como input o estado do jogo e o instante, determina os efeitos da passagem do tempo

-} 
module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import Data.List.Split

{-| Com este tipo de dado tentamos que desse para representar todo o conteúdo no mapa
-}
data Celula = Vazio | Pedra | Tijolo | BombsT | FlamesT | Bombs | Flames | Bomba (Int,Int,Int) | Jogador (Int,String) | JogBomba (Int,String,Int,Int) deriving (Show,Eq,Read)

{-| Blocos são a maneira de fazermos corresponder a cada célula do mapa coordenadas x y
-}
type Bloco = (Int,Int,Celula) -- Cada Bloco tem as coordenadas e o conteúdo da respectiva célula

{-| O Mapa do jogo tem todo o conteúdo do estado de jogo representado em blocos de coordenadas
-}
type Mapa = [Bloco] -- O mapa será agora um conjunto de blocos contendo toda a informação do jogo


{-| Função principal que recebe o estado do jogo e um instante de tempo, e retorna o efeito da passagem de tempo no estado do jogo
-}
avanca :: [String] -> Int -> [String]
avanca l t | (t<=t_limit) = if (n>0) then m1  else m2
           | otherwise = if (n>0) then m3 else m4
         where size = length (head l)
               map = take size l
               resto_blocos = rest2block (drop size l)
               t_limit = (size-2)^2
               instante = t_limit - t
               coord_spiral = (spiral size 1 size)!!instante
               bombas = filtrabombas resto_blocos 
               n = length bombas
               mapa_blocos = map2block map resto_blocos 0 size
               mapa_espiral = spiral_effect coord_spiral mapa_blocos
               detona_mapa = explodeBombas mapa_blocos bombas
               detona_mapaEspiral =explodeBombas mapa_espiral bombas
               m1 = (chunksOf size $ map2string $ detona_mapaEspiral)++(novoResto resto_blocos detona_mapaEspiral)
               m2 = (chunksOf size $ map2string $ mapa_espiral)++(novoResto resto_blocos mapa_espiral)
               m3 = (chunksOf size $ map2string $ detona_mapa)++(novoResto resto_blocos detona_mapa)
               m4 = (chunksOf size $ map2string $ mapa_blocos)++(novoResto resto_blocos mapa_blocos)
              
{-| Esta função filtra as bombas que vão explodir no próximo instante
-}

filtrabombas :: [Bloco] -> [(Int,Int,Int)]
filtrabombas [] = []
filtrabombas ((x,y,Bomba(j,r,t)):xs) | (t==0) = (x,y,r):filtrabombas xs
                                     | otherwise = filtrabombas xs
filtrabombas ((x,y,JogBomba(j,s,r,t)):xs) | (t==0) = (x,y,r):filtrabombas xs
                                          | otherwise = filtrabombas xs
filtrabombas ((x,y,c):xs) = filtrabombas xs

{-| No caso de haver múltiplas bombas no mapa usa recursivamente a função __explode__ para as detonar no mapa 
-}
explodeBombas :: Mapa -> [(Int,Int,Int)] -> Mapa
explodeBombas l [] = l
explodeBombas l ((x,y,r):xs) = explodeBombas exp xs
                             where coords= coords_X l (x,y,r)
                                   exp= explode l coords (x,y)

{-| Esta função gera um mapa de blocos a partir de uma lista de strings, recorrendo também aos possíveis powerups escondidos atrás dos tijolos e representa também as bombas e os jogadores no mapa

-}
map2block :: [String] -> [Bloco] -> Int -> Int -> Mapa
map2block [] l z s= []
map2block (h:t) l z s = (map2blockaux coord l) ++ map2block t l (z+1) s 
                      where  coord = zip3 h [0..(s-1)] (replicate s z)

{-| Função auxiliar da __map2block__
-}
map2blockaux :: [(Char,Int,Int)] -> [Bloco] -> [Bloco]
map2blockaux [] l = []
map2blockaux ((c,x,y):t) l | (c=='#') = (x,y,Pedra):rec
                           | (c==' ') = case (celula)  of { Nothing -> (x,y,Vazio):rec ; Just cel -> (x,y,cel):rec}
                           | (c=='?') = case (celula)  of { Nothing -> (x,y,Tijolo):rec; Just Bombs -> (x,y,BombsT):rec; Just Flames -> (x,y,FlamesT):rec}
                            where celula= mylookup (x,y) l
                                  rec=map2blockaux t l 


rest2block :: [String] -> [Bloco]
rest2block [] = []
rest2block (h:t) | (i=='+') = (x1,y1,Bombs):rest2block t 
                 | (i=='!') = (x1,y1,Flames):rest2block t 
                 | (i=='*') = case (lookupstring t (x1,y1)) of { Nothing -> (x1,y1,Bomba (i2,i3,i4-1)):rest2block t ; Just (Jogador (i,s)) -> (x1,y1,JogBomba (i,s,i3,i4-1)):rest2block t}
                 | (i=='0') || (i=='1') || (i=='2') || (i=='3') = (x1,y1,Jogador (n,b)):rest2block t 
                 | otherwise = rest2block t
                  where items = words h
                        x1 = (read (items!!1):: Int)
                        y1 = (read (items!!2):: Int)
                        i2 = read (items!!3) :: Int
                        i3 = read (items!!4) :: Int
                        i4 = read (items!!5) :: Int
                        n = read (items!!0) :: Int 
                        b = (filter (== '+') h)++(filter (=='!') h)
                        f = length (filter (=='!') h)
                        i = head h

{-| A função mylookup foi a nossa implementação da função já existente __lookup__  que nos retorna um tipo Maybe do ṕossível conteúdo dessas coordenadas no mapa 

-}
mylookup :: (Int,Int) -> [Bloco] -> Maybe Celula
mylookup (x,y)  [] = Nothing
mylookup (x,y) ((x1,y1,cel):t) | (x==x1) && (y==y1) = Just cel
                               | otherwise = mylookup (x,y) t




lookupstring :: [String] -> (Int,Int) -> Maybe Celula
lookupstring [] (x,y) = Nothing
lookupstring (h:t) (x,y) | (x==x1) && (y==y1) && (isDigit i)= Just (Jogador (n,b))
                         | otherwise = lookupstring t (x,y)
                         where i = head h
                               items = words h
                               x1 = (read (items!!1):: Int)
                               y1 = (read (items!!2):: Int)
                               n = read (items!!0) :: Int 
                               b = (filter (== '+') h)++(filter (=='!') h)
{-| Depois de obtidas as coordenadas da explosão altera o Mapa de jogo após o efeito da explosão
-}
explode :: Mapa -> [(Int,Int)] -> (Int,Int) -> Mapa
explode ((x,y,c):t) [] (x2,y2)= ((x,y,c):t)
explode ((x,y,c):t) ((x1,y1):xs) (x2,y2) | (x==x2) && (y==y2) = case c of {Bomba(j,r,t) -> (x,y,Bomba (j,r,t)):rec;Jogador (_,_) -> (x,y,Vazio):rec; Vazio -> (x,y,Vazio):rec;JogBomba (j,_,r,t)->(x,y,Bomba(j,r,t)):rec}
                                         | (x==x1) && (y==y1) = case c of { Tijolo -> (x,y,Vazio):rec; BombsT -> (x,y,Bombs):rec; FlamesT -> (x,y,Flames):rec; Bombs -> (x,y,Vazio):rec; Flames -> (x,y,Vazio):rec;Bomba(j,r,t) -> (x,y,Bomba (j,r,1)):rec;Jogador (_,_) -> (x,y,Vazio):rec; Vazio -> (x,y,Vazio):rec;JogBomba (j,_,r,t)->(x,y,Bomba(j,r,1)):rec}
                                         | otherwise =(x,y,c):explode t ((x1,y1):xs) (x2,y2)
                                          where rec=explode t xs (x2,y2) 

{-| A função coords_X vai calculando no mapa as coordenadas atingidas pela explosão, fazendo o cálculo do centro da explosão para fora e ordenando as mesmas para que o mapa seja percorrido uma única vez 

-}
coords_X :: Mapa -> (Int,Int,Int)  -> [(Int,Int)]
coords_X l (x,y,r) = (reverse (aux (x,y) r 1 'U' l))++(reverse(aux (x,y) r 1 'L' l))++[(x,y)]++(aux (x,y) r 1 'R' l)++(aux (x,y) r 1 'D' l) 

{-| Função auxiliar da coords_X que determina até onde podem ir os raios da explosão
-}
aux :: (Int,Int) -> Int -> Int -> Char -> Mapa -> [(Int,Int)]
aux (x,y) r z c l | (c=='U') && (z<=r) = case (mylookup up l) of {Nothing-> []; Just Tijolo -> [up];Just Pedra -> [] ;Just Bombs -> [up];Just Flames -> [up];Just BombsT -> [up];Just FlamesT -> [up];Just cel -> up:aux (x,y) r (z+1) c l  } 
                  | (c=='L') && (z<=r) = case (mylookup left l ) of {Nothing-> []; Just Tijolo -> [left];Just Pedra -> [];Just Bombs -> [left];Just Flames -> [left];Just BombsT -> [left];Just FlamesT -> [left];Just cel -> left:aux (x,y) r (z+1) c l  } 
                  | (c=='R') && (z<=r) = case (mylookup right l) of {Nothing-> []; Just Tijolo -> [right];Just Pedra -> [];Just Bombs -> [right];Just Flames -> [right];Just BombsT -> [right];Just FlamesT -> [right];Just cel -> right:aux (x,y) r (z+1) c l  } 
                  | (c=='D') && (z<=r) = case (mylookup down l ) of {Nothing-> []; Just Tijolo -> [down];Just Pedra -> [];Just Bombs -> [down];Just Flames -> [down];Just BombsT -> [down];Just FlamesT -> [down];Just cel -> down:aux (x,y) r (z+1) c l  } 
                  | otherwise = []
                   where   up= (x,y-z)
                           left=(x-z,y)
                           right=(x+z,y)
                           down=(x,y+z) 

{-| A função ’spiral’ gera uma lista de coordenadas sobre a área útil do jogo. Se o mapa for tamanho n, será fornecido como argumento n-2 que corresponde à area de jogo util a ser preenchida por pedras 

 == Exemplos de utilização num mapa de dimensão 9:
 >>> spiral 7 1 7 
[(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(6,7),(5,7),(4,7),(3,7),(2,7),(1,7),(1,6),(1,5),(1,4),(1,3),(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(6,3),(6,4),(6,5),(6,6),(5,6),(4,6),(3,6),(2,6),(2,5),(2,4),(2,3),(3,3),(4,3),(5,3),(5,4),(5,5),(4,5),(3,5),(3,4),(4,4)]
-}
spiral :: Int -> Int -> Int ->  [(Int,Int)]
spiral 1 x y = [(x,y)]
spiral size x y = l1++l2++l3++l4++spiral (size-2) (x+1) (y-1)
                   where  l1=(zip [x..lim] (replicate lim x))
                          l2=(zip (replicate lim y) [x..lim])
                          l3=(zip (reverse [lim2..y]) (replicate lim y))
                          l4=(zip (replicate lim x) (reverse [lim2..y]))
                          lim = y-1
                          lim2 = x+1

{-| Recebe como argumento as coordenadas correspondentes a um dado instante modifica o Mapa, colocando uma /Pedra/ nesse dado local
-}
spiral_effect :: (Int,Int) -> Mapa -> Mapa
spiral_effect (x,y) [] = []
spiral_effect (x,y) ((x1,y1,c):t) | (x==x1)&&(y==y1) = (x,y,Pedra):spiral_effect (x,y) t
                                  | otherwise = (x1,y1,c):spiral_effect (x,y) t


{-| Função que converte o tipo de dados para String, neste caso o mapa
-}
map2string :: [Bloco] -> String
map2string [] = ""
map2string ((x,y,c):t) = case c of {Pedra ->'#':rec;Vazio ->' ':rec;Tijolo -> '?':rec;Jogador (_,_) -> ' ':rec;BombsT -> '?':rec;FlamesT -> '?':rec;Bombs -> ' ':rec;Flames -> ' ':rec;Bomba (x,y,z) -> ' ':rec;JogBomba(_,_,_,_) -> ' ':rec}
                          where rec= map2string t  



{-| Função que cria o conteúdo adicional posterior ao mapa no estado de jogo
-}
novoResto :: [Bloco] -> Mapa -> [String]
novoResto [] l = []
novoResto ((x,y,Bombs):t) l = case (mylookup (x,y) l) of {Just Bombs -> ("+ "++show x++" "++show y):novoResto t l ;Just BombsT -> ("+ "++show x++" "++show y):novoResto t l; cell -> novoResto t l}
novoResto ((x,y,Flames):t) l = case (mylookup (x,y) l) of {Just Vazio -> novoResto t l ; cell -> ("! "++show x++" "++show y):novoResto t l} 
novoResto ((x,y,Bomba (j,r,b)):t) l = case (mylookup (x,y) l) of {Just (Bomba (j1,r1,t1)) -> if (t1/=0) then ("* "++show x++" "++show y++" "++show j++" "++show r++" "++show t1):novoResto t l else novoResto t l; cell -> novoResto t l} 
novoResto ((x,y,JogBomba (j,s,r,b)):t) l = case (mylookup (x,y) l) of {Just (JogBomba (j1,s,r1,t1)) -> if (t1/=0) then ("* "++show x++" "++show y++" "++show j++" "++show r++" "++show t1):novoResto t l else novoResto t l; cell -> novoResto t l} 
novoResto ((x,y,Jogador (i,b)):t) l = case (mylookup (x,y) l) of {Just (Jogador (i1,b1)) -> jogador:novoResto t l ;Just(JogBomba(i1,b1,r,t1)) -> jogador:novoResto t l; cell -> novoResto t l} 
                                           where jogador= if (not (null b)) then (show i++" "++show x++" "++show y++" "++b) else (show i++" "++show x++" "++show y)


{-| Função Main pré-definida que invoca a função principal __avanca__
-}
main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"

