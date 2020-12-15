module Tarefa1Alt where


import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.Char
import Data.List.Split


data Celula = Vazio | Pedra | Tijolo | BombsT | FlamesT | Bombs | Flames deriving (Eq,Show)
type Bloco = (Int,Int,Celula)
type Mapa = [Bloco]


mapa :: Int -> Int -> Mapa
mapa x s = blocklist
            where rlist = take (x*x) $ randomRs (0,99) (mkStdGen s)
                  blocklist = gerador 0 0 (x-1) rlist


gerador :: Int -> Int -> Int -> [Int] -> Mapa
gerador x y t l | ((x == 0 || x == t) && y <t) = (y,x,Pedra):gerador x (y+1) t l -- 1ª e ultima linha
                | (y == t && x /= t) = (y,x,Pedra):gerador (x+1) 0 t l -- mudança de linha
                | (y == 0 && x /= t) = (y,x,Pedra):gerador x (y+1) t l -- inicio da linha 
                | elem (x,y) cantos = (y,x,Vazio): gerador x (y+1) t l -- cantos 
                | (even x && even y && y /= t) = (y,x,Pedra): gerador x (y+1) t l
                | (x,y)==(t,t) = (y,x,Pedra): []-- caso de paragem
                | otherwise = (y,x,cell):gerador x (y+1) t (drop 1 l)
                 where cell = int2cel (head l)
                       cantos = [(1,1),(1,2),(1,t-1),(1,t-2),(2,1),(2,t-1),(t-1,1),(t-1,2),(t-1,t-2),(t-1,t-1),(t-2,1),(t-2,t-1)]

int2cel :: Int -> Celula
int2cel c | (c==0 || c==1) = BombsT 
          | (c==2 || c==3) = FlamesT 
          | (c>=4 && c<=39) = Tijolo 
          | (c>=40 && c<=99) = Vazio