{-|
Module : Main
Description : Tarefa 1 - Gerar Mapas
Copyright : Martinha Ribeiro <a68258@alunos.uminho.pt>; 
            Pedro Machado <a33524@alunos.uminho.pt>

Este módulo pertence à Tarefa 1 da 1ª fase do Projecto de LI1 - Bomberman.
Tendo como input o tamanho do mapa e um inteiro para gerar um mapa random.


-} 
module Main where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random
import Data.Char

{-| Criamos um tipo de dados chamado ’Celula’ que nos dá o conteúdo e as coordenadas (x,y)
-}
data Celula = Vazio Int Int | Pedra Int Int | Tijolo Int Int | Bombs Int Int | Flames Int Int  deriving (Show,Eq,Read)


-- | ’main’: função que controla as funções ’encode’ e ’decode’
main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"

{-| Função principal que recebe como argumentos o tamanho do mapa e um inteiro que será a semente de geração de valores random.
    Esta função invoca a função ’gerador’ criando primeiro o mapa em células e depois converte para caracteres de novo.
-}
mapa :: Int -> Int -> [String]
mapa x s = splitstring x (map cel2char (gerador 0 0 (x-1) rlist)) ++ (listaBombs(gerador 0 0 (x-1) rlist)) ++ (listaFlames (gerador 0 0 (x-1) rlist))
            where rlist = take (x*x) $ randomRs (0,99) (mkStdGen s)

{-| Função que lista as __Bombs__
-}
listaBombs :: [Celula] -> [String]
listaBombs [] = []
listaBombs (Bombs x y:t) = ('+':' ':(show x)++' ':(show y)):listaBombs t
listaBombs (_:t) = listaBombs t

{-| Função que lista as __Flames__
-}
listaFlames :: [Celula] -> [String]
listaFlames [] = []
listaFlames (Flames x y:t) =('!':' ':(show x)++' ':(show y)):listaFlames t
listaFlames (_:t) = listaFlames t

{-| A função ’gerador’ é a principal função para gerar o mapa. Recebe duas variáveis de valor 0, o tamanho do mapa e a lista random de números.
 Tem como output uma lista de Celula's. Que mais tarde será convertida para caracteres.
-}

gerador :: Int -> Int -> Int -> [Int] -> [Celula]
gerador x y t l | ((x == 0 || x == t) && y <t) = Pedra y x : gerador x (y+1) t l -- 1ª e ultima linha
                 | (y == t && x /= t) = Pedra y x:gerador (x+1) 0 t l -- mudança de linha
                 | (y == 0 && x /= t) = Pedra y x:gerador x (y+1) t l -- inicio da linha 
                 | (x,y) == (1,1) || (x,y) == (1,2) ||(x,y) == (1,t-1) ||(x,y) == (1,t-2) ||(x,y) == (2,1) || (x,y) == (2,t-1) = Vazio y x: gerador x (y+1) t l -- cantos superiores
                 | (x,y) == (t-1,1) || (x,y) ==(t-1,2) || (x,y) ==(t-1,t-2) || (x,y) ==(t-1,t-1) || (x,y) ==(t-2,1) || (x,y) ==(t-2,t-1) = Vazio y x: gerador x (y+1) t l -- cantos inferiores
                 | (even x && even y && y /= t) = Pedra x y: gerador x (y+1) t l
                 | (x,y)==(t,t) = Pedra y x: []-- caso de paragem
                 | otherwise = int2cel (head l) y x : gerador x (y+1) t (drop 1 l)
                

-- Função auxiliar para partir a string com o tamanho exacto do mapa
splitstring :: Int -> String -> [String]
splitstring x [] = []
splitstring x  l = take x l : splitstring  x (drop x l)

-- Função auxiliar para corresponder um caracter a uma célula
cel2char :: Celula -> Char
cel2char (Vazio _ _)  = ' '
cel2char (Pedra _ _)  = '#'
cel2char (Tijolo _ _) = '?'
cel2char (Bombs _ _)  = '?'
cel2char (Flames _ _ )= '?'


int2cel :: Int -> Int -> Int -> Celula
int2cel c x y | (c==0 || c==1) = Bombs x y
              | (c==2 || c==3) = Flames x y
              | (c>=4 && c<=39) = Tijolo x y
              | (c>=40 && c<=99) = Vazio x y


