{-|
Module : Main
Description : Tarefa 5 - Implementação gráfica do Jogo
Copyright : Pedro Machado <a33524@alunos.uminho.pt>
            Martinha Ribeiro <a68258@alunos.uminho.pt>; 
            

Este módulo pertence à Tarefa 5 da 2ª fase do Projecto de LI1 - Bomberman.

Através da biblioteca __Gloss__  criamos o aspecto visual do jogo.
Importamos a Tarefa 1 alterada para usar na criação de mapas, e apesar de não importarmos tarefas anteriores, algumas das funções são versões alteradas das mesmas.
Criamos também um módulo Sprites para usar no loading dos /Sprites/ do jogo.

-} 
module Main where

import Text.Read
import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import Tarefa1Alt
import Sprites

{-| Tipo de dados que representa os jogadores, incĺuímos também o score e um /Bool/ que representa  a vida do jogador no respectivo round (para efeitos de contagem de pontos)
-}
data Jogador = Jogador {num :: Int , coords :: (Int,Int), powerups :: (Int,Int), score :: Int, alive :: Bool} deriving Show
type Jogadores = [Jogador]


{-| Tipo de dados que representa as bombas, usamos o tempo como __Float__ para facilitar, uma vez que a passagem do tempo no Gloss é feita também em Float
-}
data Bomba = Bomba {jog:: Int, coordsxy :: (Int,Int), raio :: Int, tempo :: Float} deriving Show
type Bombas = [Bomba]


{-| Como há duas possíveis maneiras de morrer no jogo, representamos o tipo de morte para facilitar as funções que alteram o Estado consoante explosões e pedras da espiral
-}
data Death = ByStone | ByFlames deriving Eq

{-| Tipo de dados para representar possíveis comandos de jogo 
-}
data Comando = U | D | L | R | B deriving (Eq,Show)

type Tempo = Float
type Size = Int
type Explosion = [(Int,Int)]

{-| Tipo de dados para representar as Settings relativas ao jogo 
-}
data Settings = Settings {size:: Int, opon :: Int, rounds :: Int, current_round :: Int} deriving Show

{-| Tipo de dados para representar o Estado de jogo
-}
data Estado = Winner Jogador | Estado Mapa Jogadores Bombas Tempo Settings Explosion deriving Show



tilesize=30.0
halftile=15.0

resx=800


resy=1000


{-| Inicia o Estado de Jogo, colocando os jogadores nos sitios respectivos
-}
estadoInicial :: (Int,Int,Int) -> Estado
estadoInicial (s,o,r) = Estado m j b t set f
                    where m = mapa s 0
                          j = oponentes o s
                          b = []
                          t = (fromIntegral s)^2
                          set = (Settings s o r 1)
                          f = []

                          
{-| Dado o tamanho do mapa e o numero de oponentes, coloca os jogadores nas devidas posições
-}
oponentes :: Int -> Int -> Jogadores
oponentes x s | (x==1) = jog1:jog2:[]
              | (x==2) = jog1:jog2:jog3:[]
              | (x==3) = jog1:jog2:jog3:jog4:[]
                   where jog1=(Jogador 1 (1,1) (0,0) 0 True)
                         jog2=(Jogador 2 ((s-2),1) (0,0) 0 True)
                         jog3=(Jogador 3 (1,(s-2)) (0,0) 0 True)
                         jog4=(Jogador 4 ((s-2),(s-2)) (0,0) 0 True)


{-| Para além do __Estado__ esta função recebe também como argumento o conjunto de __Sprites__ para enviar como argumento para as diversas funções que desenham o Jogo
-}
desenhaEstado :: Sprites -> Estado ->  Picture
desenhaEstado spr (Winner j) = desenhavencedor j
desenhaEstado spr (Estado m j b t s f) = centra  tam $ (Pictures (map++jog++bombas++score++tempo++explosao))
                    where map = desenhamapa m spr
                          jog = desenhajogadores j spr 
                          bombas = desenhabombas b spr
                          tam = size s
                          score = desenhascore tam spr j 
                          tempo = desenhatempo t tam (current_round s)
                          explosao = desenhaexplosao f spr



{-| Função que determina o Estado de jogo após um input por parte de um jogador
-}
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey x) Down _ _) e = case x of {KeyUp -> move e 1 U; KeyDown -> move e 1 D; KeyLeft -> move e 1 L; KeyRight -> move e 1 R;  KeySpace -> move e 1 B; otherwise -> e}
reageEvento _ e = e




{-| Uma das funções mais importantes que vai alterando o jogo com a passagem do tempo
-}
reageTempo :: Float -> Estado -> Estado
reageTempo f (Winner jog) = Winner jog
reageTempo f (Estado m j b t s fl) | fimjogo = Winner (vencedor j)
                                   | fimround = novoRound j s
                                   | (time <= t_limit) = if bombasactivas then explodEst2 else est2 
                                   | otherwise = if bombasactivas then explodEst else est
                                where t_limit = (tamanhoU)^2
                                      time = round t2
                                      est = Estado m j b2 t2 s fl2
                                      tamanho= size s
                                      tamanhoU = tamanho -2 
                                      instante = t_limit - time
                                      coords_spiral = (spiral tamanhoU 1 tamanhoU)!!instante
                                      m2 = spiral_effect coords_spiral m
                                      t2 = t-f
                                      est2 = kill_stone coords_spiral (Estado m2 j b2 t2 s fl2)
                                      b2 = timeBombs f b
                                      bombasactivas= ((length filtrabombas) > 0) -- bombasactivas
                                      fl2= if ((length apagachamas) > 0) then [] else [] -- apagar as chamas do mapa
                                      filtrabombas= filter (\r -> tempo r <= 2.0) b
                                      apagachamas= (filter (\r -> tempo r == 0.0) b)
                                      explodEst = explodeBombas filtrabombas est
                                      explodEst2 = explodeBombas filtrabombas est2
                                      vivos = (filter (\r -> alive r == True) j)
                                      fimround = (length vivos) <= 1
                                      fimjogo = (current_round s) > (rounds s)


{-| Depois de um round, esta função recebe como argumentos __Jogadores__ e __Settings__ para criar um novo estado, correspondente ao próximo round, com scores actualizados e muda também a seed random do mapa
-}
novoRound :: Jogadores -> Settings -> Estado
novoRound j (Settings s o r ra) = Estado m j1 b t set f
                                where m = mapa s ra
                                      roundactual = ra+1
                                      j1 = reiniciajogadores j s
                                      b = []
                                      t = (fromIntegral s)^2
                                      set = Settings s o r (ra+1)
                                      f = []

{-| Esta função determina qual dos Jogadores tem mais pontos, após concluidos os rounds
-}
vencedor :: Jogadores -> Jogador
vencedor [j] = j
vencedor (j1:j2:js) | ((score j1)>(score j2)) = vencedor (j1:js)

{-| Função auxiliar que trata da pontuação dos jogadores e ressuscitá-los a cada round 
-}
reiniciajogadores :: Jogadores -> Int -> Jogadores
reiniciajogadores [] size = []
reiniciajogadores ((Jogador n (x,y) (b,f) s a):js) size = if (a==True) then (Jogador n newcords (0,0) (s+1) True):reiniciajogadores js size else (Jogador n newcords (0,0) s True):reiniciajogadores js size
                                               where coords=[(1,1),(size-2,1),(1,size-2),(size-2,size-2)]
                                                     newcords= coords!!(n-1)


{-| Frame Rate do Jogo
-}
fr :: Int
fr = 50

{-| Display do Jogo
-}
dm :: Display
dm = InWindow "Bomber Homem" (resx, resy) (0, 0)
    

{-| Função principal que recebe alguns argumentos na linha de comandos, e recria o ambiente gráfico baseado nessas definições 
-}
main :: IO ()
main = do
       putStrLn "Escolha o tamanho do mapa:"
       s <- getNum
       putStrLn "Escolha o número de oponentes (1,2,3):"
       o <- getNum
       putStrLn "Escolha o número de rounds (3,5,7):"
       r <- getNum
       sprites <- loadSprites "images"
       (play dm              
            (greyN 0.7)     
            fr              
            (estadoInicial (s,o,r))  
            (desenhaEstado sprites)  
            reageEvento     
            reageTempo)      


getNum :: IO Int
getNum = readLn

{-| __desenhamapa__ recebe o Mapa e Sprites e desenha o Mapa sob a forma de imagens
-}
desenhamapa :: Mapa-> Sprites -> [Picture]
desenhamapa [] sprites= []
desenhamapa ((x,y,cell):xs) spr     | (cell == Pedra) = (Translate x1 y1 $ pedra spr):desenhamapa xs spr
                                    | (cell == Tijolo) = (Translate x1 y1 $ tijolo spr):desenhamapa xs spr
                                    | (cell == Bombs) = (Translate x1 y1 $ bombs spr):desenhamapa xs spr
                                    | (cell == Flames) = (Translate x1 y1 $ flames spr):desenhamapa xs spr
                                    | (cell == BombsT) = (Translate x1 y1 $ tijolo spr):desenhamapa xs spr
                                    | (cell == FlamesT) = (Translate x1 y1 $ tijolo spr):desenhamapa xs spr
                                    | otherwise =desenhamapa xs spr
                                     where x1= (tilesize*(fromIntegral x))
                                           y1= negate (tilesize*(fromIntegral y))


{-| Desenha os jogadores recorrendo a Sprites
-}
desenhajogadores :: Jogadores -> Sprites -> [Picture]
desenhajogadores [] spr = []
desenhajogadores ((Jogador n (x,y) _ _ a):xs) spr = if (a==True) then (Translate x1 y1 $ pic n a spr):desenhajogadores xs spr else desenhajogadores xs spr
                                                  where x1=(tilesize*(fromIntegral x))
                                                        y1=negate(tilesize*(fromIntegral y))
{-| Função que desenha as Bombas
-}
desenhabombas :: Bombas -> Sprites -> [Picture]
desenhabombas [] spr = []
desenhabombas ((Bomba _ (x,y) _ _):xs) spr = (Translate x1 y1 $ bomba spr):desenhabombas xs spr
                                              where x1=(tilesize*(fromIntegral x))
                                                    y1=negate(tilesize*(fromIntegral y))


{-| Função que desenha as explosões causadas pelas bombas
-}
desenhaexplosao :: [(Int,Int)] -> Sprites -> [Picture]
desenhaexplosao [] spr = []
desenhaexplosao ((x,y):xs) spr = (Translate x1 y1 $ explosion spr):desenhaexplosao xs spr
                                              where x1=(tilesize*(fromIntegral x))
                                                    y1=negate(tilesize*(fromIntegral y))

{-| Função que desenha o /score/ dos vários jogadores no ecrã
-}
desenhascore :: Int -> Sprites -> Jogadores -> [Picture]
desenhascore size spr [] = []
desenhascore size spr (j:js) = (desenhascorejog size spr j):desenhascore size spr js
                                      

{-| Função auxiliar da __desenhascore__ que atribui uma imagem e desenha a pontuação e powerups de cada jogador
-}
desenhascorejog :: Int -> Sprites -> Jogador -> Picture
desenhascorejog size spr (Jogador i (x,y) (b,f) s a) = Translate x1 y1 $ Pictures (frame:texto:player:bombs:flames:[]) 
                                                    where tamanho= ((fromIntegral size)*tilesize)/4
                                                          frame  = rectangleWire tamanho (tamanho/2) 
                                                          texto  = Translate (negate (tamanho/2)) (negate(tamanho/6)) $ Scale 0.1 0.1 $ Text ("score: "++(show s))
                                                          player = Translate (negate(tamanho/3)) (tamanho/8) $ pic i a spr
                                                          bombs  = Translate (tamanho/4) (tamanho/8) $ Pictures $ (bombsicon spr):(Translate (15.0) (negate 5.0) $ Scale 0.1 0.1 $ Text(show b)):[]
                                                          flames = Translate (tamanho/4) (negate(tamanho/8)) $ Pictures $ (flamesicon spr):(Translate 15.0 (negate 5.0) $ Scale 0.1 0.1 $ Text(show f)):[]
                                                          x1= ((fromIntegral i) * tamanho)-(tamanho/2)-halftile
                                                          y1= (tamanho/2) -(tamanho/8)

{-| Atribui a cada jogador uma imagem diferente, e além disso muda a imagem se ele morrer durante o round
-}
pic :: Int -> Bool -> Sprites -> Picture
pic x a spr | (x==1) = if a then (jogador  spr) else (jogadordead  spr)
            | (x==2) = if a then (jogador2 spr) else (jogadordead2 spr)
            | (x==3) = if a then (jogador3 spr) else (jogadordead3 spr)
            | (x==4) = if a then (jogador4 spr) else (jogadordead4 spr)                                            


{-| Função que para além do tempo, desenha também o score no ecrã de jogo
-}
desenhatempo :: Tempo -> Int -> Int-> [Picture]
desenhatempo t x r = (Scale 0.2 0.2 $Translate x1 y1 $ Text $ "Time: "++(show time)):(Scale 0.2 0.2 $Translate x2 y2 $ Text $ "Round: "++(show r)):[]
                    where x1 = -(tamanho/3)
                          y1 = tamanho*4
                          x2 = tamanho*10
                          y2 = tamanho*4
                          tamanho = ((fromIntegral x)*tilesize)/4
                          time = round t

{-| Cria uma imagem para congratular o Vencedor do Jogo
-}
desenhavencedor :: Jogador -> Picture
desenhavencedor jog = Translate (negate (x/2)) 0 $ Scale 0.6 0.6 $  Text ("Parabens Jogador "++(show (num jog))++"!!")
                      where x = fromIntegral resx
                           


{-| Esta função centra a imagem de jogo, baseada no tamanho do mapa
-}
centra ::  Int -> Picture -> Picture
centra x p = Translate x1 y1 p
            where x1 = negate y1
                  y1 = (tamanho/2)-z
                  z = tilesize/2
                  tamanho = ((fromIntegral x)*tilesize)


{-| A função __move__ é uma adaptação melhorada da função move da Tarefa2
-}
move :: Estado -> Int -> Comando -> Estado
move (Estado m j b t s f) x c | c == B = Estado m j newbombs t s f
                              | otherwise = Estado newmap newplayers b t s f
                      where jogador = head (filter (\r -> num r == x) j)
                            xy = coords jogador 
                            move = move_jogador jogador m c 
                            newbombs = poebomba xy jogador b
                            newmap = fst move
                            newplayers = inserejog (snd move) j


{-| Move o /Jogador/ para outro sítio do Mapa caso seja possível
-}

move_jogador :: Jogador -> Mapa -> Comando -> (Mapa,Jogador)
move_jogador (Jogador i (x,y) (b,f) s a) m c = case (mylookup xy m) of {Just Flames -> (newmap,(Jogador i xy (b,f+1) s a)); Just Bombs -> (newmap,(Jogador i xy (b+1,f) s a)); Just Vazio -> (m,(Jogador i xy (b,f) s a)); otherwise -> (m,(Jogador i (x,y) (b,f) s a))}
                                            where xy = coordenadas c (x,y)
                                                  newmap = actualizamapa m xy

{-| Recebe como argumento o Comando e determina as coordenadas do mesmo
-}
coordenadas :: Comando -> (Int,Int) -> (Int,Int)
coordenadas c (x,y) | (c==U)    = (x,y-1)
                    | (c==D)    = (x,y+1)
                    | (c==L)    = (x-1,y)
                    | (c==R)    = (x+1,y)
                    | otherwise = (0,0)       


inserejog :: Jogador -> [Jogador] -> [Jogador]
inserejog jog [] = []
inserejog jog (j:js) | (num jog) == (num j) = jog: inserejog jog js 
                     | otherwise = j:inserejog jog js


{-| Esta função cria uma /Bomba/ no Jogo
-}
poebomba :: (Int,Int) -> Jogador -> Bombas -> Bombas
poebomba (x,y) j b = if (contabombas < bombs+1) then (Bomba jog (x,y) (1+flames) 5):b else b
                       where jog = num j
                             contabombas = conta_bombas jog b
                             bombs = fst (powerups j)
                             flames = snd (powerups j)

conta_bombas :: Int -> Bombas -> Int
conta_bombas x [] = 0
conta_bombas x (h:t) | (x==(jog h)) = 1 + conta_bombas x t
                     | otherwise = conta_bombas x t

actualizamapa :: Mapa -> (Int,Int) -> Mapa
actualizamapa [] (x1,y1) = []
actualizamapa ((x,y,c):xs) (x1,y1) | (x==x1) && (y==y1) = (x,y,Vazio):actualizamapa xs (x1,y1)
                                   | otherwise = (x,y,c):actualizamapa xs (x1,y1) 



{-| Função recrutada da Tarefa4
-}
mylookup :: (Int,Int) -> Mapa -> Maybe Celula
mylookup (x,y)  [] = Nothing
mylookup (x,y) ((x1,y1,cel):t) | (x==x1) && (y==y1) = Just cel
                               | otherwise = mylookup (x,y) t



{-| Função recrutada da Tarefa4
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

spiral_effect :: (Int,Int) -> Mapa -> Mapa
spiral_effect (x,y) [] = []
spiral_effect (x,y) ((x1,y1,c):t) | (x==x1)&&(y==y1) = (x,y,Pedra):spiral_effect (x,y) t
                                  | otherwise = (x1,y1,c):spiral_effect (x,y) t


{-| Quando se inicia o efeito /Espiral/ esta função destrói /bombas/ e /jogadores/ que estejam nessa coordenada
-}
kill_stone :: (Int,Int) -> Estado -> Estado
kill_stone (x,y) (Estado m j b t s f) = Estado m j1 b1 t s f
                                     where j1 = kill_jog [(x,y)] j
                                           b1 = kill_bomba [(x,y)] b ByStone

{-| Função que elimina um Jogador
-}
kill_jog :: [(Int,Int)] -> Jogadores -> Jogadores
kill_jog [] jogs = jogs
kill_jog ((x,y):xs) jogs = kill_jog xs (kill_jog_aux (x,y) jogs)
                              where kill_jog_aux :: (Int,Int) -> Jogadores -> Jogadores
                                    kill_jog_aux (x,y) [] =[]
                                    kill_jog_aux (x,y) ((Jogador i (x1,y1) (b,f) s a):js) = if (x==x1)&&(y==y1) then (Jogador i (x1,y1) (b,f)  s False):kill_jog_aux (x,y) js else (Jogador i (x1,y1) (b,f) s a):kill_jog_aux (x,y) js 


{-| Função que elimina bombas se cair uma pedra em cima, ou reduz o tempo para 1 se afectada por uma explosão
-}
kill_bomba :: [(Int,Int)] -> Bombas -> Death -> Bombas
kill_bomba [] bombas _ = bombas
kill_bomba ((x,y):xs) bombas death = kill_bomba xs (kill_bomba_aux (x,y) bombas death) death
                                                        where kill_bomba_aux :: (Int,Int) -> Bombas -> Death -> Bombas
                                                              kill_bomba_aux (x,y) [] d= []
                                                              kill_bomba_aux (x,y) ((Bomba j (x1,y1) r t):bs) ByStone  = if (x==x1)&&(y==y1) then kill_bomba_aux (x,y) bs ByStone else (Bomba j (x1,y1) r t):kill_bomba_aux (x,y) bs ByStone
                                                              kill_bomba_aux (x,y) ((Bomba j (x1,y1) r t):bs) ByFlames = if (x==x1)&&(y==y1) then (Bomba j (x1,y1) r 2.0):kill_bomba_aux (x,y) bs ByFlames else (Bomba j (x1,y1) r t):kill_bomba_aux (x,y) bs ByFlames



timeBombs :: Float -> Bombas -> Bombas
timeBombs d [] = []
timeBombs d ((Bomba j (x,y) r t):bs) = (Bomba j (x,y) r t2): timeBombs d bs
                                       where t2= t-d

explodeBombas :: Bombas -> Estado -> Estado
explodeBombas [] e = e  
explodeBombas ((Bomba i (x,y) r time):bs) (Estado m j b t s f) = explodeBombas bs (Estado m2 j2 b2 t s f2)
                                                       where m2 = explodemapa m coords_explosao
                                                             coords_explosao= coords_X m (x,y,r)
                                                             b2= removeBomba (Bomba i (x,y) r time) b
                                                             j2= kill_jog coords_explosao j
                                                             f2 = coords_explosao 


coords_X :: Mapa -> (Int,Int,Int)  -> [(Int,Int)]
coords_X l (x,y,r) = (reverse (aux (x,y) r 1 'U' l))++(reverse(aux (x,y) r 1 'L' l))++[(x,y)]++(aux (x,y) r 1 'R' l)++(aux (x,y) r 1 'D' l) 


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




{-| Função da Tarefa4 
-}
explodemapa :: Mapa -> [(Int,Int)] -> Mapa
explodemapa ((x,y,c):t) [] = ((x,y,c):t)
explodemapa ((x,y,c):t) ((x1,y1):xs) | (x==x1) && (y==y1) = case c of { Tijolo -> (x,y,Vazio):rec; BombsT -> (x,y,Bombs):rec; FlamesT -> (x,y,Flames):rec; Bombs -> (x,y,Vazio):rec; Flames -> (x,y,Vazio):rec; Vazio -> (x,y,Vazio):rec}
                                     | otherwise =(x,y,c):explodemapa t ((x1,y1):xs)
                                      where rec=explodemapa t xs 


{-| Remove Bombas que já tenham explodido 
-}

removeBomba :: Bomba -> Bombas -> Bombas
removeBomba b [] = []
removeBomba (Bomba j (x,y) r t) ((Bomba j1 (x1,y1) r1 t1):bs) | (x==x1) && (y==y1) && (j==j1) = removeBomba (Bomba j (x,y) r t) bs
                                                              | otherwise = (Bomba j1 (x1,y1) r1 t1): removeBomba (Bomba j (x,y) r t) bs

