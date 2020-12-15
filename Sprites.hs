module Sprites where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Control.Exception
import Control.Applicative
import System.FilePath
import System.IO

{-| Criamos um tipo de dados para conter todos os sprites para serem usados no jogo
-}
data Sprites = Sprites
  { pedra          :: Picture
  , tijolo         :: Picture
  , bombs          :: Picture
  , flames         :: Picture
  , jogador        :: Picture
  , jogador2       :: Picture
  , jogador3       :: Picture
  , jogador4       :: Picture
  , bomba          :: Picture 
  , explosion      :: Picture
  , bombsicon      :: Picture
  , flamesicon     :: Picture 
  , jogadordead    :: Picture
  , jogadordead2   :: Picture 
  , jogadordead3   :: Picture
  , jogadordead4   :: Picture
  } 


{-| Faz loading sequencial de cada imagem dado o /path/ da directoria onde se encontram
-}
loadSprites :: FilePath -> IO Sprites
loadSprites images = Sprites <$> loadSprite tilepic         (images </> "pedra.bmp")
                             <*> loadSprite tilepic         (images </> "tijolo.bmp")
                             <*> loadSprite tilepic         (images </> "bombs.bmp")
                             <*> loadSprite tilepic         (images </> "flames.bmp")
                             <*> loadSprite tilepic         (images </> "jogador.bmp")
                             <*> loadSprite tilepic         (images </> "jogador2.bmp")
                             <*> loadSprite tilepic         (images </> "jogador3.bmp")
                             <*> loadSprite tilepic         (images </> "jogador4.bmp")
                             <*> loadSprite tilepic         (images </> "bomba.bmp")
                             <*> loadSprite tilepic         (images </> "explosion.bmp")
                             <*> loadSprite tilepic         (images </> "bombsicon.bmp")
                             <*> loadSprite tilepic         (images </> "flamesicon.bmp")
                             <*> loadSprite tilepic         (images </> "jogadordead.bmp")
                             <*> loadSprite tilepic         (images </> "jogadordead2.bmp")
                             <*> loadSprite tilepic         (images </> "jogadordead3.bmp")
                             <*> loadSprite tilepic         (images </> "jogadordead4.bmp")

{-| Função que cria uma IO Picture recebendo uma imagem vazia e o /path/ da que queremos carregar e reporta os possíveis erros de imagens que não consiga carregar
-}
loadSprite :: Picture -> FilePath -> IO Picture
loadSprite emptytile imagePath = catch (loadBMP imagePath) erro
  where
    erro :: IOException -> IO Picture
    erro err = do
      hPutStrLn stderr (show err)
      return emptytile

{-| Uma imagem vazia para usar como imagem de destino para as imagens que queremos carregar
-}
tilepic :: Picture
tilepic = blank



