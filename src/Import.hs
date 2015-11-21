{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
 
pRoutes = [parseRoutes|
   / HomeR GET
   /cadastro CadastroR GET POST
   /login LoginR GET POST
   /contato ContatoR GET
   /listar ListarR GET
   /cadastrojogo CadastrojogoR GET POST
   /jogo/#JogoId JogoR GET
   /admin AdminR GET
|]