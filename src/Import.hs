{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
 
pRoutes = [parseRoutes|
   / HomeR GET
   /cadastro CadastroR GET POST
   /login LoginR GET POST
   /contato ContatoR GET
   /listarjogo ListarJogoR GET
   /cadastrojogo CadastrojogoR GET POST
   /jogo/#JogoId JogoR GET
   /admin AdminR GET
   /static StaticR Static getStatic
   /logout LogoutR GET
   /noticia/#NoticiaId NoticiaR GET
   /cadastronoticia CadastronoticiaR GET POST
   /listarnoticia ListarNoticiaR GET
|]