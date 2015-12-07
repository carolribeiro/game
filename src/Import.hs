{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static

pRoutes = [parseRoutes|
   / HomeR GET
   /login LoginR GET POST
   /logout LogoutR GET
   /admin AdminR GET 
   /static StaticR Static getStatic
   /cadastrousuario CadastroUsuarioR GET POST
   /listarusuario ListarUsuarioR GET
  
   /cadastrojogo CadastroJogoR GET POST
   /listarjogo ListarJogoR GET
   /jogo/#JogoId JogoR GET
   /cadastronoticia CadastroNoticiaR GET POST
   /listarnoticia ListarNoticiaR GET
   /noticia/#NoticiaId NoticiaR GET
|]