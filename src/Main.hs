{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Main where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Yesod
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sqlite
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration
    , createSqlitePool, runSqlPersistMPool, fromSqlKey
    )


data Sitio = Sitio {connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
    email Text
    senha Text 
    deriving Show

Jogo
    nome Text
    plataforma Text
    categoria Text
    descricao Text
    deriving Show
|]

mkYesod "Sitio" [parseRoutes|
    / HomeR GET
    /cadastro CadastroR GET POST
    /login LoginR GET POST
    /contato ContatoR GET
    /listar ListarR GET
    /cadastrojogo CadastrojogoR GET POST
    /jogo/#JogoId JogoR GET
    /jogos JogosR GET
    /logout LogoutR GET

|]

instance YesodPersist Sitio where
    type YesodPersistBackend Sitio = SqlBackend
    runDB f = do
        master <- getYesod
        let pool = connPool master
        runSqlPool f pool

instance Yesod Sitio where

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

formUsuario :: Form Usuario
formUsuario = renderBootstrap2 $ Usuario <$>
             areq emailField FieldSettings{fsId=Just "hident2",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","E-mail")]} Nothing <*>
             areq passwordField FieldSettings{fsId=Just "hident3",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Senha")]} Nothing

formJogo :: Form Jogo
formJogo = renderBootstrap2 $ Jogo <$>
             areq textField FieldSettings{fsId=Just "hident2",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Nome")]} Nothing <*>
             areq textField FieldSettings{fsId=Just "hident3",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Plataforma")]} Nothing <*>
             areq textField FieldSettings{fsId=Just "hident4",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Categoria")]} Nothing <*>
             areq textField FieldSettings{fsId=Just "hident5",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Descrição")]} Nothing


widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = [whamlet|
     <!-- header -->
     ^{menu}
     <!-- /.header -->
     <div class="container">
          <!-- wrapper-->
          <div id="wrapper" class="margin-top-15 margin-bottom-30">
               <div class="col-md-12">
                    <section class="no-border no-padding">
                             <h4 class="page-header no-margin-top">#{y}
                             <form method=post action=@{x} enctype=#{enctype}>
                                   <div class="col-md-12 col-xs-12 no-padding">
                                        <div class="row">
                                             <div class="control-group col-md-6 ">
                                                  <div class="controls">
                                                       ^{widget}
                                   <input type="submit" class="btn btn-success pull-left margin-top-15 padding-top-15 padding-bottom-15 padding-left-25 padding-right-25" value=#{val}>
|]


urlAssets :: String -> String 
urlAssets x = "https://preview.c9users.io/carolribeiro/carol/assets/" ++ x


menu :: Widget
menu = toWidget [whamlet|
<header data-type="parallax" data-speed="4" class="cover" style="background-image: url(#{urlAssets "img/cover.jpg"});">
        <div class="header-color">
             <div class="header">
                  <div class="container">
                       <a href="@{HomeR}" class="logo pull-left">
                          <i class="ion-ios-game-controller-b"></i> Uplay
                  <!-- navigation -->
                  <nav>
                       <div class="container">
                            <ul>
                                <li>
                                    <a href="@{HomeR}">Home
                                <li>
                                    <a href="@{JogosR}">Jogos
                                <li>
                                    <a href="@{CadastroR}">Cadastro
                                <li>
                                    <a href="@{LoginR}">Login
                                <li>
                                    <a href="@{ContatoR}">Contato
                  <!-- /.navigation -->
             <!-- /.header-color -->
|]

footer :: Widget
footer = toWidget [whamlet|
          <!-- footer -->
          <footer>
                  <div class="container">
                       <div class="widget row">
                            <div class="col-md-2 col-xs-12 no-padding-sm-lg">
                                 <h4 class="title">Mapa do site
                                 <ul class="nav">
                                     <li>
                                         <a href="@{HomeR}">
                                            <i class="fa fa-chevron-right"></i>Home
                                     <li>
                                         <a href="@{JogosR}">
                                            <i class="fa fa-chevron-right"></i>Jogos
                                     <li>
                                         <a href="@{CadastroR}">
                                            <i class="fa fa-chevron-right"></i> Cadastro
                                     <li>
                                         <a href="@{LoginR}">
                                            <i class="fa fa-chevron-right"></i> Login
                                     <li>
                                         <a href="@{ContatoR}">
                                            <i class="fa fa-chevron-right"></i>Contato
                            <!-- social buttons -->
                            <div class="col-md-2 col-xs-12 no-padding-sm-lg">
                                <h4 class="title">Redes sociais
                                <ul class="list-inline">
                                    <li>
                                        <a href="#" class="btn btn-circle btn-social-icon btn-twitter">
                                           <i class="fa fa-twitter">
                                    <li>
                                        <a href="#" class="btn btn-circle btn-social-icon btn-facebook">
                                           <i class="fa fa-facebook">
                                    <li>
                                        <a href="#" class="btn btn-circle btn-social-icon btn-google-plus">
                                           <i class="fa fa-google-plus">
                       <!-- /.footer widget -->
                  <!-- footer bottom -->
                  <div class="footer-bottom">
                       <div class="container">
                            <ul class="pull-center">
                                <li>&copy; 2015 Uplay. Todos os direitos reservados.
                  <!-- /.footer bottom -->
          <!-- /.footer -->

|]


widgetCss :: Widget {-trocar nome-}
widgetCss = do
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/ionicons/2.0.1/css/ionicons.min.css"
    toWidgetHead [hamlet|
        <link href="#{urlAssets "css/style.css"}" rel="stylesheet" /> 
        <link href="#{urlAssets "css/helpers.css"}" rel="stylesheet" />
        <link href="#{urlAssets "css/blue.css"}" rel="stylesheet" /> 
        <link href="#{urlAssets "css/animate.min.css"}" rel="stylesheet" >
        <link href="#{urlAssets "css/owl.carousel.css"}" rel="stylesheet"  /> 
        <link href="#{urlAssets "css/demo.css"}" rel="stylesheet">
        <script src="#{urlAssets "js/jquery-1.11.1.min.js"}">
        <script src="#{urlAssets "js/bootstrap.min.js"}">
        <script src="#{urlAssets "js/holder.js"}"> 
        <script src="#{urlAssets "js/core.js"}">
        <script src="#{urlAssets "js/modernizr.custom.js"}">  
        <script src="#{urlAssets "js/demo.js"}">
        <script src="#{urlAssets "js/masonry.pkgd.min.js"}">
        <script src="#{urlAssets "js/imagesloaded.pkgd.min.js"}">
        <script src="#{urlAssets "js/owl.carousel.min.js"}">
        <script src="#{urlAssets "js/script.js"}">      
    |]

widgetJS :: Widget {-tirar-}
widgetJS = do
  
    toWidgetHead [julius|
    
    |]


widgetHtmlHome :: Widget
widgetHtmlHome = [whamlet|
<div>  
     <!-- header -->
     ^{menu}
     <!-- /.header -->
     <div class="container">
          <!-- wrapper-->
          <div id="wrapper">
               <div class="padding-left-15 padding-right-15 no-padding-xs no-padding-sm">
                    <div id="carousel-example-captions" class="carousel slide main-carousel" data-ride="carousel">
                         <ol class="carousel-indicators clearfix">
                             <li data-target="#carousel-example-captions" data-slide-to="0" class="active">
                                 <h3>Uncharted 4 Gameplay Review
                             <li data-target="#carousel-example-captions" data-slide-to="1">
                                 <h3>Last of Us Remastered Gameplay
                             <li data-target="#carousel-example-captions" data-slide-to="2">
                                 <h3>Marvel Galaxy Contest Review
                             <li data-target="#carousel-example-captions" data-slide-to="3">
                                 <h3>Injustice Gods Among Us Gameplay
                         <div class="carousel-inner" role="listbox">
                              <div class="item active">
                                   <img src="#{urlAssets "img/slideshow/1.jpg"}" class="full-width" alt="First slide image"> 
                                   <div class="carousel-caption">
                                        <h1>
                                            <span>Global news
                                        <h2>
                                            <span>Uncharted 4 Review
                                        <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas egestas, orci id...
                              <div class="item">
                                   <img src="#{urlAssets "img/slideshow/2.jpg"}" class="full-width" alt="Second slide image">
                                   <div class="carousel-caption">
                                        <h1>
                                            <span>PS4
                                        <h2>
                                            <span>Last of Us Remastered
                                        <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas egestas, orci id...
                              <div class="item">
                                   <img src="#{urlAssets "img/slideshow/3.jpg"}" class="full-width" alt="Third slide image">
                                   <div class="carousel-caption">
                                        <h1>
                                            <span>Xbox One
                                        <h2>
                                            <span>Marvel Galaxy Review
                                        <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas egestas, orci id...
                              <div class="item">
                                   <img src="#{urlAssets "img/slideshow/4.jpg"}" class="full-width" alt="Third slide image">
                                   <div class="carousel-caption">
                                        <h1>
                                            <span>PC
                                        <h2>
                                            <span>Injustice Gods Among Us
                                        <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas egestas, orci id...
                              <a class="left carousel-control" href="#carousel-example-captions" role="button" data-slide="prev">
                                   <span class="glyphicon glyphicon-chevron-left" aria-hidden="true">
                                   <span class="sr-only">Previous
                              <a class="right carousel-control" href="#carousel-example-captions" role="button" data-slide="next">
                                   <span class="glyphicon glyphicon-chevron-right" aria-hidden="true">
                                   <span class="sr-only">Next
                    <!-- /.slideshow -->
               <!-- section -->
               <section class="border-grey-200 margin-bottom-10 relative no-padding hidden-xs">
                        <div class="section-title no-margin-top no-border padding-top-25 padding-left-25 padding-right-25"> 
                             <h3 class="color-black no-border" style="margin-left:-24px">Jogos
                        <div class="padding-left-10 padding-right-10 margin-bottom-25">
                             <div>
                                  <div class="thumbnail" style="float:left; margin-right:25px; margin-left:5px;">
                                       <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                           <a href="#">Grand Theft Auto 5
                                       <a href="@{JogosR}">
                                          <img src="#{urlAssets "img/game/1.jpg"}" alt="GTA 5">
                                       <div class="caption padding-15-20">
                                            <a href="@{JogosR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-right:25px;">
                                       <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                           <a href="#">Batman Arkham Knight
                                       <a href="#">
                                          <img src="#{urlAssets "img/game/2.jpg"}" alt="Batman Arkham Knight">
                                       <div class="caption padding-15-20">
                                            <a href="@{JogosR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-right:25px;">
                                       <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                           <a href="#">Tomb Raider
                                       <a href="#">
                                          <img src="#{urlAssets "img/game/3.jpg"}" alt="Tomb Raider">
                                       <div class="caption padding-15-20">
                                            <a href="@{JogosR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left;">
                                       <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                           <a href="#">Injustice Gods Among Us
                                       <a href="#">
                                          <img src="#{urlAssets "img/game/4.jpg"}" alt="Injustice Gods Among Us">
                                       <div class="caption padding-15-20">
                                            <a href="@{JogosR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-top:25px; margin-left:5px; margin-right:25px;">
                                       <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                           <a href="#">Metal Gear Solid V
                                       <a href="#">
                                          <img src="#{urlAssets "img/game/5.jpg"}" alt="Metal Gear Solid V">
                                       <div class="caption padding-15-20">
                                            <a href="@{JogosR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-top:25px; margin-right:25px;">
                                       <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                           <a href="#">Assassin's Creed Unity
                                       <a href="#">
                                          <img src="#{urlAssets "img/game/6.jpg"}" alt="Assassin's Creed Unity">
                                       <div class="caption padding-15-20">
                                            <a href="@{JogosR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-top:25px;margin-right:25px;">
                                       <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                           <a href="#">The Witcher 3
                                       <a href="#">
                                          <img src="#{urlAssets "img/game/7.jpg"}" alt="The Witcher 3">
                                       <div class="caption padding-15-20">
                                            <a href="@{JogosR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-top:25px;">
                                       <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                           <a href="#">God of War 3
                                       <a href="#">
                                          <img src="#{urlAssets "img/game/8.jpg"}" alt="God of War 3">
                                       <div class="caption padding-15-20">
                                            <a href="@{JogosR}" class="btn btn-block btn-primary">Ver
               <!-- section -->
          <!-- /.wrapper -->
     <!-- footer -->
     ^{footer}
     <!-- /.footer -->
     <!-- sign-in modal -->
|]


widgetHtmlJogos:: Widget    {-para maa fazer :P -}
widgetHtmlJogos = [whamlet|
<h1>Em construção
|]



widgetHtmlContato :: Widget
widgetHtmlContato = [whamlet|

     <!-- header -->
     ^{menu}
     <!-- /.header -->
     <div class="container">
          <!-- wrapper-->
          <div id="wrapper" class="margin-top-15 margin-bottom-30">
               <div class="col-md-12">
                    <section class="no-border no-padding">
                             <h4 class="page-header no-margin-top">Contato
                             <form autocomplete="off" method="POST">
                                   <div class="col-md-12 col-xs-12 no-padding">
                                        <div class="row">
                                             <div class="control-group col-md-6 ">
                                                  <div class="controls">
                                                       <input type="text" class="form-control input-lg" id="Name" placeholder="Nome" required>
                                             <div class="control-group col-md-6">
                                                  <div class="controls">
                                                       <input type="text" class="form-control input-lg" id="email" placeholder="Email" required>
                                             <div class="control-group col-md-12 col-xs-12 margin-top-15">
                                                  <div class="controls">
                                                        <textarea class="form-control input-lg" rows="4" placeholder="Messagem">
                                   <button type="button" class="btn btn-success pull-left margin-top-15 padding-top-15 padding-bottom-15 padding-left-25 padding-right-25">Enviar
          <!-- /.wrapper -->
     ^{footer}

|]


getHomeR :: Handler Html
getHomeR = defaultLayout (widgetHtmlHome >> widgetCss >> widgetJS)

getCadastrojogoR :: Handler Html
getCadastrojogoR = do
    (widget, enctype) <- generateFormPost formJogo
    defaultLayout $ widgetForm CadastrojogoR enctype widget "Cadastro de jogos" "Cadastrar" >> widgetCss

getCadastroR :: Handler Html
getCadastroR = do
    (widget, enctype) <- generateFormPost formUsuario
    defaultLayout $ widgetForm CadastroR enctype widget "Cadastro de usuários" "Cadastrar" >> widgetCss

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                        runDB $ insert usuario
                        setMessage $ [shamlet| <p> Usuário cadastrado com sucesso! |] {- por css -}
                        redirect CadastroR
                    _ -> redirect CadastroR

postCadastrojogoR :: Handler Html
postCadastrojogoR = do
                ((result, _), _) <- runFormPost formJogo
                case result of
                    FormSuccess jogo -> do
                        runDB $ insert jogo
                        setMessage $ [shamlet| <p> Jogo cadastrado com sucesso! |] {- por css -}
                        redirect CadastrojogoR
                    _ -> redirect CadastrojogoR
                    
getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost formUsuario
    defaultLayout $ widgetForm LoginR enctype widget "Login" "Entrar" >> widgetCss

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioEmail ==. usuarioEmail usr, UsuarioSenha ==. usuarioSenha usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioEmail usr)
                    redirect CadastrojogoR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |] {- por css -}
                    redirect LoginR 
        _ -> redirect LoginR

getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_ID"
    redirect HomeR

getJogosR :: Handler Html
getJogosR = defaultLayout (widgetHtmlJogos >> widgetCss >> widgetJS) 

getContatoR :: Handler Html
getContatoR = defaultLayout (widgetHtmlContato >> widgetCss)

getJogoR :: JogoId -> Handler Html
getJogoR pid = do
    jogo <- runDB $ get404 pid
    defaultLayout $ widgetCss >> [whamlet|
     <!-- header -->
     ^{menu}
     <!-- /.header -->
        <p> Nome: #{jogoNome jogo}
        <p> Plataforma: #{jogoPlataforma jogo}
        <p> Categoria: #{jogoCategoria jogo}
        <p> Descrição: #{jogoDescricao jogo}
     ^{footer}
    |] 

getListarR :: Handler Html
getListarR = do
    listaP <- runDB $ selectList [] [Asc JogoNome] 
    defaultLayout $ widgetCss >> [whamlet| 
     <!-- header -->
     ^{menu}
     <!-- /.header -->
        <h1>Jogos cadastrados:
        $forall Entity pid jogo <- listaP
            <a href=@{JogoR pid}> #{jogoNome jogo}<br>
     ^{footer}
|] 



main::IO()
main = do 
       pool <- runStdoutLoggingT $ createSqlitePool "sitio.db3" 10 -- create a new pool
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Sitio pool) 
