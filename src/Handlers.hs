{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handlers where
import Control.Applicative
import Control.Monad.Logger(runStdoutLoggingT)
import Data.Text
import Data.Time
import Foundation
import Import
import Text.Lucius
import Text.Julius
import Yesod
import Yesod.Static

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

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
             areq textareaField FieldSettings{fsId=Just "hident5",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Descrição")]} Nothing


formNoticia :: Form Noticia
formNoticia = renderBootstrap2 $ Noticia <$>
                areq textField FieldSettings{fsId=Just "hident2",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Título")]} Nothing <*>
                areq textareaField FieldSettings{fsId=Just "hident3",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Notícia")]} Nothing <*>
                lift (liftIO getCurrentTime)


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
     ^{footer}
|]

{-
urlAssets :: String -> String
urlAssets x = "https://preview.c9users.io/carolribeiro/carol/static/" ++ x {-trocar a rota-}
-}

menu :: Widget
menu = toWidget [whamlet|
<header data-type="parallax" data-speed="4" class="cover" style="background-image: url(@{StaticR cover_jpg});">
        <div class="header-color">
             <div class="header">
                  <div class="container">
                       <a href="@{HomeR}" class="logo pull-left">
                          <i class="ion-ios-game-controller-b">
                          Uplay
                  <!-- navigation -->
                  <nav>
                       <div class="container">
                            <ul>
                                <li>
                                    <a href="@{HomeR}">Home
                                <li>
                                    <a href="@{ListarJogoR}">Jogos
                                <li>
                                    <a href="@{ListarNoticiaR}">Notícias
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
                                            <i class="fa fa-chevron-right">
                                            Home
                                     <li>
                                         <a href="@{ListarJogoR}">
                                            <i class="fa fa-chevron-right">
                                            Jogos
                                     <li>
                                         <a href="@{ListarNoticiaR}">
                                            <i class="fa fa-chevron-right">
                                            Notícias
                                     <li>
                                         <a href="@{CadastroR}">
                                            <i class="fa fa-chevron-right">
                                            Cadastro
                                     <li>
                                         <a href="@{LoginR}">
                                            <i class="fa fa-chevron-right">
                                            Login
                                     <li>
                                         <a href="@{ContatoR}">
                                            <i class="fa fa-chevron-right">
                                            Contato
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


widgetJs :: Widget
widgetJs = do
   addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
   addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
   addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/owl-carousel/1.3.3/owl.carousel.min.js"
   addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.3/modernizr.min.js"
   addScript $ StaticR core_js
   addScript $ StaticR demo_js
   addScript $ StaticR script_js


widgetCss :: Widget
widgetCss = do
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/ionicons/2.0.1/css/ionicons.min.css"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/animate.css/3.4.0/animate.min.css"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/owl-carousel/1.3.3/owl.carousel.css"
    addStylesheet $ StaticR style_css
    addStylesheet $ StaticR helpers_css
    addStylesheet $ StaticR blue_css
    addStylesheet $ StaticR demo_css


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
                                   <img src=@{StaticR s1_jpg} class="full-width" alt="First slide image">
                                   <div class="carousel-caption">
                                        <h1>
                                            <span>Global news
                                        <h2>
                                            <span>Uncharted 4 Review
                                        <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas egestas, orci id...
                              <div class="item">
                                   <img src=@{StaticR s2_jpg} class="full-width" alt="Second slide image">
                                   <div class="carousel-caption">
                                        <h1>
                                            <span>PS4
                                        <h2>
                                            <span>Last of Us Remastered
                                        <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas egestas, orci id...
                              <div class="item">
                                   <img src=@{StaticR s3_jpg} class="full-width" alt="Third slide image">
                                   <div class="carousel-caption">
                                        <h1>
                                            <span>Xbox One
                                        <h2>
                                            <span>Marvel Galaxy Review
                                        <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas egestas, orci id...
                              <div class="item">
                                   <img src=@{StaticR s4_jpg} class="full-width" alt="Third slide image">
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
                                  <div class="thumbnail" style="float:left; margin-right:24px; margin-left:5px;">
                                       <h4 class="padding-10-20" style="font-size:16px;text-align:center">
                                           <a href="#">Grand Theft Auto 5
                                       <a href="@{ListarJogoR}">
                                          <img src=@{StaticR gta_jpg} alt="GTA 5">
                                       <div class="caption padding-15-20">
                                            <a href="@{ListarJogoR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-right:24px;">
                                       <h4 class="padding-10-20" style="font-size:16px;text-align:center">
                                           <a href="#">Batman Arkham Knight
                                       <a href="#">
                                          <img src=@{StaticR bat_jpg} alt="Batman Arkham Knight">
                                       <div class="caption padding-15-20">
                                            <a href="@{ListarJogoR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-right:24px;">
                                       <h4 class="padding-10-20" style="font-size:16px;text-align:center">
                                           <a href="#">Tomb Raider
                                       <a href="#">
                                          <img src=@{StaticR tomb_jpg} alt="Tomb Raider">
                                       <div class="caption padding-15-20">
                                            <a href="@{ListarJogoR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left;">
                                       <h4 class="padding-10-20" style="font-size:16px;text-align:center">
                                           <a href="#">Injustice Gods Among Us
                                       <a href="#">
                                          <img src=@{StaticR inj_jpg} alt="Injustice Gods Among Us">
                                       <div class="caption padding-15-20">
                                            <a href="@{ListarJogoR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-top:24px; margin-left:5px; margin-right:25px;">
                                       <h4 class="padding-10-20" style="font-size:16px;text-align:center">
                                           <a href="#">Metal Gear Solid V
                                       <a href="#">
                                          <img src=@{StaticR metal_jpg} alt="Metal Gear Solid V">
                                       <div class="caption padding-15-20">
                                            <a href="@{ListarJogoR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-top:24px; margin-right:25px;">
                                       <h4 class="padding-10-20" style="font-size:16px;text-align:center">
                                           <a href="#">Assassin's Creed Unity
                                       <a href="#">
                                         <img src=@{StaticR ac_jpg} alt="Assassin's Creed Unity">
                                       <div class="caption padding-15-20">
                                            <a href="@{ListarJogoR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-top:24px;margin-right:25px;">
                                       <h4 class="padding-10-20" style="font-size:16px;text-align:center">
                                           <a href="#">The Witcher 3
                                       <a href="#">
                                          <img src=@{StaticR wit_jpg} alt="The Witcher 3">
                                       <div class="caption padding-15-20">
                                            <a href="@{ListarJogoR}" class="btn btn-block btn-primary">Ver
                                  <div class="thumbnail" style="float:left; margin-top:24px;">
                                       <h4 class="padding-10-20" style="font-size:16px;text-align:center">
                                           <a href="#">God of War 3
                                       <a href="#">
                                          <img src=@{StaticR god_jpg} alt="God of War 3">
                                       <div class="caption padding-15-20">
                                            <a href="@{ListarJogoR}" class="btn btn-block btn-primary">Ver
               <!-- section -->
          <!-- /.wrapper -->
     ^{footer}
|]


widgetHtmlContato :: Widget
widgetHtmlContato = [whamlet|
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


widgetHtmlAdmin :: Widget
widgetHtmlAdmin = [whamlet|
     ^{menu}
 <!-- /.header -->
     <div class="container">
          <!-- wrapper-->
          <div id="wrapper" class="margin-top-15 margin-bottom-30">
               <div class="col-md-12">
                    <section class="no-border no-padding">
                             <h4 class="page-header no-margin-top">Seja Bem-vindo
                             <form autocomplete="off" method="POST">
                                   <div class="col-md-12 col-xs-12 no-padding">
                                        <div class="row">
                                         <ul>
                                             <li>
                                              <a href="@{CadastrojogoR}"> Cadastrar Jogo 
                                             <li>
                                              <a href="@{CadastronoticiaR}"> Cadastrar Noticia      
                                   
    
     ^{footer}

|]


widgetHtmlWelcome:: Widget
widgetHtmlWelcome = [whamlet|
     ^{menu}
   
     ^{footer}

|]

getHomeR :: Handler Html
getHomeR = defaultLayout (widgetHtmlHome >> widgetJs >> widgetCss)

getCadastrojogoR :: Handler Html
getCadastrojogoR = do
    (widget, enctype) <- generateFormPost formJogo
    defaultLayout $ widgetForm CadastrojogoR enctype widget "Cadastro de jogos" "Cadastrar" >> widgetCss

getCadastroR :: Handler Html
getCadastroR = do
    (widget, enctype) <- generateFormPost formUsuario
    defaultLayout $ widgetForm CadastroR enctype widget "Cadastro de usuários" "Cadastrar" >> widgetCss

getCadastronoticiaR :: Handler Html
getCadastronoticiaR = do
    (widget, enctype) <- generateFormPost formNoticia
    defaultLayout $ widgetForm CadastronoticiaR enctype widget "Cadastro de notícias" "Cadastrar" >> widgetCss

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                        runDB $ insert usuario
                        setMessage $ [shamlet| <p style="text-align:center;"> Usuário cadastrado com sucesso! |]
                        redirect CadastroR
                    _ -> redirect CadastroR

postCadastrojogoR :: Handler Html
postCadastrojogoR = do
                ((result, _), _) <- runFormPost formJogo
                case result of
                    FormSuccess jogo -> do
                        runDB $ insert jogo
                        setMessage $ [shamlet| <p style="text-align:center;"> Jogo cadastrado com sucesso! |]
                        redirect CadastrojogoR
                    _ -> redirect CadastrojogoR

postCadastronoticiaR :: Handler Html
postCadastronoticiaR = do
                ((result, _), _) <- runFormPost formNoticia
                case result of
                    FormSuccess noticia -> do
                        runDB $ insert noticia
                        setMessage $ [shamlet| <p style="text-align:center;"> Notícia cadastrada com sucesso! |]
                        redirect CadastronoticiaR
                    _ -> redirect CadastronoticiaR

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
                    redirect WelcomeR
                Nothing -> do
                    setMessage $ [shamlet| <p style="text-align:center;"> Usuário inválido |]
                    redirect LoginR
        _ -> redirect LoginR

getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_ID"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout (widgetHtmlAdmin >> widgetCss)

getWelcomeR :: Handler Html
getWelcomeR = do
     usr <- lookupSession "_ID"
     defaultLayout [whamlet|
        $maybe m <- usr
            <h1> Welcome #{m}
     |]
    


getContatoR :: Handler Html
getContatoR = defaultLayout (widgetHtmlContato >> widgetCss)

getJogoR :: JogoId -> Handler Html
getJogoR pid = do
    jogo <- runDB $ get404 pid
    defaultLayout $ widgetCss >> [whamlet|
     <!-- header -->
     ^{menu}
     <!-- /.header -->
         <section class="no-padding">
                  <div class="container">
                       <div class="row margin-top-50 margin-bottom-50">
                            <div class="box padding-20 clearfix">
                                 <div class="col-lg-12 padding-right-30">
                                      <div class="post no-margin-bottom">
                                           <div class="post-header margin-top-10">
                                                <div class="post-title">
                                                     <h2 class="text-uppercase">#{jogoNome jogo}
                                                     <span class="label label-primary margin-top-10 font-size-10">#{jogoPlataforma jogo}
                                                     <span class="label label-success margin-top-5 font-size-10">#{jogoCategoria jogo}
                                           <p class="margin-top-30" style="text-align:justify;">#{jogoDescricao jogo}

     ^{footer}
    |]

getListarJogoR :: Handler Html
getListarJogoR = do
    listaP <- runDB $ selectList [] [Asc JogoNome]
    defaultLayout $ widgetCss >> [whamlet|
     <!-- header -->
     ^{menu}
     <!-- /.header -->
     <div class="container">
          <!-- wrapper-->
          <div id="wrapper">
               <!-- section -->
               <section class="border-grey-200 margin-bottom-10 relative no-padding hidden-xs">
                        <div class="section-title no-margin-top no-border padding-top-25 padding-left-25 padding-right-25">
                             <h3 class="color-black no-border" style="margin-left:-24px">Jogos cadastrados:
                        <div class="padding-left-10 padding-right-10 margin-bottom-25">
                            $forall Entity pid jogo <- listaP
                             <div>
                                  <div class="thumbnail" style="float:left; margin-right:24px; margin-left:5px;">
                                        <h4 class="padding-10-20" style="font-size:18px;text-align:center">
                                            <a href=@{JogoR pid}> #{jogoNome jogo}
                                        <a href="@{JogoR pid}">
                                          <img src=@{StaticR wit_jpg} alt="jogo">
                                        <div class="caption padding-15-20">
                                            <a href="@{JogoR pid}" class="btn btn-block btn-primary">Ver
               <!-- section -->
          <!-- /.wrapper -->
     <!-- footer -->
     ^{footer}
|]

getNoticiaR :: NoticiaId -> Handler Html
getNoticiaR pid = do
    noticia <- runDB $ get404 pid
    defaultLayout $ widgetCss >> [whamlet|
     <!-- header -->
     ^{menu}
     <!-- /.header -->
         <section class="no-padding">
                  <div class="container">
                       <div class="row margin-top-50 margin-bottom-50">
                            <div class="box padding-20 clearfix">
                                 <div class="col-lg-12 padding-right-30" style="background:#fff;">
                                      <div class="post no-margin-bottom">
                                           <div class="post-header margin-top-10">
                                                <div class="post-title">
                                                     <h2 class="text-uppercase" style="font-size:24px;">#{noticiaTitulo noticia}
                                                     <span class="label label-primary margin-top-10 font-size-10">postado por 
                                                     <span class="label label-success margin-top-5 font-size-10">#{show $ utctDay $ noticiaPostado noticia}
                                           <p class="margin-top-30" style="text-align:justify;">#{noticiaNoticia noticia}

     ^{footer}
    |]

getListarNoticiaR :: Handler Html
getListarNoticiaR = do
    listaN <- runDB $ selectList [] [Asc NoticiaTitulo]
    defaultLayout $ widgetCss >> [whamlet|
     <!-- header -->
     ^{menu}
     <!-- /.header -->
     <div class="container">
          <!-- wrapper-->
          <div id="wrapper">
               <!-- section -->
               <section class="border-grey-200 margin-bottom-10 relative no-padding hidden-xs">
                        <div class="section-title no-margin-top no-border padding-top-25 padding-left-25 padding-right-25">
                             <h3 class="color-black no-border" style="margin-left:-24px">Notícias cadastradas:
                        <div class="padding-left-10 padding-right-10 margin-bottom-25">
                             $forall Entity pid noticia <- listaN
                                  <div>
                                        <h4 style="font-size:18px;">
                                            <a href=@{NoticiaR pid}>#{noticiaTitulo noticia}
               <!-- section -->
          <!-- /.wrapper -->
     <!-- footer -->
     ^{footer}
|]

connStr = "dbname=dermeni1jgqg9m host=ec2-107-21-223-72.compute-1.amazonaws.com user=woeyqqewbjxmha password=zjuk2LVEqbyVWxyvP0JH7kANYN port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)
