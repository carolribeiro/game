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
             areq textField FieldSettings{fsId=Just "hident2",
                            fsLabel= "",
                            fsTooltip= Nothing,
                            fsName= Nothing,
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Usuário")]} Nothing <*>
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
                            fsAttrs=[("class","form-control input-lg"),("placeholder","Descrição")]} Nothing <*>
             lift (liftIO getCurrentTime)


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
widgetForm x enctype widget y val = do
     $(whamletFile "hamlet/header.hamlet")
     $(whamletFile "hamlet/form.hamlet")


widgetCss :: Widget
widgetCss = do
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/font-awesome/4.4.0/css/font-awesome.min.css"
    addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/ionicons/2.0.1/css/ionicons.min.css"
    addStylesheet $ StaticR assets_css_style_css
    addStylesheet $ StaticR assets_css_helpers_css
    addStylesheet $ StaticR assets_css_blue_css


getCadastroJogoR :: Handler Html
getCadastroJogoR = do
    (widget, enctype) <- generateFormPost formJogo
    defaultLayout $ widgetForm CadastroJogoR enctype widget "Cadastro de jogos" "Cadastrar" >> widgetCss >> $(whamletFile "hamlet/footer.hamlet")


getCadastroUsuarioR :: Handler Html
getCadastroUsuarioR = do
    (widget, enctype) <- generateFormPost formUsuario
    defaultLayout $ widgetForm CadastroUsuarioR enctype widget "Cadastro de usuários" "Cadastrar" >> widgetCss >> $(whamletFile "hamlet/footer.hamlet")


getCadastroNoticiaR :: Handler Html
getCadastroNoticiaR = do
    (widget, enctype) <- generateFormPost formNoticia
    defaultLayout $ widgetForm CadastroNoticiaR enctype widget "Cadastro de notícias" "Cadastrar" >> widgetCss >> $(whamletFile "hamlet/footer.hamlet")


postCadastroUsuarioR :: Handler Html
postCadastroUsuarioR = do
                ((result, _), _) <- runFormPost formUsuario
                case result of
                    FormSuccess usuario -> do
                        runDB $ insert usuario
                        setMessage $ [shamlet| <p class="text-center"> Usuário cadastrado com sucesso! |]
                        redirect CadastroUsuarioR
                    _ -> redirect CadastroUsuarioR


postCadastroJogoR :: Handler Html
postCadastroJogoR = do
                ((result, _), _) <- runFormPost formJogo
                case result of
                    FormSuccess jogo -> do
                        runDB $ insert jogo
                        setMessage $ [shamlet| <p class="text-center"> Jogo cadastrado com sucesso! |]
                        redirect CadastroJogoR
                    _ -> redirect CadastroJogoR


postCadastroNoticiaR :: Handler Html
postCadastroNoticiaR = do
                ((result, _), _) <- runFormPost formNoticia
                case result of
                    FormSuccess noticia -> do
                        runDB $ insert noticia
                        setMessage $ [shamlet| <p class="text-center"> Notícia cadastrada com sucesso! |]
                        redirect CadastroNoticiaR
                    _ -> redirect CadastroNoticiaR


getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost formUsuario
    defaultLayout $ widgetForm LoginR enctype widget "Login" "Entrar" >> widgetCss >> $(whamletFile "hamlet/login.hamlet") >> $(whamletFile "hamlet/footer.hamlet")


postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [UsuarioNome ==. usuarioNome usr, UsuarioSenha ==. usuarioSenha usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioNome usr)
                    redirect HomeR
                Nothing -> do
                    setMessage $ [shamlet| <p class="text-center"> Usuário inválido |]
                    redirect LoginR
        _ -> redirect LoginR


getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_ID"
    redirect HomeR


getAdminR :: Handler Html
getAdminR = do
     usr <- lookupSession "_ID"
     defaultLayout $ widgetCss >> $(whamletFile "hamlet/header.hamlet") >> $(whamletFile "hamlet/admin.hamlet") >> $(whamletFile "hamlet/footer.hamlet")


getHomeR :: Handler Html
getHomeR = do
     defaultLayout $ widgetCss >> $(whamletFile "hamlet/header.hamlet") >> $(whamletFile "hamlet/home.hamlet") >> $(whamletFile "hamlet/footer.hamlet")


getListarUsuarioR :: Handler Html
getListarUsuarioR = do
    listaU <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ widgetCss >> $(whamletFile "hamlet/header.hamlet") >> $(whamletFile "hamlet/listar-usuario.hamlet") >> $(whamletFile "hamlet/footer.hamlet")


getJogoR :: JogoId -> Handler Html
getJogoR pid = do
    jogo <- runDB $ get404 pid
    defaultLayout $ widgetCss >> $(whamletFile "hamlet/header.hamlet") >> $(whamletFile "hamlet/jogo.hamlet") >> $(whamletFile "hamlet/footer.hamlet")


getListarJogoR :: Handler Html
getListarJogoR = do
    listaJ <- runDB $ selectList [] [Asc JogoNome]
    defaultLayout $ widgetCss >> $(whamletFile "hamlet/header.hamlet") >> $(whamletFile "hamlet/listar-jogo.hamlet") >> $(whamletFile "hamlet/footer.hamlet")


getNoticiaR :: NoticiaId -> Handler Html
getNoticiaR pid = do
    noticia <- runDB $ get404 pid
    defaultLayout $ widgetCss >> $(whamletFile "hamlet/header.hamlet") >> $(whamletFile "hamlet/noticia.hamlet") >> $(whamletFile "hamlet/footer.hamlet")


getListarNoticiaR :: Handler Html
getListarNoticiaR = do
    listaN <- runDB $ selectList [] [Asc NoticiaTitulo]
    defaultLayout $ widgetCss >> $(whamletFile "hamlet/header.hamlet") >> $(whamletFile "hamlet/listar-noticia.hamlet") >> $(whamletFile "hamlet/footer.hamlet")


connStr = "dbname=dermeni1jgqg9m host=ec2-107-21-223-72.compute-1.amazonaws.com user=woeyqqewbjxmha password=zjuk2LVEqbyVWxyvP0JH7kANYN port=5432"


main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)
