{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text
import Data.Time
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio { connPool :: ConnectionPool,
                     getStatic :: Static }

staticFiles "."


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
    nome Text
    UniqueUsuario nome
    senha Text
    deriving Show

Jogo
    nome Text
    UniqueJogo nome
    plataforma Text
    categoria Text
    descricao Textarea
    dtpost UTCTime default=now()
    deriving Show

Noticia
    titulo Text
    UniqueNoticia titulo
    noticia Textarea
    postado UTCTime default=now()
    deriving Show
|]


mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just $ LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized CadastroUsuarioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Só o admin acessa aqui!"


isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized


type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

