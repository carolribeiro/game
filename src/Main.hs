{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Text (Text)
import Yesod

data Pagina = Pagina

instance Yesod Pagina

mkYesod "Pagina" [parseRoutes|
    / HomeR GET
    /outro OutroR GET
    /rev/#String RevR GET
    /add/#Int/#Int AddR GET
    
|]

reverter :: String -> String
reverter xs = reverse xs

somar :: Int -> Int -> Int
somar a b = a+b

widgetHtmlHome :: Widget
widgetHtmlHome = [whamlet|
<h2>Ola Yesod!
<ul>
    <li>Teste
|]

widgetCss :: Widget
widgetCss = toWidget [lucius|
    h2{
        color:blue;
    }
|]

widgetHtmlOutro :: Widget
widgetHtmlOutro = [whamlet|
<p>Outro rota
<a href="@{HomeR}">Voltar
|]

getHomeR :: Handler Html
getHomeR = defaultLayout (widgetHtmlHome >> widgetCss)

getOutroR :: Handler Html
getOutroR = defaultLayout widgetHtmlOutro

getRevR :: String -> Handler Html
getRevR pal =  defaultLayout [whamlet|
<h1>A palavra em ordem reversa eh: #{reverter pal}
|]

getAddR :: Int -> Int -> Handler Html
getAddR a b = defaultLayout [whamlet|
<h1>Soma de #{a} e #{b} = #{somar a b}
|]

main :: IO ()
main = warp 8080 Pagina

