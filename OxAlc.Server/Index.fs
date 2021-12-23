module OxAlc.Server.Index

open Bolero
open Bolero.Html
open Bolero.Server.Html
open OxAlc

let page = doctypeHtml [] [
    head [] [
        meta [attr.charset "UTF-8"]
        meta [attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0"]
        title [] [text "Bolero Application"]
        ``base`` [attr.href "/"]
        link [attr.rel "stylesheet"; attr.href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css"]
        link [attr.rel "stylesheet"; attr.href "css/index.css"]
    ]
    body [] [
        div [attr.id "main"] [rootComp<Client.Main.MyApp>]
        boleroScript
    ]
]
