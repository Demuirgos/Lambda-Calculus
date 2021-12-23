module OxAlc.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home

/// The Elmish application's model.
type Model =
    {
        page    : Page
        input   : String Option
        output  : String Option
        error   : String Option
    }

let initModel =
    {
        page   = Page.Home
        input  = None
        output = None
        error  = None
    }

/// Remote service definition.
type LambdaService =
    {
        Interpret: String -> Async<String>

        Decompile: string -> Async<String>

        Transpile: String -> Async<String>

        Parse    : String -> Async<String>
    }

    interface IRemoteService with
        member this.BasePath = "/Api"
/// The Elmish application's update messages.
type Message =
    | SetPage   of Page
    | SetInput  of string
    | Transpile of string
    | Decompile of string
    | Interpret of string
    | RcvResult of string
    | Parse     of string
    | Error     of exn
    | Clear 

let update remote message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | Transpile code ->
        let cmd = Cmd.OfAsync.either remote.Interpret String.Empty RcvResult Error
        { model with output = None }, Cmd.none
    | Decompile code ->
        let cmd = Cmd.OfAsync.either remote.Decompile String.Empty RcvResult Error
        { model with output = None }, Cmd.none
    | Interpret code ->
        let cmd = Cmd.OfAsync.either remote.Transpile String.Empty RcvResult Error
        { model with output = None }, Cmd.none
    | Parse code     ->
        let cmd = Cmd.OfAsync.either remote.Parse     String.Empty RcvResult Error
        { model with output = None }, Cmd.none
    | SetInput text ->
        { model with input  = Some text }, Cmd.none
    | RcvResult text ->
        { model with output = Some text }, Cmd.none
    | Error exn ->
        { model with error  = Some exn.Message }, Cmd.none
    | Clear     ->
        { model with error = None; input = None; output= None}, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let MakeTextArea (model: Model) dispatch =
    div [attr.id "input"] [Node.Text "TextEditor"]

let MakeOutputArea (model: Model) dispatch =
    div [attr.id "output"] [Node.Text "TextViewer"]
    
let MakeActionArea (model: Model) dispatch =
    div [attr.id "action"] [Node.Text "ButtonViewer"]

let homePage model dispatch =
    Node.Concat [   MakeActionArea model dispatch;
                    MakeTextArea model dispatch;
                    MakeOutputArea model dispatch ]

let view model dispatch =
    Main()
        .Menu(concat [
            menuItem model Home "Home"
        ])
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch Clear)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let LambdaService = this.Remote<LambdaService>()
        let update = update LambdaService
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg Clear) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
