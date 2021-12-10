namespace OxAlc.Server

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Hosting
open Bolero
open Bolero.Remoting
open Bolero.Remoting.Server
open OxAlc

type LambdaService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<Client.Main.LambdaService>()
    //    type LambdaService =
    //    {
    //        Interpret: String -> Async<String>
    //
    //        Decompile: string -> Async<String>
    //
    //        Transpile: String -> Async<String>  
    //
    //        Parse    : String -> Async<String>
    //    }
    override this.Handler =
        {
            Interpret = fun code -> async {
                return books.ToArray()
            }

            Decompile = fun code -> async {
                books.Add(book)
            }

            Transpile = fun code -> async {
                books.RemoveAll(fun b -> b.isbn = isbn) |> ignore
            }

            Parse = fun code -> async {
                if password = "password" then
                    do! ctx.HttpContext.AsyncSignIn(username, TimeSpan.FromDays(365.))
                    return Some username
                else
                    return None
            }

        }
