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
open OxAlc.Service.Library

type LambdaService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<Client.Main.LambdaService>()
    override this.Handler =
        {
            Interpret = fun code -> async {
                return interpret code
            }

            Decompile = fun code -> async {
                return decompile code
            }

            Transpile = fun code -> async {
                return transpile code
            }

            Parse = fun code -> async {
                return parse Oxalc code
            }

        }
