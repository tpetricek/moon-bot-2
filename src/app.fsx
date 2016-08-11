#if INTERACTIVE
#r "System.Xml.Linq.dll"
#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#r "../packages/Suave/lib/net40/Suave.dll"
#load "math.fs"
#else
module App
#endif
open Suave
open System
open System.IO
open System.Web
open FSharp.Data
open Suave.Filters
open Suave.Writers
open Suave.Operators

// ----------------------------------------------------------------------------
// Various helper functions
// ----------------------------------------------------------------------------

// Find the 'web' directory with static files (like 'index.html'). This is 
// a bit ugly, because it works differently locally and on Azure
let root = 
  let asm =  
    if System.Reflection.Assembly.GetExecutingAssembly().IsDynamic then __SOURCE_DIRECTORY__
    else IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
  IO.Path.GetFullPath(Path.Combine(asm, "..", "web"))

/// Creates a URL for getting wether forecaset from OpenWeatherMap 
let weatherUrl (place:string) = 
  "http://api.openweathermap.org/data/2.5/forecast/daily?q=" +
    HttpUtility.UrlEncode(place) +
    "&mode=json&units=metric&cnt=10&APPID=cb63a1cf33894de710a1e3a64f036a27"

/// Creates a URL for getting stock prices form Yahooo
let stocksUrl stock = 
  "http://ichart.finance.yahoo.com/table.csv?s=" + stock

/// Turn Unix timestamp into .NET DataTime object     
let toDateTime (timestamp:int) =
  let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
  start.AddSeconds(float timestamp).ToLocalTime()


// ----------------------------------------------------------------------------
// Type providers for accessing data
// ----------------------------------------------------------------------------

type BBC = XmlProvider<"http://feeds.bbci.co.uk/news/rss.xml">
type Stocks = CsvProvider<"http://ichart.finance.yahoo.com/table.csv?s=MSFT">


// ----------------------------------------------------------------------------
// Web server and query answering
// ----------------------------------------------------------------------------

// TASK #1: Below is a sample function that uses the XML type provider to 
// print BBC news. Follow the example of handling `stock MSFT` request in the
// `answer` function and adapt the code below so that the bot returns nicely
// formatted news when the user sends us `news` request:

let printNews () = 
  let res = BBC.GetSample()
  let items = 
    [ for r in res.Channel.Items -> 
        sprintf " - %s" r.Title ] 
  let body = items |> Seq.take 10 |> String.concat ""
  printf "<ul>%s</ul>" body

// TASK #2: Add handler for weather forecast. To do this, use `JsonProvider` using
// the sample URL below (similar to XML and CSV). Then use `YourType.Load` to
// get weather forecast for a place (using `weatherUrl` to get the right URL)
// The resulting type has various properties - you'll find forecast in 
// `res.List` (where each item has `it.Temp.Day` etc.). You can also use 
// `toDateTime` (above) to convert the returned timestamps from `Dt`.
// 
// Sample URL for JsonProvider:
//  - http://api.openweathermap.org/data/2.5/forecast/daily?q=Prague&mode=json&units=metric&cnt=10&APPID=cb63a1cf33894de710a1e3a64f036a27
// More information about JsonProvider:
//  - http://fsharp.github.io/FSharp.Data/library/JsonProvider.html 


// TASK #3: When calling 3rd party services, we are currently blocking system
// threads - this will not scale and we should do this asynchronously. 
// To do this, wrap the body of the `answer` function in `async { ... }` 
// block and add `return` keyword to return data. Then you can change
// calls to `Load` to `AsyncLoad` and `GetSample` to `AsyncGetSample`
// and call them using `let!` keyword. Also call `answer` using `let!` in 
// the main body of the server. 
//
// More information about async:
// - https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/asynchronous-workflows-%5Bfsharp%5D


// TASK #4: For more see 'math.fs'. This implements a simple evaluator
// of binary expressions using parser combinators. The possibilities are
// limitless :-)


// ----------------------------------------------------------------------------
// Producing answers 
// ----------------------------------------------------------------------------

let answer (question:string) = 
  let words = question.ToLower().Split(' ') |> List.ofSeq
  match words with
  | "revolution" :: [] ->
      "<marquee style='color:red;font-size:30pt'>ReVoLuTiOooOn!!!</marquee>"

  | "stock" :: stock :: [] ->
      let res = Stocks.Load(stocksUrl stock)
      let items = 
        [ for r in res.Rows -> 
            sprintf "<li><strong>%s</strong>: %f</li>" 
              (r.Date.ToString("D")) r.Open ] 
      let body = items |> Seq.take 10 |> String.concat ""
      sprintf "<ul>%s</ul>" body
  
  | "eval" :: cmds -> Math.eval (String.concat " " cmds)
  | _ -> "Sorry Dave, I cannot do that." 


let app =
  choose [
    path "/" >=> Files.browseFile root "index.html"
    path "/query" >=> fun ctx -> async {
        let res = answer (HttpUtility.UrlDecode(ctx.request.rawQuery))
        return! Successful.OK res ctx }
    Files.browse root ]
