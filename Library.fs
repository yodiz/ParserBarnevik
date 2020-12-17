//#if COMPILED
module ClassLibrary1
//#endif

//type CharStreamState = 
//    {
//        LineNr : int
//        Column : int
//        Location : int
//    }


//type CharStream2(reader:System.IO.TextReader, readAheadBuffer:char array) = 
//    let mutable peaked = 0
//    let mutable peakOffset = 0
   
//    //Returns true if eof was found
//    let read start len = 
//        printfn "Wants to read %i of len %i" start len
//        let read = reader.ReadBlock(readAheadBuffer, start, len)
//        if read < len then   
//            for i = 0 to len - read - 1 do
//                readAheadBuffer.[start + read + i] <- char 65535
//        printfn "Read %i Chars" read
//        peaked <- peaked + len
//        read < len

//    member x.Peek i =
//        if i < 0 then failwithf "Cant have negative value as peek"
//        if i >= readAheadBuffer.Length then failwithf "ReadAhead buffer full, MaxSize %i" readAheadBuffer.Length

//        let needToRead = i - peaked + 1

//        let minToRead = 1024
//        //Read minimum of x size
//        //if needToRead < minToRead

//        if needToRead > 0 then
//            let leftCirc = readAheadBuffer.Length - peakOffset 
//            let start = (peakOffset + peaked) % readAheadBuffer.Length
//            if needToRead > leftCirc then
//                let eof = read start leftCirc 
//                let eof2 = read 0 (needToRead - leftCirc)
//                ()
//            else
//                read start needToRead |> ignore<bool>
            
//        let loc = peakOffset + i
//        readAheadBuffer.[loc % readAheadBuffer.Length]
//    member x.Read () = 
//        if peaked > 0 then
//            let loc = peakOffset + 0
//            let cloc = 
//                if loc >= readAheadBuffer.Length then loc - readAheadBuffer.Length
//                else loc
//            let r = readAheadBuffer.[cloc]
//            peakOffset <- (cloc + 1) % readAheadBuffer.Length
//            peaked <- peaked - 1
//            r
//        else
//            reader.Read() |> char

//    member x.Buffer() = readAheadBuffer
//    static member createBufferSize s i = CharStream2(s, Array.init i (fun _ -> '\000'))
//    static member createStrSize str i = CharStream2.createBufferSize (new System.IO.StringReader(str)) i
//    static member create str =  CharStream2.createStrSize str 4096

    

//let a = CharStream2.create "Test"
//a.Peek(3)



//type CharStream(src:string) = 
//    let mutable trace = true
//    let mutable state = { Location = 0; LineNr = 0; Column = 0 }
    
//    let peekFrom loc = 
//        if loc >= src.Length then None 
//        else Some (src.Chars loc)
//    let peek() = peekFrom state.Location

//    //Peek i
//    //Read i
//    member x.Peek () = 
//        peek ()
//    member x.Read () = 
//        let r = x.Peek ()
        
//        let lineInc, colInc = 
//            match r with 
//            |Some '\n' -> true, false
//            |Some c when not (System.Char.IsControl(c)) || System.Char.IsWhiteSpace(c) -> false, true
//            |_ -> false, false
//        state <- { state with Location = state.Location + 1; 
//                              LineNr = if lineInc then state.LineNr + 1 else state.LineNr; 
//                              Column = match lineInc, colInc with |true,_ -> 0 |false,true -> state.Column + 1 |false,false -> state.Column  }
//        r
//    member x.Location = state.Location
//    member x.State = state
//    member x.RecoverState s = state <- s

//    member x.ReadStateLine(state:CharStreamState) = 
//        let mutable i = state.Location - 1
//        while (i > 0 && peekFrom i <> Some '\n') do    
//            i <- i - 1
//        let line = System.Text.StringBuilder()
//        while (match peekFrom (i) with |Some '\n' |None -> false |_ -> true) do
//            line.Append(src.Chars (i)) |> ignore
//            i <- i + 1
//        line.ToString()
//    //static member create (str:string)             = 

            
    

//type ParseError = {
//    State : CharStreamState
//    Expected : string list
//}
    
//type ParseResult<'a> = 
//    |Success of 'a
//    |Failure of ParseError

//let parseError state msg  = 
//    Failure { State = state; Expected = [msg] }

//module ParseResult = 
//    let bindFailure fn = function |Failure f -> fn f |Success s -> Success s
//    let mapFailure fn = function |Failure f -> Failure (fn f) |Success s -> Success s
//    let bind fn = function Failure f -> Failure f |Success s -> fn s
//    let map fn = function Failure f -> Failure f |Success s -> Success (fn s)

//type Parser<'a> = CharStream -> ParseResult<'a>

//let private presentChar (c:char option) = 
//    match c with 
//    |None -> "<eof>"
//    |Some c -> 
//        match c with 
//        |'\n' -> "\\n"  |'\r' -> "\\r" |'\t' -> "\\t"
//        |'\b' -> "\\b"  |'\f' -> "\\f" 
//        |c -> string c

//let satisfy f msg : Parser<char> =
//    fun s -> 
//        match s.Peek() with 
//        |Some p when f p -> 
//            s.Read() |> ignore
//            Success p
//        |_ -> parseError s.State msg

//let pchar (c:char) : Parser<char> = 
//    satisfy (fun x -> x = c) (presentChar (Some c))

//let many<'a> (p:Parser<'a>) : Parser<'a list> = 
//    fun s -> 
//        let rec run l = 
//            p s
//            |> function |Success s -> run (s::l)
//                        |Failure f -> Success (List.rev l )
//        run [] 


//let pstring str : Parser<'a> = 
//    fun s -> 
        

    
//let eof : Parser<unit> = 
//    fun s -> match s.Peek () with |None -> Success () |Some c -> parseError s.State (presentChar None)


//let attempt (p:Parser<'a>) = 
//    fun (s:CharStream) ->
//        let state = s.State
//        p s
//        |> ParseResult.mapFailure (fun str -> s.RecoverState state; str)

//let parserOr<'a> (p1:Parser<'a>) (p2:Parser<'a>) : Parser<'a> = 
//    fun s -> 
//        attempt p1 s
//        |> ParseResult.bindFailure 
//            (fun f -> p2 s |> ParseResult.mapFailure (fun f2 -> { f2 with Expected = f.Expected @ f2.Expected }))
            
//let pipe2<'a,'b,'c> fn (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'c> = 
//    fun s -> 
//        p1 s
//        |> ParseResult.bind (fun s1 -> p2 s |> ParseResult.map (fun s2 -> fn s1 s2))

//let pipeLeft<'a,'b> (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'a> = 
//    pipe2 (fun a _ -> a) p1 p2 
        

//let pipeRight<'a,'b> (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'b> = 
//    pipe2 (fun _ b -> b) p1 p2 


//let formatError (err:ParseError) (s:CharStream) = 


//    sprintf 
//        "Error at Location: %i, LineNr: %i, Column: %i\r\n%s\r\n%s\r\nExpected: %s, Got: %s at" 
//        err.State.Location err.State.LineNr err.State.Column
//        (s.ReadStateLine err.State)
//        ((String.replicate err.State.Column " ") + "^")
//        (err.Expected |> String.concat "|")
//        (s.Peek () |> presentChar)
        


//let runParser (p:Parser<'a>) src = 
//    let cs = CharStream(src)
//    p cs
//    |> function |Success s -> Result.Ok s
//                |Failure err -> Result.Error (formatError err cs)


//let (<|>) = parserOr
//let (>>.) = pipeRight
//let (.>>) = pipeLeft
//let (.>>.) p1 p2 = pipe2 (fun a b -> a,b) p1 p2

//let ab : Parser<char> = parserOr (pchar 'a') (pchar 'b')
//let aOrB : Parser<char> = pchar 'a' <|> pchar 'b'

////runParser (pchar 'a' .>>. pchar 'b') "ab"
////runParser ab "a"

//runParser 
//    (
//        many (pchar 'a' <|> pchar 'b')
//        .>> eof
//    )
//    "abbaccaaa"
    


