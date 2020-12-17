module ParserBarnevik

type Mark = {
    Location : int
}

type PeakableBuffer<'a when 'a : equality>(blockSize, reader : 'a array -> int -> int -> int, eofConstant:'a, isLineBreak, ?preInitedBuffers) = 
    let mutable blocks = 
        match preInitedBuffers with 
        |None -> 
            Array.init 0 (fun _ -> Array.zeroCreate<'a> blockSize)
        |Some buffers -> 
            buffers |> Array.iter (fun x -> if x |> Array.length <> blockSize then failwithf "Pre initied buffer length missmatch")
            buffers


    //When EOS is found this is set to EOF location. Otherwise -1
    let mutable eosAt = -1

    //Current location in stream
    let mutable location = 0

    ///data that is truncated
    let mutable readOffset = 0

    ///last commited Read
    let mutable readStartLocation = 0

    ///Number of blocks that contain data
    let mutable blocksLoaded = 0

    let mutable uncommitedLineBreaks = []
    let mutable lineBreaks = []
    let mutable commitedLineBreaks = 0

    let mutable marks = []
    
    ////Read ahead
    let read () = 
        let pos = location - readOffset
        let block = pos / blockSize

        //make sure block exists 
        while block >= blocks.Length do
            let old = blocks
            blocks <- Array.zeroCreate (old.Length + 1) 
            for i = 0 to old.Length - 1 do 
                blocks.[i] <- old.[i]
            blocks.[blocks.Length - 1] <- Array.zeroCreate<'a> blockSize
            //printfn "Created new block, current size %i" blocks.Length

        //Make sure block is loaded
        while block >= blocksLoaded do
            let read = reader blocks.[blocksLoaded] 0 blockSize
            if read < blockSize then 
                eosAt <-  //Questionable
                    read
                    + readOffset
                    + blocksLoaded * blockSize

                //printfn "EOF found at %i" eosAt        
            blocksLoaded <- blocksLoaded + 1

        if eosAt > -1 && location >= eosAt then eofConstant
        else
            let c = blocks.[block].[pos%blockSize]
            if isLineBreak c then uncommitedLineBreaks <- location :: uncommitedLineBreaks
            location <- location + 1
            c

        // OP -, OP /, OP >=, OP >=, OP >, OP ARRAY INDEX, OP (custom), OP Location change

         

    let commit () =
        let pos = location - readOffset
        let block = pos / blockSize
                    
        let block_with_last_linebreak = 
            match uncommitedLineBreaks with 
            |a :: _ -> a
            |[] -> 0
        
        let blockMove = 
            block_with_last_linebreak
            |> min block
            |> min (blocks.Length)
        
        for i = 0 to (blocks.Length - blockMove) - 1 do
            blocks.[i] <- blocks.[i+blockMove]
        blocksLoaded <- blocksLoaded - blockMove


        //Actual
        readStartLocation <- location 

        readOffset <- readOffset + (blockMove * blockSize)
        lineBreaks <- uncommitedLineBreaks @ lineBreaks
        commitedLineBreaks <- commitedLineBreaks + List.length uncommitedLineBreaks
        uncommitedLineBreaks<- []
    
    member x.Read() = read ()
    member x.Location() = location
    member x.LineNumber() = 
        commitedLineBreaks + List.length uncommitedLineBreaks
    member x.Blocks() = blocks.Length


    member x.Mark() = 
        marks <- (location,uncommitedLineBreaks) :: marks
        location

    member x.Recover locationMark = 
        match marks with 
        |[] -> failwith "No mark set"
        |(a,lb) :: b -> 
            if a <> locationMark then failwithf "Unexpected Recover mark, got %i, expected %i" locationMark a
            location <- a
            uncommitedLineBreaks <- lb      
            marks <-b        

    member x.Release locationMark = 
        match marks with
        |[] -> failwithf "No mark to release"
        |(a,b) :: tail -> 
            if a <> locationMark then failwithf "Expected mark %i, got %i" a locationMark
            marks <- tail
            if tail = [] then 
                commit()

    member x.GetCurrentLine() = 
        let prev_linebreak_pos = 
            match lineBreaks with 
            |a :: _ -> a
            |[] -> 0

        let m = x.Mark ()
        ///Read until eofLine
        let mutable eolFound = false
        while eolFound = false do
            let c = x.Read()
            let isLb = isLineBreak c
            eolFound <- isLb || c = eofConstant
            ()

        let line = 
            seq {
                for p = prev_linebreak_pos to location do
                    let pos = p - readOffset
                    let block = pos / blockSize        
                    let c = blocks.[block].[pos%blockSize]
                    if not (isLineBreak c) then 
                        yield c
            }
            
        let l = line |> Seq.toArray
        x.Recover m
        (location - prev_linebreak_pos),l

type ParseError = {
    ExpectedError : string
}
    
type ParseResult<'a> = 
    |Success of 'a
    |Failure of ParseError

let parseError msg  = 
    Failure { ExpectedError = msg }


module ParseResult = 
    let bindFailure fn = function |Failure f -> fn f |Success s -> Success s
    let mapFailure fn = function |Failure f -> Failure (fn f) |Success s -> Success s
    let bind fn = function Failure f -> Failure f |Success s -> fn s
    let map fn = function Failure f -> Failure f |Success s -> Success (fn s)

type Parser<'a> = PeakableBuffer<char> -> ParseResult<'a>

let replaceError<'a> msg (p:Parser<'a>) = 
    fun s -> 
        p s |> ParseResult.mapFailure (fun (s:ParseError) -> { s with ExpectedError = msg } )
let appendError msg p = 
    fun s -> 
        p s |> ParseResult.mapFailure (fun (s:ParseError) -> { s with ExpectedError = s.ExpectedError + "; " + msg } )

let private presentChar (c:char) = 
    match c with 
    |'\n' -> "\\n"  |'\r' -> "\\r" |'\t' -> "\\t"
    |'\b' -> "\\b"  |'\f' -> "\\f" |c when c = char 65535 -> "<eof>"

    |c -> string c

let satisfy f : Parser<char> =
    fun s -> 
        let m = s.Mark ()
        match s.Read () with 
        |p when f p ->         
            s.Release m
            Success p
        |_ -> 
            s.Recover m
            parseError "satisfy"

let pchar (c:char) : Parser<char> = 
    satisfy (fun x -> x = c) |> replaceError (string c)

let number : Parser<char> = 
    satisfy System.Char.IsNumber |> replaceError "number"

let many<'a> (p:Parser<'a>) : Parser<'a list> = 
    fun s -> 
        let rec run l = 
            p s
            |> function |Success s -> run (s::l)
                        |Failure f -> Success (List.rev l )
        run [] 


let attempt (p:Parser<'a>) : Parser<'a> = 
    fun s ->
        let mark = s.Mark()
        p s
        |> function
            |ParseResult.Success a -> 
                s.Release mark
                ParseResult.Success a
            |ParseResult.Failure err -> 
                s.Recover mark
                ParseResult.Failure err

let parserOr<'a> (p1:Parser<'a>) (p2:Parser<'a>) : Parser<'a> = 
    fun s -> 
        attempt p1 s
        |> ParseResult.bindFailure 
            (fun f -> p2 s |> ParseResult.mapFailure (fun f2 -> { f2 with ExpectedError = f.ExpectedError + "|" + f2.ExpectedError }))

let pipe1<'a,'b> (p1:Parser<'a>) fn : Parser<'b> = 
    fun s -> 
        p1 s |> ParseResult.map fn
            
let pipe2<'a,'b,'c> (p1:Parser<'a>) (p2:Parser<'b>) fn : Parser<'c> = 
    fun s -> 
        p1 s
        |> ParseResult.bind (fun s1 -> p2 s |> ParseResult.map (fun s2 -> fn s1 s2))

let pipe3<'a,'b,'c,'d> (p1:Parser<'a>) (p2:Parser<'b>) (p3:Parser<'c>) fn : Parser<'d> = 
    fun s -> 
        p1 s
        |> ParseResult.bind (fun s1 -> p2 s|> ParseResult.map (fun s2 -> s1,s2))
        |> ParseResult.bind (fun (s1,s2) -> p3 s |> ParseResult.map (fun s3 -> fn s1 s2 s3))

let pipeLeft<'a,'b> (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'a> = 
    pipe2 p1 p2 (fun a _ -> a)
        

let pipeRight<'a,'b> (p1:Parser<'a>) (p2:Parser<'b>) : Parser<'b> = 
    pipe2  p1 p2  (fun _ b -> b)

let preturn<'a> (x:'a) : Parser<'a> = fun s -> ParseResult.Success x


let (<|>) = parserOr
let (<|>%) p1 x = p1 <|> preturn x
let (>>.) = pipeRight
let (.>>) = pipeLeft
let (.>>.) p1 p2 = pipe2  p1 p2 (fun a b -> a,b)
let (|>>) p1 fn  = pipe1 p1 fn
let (<?>) p msg = replaceError msg p
let (<??>) p msg = appendError msg p

let many1<'a> (p:Parser<'a>) : Parser<'a list> = 
    pipe2 p (many p) (fun a b -> a :: b)
    
let eof : Parser<unit> = 
    satisfy (fun x -> x = char 65535) |>> ignore<char> 


let formatError (err:ParseError) (s:PeakableBuffer<char>) = 
    let col, line = s.GetCurrentLine() 
    sprintf 
        "Error at Location: %i, LineNr: %i, Column: %i\r\n%s\r\n%s\r\nExpected: %s" 
        (s.Location())
        (s.LineNumber())
        col
        (line |> System.String)
        ((String.replicate col " ") + "^")
        (err.ExpectedError)

let runParserStr (p:Parser<'a>) src = 
    use sw = new System.IO.StringReader(src)
    let cs = PeakableBuffer(4096, (fun a b c -> sw.Read(a,b,c)), char 65535, (fun c -> c = '\n'))
    p cs
    |> function |Success s -> Result.Ok s
                |Failure err -> Result.Error (formatError err cs)

let runParserStrOrFail p src = 
    runParserStr p src |> function |Result.Ok s -> s |Result.Error f -> failwithf "%A" f


let pstringCI (str:string) : Parser<string> = 
    fun s -> 
        let m = s.Mark()
        str.ToLower().ToCharArray()
        |> Array.forall(fun x -> System.Char.ToLower(s.Read()) = x)
        |> function 
            |true -> 
                s.Release m
                Success str
            |false -> 
                s.Recover m
                parseError ("stringCI:" + str)

let pstring (str:string) : Parser<string> = 
    fun s -> 
        let m = s.Mark()
        str.ToCharArray()
        |> Array.forall(fun x -> s.Read() = x)
        |> function 
            |true -> 
                s.Release m
                Success str
            |false -> 
                s.Recover m
                parseError str

let anyOf (str:string) : Parser<char> = 
    let m = str.ToCharArray() |> Set.ofArray
    satisfy (fun s -> m |> Set.contains s) <?> ("anyOf:"+str)


let opt<'a> p : Parser<'a option> = 
    p |>> Some <|>% None
    
let spaces : Parser<unit> = 
    (many (anyOf "\r\n\t ")) |>> ignore <?> "whitespace"


let manyChars p = many p |>> (fun c -> System.String(c |> List.toArray))
let many1Chars p = many1 p |>> (fun c -> System.String(c |> List.toArray))

let pint32 : Parser<int> =   
    many1Chars number |>> fun x -> System.Int32.Parse(x)

let between<'a,'b,'c> (pStart:Parser<'a>) (pEnd:Parser<'b>) (p:Parser<'c>) : Parser<'c> = 
    pipe3 pStart p pEnd (fun a b c  -> b)

let sepBy<'a,'b> (p:Parser<'a>) (sep:Parser<'b>) : Parser<'a list> = 
    fun s -> 
        let rec run l = 
            p s
            |> function |Success x ->
                            sep s
                            |> function |Success _s -> 
                                            run (x::l)
                                        |Failure f -> 
                                            Success (List.rev (x::l) )
                        |Failure _f -> Success (List.rev l )
        run [] 

let choice<'a> (choices:Parser<'a> list) : Parser<'a> = 
    choices
    |> List.reduce parserOr

let createParserForwardedToRef<'a> () : (Parser<'a>*Parser<'a> ref) = 
    let a : Parser<'a> ref = ref (fun s -> failwithf "No definition given")
    (fun s -> !a s), a
