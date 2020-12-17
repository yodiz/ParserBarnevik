#if COMPILED
module Dry.JSON
#else
#load "ParserBarnevik.fs"
#endif


type JSObj = 
    internal |Content of List<string*JSVal>
    with 
        static member ofList items = 
            JSVal.JSObj (JSObj.Content (items))
        static member toList = function |JSObj.Content l -> l
        member x.ToList = JSObj.toList x
        member private x.Map = x.ToList |> Map.ofList
        member x.tryFind name = x.Map |> Map.tryFind name
            
and JSVal = 
    |JSStr of string
    |JSBool of bool
    |JSArr of JSVal list
    |JSObj of JSObj
    |JSInt of int
    |JSNull


let rec formatStr (jsVal:JSVal) = 
    match jsVal with
    |JSStr s -> sprintf "\"%s\"" (s.Replace("\n", "\\n").Replace("\r", "\\r").Replace("\"", "\\\""))
    |JSNull -> "null"
    |JSInt i -> sprintf "%i" i
    |JSBool b -> if b then "true" else "false"
    |JSArr a -> sprintf "[%s]" (a |> List.map formatStr |> String.concat ",")
    |JSObj o -> 
        JSObj.toList o
        |> List.map (fun (k,v) -> sprintf "\"%s\":%s" k (formatStr v))
        |> String.concat ","
        |> (fun x -> sprintf "{%s}" x)


open System
open ParserBarnevik

    
type 'a parser = Parser<'a>
let parseJson', parseJsonRef = createParserForwardedToRef()


let stringLiteral : Parser<string> =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))


//let parseIdentifier: string parser =  manyChars (noneOf "\"")
let parseString  = stringLiteral
let parseJString = parseString <?> "string" 

//Den här är inte sann, men den får duga så länge :(
let parseJNumber = pint32 <?> "number" |>> JSInt
let parseJSBool : Parser<JSVal> = 
    pstringCI "true" |>> (fun x -> true)
    <|> (pstringCI "false" |>> (fun x -> false))
    <?> "boolean"
    |>> JSBool
let parseJSNull : Parser<JSVal> = 
    pstringCI "null" <?> "null" |>> (fun x -> JSNull)
    

let parseJArray = 
    pchar '[' >>. spaces >>. sepBy parseJson' (pchar ',' .>> spaces) .>> spaces .>> pchar ']'
        <?> "array"
        |>> JSArr

let parseJProperty = 
    (parseString .>> spaces) .>>. (pchar ':' >>. spaces >>. parseJson' .>> spaces)
        <?> "property"

let parseJObject = 
    spaces >>. pchar '{' >>. spaces >>. sepBy parseJProperty (spaces >>. pchar ',' .>> spaces) .>> spaces .>> pchar '}'
        <?> "object"
        |>> JSObj.ofList

parseJsonRef := choice [
    parseJSNull;
    parseJSBool;
    parseJString |>> JSStr;
    parseJNumber;
    parseJArray;
    parseJObject;
]

//let testText = "{\"a\": {\"y\": {}}, \"b\": [\"z\", [], {}], \"c\": []}"
        
//testText |> printfn "%s"
let parseJson = parseJson' .>> eof
let parseJsonOrFail = runParserStrOrFail parseJson
let test () = 
    let formatToString jsVal = formatStr jsVal

    
    let a = runParserStrOrFail parseJson "\"Test\""
    let d = runParserStrOrFail (manyChars number) "1234"
    let b = a = JSStr "Test"

    
    let d = parseJsonOrFail "Tes\r\nt"
    let e = parseJsonOrFail """{"Properties":[],
                                "DescriptionMarkdown":"Test\r\n\"test",
                                "Name":"Min sida"      
                                "Test" : asd1   
                                }"""
    let qq = runParserStrOrFail ((sepBy pint32 (pchar ','))) "56,1238,96"
    let qqq = runParserStrOrFail parseJObject """  { "test" : 1234 }"""

    let qq = runParserStrOrFail parseJProperty "\"test\"\r\n:hej"

    let qqq = runParserStrOrFail spaces "asd "

    let qqqq = runParserStrOrFail (pint32 .>> (pchar ',')) "123,"

    

    let f = JSStr "\"" 
    let g = parseJsonOrFail (formatToString a)

    ()
