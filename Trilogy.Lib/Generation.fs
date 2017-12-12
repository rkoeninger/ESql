namespace Trilogy

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open Parser

// https://github.com/fsprojects/SQLProvider/blob/5f6352e49fee5b743940100c6fb89bb70c62d127/src/SQLProvider/SqlDesignTime.fs
// https://github.com/mausch/XmlLiteralsTypeProvider/blob/master/XmlLiteralsTypeProvider/XmlLiterals.fs
// http://gettingsharper.de/2014/12/19/having-fun-with-type-level-numbers-using-a-type-provider/

type FileParserResults = {
    FileName: string
    Statements: Statement list
}

[<TypeProvider>]
type QueryProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let logFile = new StreamWriter(File.OpenWrite("log.txt"))

    let ns = "Trilogy"
    let asm = Assembly.GetExecutingAssembly()
    let tempAsmPath = Path.ChangeExtension(Path.GetTempFileName(), ".dll")
    let tempAsm = ProvidedAssembly tempAsmPath
    let loadQueriesProvider = ProvidedTypeDefinition(asm, ns, "LoadQueries", Some typeof<obj>, IsErased = false)
    let parseQueryProvider = ProvidedTypeDefinition(asm, ns, "ParseQuery", Some typeof<obj>, IsErased = false)

    let addEmptyConstructor (t: ProvidedTypeDefinition) =
        let ctor = ProvidedConstructor []
        ctor.InvokeCode <- (fun _ -> <@@ () @@>)
        t.AddMember ctor

    let addProperty (t: ProvidedTypeDefinition) (name: string) (tipe: Type) =
        let prop = ProvidedProperty(name, tipe)
        prop.GetterCode <-
            if tipe = typeof<int> then
                (fun _ -> <@@ 38547 @@>)
            elif tipe = typeof<string> then
                (fun _ -> <@@ "qwerty" @@>)
            else
                failwith "type not supported"
        t.AddMember prop

    let rec searchTree path =
        if File.Exists path && path.EndsWith ".sql" then
            logFile.WriteLine("Reading sql from: " + path)
            let stmts = parse (File.ReadAllText path)
            logFile.WriteLine("Parse results: " + sprintf "%A" stmts)
            [{ FileName = Path.GetFileName path; Statements = [stmts] }]
        elif Directory.Exists path then
            Directory.GetFiles path |> Seq.collect searchTree |> Seq.toList
        else
            []
    
    let translateType = function
        | Int -> typeof<int>
        | Varchar | NVarchar -> typeof<string>
        | _ -> failwith "can't translate type"

    let examineStatment t stmt =
        logFile.WriteLine("Examining statement: " + sprintf "%A" stmt)
        match stmt with
        | CreateStatement x ->
            logFile.WriteLine("Found the create statment: " + sprintf "%A" x)
            for (nm, ty) in x.Columns do
                addProperty t nm (translateType ty)
        | _ -> ()

    let loadQueries t path =
        logFile.WriteLine("Loading queries from root: " + Path.GetFullPath path)
        for stmt in List.collect (fun (x: FileParserResults) -> x.Statements) (searchTree path) do
            examineStatment t stmt

    let parseQuery (t: ProvidedTypeDefinition) (text: string) =
        match parse text with
        | CreateStatement x ->
            for (nm, ty) in x.Columns do
                addProperty t nm (translateType ty)
        | _ -> ()

    do
        logFile.AutoFlush <- true

        loadQueriesProvider.DefineStaticParameters(
            [ProvidedStaticParameter("Path", typeof<string>, ".")],
            fun typeName args ->
                let path = args.[0] :?> string
                let templateType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, IsErased = false)
                addEmptyConstructor templateType
                loadQueries templateType path
                tempAsm.AddTypes [templateType]
                templateType
        )

        parseQueryProvider.DefineStaticParameters(
            [ProvidedStaticParameter("Text", typeof<string>)],
            fun typeName args ->
                let text = args.[0] :?> string
                let templateType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, IsErased = false)
                addEmptyConstructor templateType
                parseQuery templateType text
                tempAsm.AddTypes [templateType]
                templateType
        )

        this.RegisterRuntimeAssemblyLocationAsProbingFolder config
        tempAsm.AddTypes [loadQueriesProvider; parseQueryProvider]
        this.AddNamespace(ns, [loadQueriesProvider; parseQueryProvider])
