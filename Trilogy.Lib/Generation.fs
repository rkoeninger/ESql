namespace Trilogy

open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

// https://github.com/fsprojects/SQLProvider/blob/5f6352e49fee5b743940100c6fb89bb70c62d127/src/SQLProvider/SqlDesignTime.fs
// https://github.com/mausch/XmlLiteralsTypeProvider/blob/master/XmlLiteralsTypeProvider/XmlLiterals.fs
// http://gettingsharper.de/2014/12/19/having-fun-with-type-level-numbers-using-a-type-provider/

[<TypeProvider>]
type QueryProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

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

    let loadQueries (t: ProvidedTypeDefinition) (path: string) =
        let name = "Thing"
        let tipe = typeof<string>
        let field = ProvidedField("_" + name, tipe)
        field.SetFieldAttributes(FieldAttributes.Private)

        let property = ProvidedProperty(name, tipe)
        property.GetterCode <- (fun _ -> <@@ %%Expr.Value(path) :> string @@>)
        //property.SetterCode <- (fun (this :: arg :: _) -> <@@ %%Expr.FieldSet(this, field, arg) |> ignore @@>)

        t.AddMember field
        t.AddMember property
        ()

    let parseQuery (t: ProvidedTypeDefinition) (text: string) =
        ()

    do
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
