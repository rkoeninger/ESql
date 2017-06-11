namespace Generation

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections

[<TypeProvider>]
type QueryProvider(file: string) as this =
    //inherit TypeProviderForNamespaces()

    //do this.AddNamespace(namespaceName, types)

    [<assembly:TypeProviderAssembly>]
    do ()
