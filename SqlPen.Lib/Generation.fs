namespace Generation

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open ProviderImplementation.ProvidedTypes

module Meta =
    [<assembly:TypeProviderAssembly>]
    do ()

[<TypeProvider>]
type QueryProvider(cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    //do this.AddNamespace(namespaceName, types)
