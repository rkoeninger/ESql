namespace Trilogy.AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.Core.CompilerServices

[<assembly: AssemblyTitle "Trilogy.Lib">]
[<assembly: AssemblyDescription "">]
[<assembly: AssemblyCompany "Robert Koeninger">]
[<assembly: AssemblyProduct "Trilogy.Lib">]
[<assembly: AssemblyCopyright "Copyright © Robert Koeninger 2016-2017">]
[<assembly: AssemblyVersion "1.0.0.0">]
[<assembly: AssemblyFileVersion "1.0.0.0">]
[<assembly: ComVisible false>]
[<assembly: TypeProviderAssembly>]

#if DEBUG
[<assembly: AssemblyConfiguration "Debug">]
#else
[<assembly: AssemblyConfiguration "Release">]
#endif

do ()
