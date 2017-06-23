msbuild Trilogy.sln

nuget install NUnit.ConsoleRunner -Version 3.6.0 -OutputDirectory packages

.\packages\NUnit.ConsoleRunner.3.6.0\tools\nunit3-console.exe --noresult .\Trilogy.Tests\bin\Debug\Trilogy.Tests.dll
