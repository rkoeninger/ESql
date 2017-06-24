call msbuild Trilogy.sln

call nuget install NUnit.ConsoleRunner -Version 3.6.0 -OutputDirectory packages

call .\packages\NUnit.ConsoleRunner.3.6.0\tools\nunit3-console.exe --noresult .\Trilogy.Tests\bin\Debug\Trilogy.Tests.dll
