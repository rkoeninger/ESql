call msbuild Trilogy.Tests\Trilogy.Tests.sln

call nuget install NUnit.ConsoleRunner -Version 3.7.0 -OutputDirectory packages

call .\packages\NUnit.ConsoleRunner.3.7.0\tools\nunit3-console.exe --noresult Trilogy.Tests\bin\Debug\Trilogy.Tests.dll

call msbuild Trilogy.Demo\Trilogy.Demo.sln

call .\Trilogy.Demo\bin\Debug\Trilogy.Demo.exe
