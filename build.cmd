dotnet restore
dotnet build --no-restore
dotnet test --no-build --verbosity normal
dotnet pack -c Release src\Garnet\Garnet.fsproj