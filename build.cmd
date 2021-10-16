dotnet restore
dotnet build --no-restore
dotnet test --no-build --verbosity normal
dotnet pack -c Release -o publish src\Garnet\Garnet.fsproj
dotnet pack -c Release -o publish samples\Garnet.Numerics\Garnet.Numerics.fsproj
dotnet pack -c Release -o publish samples\Garnet.Toolkit\Garnet.Toolkit.fsproj
dotnet pack -c Release -o publish samples\Garnet.Processor\Garnet.Processor.fsproj