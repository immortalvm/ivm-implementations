#!/bin/sh

# See https://stackoverflow.com/a/3355423
cd "$(dirname "$0")"

dotnet clean

# See https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-publish .
# Not sure if '-f netcoreapp2.1' is useful since this is also in the project files.
dotnet publish Command -c release --self-contained -f netcoreapp2.1 -r osx-x64
dotnet publish Command -c release --self-contained -f netcoreapp2.1 -r win-x64
dotnet publish Command -c release --self-contained -f netcoreapp2.1 -r linux-x64
