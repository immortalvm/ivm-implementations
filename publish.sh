#!/bin/sh

# See https://stackoverflow.com/a/3355423
cd "$(dirname "$0")"

TAG=$(git describe --tags --abbrev=0)
echo "Publishing $TAG"

dotnet clean

# See https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-publish .
# I'm not sure if '-f netcoreapp2.1' is useful since this is also in the project files.

 pub() {
    dotnet publish Command -c release --self-contained -f netcoreapp2.1 -r $1
    pushd Command/bin/release/netcoreapp2.1/$1
    zip -r "$TAG"_$1.zip publish
    popd
}

pub osx-x64
pub linux-x64
pub win-x64
