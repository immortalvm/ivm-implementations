#!/bin/sh

# See https://stackoverflow.com/a/3355423
cd "$(dirname "$0")"

TAG=$(git describe --tags --abbrev=0)
echo "Publishing $TAG"

dotnet clean

# See https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-publish .

 pub() {
    dotnet publish Command -c release --self-contained -r $1 /p:PublishSingleFile=true /p:PublishTrimmed=true
    pushd Command/bin/release/netcoreapp3.0/$1
    zip -r "$TAG"_$1.zip publish
    popd
}

pub osx-x64
pub linux-x64
pub win-x64
