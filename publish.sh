#!/bin/bash

# See https://stackoverflow.com/a/3355423
cd "$(dirname "$0")"

TAG=$(git describe --tags --abbrev=0)
VERSION=$(echo $TAG | cut -dv -f2 -)
echo "Publishing $TAG"

perl -pi -e 's/<AssemblyVersion>\$\(VERSION\)/<AssemblyVersion>'"$VERSION/" -- Command/Command.fsproj

dotnet clean

# See https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-publish .

pub() {
    rm -rf Command/bin/release/netcoreapp3.0/$1/publish/*
    dotnet publish Command -c release --self-contained -r $1 /p:PublishSingleFile=true /p:PublishTrimmed=true
    pushd Command/bin/release/netcoreapp3.0/$1
    zip -r - publish > "$TAG"_$1.zip
    popd
}

pub osx-x64
pub linux-x64
pub win-x64

perl -pi -e 's/<AssemblyVersion>'"$VERSION"'/<AssemblyVersion>\$\(VERSION\)/' -- Command/Command.fsproj
