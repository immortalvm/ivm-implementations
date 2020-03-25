#!/bin/bash

# See https://stackoverflow.com/a/3355423
cd "$(dirname "$0")"

dotnet tool restore
dotnet paket restore

dotnet fake run build.fsx
