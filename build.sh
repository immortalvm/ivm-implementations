#!/usr/bin/env bash

set -eu
set -o pipefail

# See https://stackoverflow.com/a/3355423
cd "$(dirname "$0")"

dotnet tool restore
dotnet paket restore

# See https://github.com/TheAngryByrd/MiniScaffold/blob/master/build.sh
FAKE_DETAILED_ERRORS=true dotnet run --project ./build/build.fsproj -- -t "$@"
