#!/bin/bash
set -e
set -u
set -o pipefail

# Usage: ./prof.sh noExtFilename

filenameNoExt="${1}"
filename="${filenameNoExt}.hs"
utilsFilename="Utils.hs"

profilingDir="profiling/${filenameNoExt}"
mkdir -p "${profilingDir}"
cp -f "${filename}" "${profilingDir}"
cp -f "${utilsFilename}" "${profilingDir}"
pushd "${profilingDir}"

ghc -prof -fprof-auto "${filename}" &&\
cp "${filenameNoExt}".prof "${filenameNoExt}.prof.backup.$(date)" || true &&\
rm "${filenameNoExt}".prof || true &&\
echo "Running ${filenameNoExt} ..." &&\
chmod +x "${filenameNoExt}" && ./"${filenameNoExt}" +RTS -p

popd