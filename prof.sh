#!/bin/bash

filenameNoExt="${1}"
filename="${filenameNoExt}.hs"
utilsFilename="Utils.hs"

profilingDir="profiling/${filenameNoExt}"
mkdir -p "${profilingDir}"
cp -f "${filename}" "${profilingDir}"
cp -f "${utilsFilename}" "${profilingDir}"
pushd "${profilingDir}"

ghc -prof -fprof-auto "${filename}" &&\
echo "Finished compiling ${filenameNoExt}" &&\
rm "${filenameNoExt}".prof || true &&\
chmod +x "${filenameNoExt}" && ./"${filenameNoExt}" +RTS -p

popd