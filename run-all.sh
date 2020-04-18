set -x
set -e

stack build --ghc-options='+RTS -M2.5G -RTS'

pushd test/spring
sh run.sh
popd

pushd test/ac
sh run.sh
popd

pushd test/atmosphere
sh run.sh
popd

pushd test/prop
sh run.sh
popd
