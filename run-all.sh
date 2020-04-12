set -x
set -e

stack build

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