set -x
while true
do
    stack build

    pushd test/ac
    sh run.sh
    popd

    inotifywait -e modify --fromfile watch
done;