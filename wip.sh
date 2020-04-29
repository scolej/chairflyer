ulimit -v 2441406

set -x
while true
do
    clear

    # stack build
    stack test

    if [[ $? -eq 0 ]]; then
        stack exec webserv &
        PID=$!
    fi;

    # pushd test/ac
    # sh run.sh
    # popd

    inotifywait -e modify --fromfile watch
    kill $PID
done;
