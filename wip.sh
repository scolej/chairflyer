ulimit -v 2441406

set -x
while true
do
    clear

    # stack build
    # stack test

    if [[ $? -eq 0 ]]; then
        # stack exec webserv &
        # PID=$!

        pushd test/pidspring
        sh run.sh
        popd
    fi;

    inotifywait -e modify --fromfile watch
    # kill $PID
done;
