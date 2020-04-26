ulimit -v 2441406

set -x
while true
do
    clear

    # stack build
    stack test

    pushd plot
    sh run.sh
    popd

    # stack exec webserv &
    # PID=$!
    # inotifywait -e modify -r lib webserv
    # kill $!

    inotifywait -e modify --fromfile watch
done;
