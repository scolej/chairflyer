ulimit -v 2441406

# set -x
while true
do
    echo
    echo
    echo
    echo
    echo
    echo

    stack build chairflyer:exe:pidspring

    # stack test
    # stack ghc lib/Controller.hs

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
