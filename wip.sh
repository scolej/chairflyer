ulimit -v 2441406

set -x
while true
do
    clear

    stack build
    stack exec webserv &
    PID=$!

    inotifywait -e modify -r lib webserv
    # inotifywait -e modify --fromfile watch
    kill $!
done;
