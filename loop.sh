ulimit -v 2441406
while true
do
    sh wip.sh &
    PID=$!
    inotifywait -e modify --fromfile watch
    kill -9 $PID
done;
