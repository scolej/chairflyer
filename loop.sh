ulimit -v 2441406
while true
do
    sh wip.sh
    inotifywait -e modify --fromfile watch
done;
