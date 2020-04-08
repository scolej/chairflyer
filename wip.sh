set -x

while true
do
    stack build && stack runghc src/AcScratch.hs && gnuplot plots3.plt
    inotifywait -q -e modify -r src plots3.plt
done