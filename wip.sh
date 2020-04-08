set -x

while true
do
    stack build && stack runhaskell src/SpringTest.hs && gnuplot plot-spring.plt
    inotifywait -e modify --exclude 'flycheck_.*' -r src plot-spring.plt
done