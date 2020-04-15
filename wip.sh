set -x
while true
do
    clear

    stack build

    pushd test/console
    # stack ghc Main.hs
    stack runhaskell Main.hs &
    # hlint Main.hs
    PID=$!
    popd

    inotifywait -e modify --fromfile watch
    kill $!
done;