set -e
set -x

stack build

# Directories waiting to be incorporated into the Stack build.
HACKS=
HACKS="$HACKS test/ac"
HACKS="$HACKS test/ld"
HACKS="$HACKS test/pidspring"
HACKS="$HACKS test/prop"
HACKS="$HACKS test/spring"

for d in $HACKS; do
    pushd $d
    sh run.sh
    popd
done
