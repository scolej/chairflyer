set -e
set -x

# stack build chairflyer
# stack build chairflyer:lib
stack build chairflyer:exe:webserv
# stack build chairflyer:test:tests

stack exec webserv

# pushd test/prop
# sh run.sh
# popd

# pushd test/ac
# sh run.sh
# popd

# pushd handy
# stack runhaskell TachoTicks.hs
# popd
