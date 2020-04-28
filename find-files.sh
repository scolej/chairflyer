find lib test webserv \
     -iname \*.hs \
     -or -iname \*.plt \
     -or -iname \*.sh \
     > watch

echo chairflyer.cabal >> watch
echo wip.sh >> watch
