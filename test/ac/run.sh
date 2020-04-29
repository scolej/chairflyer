stack runhaskell Main.hs

gnuplot -e "figFile='speedChanger.png'" -e "datFile='speedChanger.dat'" plot.plt
gnuplot -e "figFile='cutThrottle.png'" -e "datFile='cutThrottle.dat'" plot.plt
