plot.png : outRK4.dat outEuler.dat plot.plt
	gnuplot plot.plt

outRK4.dat outEuler.dat : Main.hs
	stack runhaskell Main.hs