set terminal pngcairo size 500,1600 enhanced font 'Verdana,10'
set output 'fig.png'

set multiplot layout 7, 1
unset key

set title "z"
plot "output.dat" using 3 with lines

set title "vx"
plot "output.dat" using 4 with lines

set title "vz"
plot "output.dat" using 5 with lines

set title "alpha"
plot "output.dat" using 7 with lines

set title "pitch"
plot "output.dat" using 8 with lines

set title "cl"
plot "output.dat" using 9 with lines

set title "cd"
plot "output.dat" using 10 with lines