set terminal pngcairo size 500,500 enhanced font 'Verdana,10'
set output 'fig.png'

set multiplot layout 4, 1
unset key
set grid

set title "x-y"
plot "output.dat" using 2:3 with lines

set title "t-z"
plot "output.dat" using 1:4 with lines

set title "t-vx"
plot "output.dat" using 1:5 with lines

set title "t-vz"
plot "output.dat" using 1:6 with lines
