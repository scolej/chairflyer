set terminal pngcairo size 500,500 enhanced font 'Verdana,10'
set output 'fig.png'

set multiplot layout 2, 1
unset key
set grid

set title "x"
plot "output.dat" using 1:2 with lines

set title "v"
plot "output.dat" using 1:3 with lines
