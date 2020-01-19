set terminal pngcairo size 500,500 enhanced font 'Verdana,10'
set output 'fig.png'

set multiplot layout 2, 1
unset key

set title "y"
plot "output.dat" using 2 with lines

set title "vy"
plot "output.dat" using 4 with lines
