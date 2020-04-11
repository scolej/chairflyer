set terminal pngcairo size 500,500 enhanced font 'Terminus,8'
set output 'fig.png'

set multiplot layout 2, 1
unset key
set grid
set key outside right center vertical

set xlabel 't'

set ylabel 'x'
plot "outRK4.dat" using 1:2 with lines title 'RK4', \
     "outEuler.dat" using 1:2 with lines title 'Euler'

set ylabel 'v'
plot "outRK4.dat" using 1:3 with lines title 'RK4', \
     "outEuler.dat" using 1:3 with lines title 'Euler'
