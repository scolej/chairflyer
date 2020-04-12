set terminal pngcairo size 700,500 enhanced font 'Terminus,8'
set output 'fig.png'

set multiplot layout 2, 1
set grid

set xlabel 't'

unset key
set ylabel 'z'
plot "out.dat" using 1:4 with lines

set key outside bottom center
set ylabel 'v'
plot "out.dat" using 1:5 with lines title 'vx', \
     "out.dat" using 1:6 with lines title 'vy'
