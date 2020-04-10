set terminal pngcairo size 500,500 enhanced font 'Terminus,8'
set output 'tmp/fig.png'

set multiplot layout 2, 1
set grid

set xlabel 't'

unset key
set ylabel 'z'
plot "tmp/outputAc.dat" using 1:4 with lines

set key outside bottom center
set ylabel 'v'
plot "tmp/outputAc.dat" using 1:5 with lines title 'vx', \
     "tmp/outputAc.dat" using 1:6 with lines title 'vy'
