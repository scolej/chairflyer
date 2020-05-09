set terminal pngcairo size 300,600 enhanced font 'Terminus,8'
set output 'fig.png'

set multiplot layout 3, 1
set grid
unset key

set ylabel 'Kc'
set xlabel 'J'
set xrange [0:2.8]
plot "kcs.dat" using 1:2 with lines

set ylabel 'T'
unset xrange
unset yrange

set xlabel 'v'
plot "vel.dat" using 1:2 with lines

set xlabel 'rho'
plot "rhos.dat" using 1:2 with lines
