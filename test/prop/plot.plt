set terminal pngcairo size 300,600 enhanced font 'Terminus,8'
set output 'fig.png'

set multiplot layout 2, 1
set grid
unset key

set ylabel 'T'

set xlabel 'v'
plot "vel.dat" using 1:2 with lines

set xlabel 'rho'
plot "rhos.dat" using 1:2 with lines