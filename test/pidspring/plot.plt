set terminal pngcairo size 500,500 enhanced font 'Terminus,8'
set output 'fig.png'

set multiplot layout 3, 1
unset key
set grid

set xlabel 't'

set ylabel 'y'
plot "out.dat" using 1:2 with lines

set ylabel 'f'
plot "out.dat" using 1:3 with lines

set ylabel 'y'
set xrange [0:10]
plot "out.dat" using 1:2 with lines
