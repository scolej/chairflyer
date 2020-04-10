set terminal pngcairo size 1200,400 enhanced font 'Terminus,8'
set output 'tmp/fig.png'

set multiplot layout 1, 3
set grid
unset key

set ylabel 'h'

set xtics 10000
set xlabel 'pressure'
plot "tmp/atmosphere.dat" using 2:1 with lines

set xtics 10
set xlabel 'temperature'
plot "tmp/atmosphere.dat" using 3:1 with lines

set xtics 0.1
set xlabel 'density'
plot "tmp/atmosphere.dat" using 4:1 with lines
