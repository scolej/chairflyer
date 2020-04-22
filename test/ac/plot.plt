set terminal pngcairo size 700,500 enhanced font 'Terminus,8'
set output 'fig.png'

set multiplot layout 4, 1
set grid

set xtics 300
unset key

set ylabel 'z (ft)'
set ytics 2000
plot "out.dat" using 1:4 with lines

set ylabel 'vx (knots)'
set ytics 20
plot "out.dat" using 1:5 with lines

set ylabel 'vy (fpm)'
set ytics 500
plot "out.dat" using 1:6 with lines
unset ytics

set ylabel 'p (degrees)'
set ytics 5
plot "out.dat" using 1:7 with lines, \
     "out.dat" using 1:8 with lines
