set terminal pngcairo size 700,500 enhanced font 'Terminus,8'
set output figFile

set multiplot layout 4, 1
set grid

set xtics 10
unset key

set ylabel 'z (ft)'
set ytics 2000
set yrange [0:10000]
plot datFile using 1:2 with lines

set ylabel 'vx (knots)'
set ytics 50
set yrange [0:150]
plot datFile using 1:3 with lines

set ylabel 'vy (fpm)'
set ytics 500
set yrange [-1000:1000]
plot datFile using 1:4 with lines
unset ytics

set ylabel 'p (degrees)'
set ytics 5
set yrange [-5:20]
plot datFile using 1:5 with lines, \
     datFile using 1:6 with lines
