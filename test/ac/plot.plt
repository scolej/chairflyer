set terminal pngcairo size 700,500 enhanced font 'Terminus,8'
set output figFile

set multiplot layout 4, 1
set grid

set xtics 300
unset key

set ylabel 'z (ft)'
set ytics 2000
plot datFile using 1:4 with lines

set ylabel 'vx (knots)'
set ytics 20
plot datFile using 1:5 with lines

set ylabel 'vy (fpm)'
set ytics 500
plot datFile using 1:6 with lines
unset ytics

set ylabel 'p (degrees)'
set ytics 5
plot datFile using 1:7 with lines, \
     datFile using 1:8 with lines
