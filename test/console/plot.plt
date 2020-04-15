# set terminal pngcairo size 700,700 enhanced font 'Terminus,8'
# set terminal x11 size 700,700 enhanced font 'Terminus,8'
# set output 'cockpit.png'

set multiplot layout 1, 3
set grid
unset key

set xlabel 'x'
set ylabel 'y'
set xtics 1
set ytics 1
set xrange [-3:15]
set yrange [-9:9]
set size square
plot "output" using 3:4 with lines

set xtics 10
set xlabel 't'
set xrange [0:30]

set ylabel 'altitude'
set yrange [0:5000]
set ytics 1000
plot "< tail -n 30 output" using 5 with lines

set xlabel 't'
set ylabel 'airspeed (knots)'
set yrange [0:200]
set ytics 20
plot "< tail -n 30 output" using 7 with lines

pause 1
reread