set terminal pngcairo size 700,500 enhanced font 'Terminus,8'
set output 'fig.png'

set multiplot layout 1, 1
set grid

set ylabel 'z (ft)'
set xrange [-12:12]
set yrange [-1.1:1.1]
plot 'out.dat' using 1:2 with lines
