set terminal pngcairo size 700,500 enhanced font 'Terminus,8'
set output 'fig.png'

set grid

unset key

plot "out.dat" using 1:2 with lines, \
     "out.dat" using 1:3 with lines
