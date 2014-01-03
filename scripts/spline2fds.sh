#!/bin/bash

fds_file_name="../fds_files/test.fds"
fds_chid="test"
min_x=0.0
max_x=160.0
min_y=0.0
max_y=266.
min_z=0.0
max_z=5.0

div_x=160
div_y=266
div_z=10


# for stp in $(ls ../stepfiles/*.{stp,step} 2> /dev/null); do
#     bsname=$(basename $stp)
#     icomfile1=$(echo ../icom/${bsname%.*}.bspinfo)
#     icomfile2=$(echo ../icom/${bsname%.*}.bsp)
#     cat $stp | tr -d '\n' | sed 's/;/;\n/g' | ./read_step.py > $icomfile1
#     echo $icomfile1 | ../bin/splines > $icomfile2
# done


# dx=$(echo $min_x $max_x $div_x | awk '{print .7*($2-$1)/$3}')
# dy=$(echo $min_y $max_y $div_y | awk '{print .7*($2-$1)/$3}')


# x_half=$(echo $min_x $max_x | awk '{print ($2-$1)/2}')
# y_half=$(echo $min_y $max_y | awk '{print ($2-$1)/2}')


input_splines=""
for bsp in $(ls ../icom/*.bsp); do
    cat $bsp | awk -F, -v dx=$dx -v dy=$dy '{if(sqrt(($1-prevx)*($1-prevx))>dx || sqrt(($2-prevy)*($2-prevy))>dy)  {print $1", "$2;  prevx=$1 ; prevy=$2}}' > $bsp"short"
    input_splines=$input_splines" "$(echo $bsp"short")
done

./write_circum_fds.py  $min_x $max_x $min_y $max_y $min_z $max_z $div_x $div_y $div_z $fds_chid $input_splines > $fds_file_name