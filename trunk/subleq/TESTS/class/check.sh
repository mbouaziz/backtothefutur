#!/bin/sh
echo Check diff for subleq

for num in `seq 1 30`
  do
    if [ $num -lt 10 ]
    then
	echo diff imgSim0"$num" im0"$num"
	diff imgSim0"$num" im0"$num"	
    else
	echo diff imgSim"$num" im"$num"
	diff imgSim"$num" im"$num"
    fi
done