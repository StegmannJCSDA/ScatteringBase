#!/bin/bash

#read -a sizes < ./grid.txt

sizes=($(awk -F= '{print $1}' ./grid.txt))

echo ${sizes[1]}

for n in "${sizes[@]}"; 
do
	echo $n
done

exit 0
