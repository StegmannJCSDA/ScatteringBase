#/bin/bash
#
#  run_calculations.sh
#
#  Description:
#  ============
#  Performs scattering calculations
#  and computes the bulk scattering
#  properties.
#
#  Copyright © 2020 Patrick Stegmann
#
#  This file is part of ScatteringBase
#
#
INTRO="The calculations are running."
echo $INTRO
workdir=$PWD

# speed of light in m/s for frequency conversion
sol=299792458

# CRTM MW frequencies in GHz
freqs=(1.4 6.8 6.92 10.7 18.7 19.35 21.3\
 22.235 23.8 31.4 36.5 37 50.3 52.8 53.5\
 54.4 54.94 55.5 57.29 59.4 60.67 63.28 70\
 75 85 89 91.65 150 157 183.31 190.31 220)

# effective diameter table in mum for bulk scattering quadrature
sizes=(5 15 30 50 100 300 500 800 1000 1500)

# load size grid from file
sz_grid=($(awk -F= '{print $1}' $workdir/grid.txt))

# Computation Loop
# Step 1: Create the output folders
# Step 2: Run the scattering calculations
for ii in "${freqs[@]}";
do
	wavlen= echo $sol/"$ii" | bc -l
	echo "Wavlength: " $wavlen
	for jj in "${sizes[@]}";
	do
		for kk in "${sz_grid[@]}";
		do
			sz= echo "$kk"*0.000001 | bc -l
			echo "Size: " $sz
			Ordner=$workdir/database/"$ii"/"$jj"/"$kk"/
			echo $Ordner
			mkdir -p $Ordner
			cd $Ordner
			sz= echo "$kk"*0.000001 | bc -l
			wavlen= echo $sol/"$ii" | bc -l
			echo "Input: " $sz $wavlen
			parameters=(1. 1.3 0.002)
			parameters[4]="$kk"
			parameters[5]="$ii"
			parameters[6]=1000
	        echo ${parameters[*]}
			$workdir/run-bhmie.x ${parameters[*]} > mie_output.txt
		done
		# Compute the bulk scattering properties:
		cd $workdir/database/"$ii"/"$jj"/
		ln -s $workdir/grid.txt 
		reff= echo "$jj"*0.000001 | bc -l
		echo "Reff " $reff
		$workdir/bulk.x "$jj"
		rm $workdir/database/"$ii"/"$jj"/grid.txt
		cd $workdir/
	done
done

