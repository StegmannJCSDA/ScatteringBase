#Scattering Base

####Dr. Patrick Stegmann, JCSDA CRTM team

##Requirements
To compile the package, gfortran is currently required. The recommended version is 9.3.0 .

##Setup

###Folder Structure
The package currently consists of 4 folders:
<ul>
<li>`SizeGrid`: the code to compute the quadrature points for the integration over the PSD of a single effective radius.</li>
<li>`bhmie`: Bohren and Huffman Lorenz-Mie code, modified for this project.</li>
<li>`bulk`: code to compute bulk scattering properties from the single-scattering data using the trapezoidal rule.</li>
<li>`scratch`: working directory where the database is created.</li>
</ul>

###Compiling Executables
To compile the executables you need to enter every folder of this project and compile them manually.
Enter each directory except `scratch` and simply type `make`.

###Linking Executables
To link the executables in the working directory, enter `scratch` and run the linking script:
```bash
cd scratch/
./link_executables.sh
```

##Running a calculation
To run the calculations, enter the working directory `scratch` and run the calculation script `run_calculations.sh`:
```bash
cd scratch/
./run_calculations.sh
```
This will create the `./database/` folder, which is a very large folder containing subdirectories for every frequency, effective radius and quadrature point specified in `run_calculations.sh`. It contains the bulk scattering properties for every frequency and effective radius at the corresponding folder level.

##License
For details on the license please see `LICENSE.txt`.
