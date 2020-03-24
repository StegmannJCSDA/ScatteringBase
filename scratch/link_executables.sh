#/bin/bash
echo "Linking executables."
ln -s ../bhmie/run-bhmie.x # performs the Mie calculations
ln -s ../SizeGrid/grid.x   # Computes the input radii for the Mie calculations
ln -s ../bulk/bulk.x	   # Computes the bulk scattering properties
echo "Done linking executables."
