# Modify these variables according to your system.
FC=mpif90
PY=python3
LDFLAGS=-lNTPoly -fopenmp -llapack
FFLAGS=

# You can specify NTPolyPath as an environment variable
LDPATH=-L${NTPolyPath}/lib
INCLUDES=-I${NTPolyPath}/include
