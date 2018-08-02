FC=mpif90
FFLAGS=
LDPATH=-L/Users/dawson/Documents/NTPoly/Complex/Build/lib
LDFLAGS=-lNTPoly -fopenmp -llapack
INCLUDES=-I/Users/dawson/Documents/NTPoly/Complex/Build/include
EXE=IdempotentSub
BUILD=Build
PY=python3

ALL: $(BUILD)/$(EXE)

OBJECTS=$(BUILD)/SetModule.o $(BUILD)/IdempotencyModule.o $(BUILD)/main.o

$(BUILD)/$(EXE): $(OBJECTS)
	$(FC) $^ -o $@ $(LDPATH) $(LDFLAGS)

$(BUILD)/main.o: main.f90 $(BUILD)/SetModule.o $(BUILD)/IdempotencyModule.o
	$(FC) -c $< -o $@ $(INCLUDES) -I$(BUILD)

$(BUILD)/SetModule.o $(BUILD)/SetModule.mod: SetModule.f90
	$(FC) -c $< -o $@ $(INCLUDES) -J$(BUILD)

$(BUILD)/IdempotencyModule.o $(BUILD)/IdempotencyModule.mod: \
    IdempotencyModule.f90 $(BUILD)/SetModule.o
	$(FC) -c $< -o $@ $(INCLUDES) -J$(BUILD)

clean:
	rm $(BUILD)/*.o $(BUILD)/$(EXE)

test: test1 test2

test1:
	$(PY) Tests/reference.py 8 Tests/input_mul.yaml Tests/values-python.txt
	mpirun -np 1 $(BUILD)/$(EXE) --subset Tests/input_mul.yaml \
	  --density Tests/DMat.mtx --overlap Tests/SMat.mtx \
		--threshold 1e-10 --convergence_threshold 1e-5 \
		--output Tests/values-fortran.txt
	$(PY) Tests/verify.py Tests/values-fortran.txt Tests/values-python.txt 1e-5

test2:
	$(PY) Tests/reference.py 8 Tests/input_mul.yaml Tests/values-python.txt
	mpirun -np 2 $(BUILD)/$(EXE) --subset Tests/input_mul.yaml \
	  --density Tests/DMat.mtx --overlap Tests/SMat.mtx \
		--threshold 1e-10 --convergence_threshold 1e-5 \
		--output Tests/values-fortran.txt
	$(PY) Tests/verify.py Tests/values-fortran.txt Tests/values-python.txt 1e-5
