# Modify
FC=mpif90
PY=python3
LDFLAGS=-lNTPoly -fopenmp -llapack
FFLAGS=
# You can specify NTPolyPath as an environment variable
LDPATH=-L${NTPolyPath}/lib
INCLUDES=-I${NTPolyPath}/include

################################################################################
EXE=IdempotentSub
BUILD=Build

ALL: $(BUILD)/$(EXE)

OBJECTS=$(BUILD)/main.o $(BUILD)/SetModule.o $(BUILD)/IdempotencyModule.o

$(BUILD)/$(EXE): $(OBJECTS)
	$(FC) $^ -o $@ $(LDPATH) $(LDFLAGS)

$(BUILD)/%.o: %.f90
	$(FC) -c $< -o $@ $(INCLUDES) -I$(BUILD)

$(BUILD)/main.o: $(BUILD)/IdempotencyModule.o $(BUILD)/SetModule.o 
$(BUILD)/IdempotencyModule.o: $(BUILD)/SetModule.o
#
# $(BUILD)/main.o: main.f90 $(BUILD)/SetModule.o $(BUILD)/IdempotencyModule.o
# 	$(FC) -c $< -o $@ $(INCLUDES) -I$(BUILD)
#
# $(BUILD)/SetModule.o $(BUILD)/SetModule.mod: SetModule.f90
# 	$(FC) -c $< -o $@ $(INCLUDES) -J$(BUILD)
#
# $(BUILD)/IdempotencyModule.o $(BUILD)/IdempotencyModule.mod: \
#     IdempotencyModule.f90 $(BUILD)/SetModule.o
# 	$(FC) -c $< -o $@ $(INCLUDES) -J$(BUILD)

################################################################################
clean:
	rm $(BUILD)/*.o $(BUILD)/$(EXE) $(BUILD)/*.mod

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
