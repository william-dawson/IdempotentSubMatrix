################################################################################
include make.inc

################################################################################
EXE=IdempotentSub
BUILD=Build

ALL: $(BUILD)/$(EXE)

OBJECTS=$(BUILD)/main.o $(BUILD)/SetModule.o $(BUILD)/IdempotencyModule.o

$(BUILD)/$(EXE): $(OBJECTS)
	$(FC) $^ -o $@ $(LDPATH) $(LDFLAGS)

$(BUILD)/%.o: %.f90
	$(FC) -c $< -o $@ $(INCLUDES) -I$(BUILD) -J$(BUILD)

$(BUILD)/main.o: $(BUILD)/IdempotencyModule.o $(BUILD)/SetModule.o
$(BUILD)/IdempotencyModule.o: $(BUILD)/SetModule.o

################################################################################
clean:
	rm $(BUILD)/*.o $(BUILD)/$(EXE) $(BUILD)/*.mod

test:
	for proc in 1 2 ; do \
		$(PY) Tests/reference.py 8 Tests/input_mul.yaml Tests/values-python.txt; \
		mpirun -np $$proc $(BUILD)/$(EXE) --subset Tests/input_mul.yaml \
	  	--density Tests/DMat.mtx --overlap Tests/SMat.mtx \
			--threshold 1e-10 --convergence_threshold 1e-5 \
			--output Tests/values-fortran.txt; \
		$(PY) Tests/verify.py Tests/values-fortran.txt Tests/values-python.txt 1e-5; \
	done
