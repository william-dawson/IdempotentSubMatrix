# Idempotent Submatrix Checker

Given a set of indices, density matrix, and overlap matrix, this program will
check if the submatrix of S^-1*D defined by those indices is idempotent.

## Usage

Modify the the variables at the top of the `make.inc` file, and type make in the
base directory. The variable `${NTPolyPath}` may be specified as an
environment variable, and it should point to the Build directory of NTPoly.

Here is an example run command:

> mpiexec ./IdempotentSub --subset list_of_subsets.yaml \
  --density density_matrix.mtx --overlap overlap_matrix.mtx \
  --threshold 1e-10 --convergence_threshold 1e-5 \
  --output output_file.yaml

### Subset Format

The `threshold` value is used to remove small values from the product of
matrices. The `convergence_threshold` is the convergence criteria for
inverting the overlap matrix.

The `list_of_subsets.yaml` file has the following structure:

> inputs:
> - [ 1, 2 ]
> - [ 4, 7 ]
> - [8]

That is to say, a dictionary with one entry `inputs` which has a list
of values. Each list is a list of indices.

### Output Format

The output of a run `output_file.yaml` has the following format:

>values:
>- val1
>- val2
>- val3

That is, a dictionary with one entry `values`, which stores a list of
idempotency deviations for a given set of indices.
