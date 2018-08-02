# Idempotent Submatrix Checker

Given a set of indices, density matrix, and overlap matrix, this program will
check if the submatrix of S^-1*D defined by those indices is idempotent.

## Usage

Here is an example run command:

> mpiexec ./IdempotentSub --subset list_of_subsets.yaml \
  --density density_matrix.mtx --overlap overlap_matrix.mtx \
  --threshold 1e-10 --convergence_threshold 1e-5 \
  --output output_file.yaml

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
