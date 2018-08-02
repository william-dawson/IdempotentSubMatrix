from scipy.io import mmread, mmwrite
from scipy.sparse import csr_matrix
from numpy.linalg import inv
from sys import argv
from yaml import load, dump

def make_matrices(mat_dim):
    '''Generate the random matrix
    '''
    from numpy.random import rand
    dmat = rand(mat_dim, mat_dim)/(mat_dim**2)
    dmat = dmat + dmat.T
    smat = rand(mat_dim, mat_dim)
    smat = smat.dot(smat.T)

    return smat, dmat

def compute(set, matrix):
    '''For a given set of rows and columns, compute the idempotency
    of a matrix.
    '''
    from numpy.linalg import norm
    set_0 = [x - 1 for x in set]
    s = matrix[:,set_0]
    s = s[set_0,:]

    s2 = s.dot(s)
    return norm(s2 -s, ord=1)

if __name__ == "__main__":
    mat_dim = int(argv[1])
    data_file = argv[2]
    out_file = argv[3]

    # Read Input
    smatrix, dmatrix = make_matrices(mat_dim)
    with open(data_file, 'r') as ifile:
        data = load(ifile)

    # Compute
    sinv = inv(smatrix)
    val_list = []
    for set in data["inputs"]:
        val_list.append(compute(set,sinv.dot(dmatrix)))

    # Write to file
    mmwrite("Tests/SMat.mtx", csr_matrix(smatrix))
    mmwrite("Tests/DMat.mtx", csr_matrix(dmatrix))

    val_dict = {"values":list([float(x) for x in val_list])}
    with open(out_file, "w") as ofile:
        dump(val_dict, ofile, default_flow_style=False)
