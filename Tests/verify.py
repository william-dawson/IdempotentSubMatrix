'''Check that the two value dictionaries match.
'''
from sys import argv
from yaml import load
from numpy import array
from numpy.linalg import norm

if __name__ == "__main__":
    ffile = argv[1]
    pfile = argv[2]
    threshold = float(argv[3])

    with open(ffile, "r") as ifile:
        fval = load(ifile)
    fval = array(fval["values"])

    with open(pfile, "r") as ifile:
        pval = load(ifile)
    pval = array(pval["values"])

    print(norm(pval - fval), threshold)
    assert norm(pval - fval) < threshold
