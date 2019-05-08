#!/usr/bin/env python
import uproot
import h5py
import numpy as np
from tempfile import mkdtemp
import os

import argparse

# if we want multiple custom formatters, use inheriting
class CustomFormatter(argparse.ArgumentDefaultsHelpFormatter):
    pass


parser = argparse.ArgumentParser(
    description='Convert MLB Lund NTuples to HDF5 files',
    formatter_class=lambda prog: CustomFormatter(prog, max_help_position=30),
)
parser.add_argument('fname', type=str, help='File to process')
parser.add_argument('--num-jets', type=int, help='Number of jets per event', default=2)
parser.add_argument(
    '--num-emissions', type=int, help='Number of emissions per jet', default=20
)
parser.add_argument(
    '--num-entries',
    type=int,
    help='Number of entries to process in file. -1 to process all.',
    default=-1,
)
parser.add_argument(
    '--treename',
    type=str,
    help='Name of tree to use in file',
    default='lundjets_InDetTrackParticles',
)
args = parser.parse_args()

branches = [
    b'mcEventWeight',
    b'NPV',
    b'averageInteractionsPerCrossing',
    b'nfatjet',
    b'fatjet_Ntrk500',
    b'fatjet_pt',
    b'fatjet_eta',
    b'fatjet_eta_detector',
    b'fatjet_phi',
    b'fatjet_CaloMass',
    b'fatjet_TrackAssistedMassCalibrated',
    b'fatjet_Split12',
    b'fatjet_Split23',
    b'fatjet_ECF1',
    b'fatjet_ECF2',
    b'fatjet_ECF3',
    b'fatjet_C2',
    b'fatjet_D2',
    b'fatjet_Tau1_wta',
    b'fatjet_Tau2_wta',
    b'fatjet_Tau3_wta',
    b'fatjet_Tau21_wta',
    b'fatjet_Tau32_wta',
    b'fatjet_KtDR',
    b'fatjet_Qw',
    b'fatjet_FoxWolfram20',
    b'fatjet_Angularity',
    b'fatjet_Aplanarity',
    b'fatjet_Charge',
    b'fatjet_dRmatched_maxEParton_flavor',
    b'fatjet_ghost_assc_flavor',
    b'fatjet_Lund1_Pt1',
    b'fatjet_Lund1_Pt2',
    b'fatjet_Lund1_Z',
    b'fatjet_Lund1_DeltaR',
    b'fatjet_Lund2_Pt1',
    b'fatjet_Lund2_Pt2',
    b'fatjet_Lund2_Z',
    b'fatjet_Lund2_DeltaR'
]

# open file and get the tree
f = uproot.open(args.fname)
tree = f[args.treename]

# figure out numvber of entries
num_entries = args.num_entries if args.num_entries > 0 else tree.numentries
maxshape = (num_entries, args.num_jets, args.num_emissions, len(branches))

# make memory-mapped file to hold large arrays
fname_mmap = os.path.join(mkdtemp(), 'mmap.dat')
fp_mmap = np.memmap(fname_mmap, dtype=float, mode='w+', shape=maxshape)
fp_mmap[:] = 1.0

itotal = 0
for start, stop, events in tree.iterate(
    branches, entrystop=num_entries, reportentries=True
):
    for branch in branches:
        try:
            fp_mmap[start:stop, :, :, branches.index(branch)] = np.einsum('ij,ij...->ij...', events[branch].pad(args.num_jets, clip=True).fillna(0).regular(), np.ones((stop-start, args.num_jets, args.num_emissions)))
        except AttributeError:
            for ievent, event in enumerate(events[branch]):
                for ijet, jet_emissions in enumerate(event):
                    if ijet >= args.num_jets: continue
                    # this occurs when dealing with doubly-nested arrays (jet_emissions properties)
                    num_emissions = min(len(jet_emissions), 20)
                    fp_mmap[itotal + ievent, ijet, :, branches.index(branch)] = 0
                    fp_mmap[itotal + ievent, ijet, :num_emissions, branches.index(branch)] = jet_emissions
    itotal += stop - start

outfilename = args.fname.replace('.root', '.h5')
print('Creating {}'.format(outfilename))
with h5py.File(outfilename, 'w') as h5f:
    h5f.create_dataset(args.treename, data=fp_mmap)
    h5f.create_dataset('branches', data=branches, dtype='S34')

del fp_mmap
