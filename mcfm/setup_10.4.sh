module load python/3.11

rsync -ravz files/Cuts/ MCFM-10.4-pre/src/Cuts/
rsync -ravz files/User/ MCFM-10.4-pre/src/User/
rsync -ravz files/Bin/ MCFM-10.4-pre/Bin/
cd MCFM-10.4-pre/Bin

# to configure for compile
# cmake -S .. -B . -Duseinternal_lhapdf=OFF -Dlhapdf_include_path=$PWD/../../lhapdf/include -Duse_mpi=ON -DCMAKE_C_COMPILER=mpicc -DCMAKE_CXX_COMPILER=mpic++ -DCMAKE_Fortran_COMPILER=mpifort -Dwith_vvamp=OFF