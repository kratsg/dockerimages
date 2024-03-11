#!/usr/bin/sh

#SBATCH --account=m2616
#SBATCH --qos=regular
#SBATCH --constraint=cpu
#SBATCH --job-name=mcfm-collinearw
#SBATCH --nodes=4
#SBATCH --ntasks=6
#SBATCH --cpus-per-task=32
#SBATCH --mail-user=gistark@ucsc.edu
#SBATCH --mail-type=ALL
#SBATCH -t 00:30:00

export OMP_STACKSIZE=512000
export OMP_NUM_THREADS=64
export OMP_PLACES=threads
export OMP_PROC_BIND=spread

# see: https://stackoverflow.com/a/53759961
srun --mpi=cray_shasta --nodes=1 --ntasks=1 --cpu_bind=cores --job-name='collinear' ./mcfm CollinearW.ini -general%part=snlo -general%nproc=11 -general%runstring=Wp_collinear &
srun --mpi=cray_shasta --nodes=1 --ntasks=1 --cpu_bind=cores --job-name='collinear' ./mcfm CollinearW.ini -general%part=snlo -general%nproc=16 -general%runstring=Wm_collinear &
srun --mpi=cray_shasta --nodes=1 --ntasks=1 --cpu_bind=cores --job-name='inclusive' ./mcfm CollinearW.ini -general%part=snlo -general%nproc=11 -general%runstring=Wp_inclusive &
srun --mpi=cray_shasta --nodes=1 --ntasks=1 --cpu_bind=cores --job-name='inclusive' ./mcfm CollinearW.ini -general%part=snlo -general%nproc=16 -general%runstring=Wm_inclusive &
srun --mpi=cray_shasta --nodes=1 --ntasks=1 --cpu_bind=cores --job-name='inclusi2j' ./mcfm CollinearW.ini -general%part=snlo -general%nproc=11 -general%runstring=Wp_inclusive2j &
srun --mpi=cray_shasta --nodes=1 --ntasks=1 --cpu_bind=cores --job-name='inclusi2j' ./mcfm CollinearW.ini -general%part=snlo -general%nproc=16 -general%runstring=Wm_inclusive2j &
wait
