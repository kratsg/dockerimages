#!/usr/bin/sh

# NB: cpus-per-task = 2*floor(128/ntasks-per-node)

#SBATCH --account=m2616
#SBATCH --qos=regular
#SBATCH --constraint=cpu
#SBATCH --job-name=mcfm-nnlo
#SBATCH --nodes=128
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=256
#SBATCH --mail-user=gistark@ucsc.edu
#SBATCH --mail-type=ALL
#SBATCH -t 24:00:00

export OMP_STACKSIZE=512000
export OMP_NUM_THREADS=256
export OMP_PLACES=threads
export OMP_PROC_BIND=spread

# see: https://stackoverflow.com/a/53759961
srun --mpi=cray_shasta --cpu_bind=cores --job-name='collinear W+' ./mcfm CollinearW.ini -general%part=nnlocoeff -general%rundir=collinearW_nnlocoeff -general%nproc=11 -general%runstring=Wp_collinear
srun --mpi=cray_shasta --cpu_bind=cores --job-name='collinear W-' ./mcfm CollinearW.ini -general%part=nnlocoeff -general%rundir=collinearW_nnlocoeff -general%nproc=16 -general%runstring=Wm_collinear
srun --mpi=cray_shasta --cpu_bind=cores --job-name='inclusive W+' ./mcfm CollinearW.ini -general%part=nnlocoeff -general%rundir=collinearW_nnlocoeff -general%nproc=11 -general%runstring=Wp_inclusive
srun --mpi=cray_shasta --cpu_bind=cores --job-name='inclusive W-' ./mcfm CollinearW.ini -general%part=nnlocoeff -general%rundir=collinearW_nnlocoeff -general%nproc=16 -general%runstring=Wm_inclusive
srun --mpi=cray_shasta --cpu_bind=cores --job-name='inclusi2j W+' ./mcfm CollinearW.ini -general%part=nnlocoeff -general%rundir=collinearW_nnlocoeff -general%nproc=11 -general%runstring=Wp_inclusive2j
srun --mpi=cray_shasta --cpu_bind=cores --job-name='inclusi2j W-' ./mcfm CollinearW.ini -general%part=nnlocoeff -general%rundir=collinearW_nnlocoeff -general%nproc=16 -general%runstring=Wm_inclusive2j
srun --mpi=cray_shasta --cpu_bind=cores --job-name='back2back W+' ./mcfm CollinearW.ini -general%part=nnlocoeff -general%rundir=collinearW_nnlocoeff -general%nproc=11 -general%runstring=Wp_backtoback
srun --mpi=cray_shasta --cpu_bind=cores --job-name='back2back W-' ./mcfm CollinearW.ini -general%part=nnlocoeff -general%rundir=collinearW_nnlocoeff -general%nproc=16 -general%runstring=Wm_backtoback
