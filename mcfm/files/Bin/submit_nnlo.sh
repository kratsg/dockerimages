for i in {1..8}; do
  sbatch "job${i}_nnlo.sh"
  sbatch "job${i}_nnlo_scalevar.sh"
done
