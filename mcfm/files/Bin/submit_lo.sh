for i in {1..8}; do
  sbatch "job${i}_lo.sh"
done
