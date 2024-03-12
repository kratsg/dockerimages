for i in {1..6}; do
  sbatch "job${i}_nlo.sh"
done
