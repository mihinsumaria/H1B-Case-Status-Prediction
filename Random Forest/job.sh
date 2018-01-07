#!/bin/bash
#SBATCH -N 2
#SBATCH -n 4
#SBATCH -p short
#SBATCH --output=output.out
#SBATCH --error=error.err
#SBATCH -t 01:00:00
#SBATCH --mem 64G
R --no-save < randfor_h1b.R
