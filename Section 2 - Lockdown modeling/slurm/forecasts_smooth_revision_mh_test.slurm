#!/bin/bash

#SBATCH --mail-type=END
#SBATCH --time=0-20:00
#SBATCH --job-name=wave2_test
#SBATCH --mem-per-cpu=8G
#SBATCH --array=1-6
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

export http_proxy=http://proxy.arc-ts.umich.edu:3128/
export FTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export ftp_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTPS_PROXY=http://proxy.arc-ts.umich.edu:3128/
export https_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export production=FALSE

module load Rgeospatial/3.6.1-2019-09-29
module load jags

Rscript --vanilla ~/projects/covind_wave2/forecasts_smooth_revision_mh.R
