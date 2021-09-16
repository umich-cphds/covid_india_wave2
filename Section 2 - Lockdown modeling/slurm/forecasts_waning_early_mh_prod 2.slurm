#!/bin/bash

#SBATCH --mail-type=END
#SBATCH --time=0-20:00:00
#SBATCH --job-name=early_mh_prod
#SBATCH --mem-per-cpu=40G
#SBATCH --array=1-2
#SBATCH --output=/home/%u/slurm_output/slurm-%A_%a.out
#SBATCH --account=covid19_project1
#SBATCH --partition=standard

export http_proxy=http://proxy.arc-ts.umich.edu:3128/
export FTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export ftp_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTPS_PROXY=http://proxy.arc-ts.umich.edu:3128/
export https_proxy=http://proxy.arc-ts.umich.edu:3128/
export HTTP_PROXY=http://proxy.arc-ts.umich.edu:3128/
export production=TRUE

module load Rgeospatial/3.6.1-2019-09-29
module load jags

Rscript --vanilla ~/projects/science_revision/r/models/early_intervention/forecasts_waning_early_mh.R
