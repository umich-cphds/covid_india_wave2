# Lessons from SARS-CoV-2 in India: A data-driven framework for pandemic resilience


Maxwell Salvatore, Soumik Purkayastha, Lakshmi Ganapathi, Rupam Bhattacharyya, Ritoban Kundu, Lauren Zimmermann, Debashree Ray, Aditi Hazra, Michael Kleinsasser, Sunil Solomon, Ramnath Subbaraman, Bhramar Mukherjee 

## Materials

This repository contains scripts used to perform the analyses and generate the figures presented in the above-titled manuscript.

* The `figure/` folder contains subfolders corresponding to each figure. Each subfolder contains an R script that generates and saves the figure in its respective folder. These folders may also contain additional files/scripts that are needed to recreate the image. The scripts in these folder may also source scripts in the `model/` folder

* The `model/` folder contains the eSAIR model scripts (e.g.,, `tvt.wi.eSAIR.R`) that are used to generate the results. It also contains a toy example that runs the model script (`toy_run.R`). It also contains some data files that are used to specify conditions in the model, like the intervention effect schedules (`pi_schedule_extended.csv`).

* The `model/seir/` folder contain the scripts to run the modified SEIR model in our paper. The scripts with the prefix `Run_*` are scripts that run the model scripts. We also provide SEIRfansy scripts in `model/seirfansy_f=0`, which was modified for our SEIR model.

* The `model/seir_results/` folder contains the model results in `./results/` and the data for plotting in `./plot_data/`.

* The `tables/` folder contains subfolders for each table from which the model results were extracted from model results.

* The `old versions/` folder contains scripts from previous submissions and revisions.