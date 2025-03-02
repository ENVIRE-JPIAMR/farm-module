# farm-module

This repository provides code and data to for the 
QMRA model for the farm module, intended to quantify
ESBL producing E. coli in broiler manure.

## Directory layout

There is one subdirectory in this repository:

* [`results`](./results/) provides a scripts and Rmds to visualize results on baseline/intervention scenarios. 
* [`docs`](./docs/) provides documentaion and  Rmds for the model. 
* [`utilities`](./docs/) provides scripts containing utility functions. 


The roles of the scripts in this repository:

* [`inputs.csv`](./inputs.csv): all input variables 
* [`farm_module.R`](./farm_module.R): all necessary functions corresponding different steps
* [`load_inputs.R`](./load_inputs.R): read input csv file
* [`load_libraries.R`](./load_libraries.R): load necessary R packages
* [`run_farm_module.R`](./run_farm_module.R): simulate one single batch of production
* [`run_farm_module_parallel.R`](./run_farm_module_parallel.R): simulate multiple batches in parallel
* [`visualization.R`](./visualization.R): generate plots/results

## Related resources

Bibliographic resources for this work can be found in this 
[repository](https://github.com/ENVIRE-JPIAMR/bibliography/tree/main/farm-module).

