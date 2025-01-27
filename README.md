# Effects of Demand and Recycling on the When and Where of Lithium Extraction


Replication materials for Busch *et al.* (2024), submitted article.

The following code and data allows for the reproduction of all the tables, figures and calculations made in the article, both in the main body and supplementary information.

If you identify any error in the source code or have any further suggestions please contact Pablo Busch at pmbusch@ucdavis.edu.


# Organization

* **Data**: Data inputs used in the analysis. 
* **Figures**: PNG version of the figures of the article main body. 
* **Scripts**: All code to process the data, run models and create figures. Each script starts with a description of the file purpose. Through the file there are several explanatory  comments.  
* **Parameters**: Intermediate Results needed to run Optimization or re-create figures.
* **Results**: Aggregated results stored to recreate tables and figures.

# Instructions

The repository is ~294Mb unzipped. 

Users can run all the code for replication using the "Lithium-Supply.Rproj" file, or by setting their own working directory and running scripts independently.

This GitHub contains organization notes in each folder describing the, and each scripts is properly docummented.

Users can either used the uploaded model results to replicate figures, or run new instances to generate results for the demand or supply model. Please note that each optimization run takes around 5 to 60 seconds to run. Some results are uploaded in the GitHub in a compressed format, so please make sure to unzip them.

# Software required

The script code was developed with **R** software version 4.4.1. 

The optimization code was developed with **julia** version 1.11.0, and using Gurobi as solver. Instruction to get a license from Gurobi can be found ![here](https://www.gurobi.com/solutions/licensing/) Alternatively, the optimization scripts can be adapted to run with another standard optimization solver.

The R code requires the following packages: *tidyverse*, *readr*,*readxl*,*ggplot2*,*data.table*,*dplyr*,*gridExtra*,*reshape2*,*scales*,*RColorBrewer*,*sf*,*ggrepel*. All libraries can be installed with the following command: 
```
install.packages(c("tidyverse","readr","readxl","ggplot2","data.table","dplyr","gridExtra","reshape2","scales","RColorBrewer","sf","ggrepel"), dependencies = T)
```
The julia code requires the following packages: *CSV*,*DataFrames*,*JuMP*,*Gurobi*,*LinearAlgebra*. The following command can install these dependencies:
```
using Pkg
Pkg.add("CSV")
Pkg.add("DataFrames")
Pkg.add("JuMP")
Pkg.add("Gurobi")
Pkg.add("LinearAlgebra")
```

The model has only been tested using OS Windows 10 and 11, but it should work on Mac and Linux as well using **R** and **julia**

# License
This project is covered under the **MIT License**