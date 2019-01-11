# RSAlgae
https://doi.org/10.5281/zenodo.2538202

## Background
Remote sensing provides a number of potential tools for better understanding and monitoring surface water quality conditions. Water bodies around the world have been affected by algal blooms, resulting in many problems, including habitat degredation, public health concerns, limited recreation, increases in treatment costs. The tools created in this respository were developed to address remote sensing needs for lakes in the Great Salt Lake surface water system. Utah has experienced several major algal blooms in a number of lakes and reservoirs in recent years (especially in heavily recreated areas near urban centers of Salt Lake City - Provo), which have raised concerns over water quality conditions.

## Purpose
This repository is a collection of the scripts and functions used with Google Earth Engine and an R package called RSAlgaeR for processing, model development, and analysis of algal blooms using an open source, data-driven approach to remote sensing and modeling. Many of the scripts and functions could easily be adapted for other remote sensing and water quality related purposes.

---
An example workflow is as follows:
### Use Python scripts to download reflectance data:
* Earth_Engine_Scripts>SampleExport.py

This script uses the function getSRDataLS57 (defined in EEExport.py) that reads in dates and station IDs from csv file of field/sample data and Google Fusion Table of sampling locations. Saves reflectance data in .csv.

### Use RSAlgaeR Package for formatting/modeling/analyzing data:
To install and use the functions in this package:

```
library(devtools)
install_github("cahhansen/RSAlgae/RSAlgaeR")
```

#### 1. Format data. 
This includes formatting dates, removing negative values, cloud pixels, etc. 
(See ```sampleformatreflectancedata.R``` script for example which uses ```formatSRdata``` function)
#### 2. Create model variables.
Use the ```create.model.vars``` function
#### 3. Parameterize model.
(Optional)
Use the ```step.model``` function (stepwise regression based on a user-specified timewindow) to explore performance for various parameters, definitions of near coincident data and seasons.
Examine model performance using k-fold cross validation and exploring the goodness of fit (```cv.model``` and ```modresults``` functions)

Use glm() to develop the final model, based on an appropriate season, timewindow, and parameters. 
#### 4. Apply model.
Apply the model to remotely sensed imagery for a user-specified season using ```apply.mod.seasonal``` 
#### 5. Plot estimates.
* With error bars: ```plotrecord.errors```
* With data used in calibration: ```plotrecord.cal```
* With field-sampled data: ```plotrecord```
#### 6. Explore trends.
Long term, linear changes in values/year can be explored using the Theil-Sen Estimator, which is more robust than a simple OLS regression.
* Annually: ```annualtrend.ts```
* Monthly: ```monthlytrend.ts```
#### 7. Explore connections to local climate conditions.
* Overall, monthly, or by location: ```climate.factor.effect```

This process is summarized in the following graphic:

![alt text](https://github.com/cahhansen/GSLAlgae/blob/master/WorkflowDiagram.png) 
