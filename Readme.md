# RSAlgae

## Background
Remote sensing provides a number of potential tools for better understanding and monitoring surface water quality conditions. Water bodies around the world have been affected by algal blooms, resulting in many problems, including habitat degredation, public health concerns, limited recreation, increases in treatment costs. The tools created in this respository were developed to address remote sensing needs for lakes in the Great Salt Lake surface water system. Utah has experienced several major algal blooms in a number of lakes and reservoirs in recent years (especially in heavily recreated areas near urban centers of Salt Lake City - Provo), which have raised concerns over water quality conditions.

## Purpose
This repository is a collection of the scripts and functions used in Google Earth Engine and R for processing, model development, and analysis of algal blooms using an open source, data-driven approach to remote sensing and modeling.

---
An example workflow is as follows:
![alt text](https://github.com/cahhansen/GSLAlgae/blob/master/WorkflowDiagram.png) 
### Use Python scripts to download reflectance data:
* Earth_Engine_Scripts>SampleExport.py

This script uses the function getSRDataLS57 (defined in EEExport.py) that reads in dates and station IDs from csv file of field/sample data and Google Fusion Table of sampling locations. Saves reflectance data in .csv.

### Use R Package for formatting/modeling/analyzing data:
GSLAlgaeR

To install and use these functions:

```
library(devtools)
install_github("cahhansen/GSLAlgae/GSLAlgaeR")
```

