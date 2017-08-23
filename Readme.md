# GSL-Algae

## Background
Utah has experienced several major algal blooms in a number of lakes and reservoirs in recent years (especially in heavily recreated areas near urban centers of Salt Lake City - Provo), which have raised concerns over water quality conditions. Remote sensing provides a number of potential tools for better understanding and monitoring surface water quality conditions.

## Purpose
This repository is a collection of the scripts and functions used in Google Earth Engine and R for processing, model development, and analysis of algal blooms using remote sensing.

---
An example workflow is as follows:
![alt text](https://github.com/cahhansen/GSLAlgae/blob/master/WorkflowDiagram.png) 
### Download reflectance data:
* GEE_Scripts>GetSRDataLS5-7.py

This script reads in dates and station IDs from a field record file and Google Fusion Table of sampling locations. Can be adapted to retrieve data over the entire satellite record. Save data in .csv.

### R Package for formatting/modeling/analyzing data:
GSLAlgaeR

To install and use these functions:

```
library(devtools)
install_github("cahhansen/GSLAlgae/GSLAlgaeR")
```

