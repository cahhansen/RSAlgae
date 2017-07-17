# GSL-Algae

## Background
Utah has experienced several major algal blooms in a number of lakes and reservoirs in recent years (especially in heavily recreated areas near urban centers of Salt Lake City - Provo), which have raised concerns over water quality conditions. Remote sensing provides a number of potential tools for better understanding and monitoring surface water quality conditions.

## Purpose
This repository is a collection of the scripts and functions used in Google Earth Engine and R for processing, model development, and analysis of algal blooms using remote sensing.

---
An example workflow is as follows:
### Download reflectance data:
-GEE_Scripts>GetSRDataLS5-7.py
	Read in dates and station IDs from a field record file and Google Fusion Table of sampling locations. Can be adapted to retrieve data over the entire satellite record. Save data in .csv.

### Format data:
-R_Scripts>FormatCalibrationReflectanceData.R
-R_Scripts>FormatHistoricReflectanceData.R
Save data in .csv format.

### Create model variables:
-R_Scripts>CreateModelVariables.R
Save data in .RData format (XData.RData)

### Testing different time windows and obtaining significant variables:
-R_Scripts>
More content will be added as functions are generalized and finalized.
