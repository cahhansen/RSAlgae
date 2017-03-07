# Formatting Files
|Name | Purpose|
| ------------- | ------------- |------------- |
|FormatReflectanceData.R  |Reads in surface reflectance and removes duplicates, non-cloud-free reflectance, etc.  |
|CreateModelVariables.R  |Calculates the interaction variables (band ratios) for use in model development.  |
# Model Development Files
|Name | Purpose|
| ------------- |------------- |
|Stepwise.R  |Performs stepwise GL regression for all data within a user-specified time window. Uses k-means (if sample size is sufficient) to look at parameterization.  |
|GLM.R   |Calibrates GLM (using user-specified parameters) for all data within a user-specified time window. |    
|Seasonal.R   |Calibrates GLM based on user-specified seasons, (using user-specified parameters) within a user-specified time window. |    
 
