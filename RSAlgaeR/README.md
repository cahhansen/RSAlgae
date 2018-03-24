#Purpose
This package was developed for processing, modeling, cross-validation, and analysis of water quality conditions (specifically chl-a).

#Example analysis 
One useful analysis that can be done with this package is evaluating trends in the timing of maximum chl-a conditions.
```
library(RSAlgaeR)
#Load a record of water quality data
wqrecord <- readRDS('EstimatedRecord.rds')

#Use doy.max.trend function to find the location, value, and day of year when the maximum value occured

wqdoytrend <- doy.max.trend(data=wqrecord,date="ImageDate",value="Chla",location="StationID")
```
This function calculates the DOY when the maximum value occurs, where this occured, and returns a list containing a dataframe of the annual maxima information, summary of the model fit (DOY vs year) and a plot of the DOY of maximum vs year.


#Installation instructions
The package can be installed and loaded using the following commands:
```
install.packages("RSAlgaeR")
library(RSAlgaeR)
```


#An overview that describes the main components of the package.

### A suggested workflow for using the RSAlgaeR Package for formatting/modeling/analyzing data:

#### 1. Format data. 
This includes formatting dates, removing negative values, cloud pixels, etc. 
(See ```sampleformatreflectancedata.R``` script for example which uses ```formatSRdata``` function)

#### 2. Create model variables.
Use the ```create.model.vars``` function

#### 3. Parameterize model.
Use glm() to develop the final model, based on an appropriate season, timewindow, and parameters. 

(Optional)
Use the ```step.model``` function (stepwise regression based on a user-specified timewindow) to explore performance for various parameters, definitions of near coincident data and seasons.
Examine model performance using k-fold cross validation and exploring the goodness of fit (```cv.model``` and ```modresults``` functions)

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
* Immediate connections Overall, monthly, or by location: ```climate.factor.effect```
* Seasonal summaries of water quality and climate conditions: ```annual.summary.wq``` and ```annual.summary.climate```

