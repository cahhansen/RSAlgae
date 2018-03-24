## Test environments
* local Windows 10 install, R 3.3.2
* ubuntu 14.04.4 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking R code for possible problems ... NOTE
  climate.factor.effect: no visible binding for global variable 'Month'
  climate.factor.effect: no visible binding for global variable 'Lag'
  climate.factor.effect: no visible binding for global variable 'Event'
  climate.factor.effect: no visible binding for global variable 'NoEvent'
  create.model.vars: no visible global function definition for 'read.csv'
  doy.max.trend: no visible binding for global variable 'Yearnorm'
  doy.max.trend: no visible binding for global variable 'DOYmax'
  plotrecord: no visible binding for global variable 'Value'
  plotrecord: no visible binding for global variable 'Date'
  plotrecord: no visible binding for global variable 'Dataset'
  plotrecord.errors: no visible binding for global variable 'lower'
  plotrecord.errors: no visible binding for global variable 'upper'
  Undefined global functions or variables:
    DOYmax Dataset Date Event Lag Month NoEvent Value Yearnorm lower
    read.csv upper
  Consider adding
    importFrom("utils", "read.csv")
  to your NAMESPACE file.
  
These suggestions do not affect the functions' ability to run correctly.

## Downstream dependencies
There are currently no downstream dependencies for this package.
