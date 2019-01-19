#' Surface reflectance data from Landsat 5 and 7
#'
#' A dataset containing the surface reflectance for
#' locations in Utah Lake from Landsat 5 and 7
#' (precollection dataset) in Google Earth Engine.
#'
#'
#' @format A data frame with 215 rows and 16 variables:
#' \describe{
#'   \item{Blue}{reflectance in the blue band}
#'   \item{CloudMask}{classes used for masking clouds/haze (0 or 1 are clear/water)}
#'   \item{FieldValue}{sampled or observed chlorophyll a value}
#'   \item{AbsDiffInDays}{calculated difference between imagery date and sampling date}
#'   \item{Green}{reflectance in the green band}
#'   \item{ImageDate}{date of imagery acquisition}
#'   \item{ImageName}{name of Landsat scene}
#'   \item{Method}{method used in sampling}
#'   \item{NIR}{reflectance in the Near infrared band}
#'   \item{Organization}{agency responsible for collecting sample data}
#'   \item{Red}{reflectance in the red band}
#'   \item{SWIR1}{reflectance in the short wave infrared 1 band}
#'   \item{SWIR2}{reflectance in the short wave infrared 2 band}
#'   \item{SamplingDate}{date of sample collection}
#'   \item{Sensor}{sensor used to measure surface reflectance}
#'   \item{StationID}{location of sample}
#'   ...
#' }
#' @source {Utah Division of Water Quality and Landsat}
"srdata"

#' Surface reflectance data from Landsat 5 and 7
#'
#' A dataset containing the surface reflectance for
#' locations in Utah Lake from Landsat 5 and 7
#' (precollection dataset) in Google Earth Engine, used for applying models.
#'
#'
#' @format A data frame with 2313 rows and 25 variables:
#' \describe{
#'   \item{Blue}{reflectance in the blue band}
#'   \item{Green}{reflectance in the green band}
#'   \item{ImageDate}{date of imagery acquisition}
#'   \item{NIR}{reflectance in the Near infrared band}
#'   \item{Red}{reflectance in the red band}
#'   \item{SWIR1}{reflectance in the short wave infrared 1 band}
#'   \item{SWIR2}{reflectance in the short wave infrared 2 band}
#'   \item{StationID}{location of sample}
#'   \item{Green_Blue}{reflectance in the green/blue band}
#'   \item{Red_Blue}{reflectance in the red/blue band}
#'   \item{Red_Green}{reflectance in the red/green band}
#'   \item{Red_NIR}{reflectance in the red/NIR band}
#'   \item{Red_SWIR1}{reflectance in the red/SWIR1 band}
#'   \item{Green_SWIR1}{reflectance in the green/SWIR1 band}
#'   \item{Blue_SWIR1}{reflectance in the blue/SWIR1 band}
#'   \item{Red_SWIR2}{reflectance in the red/SWIR2 band}
#'   \item{Green_SWIR2}{reflectance in the green/SWIR2 band}
#'   \item{Blue_SWIR2}{reflectance in the blue/SWIR2 band}
#'   \item{NIR_SWIR1}{reflectance in the nir/swir1 band}
#'   \item{NIR_SWIR2}{reflectance in the nir/swir2 band}
#'   \item{NIR_Blue}{reflectance in the nir/blue band}
#'   \item{NIR_Green}{reflectance in the nir/green band}
#'   \item{NDVI}{NDVI}
#'   \item{avgRGB}{average of reflectance in the visible bands}
#'   \item{avgSWIR}{average of reflectance in the SWIR bands}
#'   ...
#' }
#' @source {Landsat}
"srdataforapplication"


#' Climate data
#'
#' A dataset containing precipitation and maximum daily temperature
#' for the Provo BYU NOAA Station
#'
#'
#' @format A data frame with 12238 rows and 3 variables:
#' \describe{
#'   \item{DATE}{date of observation}
#'   \item{PRCP}{precipitation volume (mm/day)}
#'   \item{TMAX}{max temperature (degrees C)}
#'   ...
#' }
#' @source {NOAA Climate Data Archive}
"climatedata"

#' Estimated  data
#'
#' A dataset containing remotely sensed estimates of chlorophyll for Utah Lake
#'
#'
#' @format A data frame with 2041 rows and 5 variables:
#' \describe{
#'   \item{ImageDate}{date of image acquisition}
#'   \item{StationID}{location (corresponding to sampling locations)}
#'   \item{EstChlValue}{estimated chlorophyll value}
#'   \item{Lower}{lower end of confidence interval}
#'   \item{Upper}{upper end of confidence interval}
#'   ...
#' }
#' @source {derived from models developed by
#' Carly Hansen and Landsat surface reflectance data}
"estimatedrecord"

#' Example chlorophyll estimation model
#'
#' A dataset containing model information for Utah Lake - summer
#'
#'
#' @format A list of 30 items:
#' \describe{
#'   \item{coefficients}{}
#'   \item{residuals}{}
#'   \item{fitted.values}{}
#'   \item{effects}{}
#'   \item{R}{}
#'   \item{rank}{}
#'   \item{qr}{}
#'   \item{family}{}
#'   \item{linear.predictors}{}
#'   \item{deviance}{}
#'   \item{aic}{}
#'   \item{null.deviance}{}
#'   \item{iter}{}
#'   \item{weights}{}
#'   \item{prior.weights}{}
#'   \item{df.residual}{}
#'   \item{df.null}{}
#'   \item{y}{}
#'   \item{converged}{}
#'   \item{boundary}{}
#'   \item{model}{}
#'   \item{call}{}
#'   \item{formula}{}
#'   \item{terms}{}
#'   \item{data}{}
#'   \item{offset}{}
#'   \item{control}{}
#'   \item{method}{}
#'   \item{contrasts}{}
#'   \item{xlevels}{}
#'   ...
#' }
#' @source {developed by Carly Hansen}
"utahsummermod"

#' HAB and Climate Data for Utah Lake
#'
#' A dataset containing annual lake-wide estimates of chlorophyll for Utah Lake and climate data
#'
#'
#' @format A data frame with 34 rows and 9 variables:
#' \describe{
#'   \item{Year}{year of summary}
#'   \item{LocationID}{Name of lake}
#'   \item{MaxAvgChl}{Maximum average chl}
#'   \item{MaxMaxChl}{Maximum maximum chl}
#'   \item{MaxBloom}{Max bloom extent}
#'   \item{TotalWinterPrecip}{total precipitation during the winter}
#'   \item{TotalSummerPrecip}{total precipitation during the summer}
#'   \item{MeanSummerTemp}{Average summer temperature}
#'   \item{TotalSWE}{total snow water equivalent}
#'   ...
#' }
#' @source {derived from models developed by
#' Carly Hansen and Landsat surface reflectance data}
"utahlake_hab_climatedata"
