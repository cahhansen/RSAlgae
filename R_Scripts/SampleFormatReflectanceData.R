#Read in reflectance data for calibration
setwd('~/GitHub/RSAlgae/RSAlgaeR/Data')
calibrationdata <- read.csv('SampleLS5-7EarthEngineExport_Calibration.csv')

#Format data used for model calibration-------------------------------------------------------------------

#Specify Methods to be used (may vary by lake)
calibrationdata <- calibrationdata[!(calibrationdata$Method=="Chlorophyll-A Uncorrected for pheophytin"),]
#Format data used for model calibration
formattedcalibrationdata <- formatSRdata(data=calibrationdata,imagerydate="ImageDate",samplingdate="SamplingDate",
                                         location="StationID",datatype="Calibration")
write.csv(formattedcalibrationdata,file="SampleLS57CalibrationFormat.csv")

#Format data used for model application-------------------------------------------------------------------
historicdata <- read.csv('SampleLS5-7EarthEngineExport_Historic.csv')
formattedhistoricdata <- formatSRdata(data=historicdata,imagerydate="ImageDate",
                                      location="StationID",datatype="Estimated")
write.csv(formattedhistoricdata,file="SampleLS57HistoricFormat.csv")
