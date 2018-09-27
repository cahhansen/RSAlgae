# Import necessary modules
import ee
import csv
import pandas as pd
import datetime
from datetime import date, datetime, timedelta
import pprint
import json
import timeit
start = timeit.default_timer()

# Initialize the Earth Engine object, using authentication credentials associated with google account.
ee.Initialize()

# User Input--------------------------------------------------------------------
#Specify basefolder location
basefolder = 'C:/Users/carly/Google Drive/University of Utah - Research/Dissertation Research/RemoteSensingPaper'
lakename = 'Farmington Bay'
satellites = ["LANDSAT/LT05/C01/T1_SR","LANDSAT/LE07/C01/T1_SR"]
startdate='1984-05-01'
enddate='1985-9-30'
#Specify spatial data (in Google Fusion Table) for application
lake = ee.FeatureCollection('ft:1BaVai9gDJdsANDtmjPjpO4TpGNr-MNrUDBvviRRe').filterMetadata('Name', 'equals', lakename) 

def maskBadData(image):
    invalid = image.select('sr_cloud_qa').bitwiseAnd(0x6).neq(5)
    clean = image.mask(invalid.Not())
    return(clean)

# Import Landsat collection
LSimagecollection = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR").filterBounds(lake).filterDate(startdate, enddate)

# Subset and mask collection
LS_clean = LSimagecollection.map(maskBadData).select(['B1', 'B2', 'B3', 'B4', 'B5', 'B7']);

#Get region data
lakedata = LS_clean.reduceRegion(lake, 500)
lakedatar=lakedata.getInfo()

# Extract time values overlayed with feature collection
out = LT5_clean.getRegion(samples, 30).getInfo()

# Coerce to df
df = pd.DataFrame(out[1:], columns = out[0])
print(df)

