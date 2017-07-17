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
basefolder = ''
#Specify dates of interest
#For this case, we use a csv file to obtain a time series for summer months (May-September) from 1984-2016
datefile = basefolder+'Dates.csv'
#Specify locations of interest
locationfile = basefolder+'/StationsByLake/LocationsFB.csv'
#-------------------------------------------------------------------------------
# Read in necessary files and format
datedf=pd.io.parsers.read_csv(datefile, header=0)
length=len(datedf)
locationdf=pd.io.parsers.read_csv(locationfile, header=0)
lengthloc=len(locationdf)
pixel_df=pd.DataFrame()
satellites = ["LANDSAT/LT5_SR","LANDSAT/LE7_SR"]

for m in range(0,length):
    #Calculate the start and end dates of the timewindow
    startdate=datedf['DateStart'].iloc[m]
    enddate=datedf['DateEnd'].iloc[m]
    #Specify the station and retrieve the geometry (location) from the Google Fusion Table 
    for x in range(0,lengthloc):
        Station = str(int(locationdf['StationID'].iloc[x]))
        stations = ee.FeatureCollection('ft:1NTFB0ptBnEvXeJhOHxjVssFMFJ1WKdFebNcMIWSk').filterMetadata('StationID', 'equals', Station)
        #Print statement for tracking progress
        print(str(m)+' Station: '+Station+ ' between '+str(startdate)+' and '+str(enddate))
        for n in range(0,len(satellites)):
            try:
                #Retrieve image collection from Earth Engine for the specified satellite during the specified time window, limiting the collection to images containing the station
                collection = ee.ImageCollection(satellites[n]).filterDate(startdate,enddate).filterBounds(stations)
                def addTime(image):
                    return image.addBands(image.metadata('system:time_start'))
                collection=collection.map(addTime)
                #Retrieve image data for the station
                stationpoint = collection.getRegion(stations, 1)
                pixelr=stationpoint.getInfo()
                numimages=len(pixelr)
                for i in range(0,numimages):
                    imgtimestamp=datetime.fromtimestamp(pixelr[i+1][20]/1e3)
                    imgdate=datetime(imgtimestamp.year,imgtimestamp.month,imgtimestamp.day)
                    #Format reflectance data as a dataframe
                    pixeldf = pd.DataFrame({'StationID': [Station],
                                'Organization': [locationdf['Organization'].iloc[x]],
                                'Lake': [locationdf['Lake'].iloc[x]],
                                'Sensor': [satellites[n]],
                                'ImageName': [pixelr[i+1][0]],
                                'ImageDate': [imgdate],
                                'Blue': [pixelr[i+1][4]],
                                'Green': [pixelr[i+1][5]],
                                'Red': [pixelr[i+1][6]],
                                'NIR': [pixelr[i+1][7]],
                                'SWIR1': [pixelr[i+1][8]],
                                'SWIR2': [pixelr[i+1][9]],
                                'CloudMask': [pixelr[i+1][10]],
                                'CloudMaskConfidence': [pixelr[i+1][11]]})
                    print(satellites[n])
                    if len(pixel_df)>=1:
                        frames=[pixel_df,pixeldf]
                        pixel_df=pd.concat(frames)
                    else:
                        pixel_df=pixeldf
            except:
                pass
   
print('Number of successful data retrievals: '+str(len(pixel_df)))
pixel_df.to_csv(basefolder+'LS5-7EarthEngineExportHistorical.csv',index=False)
print('Finished exporting reflectance data!')
stop = timeit.default_timer()
time=(stop - start)/60
print('Time to run: '+str(time))
