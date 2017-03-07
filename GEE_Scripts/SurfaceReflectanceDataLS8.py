# Import necessary modules
import ee
import ee.mapclient
import csv
import pandas as pd
import geopandas
from geopandas import GeoDataFrame
import datetime
from datetime import date, datetime, timedelta
import pprint
import json
import timeit
start = timeit.default_timer()

# Initialize the Earth Engine object, using the authentication credentials.
ee.Initialize()

# User Input--------------------------------------------------------------------
#Specify basefolder location
basefolder = 'C:/Users/'
# Specify location of file with the dates of chl-a measurements
samplingfile = basefolder+'UtahChlData.csv'
# Read in necessary files and format
samplingdf=pd.io.parsers.read_csv(samplingfile, header=0)
samplingdf['SampleDate']=pd.to_datetime(samplingdf['Date'])
length=len(samplingdf)
#Specify the time-window for including non-coincident imagery (+/- days from the sampling date)
print('The total number of samples of chl-a is: '+str(length))
timewindow=10
print('Time window is: '+str(timewindow))
# Determine date ranges for the imagery
samplingdf['DateStart']=samplingdf['SampleDate']-timedelta(days=timewindow)
samplingdf['DateEnd']=samplingdf['SampleDate']+timedelta(days=timewindow+1)

pixel_df=pd.DataFrame()
satellites = "LANDSAT/LC8_SR"

for m in range(0,length):
    #Calculate the start and end dates of the timewindow
    startdate=samplingdf['DateStart'].iloc[m]
    enddate=samplingdf['DateEnd'].iloc[m]
    if startdate>datetime(2013,4,11):
        #Specify the station and retrieve the geometry from the Google Fusion Table 
        Station = str(int(samplingdf['StationID'].iloc[m]))
        stations = ee.FeatureCollection('ft:1NTFB0ptBnEvXeJhOHxjVssFMFJ1WKdFebNcMIWSk').filterMetadata('StationID', 'equals', Station)
        #Print statement for tracking progress
        print(str(m)+' Station: '+Station+ ' on '+str(samplingdf['SampleDate'].iloc[m]))
        try:
            #Retrieve image collection from Earth Engine for the specified satellite during the specified time window, limiting the collection to images containing the station
            collection = ee.ImageCollection(satellites).filterDate(startdate,enddate).filterBounds(stations)
            def addTime(image):
                return image.addBands(image.metadata('system:time_start'))
            collection=collection.map(addTime)
            #Retrieve image data for the station
            stationpoint = collection.getRegion(stations, 1)
            pixelr=stationpoint.getInfo()
            numimages=len(pixelr)
            for i in range(0,numimages):
                imgtimestamp=datetime.fromtimestamp(pixelr[i+1][13]/1e3)
                imgdate=datetime(imgtimestamp.year,imgtimestamp.month,imgtimestamp.day)
                #Format reflectance data as a dataframe
                pixeldf = pd.DataFrame({'StationID': [Station],
                                'Organization': [samplingdf['Organization'].iloc[m]],
                                'Lake': [samplingdf['Lake'].iloc[m]],
                                'Sensor': [satellites],
                                'ImageName': [pixelr[i+1][0]],
                                'ImageDate': [imgdate],
                                'SamplingDate': [samplingdf['SampleDate'].iloc[m]],
                                'DiffInDays': [imgdate-samplingdf['SampleDate'].iloc[m]],
                                'FieldValue': [samplingdf['Value'].iloc[m]],
                                'MaxDepth': [samplingdf['MaxDepth_m'].iloc[m]],
                                'SampleDepth':[samplingdf['SampleDepth_m'].iloc[m]],
                                'Method': [samplingdf['Method'].iloc[m]],
                                'CoastalAerosol': [pixelr[i+1][4]],
                                'Blue': [pixelr[i+1][5]],
                                'Green': [pixelr[i+1][6]],
                                'Red': [pixelr[i+1][7]],
                                'NIR': [pixelr[i+1][8]],
                                'SWIR1': [pixelr[i+1][9]],
                                'SWIR2': [pixelr[i+1][10]],
                                'CloudMask': [pixelr[i+1][11]],
                                'CloudMaskConfidence': [pixelr[i+1][12]]})
                print(satellites)
                if len(pixel_df)>=1:
                    frames=[pixel_df,pixeldf]
                    pixel_df=pd.concat(frames)
                else:
                    pixel_df=pixeldf
        except:
            pass
   
print('Number of successfual matches: '+str(len(pixel_df)))
pixel_df.to_csv(basefolder+'UtahLS8EarthEngineExport.csv',index=False)
print('Finished exporting reflectance data!')
stop = timeit.default_timer()
time=(stop - start)/60
print('Time to run: '+str(time))