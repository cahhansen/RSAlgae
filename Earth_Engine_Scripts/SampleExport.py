# Import necessary modules and functions
import EEExport
from EEExport import getSRDataLS57
	
# User Input--------------------------------------------------------------------
# Specify basefolder location
basefolder = 'C:\Users\carly\OneDrive\Documents\GitHub\GSLAlgae\DataFiles'
# Specify name of file with the dates of field samples to match to the imagery (must have column called 'Date' and column with location id called 'StationID')
samplingfile = basefolder+'\GSLSampleChlData.csv'
# Specify Google Fusion Table ID which has locations of sampling points (stations) with identifiers called 'StationID'
gftable = 'ft:1x46JxgxBWyMoSqRKQzsinmJ_pA_GX4eYNN2NFx4z'
# Specify location to save the results
resultsfile=basefolder+'\GSLSampleLS57Export.csv'
# Specify time window (+/- range of days to include imagery data)
timewindow=10

getSRDataLS57(samplingfile,timewindow,gftable,resultsfile)
