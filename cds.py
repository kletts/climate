
# CDS data request 
# this downloads the gridded monthly average temperature 
# data on a bounding box covering Australia  [-9, 112, -44, 160]
# requires the API key saved to ~/.cdsapirc 

import cdsapi
import datetime
import netCDF4
import numpy as np 
import geopandas as gp

ct = datetime.datetime.now() + datetime.timedelta(days=-30)

dataset = "reanalysis-era5-land-monthly-means"

request = {
    "product_type": ["monthly_averaged_reanalysis"],
    "variable": ["2m_temperature"],
    "year": [ ct.year ],
    "month": [ ct.month ],
    "time": ["00:00"],
    "data_format": "netcdf",
    "download_format": "unarchived",
    "area": [-9, 112, -44, 160]
}
target = 'download.nc'
client = cdsapi.Client()

client.retrieve(dataset, request, target)

# read netcdf ... etc
# data.dimensions: ('valid_time', 'latitude', 'longitude')

grid = netCDF4.Dataset('data/download.nc', 'r')
data = grid.variables['t2m']
long = grid.variables['longitude'][:]
lat = grid.variables['latitude'][:]

i = np.abs(long - 160).argmin()
j = np.abs(lat - -10).argmin()

data[:,j,i]

# ---- read shapefile ---- 

#aumap = gp.read_file("/Users/cklettner/Documents/ABSMAPS/2021 Structures/ASGS_2021_Main_Structure_GDA2020.gpkg", 
#                    layer="STE_2021_AUST_GDA2020").query("STATE_CODE_2021 not in ['9','Z']")


