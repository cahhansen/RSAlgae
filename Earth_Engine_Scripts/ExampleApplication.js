var dateStart = '2016-7-10';
var dateEnd = '2016-7-12';
var lat = 40.2
var long = -111.8
//Load image collections and filter to desired dates
var collection = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
var ndwicollection = ee.ImageCollection('MODIS/MYD09GA_NDWI')
var filter = collection.filterDate(dateStart, dateEnd);
var img=filter.median()

var ndwifilter = ndwicollection.filterDate(dateStart, dateEnd);
var ndwiimg=ndwifilter.median()

//Set model
var modelimg = img.expression(
    'exp(7.325-0.0039*b1-0.0512*b2/b7+0.0127*b3/b5)',
    {
      b1: img.select('B1'),
      b2: img.select('B2'),
      b3: img.select('B3'),
      b4: img.select('B4'),
      b5: img.select('B5'),
      b7: img.select('B7'),
    });
//Apply model to water-masked image
var modelmask = modelimg.mask(ndwiimg)

Map.setCenter(long,lat, 10);
//NDWI Mask
Map.addLayer(ndwiimg,{min:-0,max:1},'Water Mask')
var Model_PALETTE = '9400D3,4B0082,0000FF,00FF00,FFFF00,FF7F00,FF0000';
//Model applied to water mask image
Map.addLayer(modelmask, {min:0, max: 200,'palette': Model_PALETTE}, 'Model');
