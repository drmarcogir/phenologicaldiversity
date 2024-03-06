#!/usr/bin/env python
# coding: utf-8

# In[2]:


import os
import ee
ee.Initialize()


# In[18]:


# add dn for product aggregated into 8 days
def add_dn_date(img,beginDate=None,n=None,IncludeYear=False):
    if beginDate is None:
        beginDate = img.get('system:time_start')
    else:
        beginDate = beginDate
    if IncludeYear is False:
        IncludeYear = True
    if n is None:
        n = 8
    beginDate = ee.Date(beginDate)
    year  = beginDate.get('year')
    month = beginDate.get('month')
    diff  = beginDate.difference(ee.Date.fromYMD(year, 1, 1), 'day').add(1)
    dn    = diff.subtract(1).divide(n).floor().add(1).int()
    yearstr  = year.format('%d') 
    dn = dn.format('%02d')
    return ee.Image(img).set('system:time_start', beginDate.millis()).set('date', beginDate.format('yyyy-MM-dd')).set('Year', yearstr).set('Month',beginDate.format('MM')).set('YearMonth', beginDate.format('YYYY-MM')).set('dn', dn)

# wrapper function for add_dn_date
def add_dn_date_all(Year,days):
    def wrapper(image0):
        tmp = add_dn_date(img = image0,IncludeYear=Year,n = days)
        return tmp
    return (wrapper)

# add NDVI to data
def addNDVI(image):
    #image = image.updateMask(MakMarco.eq(1))
    return image.addBands(image.normalizedDifference(['sur_refl_b02','sur_refl_b01']).rename('NDVI')).float()

# function for extracting quality bits
def getQABits(image, start, end, mascara):
    # Compute the bits we need to extract.
    pattern = 0
    for i in range(start,end+1):
        pattern += 2**i
    # Return a single band image of the extracted QA bits, giving the     band a new name.
    return image.select([0], [mascara]).bitwiseAnd(pattern).rightShift(start)

# mask out low quality pixels (based on flags)
def maskPixels(image0):
    #Select the QA band
    QA = image0.select('StateQA')
    # Get the land_water_flag bits
    landWaterFlag = getQABits(QA, 3, 5, 'land_water_flag')
    #Get the cloud_state bits and find cloudy areas.
    cloud = getQABits(QA, 0, 1, 'cloud_state').expression("b(0) == 1 || b(0) == 2")
    # Get the cloud_shadow bit
    cloudShadows = getQABits(QA, 2, 2, 'cloud_shadow')
    # Get the Pixel is adjacent to cloud bit
    cloudAdjacent = getQABits(QA, 13, 13, 'cloud_adj')
    # Get the internal cloud flag
    cloud2 = getQABits(QA, 10, 10, 'cloud_internal')
    # Get the internal fire flag
    fire = getQABits(QA, 11, 11, 'fire_internal')
    # Get the MOD35 snow/ice flag
    snow1 = getQABits(QA, 12, 12, 'snow_MOD35')
    # Get the internal snow flag
    snow2 = getQABits(QA, 15, 15, 'snow_internal')
    # create mask
    mask = landWaterFlag.eq(1).And(cloud.Not()).And(cloudShadows.Not()).And(cloudAdjacent.Not()).And(cloud2.Not()).And(fire.Not()).And(snow1.Not()).And(snow2.Not())
    return image0.updateMask(mask) 

def smooth_func(image): 
    collection = ee.ImageCollection.fromImages(image.get('images'))
    return ee.Image(image).addBands(collection.mean().rename(['mean']))

def clim5y(month):
    month = ee.String(month)
    seqNDVI = MOD09ndviY.filterMetadata('dn', 'equals',month)
    return seqNDVI.median().copyProperties(seqNDVI.first(), ['system:time_start','system:time_end','dn'])

# filter smoothed map
def filt_smoothed(image):
    image = image.select('NDVI')
    image = image.unmask()
    #image = image.where(image.eq(0),MinNDVI)
    return image.updateMask(count_valid.gte(23)).updateMask(image.select('NDVI').gt(0))

# function for accumulating NDVI
def accumulate(image,list):
    # Get the latest cumulative NDVI of the list with
    # get(-1).  Since the type of the list argument to the function is unknown,
    # it needs to be cast to a List.  Since the return type of get() is unknown,
    # cast it to Image.
    previous = ee.Image(ee.List(list).get(-1)).toFloat()
    # Add the current anomaly to make a new cumulative NDVI image and Propagate metadata to the new image.
    added = image.toFloat().add(previous).toFloat().set('system:time_start', image.get('system:time_start'))
    return ee.List(list).add(added)

# cumulative normalized
def cum_dividelast(image):
    tmp = image.divide(last).multiply(Cumulative_mean_at_5km)
    return tmp

# calculate deviations
def deviations_calc(image):
    tmp = image.select('NDVI').reproject(crs = 'SR-ORG:6974',scale = 463.3127165275).reduceResolution(ee.Reducer.stdDev(), False, 65536).reproject(ee.Projection('EPSG:4326').scale(0.05, 0.05)).updateMask(1).rename('stdDev')
    return tmp


def addTimeBand(img):
    ## make sure mask is consistent ##
    mask = img.mask()
    time = img.metadata('system:time_start').rename("time").mask(mask)
    return img.addBands(time)


def replace_mask(img, newimg, nodata):
    if frame is None:
        nodata = 8
    # var con = img.mask();
    # var res = img., NODATA
    mask = img.mask()
    # The only nsolution is unmask & updatemask */
    img = img.unmask(nodata)
    img = img.where(mask.Not(), newimg)
    img = img.updateMask(img.neq(nodata))
    return img
    
def linearInterp(imgcol,frame = None,nodata = None):
    if frame is None:
        frame = 32
    if nodata is None:
        nodata = 0
    timestart   = 'system:time_start'
    imgcol = imgcol.map(addTimeBand)
    
    # We'll look for all images up to 32 days away from the current image.
    maxDiff = ee.Filter.maxDifference(frame * (1000*60*60*24), timestart, None, timestart)
    
    #cond    = {'leftField':timestart, 'rightField':timestart}
    # Images after, sorted in descending order (so closest is last).
    #var f1 = maxDiff.and(ee.Filter.lessThanOrEquals(time, null, time))
    f1 = ee.Filter.And(maxDiff, ee.Filter.lessThanOrEquals(leftField = timestart,rightField = timestart))
    c1 = ee.Join.saveAll(matchesKey = 'after', ordering = timestart, ascending = False).apply(imgcol, imgcol, f1)
    # Images before, sorted in ascending order (so closest is last).
    # var f2 = maxDiff.and(ee.Filter.greaterThanOrEquals(time, null, time))
    
    f2 = ee.Filter.And(maxDiff, ee.Filter.greaterThanOrEquals(leftField = timestart,rightField = timestart))
    c2 = ee.Join.saveAll(matchesKey = 'before', ordering = timestart, ascending = True).apply(c1, imgcol, f2)
    
    # interpolation 
    def func_its(img):
        img = ee.Image(img)
        before = ee.ImageCollection.fromImages(ee.List(img.get('before'))).mosaic()
        after  = ee.ImageCollection.fromImages(ee.List(img.get('after'))).mosaic()
        img = img.set('before', {}).set('after', {})
        
        # constrain after or before no NA values, confirm linear Interp having result
        before = replace_mask(before, after, nodata)
        after  = replace_mask(after , before, nodata)
        
        # Compute the ratio between the image times.
        x1 = before.select('time').double()
        x2 = after.select('time').double()
        now = ee.Image.constant(img.date().millis()).double()
        ratio = now.subtract(x1).divide(x2.subtract(x1))  # this is zero anywhere x1 = x2
        
        # Compute the interpolated image.
        before = before.select(0); #remove time band now
        after  = after.select(0)
        img    = img.select(0)
        interp = after.subtract(before).multiply(ratio).add(before)
        qc = img.mask().Not().rename('qc')
        interp = replace_mask(img, interp, nodata)
        
        # Map.addLayer(interp, {}, 'interp')
        return interp.addBands(qc).copyProperties(img, img.propertyNames())
    interpolated = ee.ImageCollection(c2.map(func_its))
    return interpolated

# convert feature collection into feature collection with no geometry (easier to save)
def convert(feature):
    res = ee.Feature(None,feature.toDictionary())
    return(res)


# ### Load in collections and required images

# In[4]:


# load collections and required images
collection = ee.ImageCollection('MODIS/006/MOD09A1').filterDate('2000-01-01', '2020-12-31')
forestmask = ee.Image("users/marcogirardello/phenoutils/mask_unchanged_500m")
worldgrid = ee.FeatureCollection('users/marcogirardello/phenoutils/grid_export_phenology1')
#smallgrid = ee.FeatureCollection('users/marcogirardello/phenoutils/small5kmit')
testarea = ee.Image('users/marcogirardello/phenoutils/NDVI_small')
smallgrid = ee.FeatureCollection('users/marcogirardello/phenoutils/grid_export_phenology2')


# ### <span style="color:blue">Pre-processing step 1: filtering data by quality flags and calculated NDVI.</span>
# This include snow, cloud, fire, cloud shadows and the land/water mask

# In[5]:


# end and start date of period of interest
start_date = ee.Date.fromYMD(2001, 1, 1)
end_date   = ee.Date.fromYMD(2020, 12, 31)


# In[6]:


# add dn
collection = collection.map(add_dn_date_all(Year = False, days = 8))


# In[7]:


# mask out crap pixels
MOD09masked = collection.filterDate(start_date, end_date).map(maskPixels)


# In[8]:


# add NDVI as a new band
MOD09ndvi = MOD09masked.map(addNDVI).select('NDVI')


# ### <span style="color:blue">Main calculations</span>

# In[9]:


tmpseas = ["%02d" % x for x in list(range(1, 46+1))]
tmpseas1 = ee.List(tmpseas)


# In[11]:


#year = int(os.environ["year"])
year = 2002


# In[22]:


# climatology
yearp5 = ee.Number(year).add(5)
start_date = ee.Date.fromYMD(year,1,1)
end_date = ee.Date.fromYMD(yearp5,12,31)
MOD09ndviY = MOD09ndvi.filterDate(start_date, end_date)
MinNDVI = MOD09ndviY.min()

# climatology 5 years (monthly composites)
seasons = ee.ImageCollection.fromImages(tmpseas1.map(clim5y)) # problems start here!!!

frame  = 8*5
nodata = -9999
seasons2 = linearInterp(seasons, frame, nodata)

count_valid = seasons2.select('qc').count()
smoothed = seasons2.map(filt_smoothed)
smoothed = smoothed.select('NDVI')

#Get the timestamp from the most recent image in the reference collection.
time0 = smoothed.first().get('system:time_start')

# Rename the first band 'NDVI'.
first = ee.List([ee.Image(0).set('system:time_start', time0).select([0],['NDVI']).toFloat()])

# Since the return type of iterate is unknown, it needs to be cast to a List.
cumulative = ee.ImageCollection(ee.List(smoothed.iterate(accumulate, first)))

# normalise
last = cumulative.sort('system:time_start', False).first()
last = last.updateMask(last.gte(9))

# mean of cumulative values
Cumulative_mean = last.divide(46)
Cumulative_mean_at_5km = Cumulative_mean.reproject(crs = 'SR-ORG:6974',scale = 463.3127165275).reduceResolution(ee.Reducer.mean(), False, 65536).reproject(ee.Projection('EPSG:4326').scale(0.05, 0.05)).updateMask(1) 

# cumulative map normalised
cumulativeNorm = cumulative.map(cum_dividelast)

# deviations (these are described in the document sent by Alessandro)
cumulativeStd10 = cumulativeNorm.map(deviations_calc)

# calculate the mean of the deviations
cumulativeStd10_mean = cumulativeStd10.mean().multiply(10000)
#cumulativeStd10_mean = cumulativeStd10_mean.updateMask(last.gte(9))
#cumulativeStd10_mean_at_5km = cumulativeStd10_mean.reproject(crs = 'SR-ORG:6974',scale = 463.3127165275).reduceResolution(ee.Reducer.mean(), False, 65536).reproject(ee.Projection('EPSG:4326').scale(0.05, 0.05)).updateMask(1)

# mask out results for areas where there is forest
cumulativeStd10_mean_at_5km1 = cumulativeStd10_mean.updateMask(forestmask)


# In[23]:


cumulativeStd10_mean_at_5km1


# 
# ### Export using the tile method. File to use is Guido's reprojected 5km file

# In[30]:


minpoly = int(os.environ["minpoly"])
maxpoly = int(os.environ["maxpoly"])


# In[ ]:


polyl = list(range(minpoly,maxpoly+1))


# In[ ]:


# Remember year!
for poly in polyl:
  # filter a given square
  onesquare = smallgrid.filterMetadata('polyID','equals',poly)
  roi1 = ee.Geometry.Polygon(onesquare.geometry().getInfo().get('coordinates'))
  # export tile
  filename = 'Y_'+str(year)+'_'+str(poly)
  task = ee.batch.Export.image.toCloudStorage(image = cumulativeStd10_mean_at_5km1,
                                     bucket = 'phenology_5km',fileNamePrefix = filename,
                                     scale = 5565.974539663679,fileFormat='GeoTIFF',
                                    skipEmptyTiles = True, crs ='EPSG:4326',maxPixels=1e13,
                                    region = roi1)
  task.start()

