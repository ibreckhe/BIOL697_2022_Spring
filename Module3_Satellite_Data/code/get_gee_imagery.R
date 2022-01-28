##You will need to sign up for Google Earth Engine (through your Google Account) here:
#https://signup.earthengine.google.com/

##Installs Earth Engine R package and python environment.
remotes::install_github("r-spatial/rgee")
library(rgee)
library(sf)
library(geojsonio)
ee_install(py_env = "rgee") # It is just necessary once!

##Sets credentials
library(rgee)
ee_Initialize(user = 'ibreckhe@gmail.com', drive = TRUE)

##Makes a connection to a dataset, in this case a global DEM
#ee_Initialize()
srtm <- ee$Image("USGS/SRTMGL1_003")

##Puts it on a map.
viz <- list(
  max = 4000,
  min = 0,
  palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF")
)
Map$addLayer(
  eeObject = srtm,
  visParams =  viz,
  name = 'SRTM'
)

# Input imagery is a cloud-free Landsat 8 composite.
l8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1")

image <- ee$Algorithms$Landsat$simpleComposite(
  collection = l8$filterDate("2018-01-01", "2018-12-31"),
  asFloat = TRUE
)

# Use these bands for prediction.
bands <- c("B2", "B3", "B4", "B5", "B6", "B7", "B10", "B11")

# Load exising training points. The numeric property 'class' stores known labels.
sf_points <- st_read("../BIOL697_2022_Spring/Module1_Getting_Started/data/example_points.gpkg")
gee_points <- sf_as_ee(sf_points)

# Overlay the points on the imagery to get training.
training <- image$select(bands)$sampleRegions(
  collection = gee_points,
  scale = 30
)

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 0,
  max = 1,
  gamma = 1.3
)

Map$centerObject(gee_points, 10)
Map$addLayer(image, vizParams, "Image") +
  Map$addLayer(gee_points, list(color = "yellow"), "Training points")

ee_print(training)

##Converts sampled data back to a local object.
