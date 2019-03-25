#
## CREATING A REGULAR GRID
#

#CHECK, INSTALL AND LOAD REQUIRED PACKAGES
pkgs <- c("sp","rgdal","plyr","dplyr","data.table","proj4","geojsonR","downloader","SpatialTools","sf",
          "spdep","rgeos","raster","stringr","rgdal","devtools","spatialEco","transformr","stringi")
for (pkg in pkgs) {
  if(pkg %in% rownames(installed.packages()) == FALSE) {install.packages(pkg)
    lapply(pkgs, require, character.only = TRUE)}
  else {
    lapply(pkgs, require, character.only = TRUE)}
}
rm(pkg,pkgs)
#SET WORKING DIRECTORY TO FILEPATH OF SCRIPT (PREFERRED DIRECTORY WHEN CLONING THE REPOSITORY)
#THIS WILL ONLY WORK WHEN USING R STUDIO, ELSE SET WD MANUALLY
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#LOAD GREATER LONDON SHAPE:
gla_full = read_sf("https://mapit.mysociety.org/area/2247.geojson")
#LOAD BOROUGH SHAPE (NEEDED FOR GRIDDING SPECIFIC BOROUGHS / WARDS)
gla = readOGR(dsn=path.expand("statistical-gis-boundaries-london"),layer="London_Borough_Excluding_MHW")
gla = st_as_sf(gla)
gla = gla[gla$NAME=="Bexley" | gla$NAME=="Westminster",]
gla = st_transform(gla,crs=st_crs(gla_full))

#CREATE GRID
grid = st_sf(
  st_make_grid(
    gla, cellsize = 0.008, square = TRUE, crs=st_crs(gla))) #Set grid resolution
grid = grid[unlist(st_intersects(gla,grid)),] #Crop grid to only use cells intersecting with the (only needed for buffer polygon)
grid$area_m2 = as.numeric(st_area(st_geometry(grid))) 
grid$id = rownames(grid) #Set ID for each grid cell
st_write(grid, "grid.shp") #Write grid