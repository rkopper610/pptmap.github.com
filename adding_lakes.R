#Create bounding box of the East Africa Region
my_points.df <-
  data.frame(lon = c(29, 47.9, 47.9, 29), 
             lat = c(-12, -12, 15.5, 15.5))
#Using sf package to create a sf spatial object with

library(sf)
my_points.sf <- st_as_sf(my_points.df, coords = c("lon", "lat"), crs = 4326)


my_bbox <- c(xmin = min(my_points.df$lon),
             xmax = max(my_points.df$lon),
             ymin = min(my_points.df$lat),
             ymax = max(my_points.df$lat))

my_bbox.m <- 
  matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'], 
           my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']),
         ncol = 2)
my_bbox.sf <- st_geometry(st_polygon(x = list(my_bbox.m)))
st_crs(my_bbox.sf) <- 4326

my_bbox_buff_2500.sf <- 
  my_bbox.sf %>%
  st_transform(crs = 32632) %>%
  st_buffer(dist = 2500) %>% # 2.5 kilometers
  st_transform(crs = 4326)

my_bbox_buff_5000.sf <- 
  my_bbox.sf %>%
  st_transform(crs = 32632) %>%
  st_buffer(dist = 5000) %>% # 5 kilometers
  st_transform(crs = 4326)

my_bbox_buff_25000.sf <- 
  my_bbox.sf %>%
  st_transform(crs = 32632) %>%
  st_buffer(dist = 25000) %>% # 25 kilometers
  st_transform(crs = 4326)
#This is how my point data look like in its essence.

library(ggplot2)

my_world_map <- map_data('world')
my_world_map <- my_world_map[my_world_map$region %in% c("Ethiopia","Uganda", "Tanzania", "Kenya"),]
#my_world_map <- my_world_map[my_world_map$region %in% ("Ethiopia"),]


gpo <- ggplot() + 
  geom_sf(data = my_points.sf) + 
  geom_sf(data = my_bbox_buff_2500.sf, fill = NA) +
  coord_sf(xlim = c(st_bbox(my_bbox_buff_25000.sf)['xmin'], st_bbox(my_bbox_buff_25000.sf)['xmax']), 
           ylim = c(st_bbox(my_bbox_buff_25000.sf)['ymin'], st_bbox(my_bbox_buff_25000.sf)['ymax'])) +
  geom_polygon(data = my_world_map, 
               aes(x=long, y = lat, group = group,
                   fill = region), colour = 'black', alpha = .4) +
  theme_bw()

#Load DEM and mask to East Africa
library(raster)
DEM <- raster("/Users/ryankopper/Desktop/R work/PPTmap/gt30e020n40_dem/gt30e020n40.dem")
DEM2 <-raster("/Users/ryankopper/Desktop/R work/PPTmap/gt30e020s10_dem/gt30e020s10.dem")
DEM <- merge(DEM, DEM2)
msk_DEM <- mask(DEM, E_Af_No_b)

msk_DEM <- projectRaster(msk_DEM, 
                         crs = crs(E_Af_No_b))
#DEM
dem.raster <- crop(msk_DEM, as(my_bbox_buff_25000.sf, 'Spatial'), snap='out')

dem.m  <-  rasterToPoints(dem.raster)
dem.df <-  data.frame(dem.m)
colnames(dem.df) = c("lon", "lat", "alt")

ggplot() +
  geom_raster(data = dem.df, aes(lon, lat, fill = alt), alpha = .45) +
  scale_fill_gradientn(colours = terrain.colors(100)) +
  geom_sf(data = my_bbox_buff_2500.sf, fill = NA) +
  coord_sf(xlim = c(st_bbox(my_bbox_buff_25000.sf)['xmin'], st_bbox(my_bbox_buff_25000.sf)['xmax']), 
           ylim = c(st_bbox(my_bbox_buff_25000.sf)['ymin'], st_bbox(my_bbox_buff_25000.sf)['ymax'])) +
  geom_polygon(data = my_world_map, 
               aes(x=long, y = lat, group = group), fill = NA, colour = 'black') +
  theme_bw()

#Hillshading---Works BUT too big of an area 

# slope.raster <- terrain(dem.raster, opt='slope')
# aspect.raster <- terrain(dem.raster, opt='aspect')
# hill.raster <- hillShade(slope.raster, aspect.raster, 40, 270)
# 
# hill.m <- rasterToPoints(hill.raster)
# hill.df <-  data.frame(hill.m)
# colnames(hill.df) <- c("lon", "lat", "hill")
# 
# 
# ggplot() +
# geom_raster(data = hill.df, aes(lon, lat, fill = hill), alpha = .45) +
# scale_fill_gradientn(colours = grey.colors(100)) +
#   geom_sf(data = my_bbox_buff_2500.sf, fill = NA) +
#   coord_sf(xlim = c(st_bbox(my_bbox_buff_25000.sf)['xmin'], st_bbox(my_bbox_buff_25000.sf)['xmax']),
#            ylim = c(st_bbox(my_bbox_buff_25000.sf)['ymin'], st_bbox(my_bbox_buff_25000.sf)['ymax'])) +
#   geom_polygon(data = my_world_map,
#                aes(x=long, y = lat, group = group), fill = NA, colour = 'black') +
#   theme_bw()


#Add bodies of water

#We can use the osmdata package to query the Open Street Map (OSM) Overpass API to download
#information about lakes and rivers. With the function osmdata::opq() we define the bbox for the query 
#and with osmdata::add_osm_feature() we extract specific OSM features using the tags (key and value).
# 
# library(osmdata)
# osm_lakes.sf <- 
#   opq(bbox = st_bbox(my_bbox_buff_25000.sf)) %>%
#   add_osm_feature(key = 'water', value = 'lake') %>%
#   osmdata_sf()
# osm_lakes.sf <- osm_lakes.sf$osm_multipolygons
# 
# osm_rivers.sf <- 
#   opq(bbox = st_bbox(my_bbox_buff_25000.sf)) %>%
#   add_osm_feature(key = 'waterway', value = 'river') %>%
#   osmdata_sf()
# osm_rivers.sf <- osm_rivers.sf$osm_lines
# 
# ggplot() +
#   geom_raster(data = hill.df, aes(lon, lat, fill = hill), alpha = .45) +
#   scale_fill_gradientn(colours = grey.colors(100)) +
#   geom_sf(data = osm_lakes.sf, fill = '#9ecae1', colour = NA) +
#   geom_sf(data = osm_rivers.sf, colour = '#9ecae1', size = 0.05) +
#   geom_sf(data = my_bbox_buff_2500.sf, fill = NA) +
#   coord_sf(xlim = c(st_bbox(my_bbox_buff_25000.sf)['xmin'], st_bbox(my_bbox_buff_25000.sf)['xmax']), 
#            ylim = c(st_bbox(my_bbox_buff_25000.sf)['ymin'], st_bbox(my_bbox_buff_25000.sf)['ymax'])) +
#   geom_polygon(data = my_world_map, 
#                aes(x=long, y = lat, group = group), fill = NA, colour = 'black') +
#   theme_bw()


#-------------------------------------------------------------------------------------------------
#Downloading data from DIVA

library(rgdal)
data.shapeken <- readOGR(dsn = "/Users/ryankopper/Desktop/R work/PPTmap/map_files/Water_shp_files/KEN_wat/KEN_water_areas_dcw.shp")
data.shapetz <- readOGR(dsn = "/Users/ryankopper/Desktop/R work/PPTmap/map_files/Water_shp_files/TZA_wat/TZA_water_areas_dcw.shp")
data.shapeeth <- readOGR(dsn = "/Users/ryankopper/Desktop/R work/PPTmap/map_files/Water_shp_files/ETH_wat/ETH_water_areas_dcw.shp")
data.shapeug <- readOGR(dsn = "/Users/ryankopper/Desktop/R work/PPTmap/map_files/Water_shp_files/UGA_wat/UGA_water_areas_dcw.shp")

u_vic <-   subset(data.shapeug, NAME == "LAKE VICTORIA")
k_vic <-   subset(data.shapeken, NAME == "LAKE VICTORIA")
t_vic <-   subset(data.shapetz, NAME == "LAKE VICTORIA")                    
e_tan <-   subset(data.shapeeth, NAME == "TANA HAYK")

E_Af_water<- bind(e_tan, t_vic, k_vic, u_vic)

plot(E_Af_No_b)
plot(E_Af_water, add = TRUE, col = "light blue")




