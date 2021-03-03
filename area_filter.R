library(tidyverse)
library(sf)
library(mapview)

r6_boundary <- st_read("Region_6_outline_/Region6.shp")
study_areas <- st_read("StudyAreas_Oversample/StudyAreas_Oversample2.shp")
military <- st_read("installations_ranges/FY19_MIRTA_Boundaries.shp")
ca_places <- st_read("ca-places-boundaries/CA_Places_TIGER2016.shp")
cat_detections <- st_read("bobcat_detections.geojson")
wilderness <- st_read("Wilderness_Areas/Wilderness_Areas_103019.shp")

study_areas %>% st_crs -> project_crs
r6_boundary %>% st_transform(project_crs) -> r6_boundary
ca_places %>% st_transform(project_crs) -> ca_places


## Military @@
military %>% st_transform(project_crs) -> military
military %>% 
  filter(STATE_TERR == "California") %>%
  filter(st_intersects(geometry, r6_boundary, sparse=FALSE)) %>% 
  st_union ->
  r6_military_zone

## Developed Areas ##
ca_places  %>% 
  filter(st_intersects(geometry, r6_boundary, sparse=FALSE)) %>% 
  st_simplify(dTolerance=0)  %>% 
  st_write("r6_developed_areas.geojson", append=FALSE)

ca_places  %>% 
  filter(st_intersects(geometry, r6_boundary, sparse=FALSE)) %>% 
  st_union() %>% 
  st_simplify() ->
  r6_developed

## Wilderness ##
wilderness %>% 
  filter(STATE=="CA") %>% 
  mapview

study_areas %>% 
  filter(st_intersects(geometry, r6_boundary, sparse=FALSE)) %>% 
  filter(!(st_intersects(geometry, r6_military_zone, sparse=FALSE))) %>% 
  filter(!(st_intersects(geometry, r6_developed, sparse=FALSE))) %>% 
  mutate(name=row_number()) ->
  study_areas_filtered

study_areas_filtered %>% mapview
  
    
  # st_transform("EPSG:3857") %>% 
  # st_set_crs("EPSG:3857") %>% 
  # st_write("r6_studyareas_nomilitary.geojson", append=FALSE)


study_areas[217,] -> bodie
bodie %>% 
  st_buffer(dist=5000) %>% 
  st_make_grid(cellsize=c(1000,1000)) -> bodie_grid

bodie %>% st_buffer(dist=5000) -> bodie_area
# pick a grid cell size
ovrGridSize <- 1000
# make grid and number cells
reqGrid <- st_make_grid(bodie_area, cellsize = ovrGridSize) %>% st_sf %>% 
  dplyr::mutate(id = 1:nrow(.))

# make bounding box
reqGridBbox <- st_bbox(bodie_grid)

# calculate number of rows/columns
nCols <- (reqGridBbox[3]-reqGridBbox[1])/ovrGridSize
nRows <- (reqGridBbox[4]-reqGridBbox[2])/ovrGridSize

# label by row / column number and combine labels
reqGrid.l <- reqGrid %>% 
  mutate(cols = rep(letters[1:nCols],nRows),
         rows = unlist(lapply(1:nRows, rep, nCols))) %>% 
  group_by(id) %>% 
  mutate(lab = paste(cols,rows,collapse='')) %>% 
  dplyr::filter(id %in% unlist(st_intersects(bodie_area,.))) # filter by intersection

# plot
ggplot(reqGrid.l) + 
  geom_sf(data=reqGrid.l) + 
  geom_label(data=reqGrid.l,
             aes(label = lab, geometry = geometry),
             stat = "sf_coordinates") +
  coord_sf(datum=st_crs(32119))

reqGrid.l %>% 
  transform(label=lab) %>%
  select(-lab) %>% 
  st_transform(3857) %>% 
  st_write("bodie_grid_large.kml", append=FALSE)



########### NOTES FOR NICK #############
study_areas %>% 
  filter(st_intersects(geometry, r6_boundary, sparse=FALSE)) %>% 
  filter(!(st_intersects(geometry, r6_military_zone, sparse=FALSE))) %>% 
  filter(!(st_intersects(geometry, r6_developed, sparse=FALSE))) %>% 
  mutate(name=row_number()) %>% 
  mapview
