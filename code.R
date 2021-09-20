rm(list=ls())

# ----- Definitions
    # map = zone shapefiles
    # r = population data as raster

# ----- Call libraries
library(sf)
library(tidyverse)
library(magrittr)
library(rio)
library(raster)
library(tmap)
library(RColorBrewer)

# ----- Define 'not in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

# ----- set working directory
setwd("/Users/jeppeviero/Dropbox/02 PhD/18 tutorials/extract")

# ----- Load map data
map <- st_read(dsn = "ne_10m_admin_0_countries/ne_10m_admin_0_countries",
               layer = "ne_10m_admin_0_countries",
               crs = 4326)

# ----- Filter map data to Denmark and Sweden
map <- map %>% 
  filter(GEOUNIT == "Denmark" | GEOUNIT == "Sweden" | GEOUNIT == "Norway" | GEOUNIT == "Germany" | GEOUNIT == "Finland") %>% 
  dplyr::select(geometry,
                GEOUNIT)

map <- st_crop(map,
               st_bbox(c(xmin = 0, xmax = 32,
                         ymin = 45, ymax = 71),
                       crs = st_crs(map)))

# ----- Create a borderless map
map_union <- map %>% 
  st_union() %>% 
  st_as_sf()

# ----- Plot map data
ggplot(map) + 
  geom_sf(aes(fill = GEOUNIT), alpha = 0.4)

ggplot(map_union) + 
  geom_sf(fill = "steelblue1", alpha = 0.4)


# ----- Load raster data
r <- raster("2000AD_pop/popc_2000AD.asc")
proj4string(r) <- CRS("+init=epsg:4326")

r <- crop(r, extent(map_union))
r <- mask(r, map_union)

# ----- Log transform the underlying data (for visual purposes), keep original
df <- r@data@values %>%
  as.data.frame() %>%
  dplyr::mutate(row_id = row_number())

colnames(df) <- c("hyde_pop", "row_id")

df <- df %>%
   mutate(logpop = log(hyde_pop + 1))

r2 <- r
values(r2) <- df$logpop

# ----- Plot raster data
col_scale <- brewer.pal(9, "Greens")

tm_shape(r2) +
  tm_raster(palette = col_scale) +
  tm_legend(position = c("right", "bottom"),
            title = "",
            legend.show = T,
            legend.outside = F,
            legend.title.size = .01,
            legend.title.color = "white")

# ----- Extract raster data (from original)
pop_data <- extract(r, map)
map$pop_data <- unlist(lapply(pop_data,
                              function(x) if (!is.null(x)) sum(x, na.rm=TRUE) else NA ))


# ----- Calculate area
map <- map %>% 
  mutate(area = st_area(.), # m^2
         area_num = unclass(st_area(.))) %>% 
  mutate(area_km2 = area_num / 1000000) # km^2

# ----- Calculate population density
map <- map %>% 
  mutate(pop = pop_data * 1000) %>% 
  mutate(pop_dens = pop / area_km2)

# ----- Plot it
library(hrbrthemes)

ggplot(map) + 
  geom_sf(aes(fill = pop_dens), alpha = 0.4) +
  scale_fill_distiller(direction = 1, 
                       name = "Pop. density (1,000 / km2)") +
  theme_ipsum()
  
