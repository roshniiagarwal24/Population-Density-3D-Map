#installing all required packages
install.packages("rayshader")
install.packages("sf")
install.packages("MetBrewer")
install.packages("tidyverse")
install.packages("colorspace")
install.packages("geodata")


#loading required libraries
library(sf) #can read data
library(rayshader) #3d plotting using rayshader
library(geodata) #download administrative boundaries data
library(MetBrewer) #different colour palettes
library(colorspace) #tools to work with colour palette
library(tidyverse) #glimpse,ggplot and other functions



# loading kontur data that has population density data
data <- st_read("dataset/kontur_population_IN_20220630.gpkg")

# loading administrative boundaries data
india <- gadm(country="India", level = 3, path= "~/Population-Density-3D-Map/dataset", version="latest")

#converting spatial object into sf object
st <- sf::st_as_sf(india)

# filtering Delhi from the dataset
Delhi <- st |> 
  filter(NAME_1 == "NCT of Delhi") |>
  st_transform(crs=crs(data))

#plotting Delhi map
Delhi |> 
  ggplot() +
  geom_sf()

# do intersection on data to limit kontur dataset to delhi
st_delhi <-st_intersection(data, Delhi)

# define aspect ratio based on bounding box
bb <- st_bbox(st_delhi)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

# checking boundaries by plotting points
Delhi |> 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

width <- st_distance(bottom_left, bottom_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side
if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ratio <- 1
  w_ratio <- width / height
}

# convert to raster so we can then convert to matrix
size <- 5000

delhi_rast <- st_rasterize(st_delhi, 
                          nx = floor(size * h_ratio),
                          ny = floor(size * w_ratio))

mat <- matrix(delhi_rast$population, 
              nrow = floor(size * h_ratio),
              ncol = floor(size * w_ratio))

# create color palette
c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

# plotting the 3d map
mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat,
          zscale = 100 / 5,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = -20, phi = 45, zoom = .8)
outfile <- "images/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}

#rgl::rgl.close() - use this for closing the map window if required
