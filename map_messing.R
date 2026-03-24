
# A 2x2 matrix
delemere_bb <- matrix(data = c(-2.55, -2.45, 53.2, 53.3),
                    nrow = 2,
                    byrow = TRUE)
# Update column and row names
colnames(delemere_bb) <- c("min", "max")
rownames(delemere_bb) <- c("x", "y")
# Print the matrix to the console
delemere_bb

available_tags(feature = "water")


delemere <- delemere_bb %>%
opq() %>%
  add_osm_feature(key = "natural", 
                  value = c("water", "wetland")) %>%
  osmdata_sf()

delemere


street_plot <- ggplot() +
  geom_sf(data = delemere$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.2)
# Print the plot
street_plot

available_tags("natural")

plot(delemere_bb)


library(terra)

r <- rast("C:/Users/ehonan/Downloads/Download_aerial_image_2953620/getmapping-rgb-25cm-2023_6348331/sj/sj5571_rgb_250_04.jpg")
plotRGB(r)

files <- list.files("C:/Users/ehonan/Downloads/Download_aerial_image_2953620/getmapping-rgb-25cm-2023_6348331/sj/", pattern = "\\.jpg$|\\.tif$|\\.jp2$", full.names = TRUE)

rasters <- lapply(files, rast)

merged <- do.call(mosaic, rasters)

plotRGB(merged)


########
folder <- "C:/Users/ehonan/Downloads/Download_aerial_image_2953620/getmapping-rgb-25cm-2023_6348331/sj/"

files <- list.files(folder, pattern = "\\.jpg$", full.names = TRUE)

rasters <- lapply(files, rast)

# check that extents are real map coordinates
lapply(rasters, ext)
#http://127.0.0.1:25136/graphics/plot_zoom_png?width=1186&height=863
# assign CRS manually
rasters <- lapply(rasters, function(x) {
  crs(x) <- "EPSG:27700"
  x
})

# now merge/mosaic
merged <- do.call(mosaic, rasters)

plotRGB(merged)

ext(350000, 360000, 370000, 380000)

# define your bounding box (in EPSG:27700!)
bb <- ext(350000, 356000, 370500, 371500)

# plot map
plotRGB(merged)

# overlay bounding box
rect(bb, border = "red", lwd = 2)

bb <- ext(354500, 355600, 370500, 371500)

plotRGB(merged, axes = T)
grid()

rect(
  xmin(bb), ymin(bb),
  xmax(bb), ymax(bb),
  border = "red", lwd = 2
)

merged_crop <- crop(merged, bb)

plotRGB(merged_crop)



library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# main raster and bounding box
# merged
# bb2 <- ext(354500, 356000, 371000, 371700)


library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# UK outline
uk <- ne_countries(country = "United Kingdom", returnclass = "sf")

# use the extent of the cropped raster for the overview box
crop_poly <- as.polygons(ext(merged_crop), crs = crs(merged_crop))
crop_sf <- st_as_sf(crop_poly)
crop_sf_ll <- st_transform(crop_sf, st_crs(uk))

# save current plotting settings
op <- par(no.readonly = TRUE)

# main map
par(fig = c(0, 1, 0, 1), mar = c(3, 3, 1, 1))
plotRGB(merged_crop, axes = TRUE)

# optional: draw crop outline on the main map
plot(crop_poly, add = TRUE, border = "red", lwd = 2)

# inset
par(fig = c(0.62, 0.95, 0.62, 0.95), new = TRUE, mar = c(0, 0, 0, 0))
plot(st_geometry(uk), col = "grey90", border = "black")
plot(st_geometry(crop_sf_ll), add = TRUE, border = "red", lwd = 2)
box()

# restore plotting settings
par(op)

####
# 1. Get UK map
uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf")

# 2. Turn extent of merged_crop into a polygon
crop_poly <- as.polygons(ext(merged_crop), crs = crs(merged_crop))
crop_sf <- st_as_sf(crop_poly)

# 3. Transform crop extent to the CRS of the UK map
crop_sf_ll <- st_transform(crop_sf, st_crs(uk))

# 4. Save plotting settings
op <- par(no.readonly = TRUE)

# 5. Main map
par(fig = c(0, 1, 0, 1), mar = c(4, 4, 1, 1))
plotRGB(merged_crop, axes = FALSE, asp = 1)

axis(1)
axis(2)
mtext("Easting (m)", side = 1, line = 2.5)
mtext("Northing (m)", side = 2, line = 2.5)

# 6. Inset in bottom-right
par(fig = c(0.68, 0.96, 0.06, 0.34), new = TRUE, mar = c(0, 0, 0, 0))

plot(
  st_geometry(uk),
  col = "grey90",
  border = "black",
  axes = FALSE
)

plot(
  st_geometry(crop_sf_ll),
  add = TRUE,
  border = "red",
  lwd = 2
)

box()

# 7. Restore plotting settings
par(op)


#####
df <- as.data.frame(merged_crop, xy = TRUE)
names(df)[3:5] <- c("r", "g", "b")

plot <- ggplot(df) +
  geom_raster(aes(x = x, y = y, fill = rgb(r, g, b, maxColorValue = 255))) +
  scale_fill_identity() +
  coord_equal() +
  theme_minimal()

head(df)


uk <- ne_countries(country = "United Kingdom", scale = "medium", returnclass = "sf")

crop_poly <- as.polygons(ext(merged_crop), crs = crs(merged_crop))
crop_sf <- st_as_sf(crop_poly)
crop_sf_ll <- st_transform(crop_sf, st_crs(uk))


p_main <- ggplot(df) +
  geom_raster(aes(x = x, y = y, fill = rgb(r, g, b, maxColorValue = 255))) +
  scale_fill_identity() +
  coord_equal() +
  theme_bw()

p_inset <- ggplot() +
  geom_sf(data = uk, fill = "grey90", color = "black") +
  geom_sf(data = crop_sf_ll, color = "red") +
  coord_sf(xlim = c(-8, 2), ylim = c(49, 59)) +
  theme_void()

ggdraw() +
  draw_plot(p_main) +
  draw_plot(p_inset, x = 0.15, y = 0.1, width = 0.3, height = 0.3)


p_main <- ggplot(df) +
  geom_raster(aes(x = x, y = y, fill = rgb(r, g, b, maxColorValue = 255))) +
  scale_fill_identity() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)") +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    bar_cols = c("black", "white"),
    text_cex = 0.8,
    line_width = 0.7
  ) +
  theme_minimal()

p_main