# install.packages("igraph")
library(igraph)
set.seed(123) # for reproducibility
g <- sample_pa(n=6, m=2, directed=TRUE) 
# Find the edges by their end-points and delete them
edges_to_delete <- c( E(g)[4 %->% 1], E(g)[5 %->% 1] )
g <- delete_edges(g, edges_to_delete)
# Add the edge from node 6 to node 4
g <- add_edges(g, c(6, 4))

# Calculate the standard circular layout
l <- layout_in_circle(g)

# Define a rotation function
rotate_layout <- function(layout, angle_rad) {
  x <- layout[, 1]
  y <- layout[, 2]
  
  # Rotation transformation
  new_x <- x * cos(angle_rad) - y * sin(angle_rad)
  new_y <- x * sin(angle_rad) + y * cos(angle_rad)
  
  cbind(new_x, new_y)
}

# Rotate by 45 degrees (for example)
angle_degrees <- 30
angle_radians <- angle_degrees * (pi / 180)

l_rotated <- rotate_layout(l, angle_radians)

png("network_plot.png", width=2000, height=2000, res=300)  # 2000x2000 pixels at 300 dpi
plot(g, 
     layout             = l_rotated, 
     edge.arrow.size    = 3,  # adjust this for arrow size
     edge.width         = 5,       # thicker edge lines
     edge.color         = "black", # make arrows darker
     
     vertex.frame.color = "black",  # hollow nodes with black border
     vertex.color       = NA,# no fill color for hollow effect
     vertex.size        = 30, 
     vertex.label       = NA,   # no numbering on the nodes
     vertex.frame.width = 5,  # thicker node border lines
     vertex.label.cex   = 0.8)
dev.off()  # Close the graphics device


# getwd()
library(hexSticker)
library(ggplot2)
library(magick)
library(tidyverse)
# getwd()
img <- image_read(file.path(getwd(), "network_plot.png"))

geom_pkgname("causalverse")

sticker(
  subplot  = img,
  package  = "Causal Verse",
  p_color  = "black",
  p_size   = 13,
  p_x      = 1, 
  p_y      = 1,
  p_fontface = "bold",
  s_width  = 2.3,
  s_height = 2.3,
  s_x      = 0.95,
  s_y      = 0.95,
  
  
  h_fill   = "white", 
  h_color  = "black", 
  h_size   = 1.5
) |> print()
save_sticker("logo.png")
