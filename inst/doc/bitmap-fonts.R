## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lofifonts)

## -----------------------------------------------------------------------------
get_lofi_names('bitmap')

## ----bitmap-coords, fig.height = 3--------------------------------------------
library(lofifonts)
bitmap_text_coords("Hello", font = 'unifont') |>
  head()

## ----bitmap-raster, fig.height = 3--------------------------------------------
bitmap_text_raster("Hello", "unifont") |>
  plot()

## ----bitmap-bespoke, fig.height = 4, fig.width = 8----------------------------
library(grid)
coords <- bitmap_text_coords("Hello\n#RStats", "spleen-6x12")
head(coords)

grid.newpage()
grid.rect(
  x = coords$x * 4, 
  y = coords$y * 4, 
  width  = 3, 
  height = 3, 
  default.units = 'mm',
  gp = gpar(fill = rainbow(nrow(coords)), col = NA)
)

