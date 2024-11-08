## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lofifonts)

## -----------------------------------------------------------------------------
get_lofi_names('vector')

## ----vector-coords, fig.height = 3--------------------------------------------
library(lofifonts)
vector_text_coords("Hello", font = 'gridfont_smooth') |> 
  head()

## ----vector-raster, fig.height = 3--------------------------------------------
vector_text_raster("Hello", "gridfont_smooth") |>
  plot()

## ----vector-bespoke, fig.height = 3-------------------------------------------
library(ggplot2)

coords <- vector_text_coords("Hello", "gridfont_smooth")
head(coords)

ggplot(coords) +
  geom_path(aes(x, y, 
                group = interaction(char_idx, stroke_idx), 
                colour = as.factor(char_idx)), 
            linewidth = 4) +
  geom_point(aes(x, y), color = 'yellow') + 
  theme_bw() + 
  theme(legend.position = 'none') + 
  coord_equal()

