## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggplot2)
library(lofifonts)

## ----vectorsheets, fig.height = 3, fig.width = 4------------------------------
lo <- utf8ToInt('a')
hi <- utf8ToInt('z')
txt <- intToUtf8(seq(lo, hi))

for (font_name in get_lofi_names('vector')) {
  coords <- vector_text_coords(txt, font_name)
  
  p <- ggplot(coords) +
    geom_path(aes(x0, y0, group = stroke_idx)) +
    facet_wrap(~char_idx, ncol = 9)+
    theme_minimal(15) +
    coord_equal() + 
    labs(title = font_name) +
    theme(
      strip.text = element_blank(),
      axis.text  = element_blank(),
      axis.title = element_blank()
    )
  
  print(p)
}

