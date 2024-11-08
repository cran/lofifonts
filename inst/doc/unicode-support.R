## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(lofifonts)

## ----unicode, fig.height = 3, fig.width = 8-----------------------------------
txt <- "二項分布\xF0\x9F\x8E\xB2の英語表記は\n「Binomial distribution」である。"

bitmap_text_raster(txt, "unifont") |>
  plot()

## -----------------------------------------------------------------------------
codepoints <- c(0x1f44d, 0x1f44e)

unifont <- get_lofi_font('unifont')
unifont
# Unifont has both of these
codepoints %in% unifont$glyph_info$codepoint
bitmap_text_raster(intToUtf8(codepoints), 'unifont') |> plot()

spleen <- get_lofi_font('spleen-5x8')
spleen
# This spleen font has neither
codepoints %in% spleen$glyph_info$codepoint
bitmap_text_raster(intToUtf8(codepoints), 'spleen-5x8') |> plot()

