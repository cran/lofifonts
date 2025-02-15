% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bitmap-text.R
\name{bitmap_text_matrix}
\alias{bitmap_text_matrix}
\title{Create a binary matrix of the rendered text}
\usage{
bitmap_text_matrix(
  text,
  font = "unifont",
  dx = 0L,
  dy = 0L,
  scale_matrix = 1,
  missing = NULL
)
}
\arguments{
\item{text}{Single text string. Can include carriage returns to split text 
over multiple lines.}

\item{font}{Name of bitmap font, or a 'lofi' font object.  Default: 'unifont'.
Use \code{get_lofi_names('bitmap')} to retrieve a list of all valid
bitmap fonts included in this package.  To create a 'lofi' font object
use \code{\link{convert_bm_font_to_lofi}()}}

\item{dx}{Additional character spacing in the horizontal direction. Default: 0}

\item{dy}{Additional character spacing in the vertical direction i.e. between 
rows of text. Default: 0}

\item{scale_matrix}{Integer size scale factor. Default: 1.  Must be an integer value >= 1.
Scale up the matrix or raster result by this factor}

\item{missing}{Codepoint to use if glyph not found in font. 
Default: NULL means to use the default specified by the font internally.
Otherwise it will default to the codepoint for '?'}
}
\value{
Binary matrix representation of the rendered text
}
\description{
Create a binary matrix of the rendered text
}
\examples{
bitmap_text_matrix('Hi')
}
\seealso{
Other bitmap text functions: 
\code{\link{bitmap_text_coords}()},
\code{\link{bitmap_text_raster}()}
}
\concept{bitmap text functions}
