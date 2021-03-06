#' Laputa Color Palette (Discrete)
#'
#' ...
#'
#' @param n Number of colors
#'
#' @export
laputa_pal <- function(type = 'seq', scheme = 1, reverse = FALSE) {
  if (!(type %in% c('qual', 'div', 'seq'))) {
    stop('`type` must be one of "qual", "div", or "seq"', call. = FALSE)
  }

  pal <- palette_helper('laputa', type, scheme)

  function(n) {
    if (n < 0) {
      stop('expected number of colors to be greater than 1', call. = FALSE)
    }

    if (type == 'qual') {
      cols <- pal[seq_len(n)]
    } else {
      cols <- colorRampPalette(pal)(n)
    }

    if (reverse) {
      cols <- rev(cols)
    }

    cols
  }
}
