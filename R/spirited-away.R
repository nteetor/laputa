#' Spirited Away Color Palette (Discrete)
#'
#' ...
#'
#' @export
spirited_away_pal <- function(type = 'seq', scheme = 1, reverse = FALSE) {
  if (!(type %in% c('qual', 'div', 'seq'))) {
    stop('`type` must be one of "qual", "div", or "seq"', call. = FALSE)
  }

  pal <- palette_helper('spirited-away', type, scheme)

  function(n) {
    if (n < 1) {
      stop('expected number of colors to be greater than 1', call. = FALSE)
    }

    if (type == 'qual') {
      cols <- pal[seq_len(n)]
    } else {
      cols <- colorRampPalette(pal)(n)
    }

    if (reverse) {
      cols <- reverse(cols)
    }

    cols
  }
}