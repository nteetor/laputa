palette_helper <- function(name, type = c('seq', 'div', 'qual'), scheme) {
  verify(
    is.character(name), is.character(type),
    is.numeric(scheme) || is.character(scheme)
  )

  if (!(name %in% names(.palettes))) {
    stop('unknown palette ', name, call. = FALSE)
  }

  type <- match.arg(type)
  if (!(type %in% names(.palettes[[name]]))) {
    stop('unknown type ', type, ' for palette ', name, call. = FALSE)
  }

  if (is.character(scheme) && !(scheme %in% names(.palettes[[name]][[type]]))) {
    stop('unknown scheme ', scheme, ' for type ', type, ' and palette ',
         name, call. = FALSE)
  }
  if (is.numeric(scheme) && scheme > length(.palettes[[name]][[type]])) {
    stop('scheme index ', scheme, ' out of bounds for type ', type,
         ' and palette ', name, call. = FALSE)
  }

  .palettes[[name]][[type]][[scheme]]
}

prev_palette <- function(pal, n) {
  scales::show_col(colorRampPalette(pal)(n))
}

#' Color Sets Used By Miyazaki Package
#'
#'
#' @keywords internal
#' @export
.palettes <- list(
  laputa = list(
    values = c('#618BCC', '#336BBF', '#304337', '#4E693C', '#78A571', '#8E3637', '#A67851',
               '#975D37', '#E1D145', '#D9DA75'),
    seq = list(
      blues = c("#DEEBF7", "#618BCC", "#336BBF"),
      greens = c("#E5F5E0", "#78A571", "#4E693C"),
      browns = c("#FEE6CE", "#A67851", "#975D37"),
      yellows = c("#FFFFE5", "#D9DA75", "#E1D145")
    ),
    div = list(
      brgn = c('#975D37', '#A67851', '#F5F5F5', '#78A571', '#304337'),
      rdylgn = c('#8E3637', '#E1D145', '#FFFFBF', '#78A571', '#304337'),
      spectral = c('#8E3637', '#E1D145', '#FFFFBF', '#78A571', '#336BBF')
    ),
    qual = list(
      set1 = c('#8E3637', '#336BBF', '#4E693C', '#A67851', '#E1D145'),
      paired = c('#618BCC', '#336BBF', '#78A571', '#4E693C', '#A67851',
                 '#975D37', '#D9DA75', '#E1D145')
    )
  ),
  `spirited-away` = list(
    values = c('#58A557', '#2D9B8E', '#00285C', '#8C7F99', '#000000', '#ECE3D4',
               '#73787B', '#6D3A1D', '#92272D', '#B93D47', '#E06168'),
    seq = list(
      greys = c('#FFFFFF', '#73787B', '#000000'),
      pinks = c('#F7F4F9', '#E06168', '#B93D47'),
      pkrd = c('#E06168', '#B93D47', '#92272D'),
      purd = c('#ECE3D4', '#8C7F99', '#B93D47'),
      crgnbu = c('#ECE3D4', '#2D9B8E', '#00285C')
    ),
    div = list(
      pkbu = c('#E06168', '#ECE3D4', '#00285C'),
      rdbu = c('#92272D', '#ECE3D4', '#00285C'),
      brgn = c('#6D3A1D', '#ECE3D4', '#58A557'),
      pkgn = c('#E06168', '#ECE3D4', '#58A557'),
      pugn = c('#8C7F99', '#ECE3D4', '#58A557'),
      spectral = c('#92272D', '#6D3A1D', '#ECE3D4', '#58A557', '#00285C')
    ),
    qual = list(
      set1 = c('#92272D', '#00285C', '#58A557', '#6D3A1D', '#E06168', '#73787B')
    )
  ),
  kikis = list(
    values = c('#4298A2', '#2888A5', '#1F3F6A', '#1D4A9B', '#8E8656', '#D4D676',
               '#3F416B', '#CAB9B3', '#2A0909', '#C33A43')
  )
)
