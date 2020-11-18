require(magrittr)
if(!require(ggthemes)) install.packages(c('ggthemes')); require(ggthemes)


.CREAtheme <- new.env()

getpal <- function(x) {
  x %<>% textConnection %>% readLines
  x %>% gsub('.*#', '#', .) -> pal
  x %>% gsub(' #.*', '', .) %>% make.names -> names(pal)
  return(pal)
}

.CREAtheme$pal_crea <-
  "Dark blue #35416C
Blue #8cc9D0
Light blue #cce7eb
Turquoise #27a59c
Green #75b44c
Dark gray #333333
Light gray #cacaca" %>% getpal

.CREAtheme$pal_crea.change <-
  "Dark blue #35416C
Blue #8cc9D0
Light blue #cce7eb
Yellow #fff2cc
Orange #f6b26b
Red #cc0000
Dark red #990000" %>% getpal

.CREAtheme$pal_crea.heatmap <-
  "Black #000000
Dark violet #351c75
Dark purple #741b47
Dark red #990000
Red #cc0000
Orange #f6b26b
Yellow #fff2cc" %>% getpal

.CREAtheme$pal_crea.dramatic <- .CREAtheme$pal_crea.heatmap[c(4,2,3,1,5,6,7)]

.CREAtheme$crea_palettes <- list(CREA = .CREAtheme$pal_crea,
                                 change = .CREAtheme$pal_crea.change,
                                 heatmap = .CREAtheme$pal_crea.heatmap,
                                 dramatic = .CREAtheme$pal_crea.dramatic)

.CREAtheme$makepal <- function(pal, alpha=1, col.index=T) {
  cols <- paste0(crea_palettes[[pal]][col.index],
                 format(as.hexmode(round(alpha*255, 0)), width=2))
  scales::manual_pal(unname(cols))
}

.CREAtheme$makegrad <- function(pal, alpha=1, bias=1, reverse.order=F) {
  cols <- paste0(crea_palettes[[pal]],
                 format(as.hexmode(round(alpha*255, 0)), width=2))
  if(reverse.order) cols <- rev(cols)
  function(x) { scales::col_numeric(unname(cols), c(0,1))(x^bias) }
}


.CREAtheme$scale_color_crea_d <- function(palette = "CREA", alpha = 1, col.index=T, ...) {
  discrete_scale("colour", palette, makepal(palette, alpha, col.index), ...)
}

.CREAtheme$scale_fill_crea_d <- function(palette = "CREA", alpha = 1, col.index=T,...) {
  discrete_scale("fill", palette, makepal(palette, alpha, col.index), ...)
}

.CREAtheme$scale_color_crea_c <- function(palette = "CREA", alpha = 1, reverse.order=F, ...) {
  continuous_scale("colour", palette, makegrad(palette, alpha, reverse.order), ...)
}

.CREAtheme$scale_fill_crea_c <- function(palette = "CREA", alpha = 1, reverse.order=F, ...) {
  continuous_scale("fill", palette, makegrad(palette, alpha, reverse.order), ...)
}

.CREAtheme$theme_crea <- function(base_size=11, ...) {
  (theme_calc(base_size=base_size) +
     theme(#title = element_text(family='SourceSansPro'),
           plot.title = element_text(size=rel(2), face='bold', color=unname(pal_crea['Dark.blue'])),
           plot.subtitle = element_text(face='italic', color='black'),
           plot.caption = element_text(face='italic', color=pal_crea['Dark.blue']),
           strip.background = element_rect(fill=pal_crea['Blue'],
                                           linetype=0),
           plot.background = element_rect(color='white', fill='white'),
           ...))
}

.CREAtheme$crea.theme <- function(colors='CREA', reverse.order=F) {
  pars = standard.theme(color = FALSE)
  pars$strip.background$col <- pal_crea['Blue']
  pars$par.main.text$col <- pal_crea['Dark.blue']
  pars$par.main.text$cex <- 1.5
  cols <- colorRampPalette(crea_palettes[[colors]])(100)
  if(reverse.order) cols %<>% rev
  pars$regions$col <- cols
  return(pars)
}


attach(.CREAtheme)

c('ribbon', 'col', 'area', 'bar') %>% lapply(ggplot2::update_geom_defaults, list(fill = pal_crea[2])) -> t1
c('line', 'point') %>% lapply(ggplot2::update_geom_defaults, list(color = pal_crea[1])) -> t1
rm(t1)
rm(getpal)

print('CREA theme loaded')