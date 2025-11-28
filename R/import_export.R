#' Package-level imports and exports
#'
#' - Export pipe operator
#' - Centralize package imports
#'
#' @name creadeweather-imports
#' @keywords internal
NULL

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Core plotting and data imports
#'
#' Keep heavy imports here to avoid attaching in Depends.
#'
#' @keywords internal
#' @import ggplot2
#' @importFrom worldmet importNOAA
NULL

#' @importFrom dplyr all_of all_vars any_vars arrange bind_rows distinct filter
#'   filter_at first full_join group_by group_by_at left_join mutate mutate_at
#'   mutate_if pull rowwise select starts_with summarise ungroup
NULL

#' @importFrom lubridate date ymd
NULL

#' @importFrom tidyr unnest gather spread nest complete
NULL

#' @importFrom glue glue
NULL

#' @importFrom tibble tibble
NULL

#' @importFrom raster brick
NULL

#' @importFrom rlang "%||%"
NULL

#' @importFrom terra rast
NULL
