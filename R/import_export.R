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

#' @importFrom dplyr ungroup group_by rowwise mutate bind_rows distinct pull select
NULL

#' @importFrom lubridate date
NULL
=======
#' @importFrom lubridate date ymd
NULL

#' @importFrom tidyr unnest gather spread nest complete
NULL

#' @importFrom glue glue
NULL
