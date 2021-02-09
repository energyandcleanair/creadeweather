#' Pseudo-function to import \strong{dplyr}'s common functions. 
#'
#' @importFrom dplyr select rename mutate mutate_at filter filter_at arrange distinct
#'     summarise summarize do group_by group_by_at ungroup do left_join inner_join everything bind_rows 
#'     pull tibble as_tibble rowwise any_vars all_vars vars collect
NULL

#' Pseudo-function to import \strong{tibble}'s common functions. 
#'
#' @importFrom tibble tibble
NULL


#' Pseudo-function to import and reexport \strong{magrittr}'s pipe. 
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL

' Pseudo-function to import sf functions
#'
#' @importFrom sf st_as_sf st_coordinates st_as_sfc st_buffer st_union st_centroid
NULL