
#' Get parameters from a process using its process id
#'
#' @param process_id 
#'
#' @return
#' @export
#'
#' @examples
process_id_to_parameters <- function(process_id, key){
  processes <- rcrea::processes()
  process <- processes[processes$id == process_id,]
  parameters <- rjson::fromJSON(process[[key]])
  return(parameters)
}