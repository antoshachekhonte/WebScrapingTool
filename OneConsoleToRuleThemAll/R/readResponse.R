#' Writing function to obtain user input...
#' @param p text prompt to guide user
#' @return returns response entered by user
#' @export 
readResponse <- function(p="Enter Value:"){
  r <- readline(prompt=p)
  return(as.character(r))
}