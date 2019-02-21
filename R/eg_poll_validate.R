#' Validate Poll Dataframe
#'
#' @param poll a data.frame to validate
#'
#' @return nothing, but stops on error
#' @export
eg_poll_validate <- function(poll){
  #Data validation
  if ( missing(poll) ) 
    stop("Need Polling Data")
  if ( sum(c('Date','value','variable','fullname') %in% names(poll)) < 4 )  
    stop('Columns are missing in polls data.frame.')
}