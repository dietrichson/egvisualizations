#' Get NRC Emotions
#'
#' @param language
#'
#' @return a labeled character vector of the emotions used in the NRC dictionary
#' @export
get_nrc_dimensions <- function(language=c('english','spanish')){
  c( 'anger',
     'anticipation',
     'disgust',
     'fear',
     'joy',
     'sadness',
     'surprise',
     'trust')
}


