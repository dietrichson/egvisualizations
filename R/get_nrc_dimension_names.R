#' Get NRC Emotions
#'
#' @param language
#'
#' @return a labeled character vector of the emotions used in the NRC dictionary
#' @export
get_nrc_dimension_names <- function(language=c('english','spanish')){
  if(language[1]=='spanish'){
    ret <-
      c('Enojo' = 'anger',
        'Anticipación' = 'anticipation',
        'Asco' = 'disgust',
        'Miedo' = 'fear',
        'Alegría' = 'joy',
        'Tristeza'= 'sadness',
        'Sorpresa' ='surprise',
        'Confianza'= 'trust')

  }

  if(language[1]=='english'){
    ret <-
      c('Anger' = 'anger',
        'Anticipation' = 'anticipation',
        'Disgust'      = 'disgust',
        'Fear'         = 'fear',
        'Joy'          = 'joy',
        'Sadness'      = 'sadness',
        'Surprise'     = 'surprise',
        'Trust'        =    'trust')
  }

  ret
}


