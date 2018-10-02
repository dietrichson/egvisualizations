#' Circular Plot of Emotions
#'
#' @param df
#' @param language english or spanish
#' @param ... other parameters passed to eg_radar_plot
#' @import ggplot2
#' @import dplyr
#' @return a ggplot
#' @export

syu_radar <- function(df, fun='mean',grouping.var=NULL,...){

  myOrder <- c('anticipation', 'trust', 'anger', 'joy', 'surprise', 'fear', 'disgust', 'sadness')
  myDims <- get_nrc_dimensions()
  
  tmp <- get_emotion_summary(df,grouping.var = grouping.var, fun=fun)
  
  tmp[myDims] <- tmp[myDims]/max(tmp[myDims])
  colnames(tmp) <- c('group',get_nrc_dimensions())
  tmp %>% eg_radar_plot(grouping.var = 'group',...)

}
