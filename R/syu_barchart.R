#' Barchart Visualization
#'
#' @param df
#' @param language
#'
#' @return
#' @import ggplot2
#' @export
syu_barchart <- function(df, language='english'){
  ggplot(get_emotion_tally(df),
         aes(x=variable,y=value))+
    geom_bar(stat='identity')+
    xlab('Emotion')+ylab('Value')+
    scale_x_discrete(breaks = get_nrc_dimension_names(), labels = names(get_nrc_dimension_names()))
}
