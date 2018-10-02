#' Circular Plot of Emotions
#'
#' @param df
#' @param language english or spanish
#' @import ggplot2
#' @import dplyr
#' @return a ggplot
#' @export

syu_circular <- function(df, language='english', grouping.var=NULL){

  myOrder <- c('anticipation', 'trust', 'anger', 'joy', 'surprise', 'fear', 'disgust', 'sadness')

  get_emotion_tally(df, grouping.var=grouping.var) %>%
    mutate(variable=factor(variable, levels= myOrder)) %>%
    arrange(variable) %>%
    ggplot(aes(variable,value)) +
    geom_point(color='red') +
    geom_segment(aes(x=variable,xend=variable,y=0,yend=value),color='red')+coord_polar() +
    scale_x_discrete(breaks=get_nrc_dimension_names(), labels=names(get_nrc_dimension_names()) ) +
    scale_y_discrete(labels=NULL)+
    labs(x = NULL, y=NULL)

}
