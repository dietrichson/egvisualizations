
#' Timeline Visualization of
#'
#' @param df The result of get_NRC_sentiment()
#' @param language english or spanish
#' @param dimensions include specific dimensions, or all (default)
#' @param dateTimeCol Where is the time
#' @import dplyr
#' @import reshape2
#' @return a ggplot with a timeline visualization
#' @export
syu_timeline <- function(df, language = 'english',
                         dimensions = c('all'),
                         dateTimeCol = 'dateTime' ){
  if(!has_emotion_cols(df))stop('Missing columns for emotions')
  df[[dateTimeCol]] <- df[[dateTimeCol]]
  # Find Specificity
  df <- df %>%
    group_by(dateTime=as.Date(dateTime)) %>%
    summarize_at(get_nrc_dimensions(), .fun='sum') %>%
    melt('dateTime')
  ggplot(df,aes(x=dateTime,y=value,color=variable))+geom_smooth(se=F)+
    scale_color_discrete(breaks=get_nrc_dimensions(), labels=names(get_nrc_dimension_names(language)))+
    xlab('Date/Time')
}

