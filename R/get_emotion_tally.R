#' Tally of Emotions
#'
#'
#' @param df
#' @import dplyr
#' @import reshape2
#' @return a data.frame with variable and Value
#' @export
get_emotion_tally <- function(df, grouping.var=NULL){
  tmp <- get_emotion_summary(df,grouping.var = grouping.var)
  tmp%>%
    melt (id.vars = c(grouping.var))
}
