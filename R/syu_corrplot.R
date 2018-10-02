#' Correlation Plot
#'
#' @param df output from get_nrc_sentiment
#' @param ... passed to ggcorrplot
#' @import ggcorrplot
#' @return a ggcorrplot
#' @export
syu_corrplot <- function(df, ...){
  get_emotion_correlation(df) %>%
    ggcorrplot(...)
}
