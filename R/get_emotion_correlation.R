#' Correlation Between Emotions
#'
#'Calculates the correlation between emotions present in the df
#' @param df a data.frame from syuzhet::get_nrc_sentiment()
#'
#' @return
#' @export
#' @import stats
#' @examples
get_emotion_correlation <- function(df){
  if(!has_emotion_cols(df))stop("The data.frame does not have emotion columns.")
  df %>%
    select(get_nrc_dimension_names()) %>%
    cor()
}
