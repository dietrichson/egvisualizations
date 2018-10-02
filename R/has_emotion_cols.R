#' Validate Syuzhet data.frame
#'
#' Checks that the data.frame passed to it is indeed the result of a syuzhet analysis.
#' Checks to see if the correct columns exist in the data.frame.
#' @param df
#'
#' @return TRUE if it does FALSE if not.
#' @export

has_emotion_cols <- function(df){
 ( sum(names(df)%in%get_nrc_dimensions()) == 8 )
}
