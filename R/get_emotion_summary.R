#' Get Summary of Emitions in a syuzhet df
#'
#' @param df
#' @param grouping.var
#'
#' @return
#' @export
get_emotion_summary <- function(df, grouping.var=NULL, fun='sum'){
  if(!has_emotion_cols(df))stop('Emotion columns not found. The data frame needs to include these columns: ', get_nrc_dimensions() )
  myDims <- get_nrc_dimensions()
  tmp <- df[c(myDims,grouping.var)]
  if(!is.null(grouping.var))
    tmp <- tmp %>% group_by_(grouping.var)
  tmp <- tmp %>%  summarize_at(.vars=myDims, .funs=c(x=fun))
  names(tmp) <- c(grouping.var, get_nrc_dimensions())
  tmp
}
