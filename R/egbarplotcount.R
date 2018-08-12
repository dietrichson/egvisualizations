#' Return a ggplot barplot with configuration by default for Ethergeist.
#'
#' @description
#' Plot based on ggplot2 package.
#' @param data
#' @param xCol
#' @param yCol
#' @param fillCol
#' @param xlab
#' @param ylab
#' @param legendlab
#' @param coord_flip
#' @param geom_bar_stat - Usually are used "count" or "identity"
#' @author Pablo Pagnone
#' @import ggplot2
#' @export
egbarplotcount <- function(data,
                           xCol,
                           yCol,
                           fillCol = NA,
                           xlab = "",
                           ylab = "",
                           legendlab = "",
                           coord_flip = FALSE,
                           geom_bar_stat = "count"){

  if(!is.data.frame(data)){
    stop("data must be a data.frame.")
  }
  if(!xCol %in% colnames(data)){
    stop("Column xCol not exist in data.frame 'data'.")
  }

  if(!is.na(fillCol) && !fillCol %in% colnames(data)) {
    stop("Column fillCol not exist in data.frame 'data'.")
  }

  if(!is.na(fillCol)) {
    result <- ggplot(data) + geom_bar(aes_string(xCol, y = yCol, fill=fillCol), stat=geom_bar_stat) +
                             labs(fill = legendlab, x=xlab, y=ylab) +
                             theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {
    result <- ggplot(data) + geom_bar(aes_string(xCol, y = yCol), stat=geom_bar_stat) +
                             labs(x=xlab, y=ylab) +
                             theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }

  if(coord_flip) {
    result <- result + coord_flip()
  }

  result
}
