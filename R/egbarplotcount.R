#' Return a ggplot barplot with configuration by default for Ethergeist.
#'
#' @description
#' Plot based on ggplot2 package.
#' @author Pablo Pagnone
#' @import ggplot2
#' @export
egbarplotcount <- function(data, xCol, fillCol, xlab = "", ylab = "", legendlab = "", coord_flip = FALSE){

  result <- ggplot(data) + geom_bar(aes(data[,xCol], y = (..count..), fill=data[,fillCol])) +
                           labs(fill = legendlab, x=xlab, y=ylab) +
                           theme(axis.text.x = element_text(angle = 90, hjust = 1))

  if(coord_flip) {
    result <- result + coord_flip()
  }

  result
}
