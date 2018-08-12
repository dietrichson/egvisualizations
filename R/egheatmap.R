#' Return a ggplot heatmap with configuration by default for Ethergeist.
#'
#' @description
#' Plot based on ggplot2 package.
#'
#' @param data- data.frame to plot
#' @param colx
#' @param coly
#' @param colFill - Column to use as fill and group.
#' @param xlab
#' @param ylab
#' @param legendlab
#' @author Pablo Pagnone
#' @import ggplot2
#' @export
egheatmap <- function(data, colx, coly, colfill, xlab ="", ylab= "", legendlab = ""){
  ggplot(data = data, aes_string(x=colx, y=coly, fill=colfill)) +
         geom_tile(colour = "white") +
         theme(panel.background=element_rect(fill="white", colour="white")) +
         labs(fill = legendlab, x=xlab, y=ylab) +
         scale_fill_gradient(low="pink", high="red") +
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
