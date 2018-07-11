#' Return a ggplot timeseries with configuration by default for Ethergeist.
#'
#' @description
#' Plot based on ggplot2 package.
#' @param df - data.frame to plot
#' @param colFill - Column to use as fill and group.
#' @param labcolor - Lab for color.
#' @param labx - Lab for X
#' @param laby - Lab for Y
#' @author Pablo Pagnone
#' @import ggplot2
#' @import dplyr
#' @export
egdataseries <- function(df, colDate, colFill, ylab= "Date", xlab ="Count", legendlab = "") {

  df$date <- as.Date(df[,colDate])
  dates <- data.frame(date = seq(min(df[,"date"]), max(df[,"date"]), "day"))
  entities <- df %>% distinct_(colFill)
  expected <- base::merge(dates,entities)

  dataSeries <- df %>% group_by_("date", colFill) %>% summarise(count = n())
  dataSeriesFinal <- expected %>% left_join(dataSeries, by=c(colFill, "date"))
  dataSeriesFinal$count <- ifelse(is.na(dataSeriesFinal$count), 0, dataSeriesFinal$count)

  ggplot(dataSeriesFinal, aes(date, count, group=dataSeriesFinal[,colFill], color=dataSeriesFinal[,colFill])) +
        geom_line() +
        scale_x_date(date_labels = "%d-%m", date_breaks = "1 day") +
        theme(axis.text.x = element_text(angle = 0, size = 8),
              legend.position = "right") +
        labs(color = legendlab, x=xlab, y=ylab)

}
