#' Return a ggplot timeseries with configuration by default for Ethergeist.
#'
#' @description
#' timeseries Plot based on ggplot2 package.
#'
#' @param df - data.frame to plot
#' @param colFill - Column to use as fill and group.
#' @param labcolor - Lab for color.
#' @param laby - Lab for Y
#' @param labx - Lab for X
#' @param legendlab
#' @param defaultLineColor - Used when colFill is NA.
#' @param forceDatePeriod - You can force a start/end date. Ex: forceDatePeriod = c(Sys.Date() -4, Sys.Date())
#' @author Pablo Pagnone
#' @import ggplot2
#' @import dplyr
#' @export
egdataseries <- function(df,
                         colDate,
                         colFill = NA,
                         xlab ="Date",
                         ylab= "Count",
                         legendlab = "",
                         defaultLineColor = "blue",
                         forceDatePeriod = NA) {

  if(!is.data.frame(df)){
    stop("df must be a data.frame.")
  }
  if(!colDate %in% colnames(df)){
    stop("colDate Column not exist in data.frame 'df'.")
  }

  if(!is.na(colFill) & !colFill %in% colnames(df)) {
    stop("collFill column not exist in data.frame 'df'.")
  }

  # Create column "date" to use as group.
  df$date <- as.Date(df[[colDate]])
  start <- Sys.Date() -7
  end <- Sys.Date()

  if(length(df$date) > 0){
    start <- min(df$date,na.rm = TRUE)
    end <- max(df$date,na.rm = TRUE)
  }

  if(!is.na(forceDatePeriod)) {
    start <- forceDatePeriod[1]
    end <- forceDatePeriod[2]
  }

  expecteddates <- data.frame(date = seq(start, end, "day"))

  if(is.na(colFill)) {
    expected <- expecteddates
    dataSeries <- df %>% group_by_("date") %>% summarise(count = n())
    dataSeriesFinal <- expected %>% left_join(dataSeries, by=c("date"))
  } else {
    expected <- base::merge(expecteddates, df %>% distinct_(colFill))
    dataSeries <- df %>% group_by_("date", colFill) %>% summarise(count = n())
    dataSeriesFinal <- expected %>% left_join(dataSeries, by=c(colFill, "date"))
  }

  # Replace NA by 0 in count.
  dataSeriesFinal$count <- ifelse(is.na(dataSeriesFinal$count), 0, dataSeriesFinal$count)

  if(!is.na(colFill)) {
    plot <- ggplot(dataSeriesFinal, aes(date,
                                        count,
                                        group=dataSeriesFinal[,colFill],
                                        color=dataSeriesFinal[,colFill])) +
      geom_line()

  } else {
    plot <- ggplot(dataSeriesFinal, aes(date, count)) +
      geom_line(color = defaultLineColor)
  }

  result <- plot +
    scale_x_date(date_labels = "%d-%m", date_breaks = "1 day") +
    theme(axis.text.x = element_text(angle = 90, size = 8),
          legend.position = "right") +
    labs(color = legendlab, x=xlab, y=ylab)

  result
}
