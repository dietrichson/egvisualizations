#' Polls on Timeline
#'
#' This plot function plots a poll over a timeline.
#' If the data.frame has only one candidate 
#' 
#' @param type should a ggplot or a plotly be returned.
#' @param poll a data.frame with c('Date','value','variable','fullname')
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @return a ggplot or a plotly object
#' @export
eg_poll_timeline <- function(poll){
  #Data validation
  if(missing(poll)) 
    stop("Need Polling Data to Plot.")
  if(sum(c('Date','value','variable','fullname') %in% names(poll)) <4)  
    stop('Columns are missing in polls data.frame.')

  poll %>% 
    dplyr::filter(!is.na(value)) %>% 
  ggplot(aes(x=as.Date(Date),y=value,color=variable))+
    geom_point()+
    geom_line()+
    scale_y_continuous(labels=scales::percent, limits = c(0,1))+
    xlim(c(as.Date("2018-01-01"),as.Date("2019-01-01")))+
    xlab("Date")
  
}