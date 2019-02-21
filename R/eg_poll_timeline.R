#' Polls on Timeline
#'
#' This plot function plots a poll over a timeline.
#' 
#' @param poll a data.frame with c('Date','value','variable','fullname')
#' @param abbr Should names be abbreviated (for legend legibility)
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @return a ggplot or a plotly object
#' @export
eg_poll_timeline <- function(poll, abbr=TRUE){
  
  eg_poll_validate(poll)
  
  if (isTRUE(abbr))
    poll$fullname <- abbreviate(poll$fullname,2L)
  # Get max and min dates
  maxDate <- as.Date(max(lubridate::ymd(poll$Date))) %>% lubridate::ceiling_date(unit = 'month')
  minDate <- as.Date(min(lubridate::ymd(poll$Date))) %>% lubridate::floor_date(unit = 'month')
  
  gg <- poll %>% 
    dplyr::filter(!is.na(value)) %>% 
    ggplot(aes(x=as.Date(Date),y=value))
  
  #If there is more than one entity add color aesthetic
  if (length(unique(poll$shortname))>1)
    gg <- gg + aes(color=fullname)
  
  #If there is more than one entity add linetype aesthetic
  if (length(unique(poll$variable))>1)
    gg <- gg + aes(linetype=variable)
    
  gg+
    geom_point()+
    geom_line()+
    scale_y_continuous(labels=scales::percent, limits = c(0,1))+
    xlim(c(minDate,maxDate))+
    xlab("Date")+theme(legend.title = element_blank())
  
}