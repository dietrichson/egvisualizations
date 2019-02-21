#' Box Plot of Poll
#'
#' This plot function plots a poll over a timeline.
#' 
#' @param poll a data.frame with c('Date','value','variable','fullname')
#' @param abbr Should names be abbreviated (for legend legibility)
#' 
#' @import dplyr
#' @import ggplot2
#' @return a ggplot object
#' @export
eg_poll_boxplot <- function(poll, abbr=TRUE){
  eg_poll_validate(poll)
  if(isTRUE(abbr))
    poll$fullname <- abbreviate(poll$fullname,2L)
  
 gg <- poll %>% 
  ggplot(aes(x=fullname,y=value))+
    geom_boxplot(position='identity')
  
 #If more than one variable is present add color aesthetics
 if (length(poll$variable %>% unique) > 1)
  gg <- gg+aes(color=variable)
 
 #Add percentage scale 
 gg <- gg + 
   scale_y_continuous(breaks=c(0,.25,.5,.75,1),labels=scales::percent,limits = c(0,1))+
   xlab('Entity')+ylab('')
 
 gg
}