% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syu_timeline.R
\name{syu_timeline}
\alias{syu_timeline}
\title{Timeline Visualization of}
\usage{
syu_timeline(df, language = "english", dimensions = c("all"),
  dateTimeCol = "dateTime")
}
\arguments{
\item{df}{The result of get_NRC_sentiment()}

\item{language}{english or spanish}

\item{dimensions}{include specific dimensions, or all (default)}

\item{dateTimeCol}{Where is the time}
}
\value{
a ggplot with a timeline visualization
}
\description{
Timeline Visualization of
}
