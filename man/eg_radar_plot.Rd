% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eg_radar_plot.R
\name{eg_radar_plot}
\alias{eg_radar_plot}
\title{Radar Plot}
\usage{
eg_radar_plot(plot.data, grouping.var = "group",
  font.radar = "Times New Roman", values.radar = c("0\%", "50\%",
  "100\%"), axis.labels = colnames(plot.data)[-1], grid.min = 0,
  grid.mid = 0.5, grid.max = 1, centre.y = grid.min - ((1/9) *
  (grid.max - grid.min)), plot.extent.x.sf = 1, plot.extent.y.sf = 1.2,
  x.centre.range = 0.02 * (grid.max - centre.y),
  label.centre.y = FALSE, grid.line.width = 0.5,
  gridline.min.linetype = "longdash",
  gridline.mid.linetype = "longdash",
  gridline.max.linetype = "longdash", gridline.min.colour = "grey",
  gridline.mid.colour = "#007A87", gridline.max.colour = "grey",
  grid.label.size = 7, gridline.label.offset = -0.1 * (grid.max -
  centre.y), label.gridline.min = TRUE, label.gridline.mid = TRUE,
  label.gridline.max = TRUE, axis.label.offset = 1.15,
  axis.label.size = 8, axis.line.colour = "grey",
  group.line.width = 1.5, group.line.alpha = 0.7,
  group.point.size = 6, group.point.alpha = 0.7,
  group.colours = NULL, background.circle.colour = "#D7D6D1",
  background.circle.transparency = 0.2, plot.legend = if
  (nrow(plot.data) > 1) TRUE else FALSE, legend.title = "",
  plot.title = "", legend.text.size = grid.label.size)
}
\arguments{
\item{legend.text.size}{}
}
\value{
a ggplot
}
\description{
Radar Plot
}
