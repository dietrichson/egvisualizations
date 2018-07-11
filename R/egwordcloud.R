#' Print a wordcloud with configuration by default for Ethergeist.
#' @description
#' Plot based on wordcloud package.
#' @author Pablo Pagnone
#' @import wordcloud
#' @export
egwordcloud <- function(words,
                        freq,
                        scale = c(4,1),
                        max.words = 70,
                        min.freq=3,
                        random.order = FALSE,
                        colors = c("#606060","#000000","#800000")) {
  wordcloud(words,
            freq,
            scale = scale,
            max.words = max.words,
            min.freq=min.freq,
            random.order = random.order,
            colors = colors)
}
