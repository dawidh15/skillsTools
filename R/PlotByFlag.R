#' Plot relative catches by flag and time period.
#'
#' Creates a barplot of catches scaled by maximum value, within a time period and by country flag..
#'
#' @param plotData A filtered table containing data from only one time period and one flag.
#' Data must come from \link{GetTable} using the value \emph{Proportional Catches by Period-Flag} for the argument \code{table_name}.
#'
#' @export

PlotByFlag <- function(plotData)
{
  flag <- plotData %>% pull(Flag) %>% first()
  minyear <- plotData %>% pull(MinYear) %>%  first()
  maxyear <- plotData %>% pull(MaxYear) %>%  first()
  ggplot(plotData,
         aes(x = reorder(Species,desc(Ranking)), y = PropMaxCatch)) +
    geom_bar(stat = "identity", orientation = "x") +
    ggtitle("Main target species by Flag and Period"
            , subtitle = glue("Flag: {flag}. From {minyear} to {maxyear}")) +
    xlab("Species") +
    ylab("Scaled Catches") +
    coord_flip()
}
