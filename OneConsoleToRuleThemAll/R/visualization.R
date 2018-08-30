#' Presents histograms and tables of data for the user's analysis
#' @param x A list of five dataframes
#' @return Prints histograms and tables of data
#' @import rvest
#' @import httr
#' @import ggplot2
#' @import gridExtra
#' @import grid
#' @export
visualization <- function(x = OnePlatformToRuleThemAll()){
  # Unpacking the function input...
  summaryStatistics <- x[1][[1]]
  ps4Exclusives <- x[2][[1]]
  pcExclusives <- x[3][[1]]
  switchExclusives <- x[4][[1]]
  xboxoneExclusives <- x[5][[1]]
  
  # Histogram of ps4 game scores...
  a <- ggplot(data=ps4Exclusives,aes(ps4Exclusives$scores))+
    geom_histogram(breaks=seq(80,100,by=2),
                   col="red",
                   fill="green",
                   alpha=.2)+
    labs(title="Histogram of ps4 game scores")+
    labs(x="Score",y="Count")
  
  # Histogram of xbox one game scores...
  b <- ggplot(data=xboxoneExclusives,aes(xboxoneExclusives$scores))+
    geom_histogram(breaks=seq(80,100,by=2),
                   col="red",
                   fill="green",
                   alpha=.2)+
    labs(title="Histogram of xbox one game scores")+
    labs(x="Score",y="Count")
  
  # Histogram of pc game scores...
  c <- ggplot(data=pcExclusives,aes(pcExclusives$scores))+
    geom_histogram(breaks=seq(80,100,by=1),
                   col="red",
                   fill="green",
                   alpha=.2)+
    labs(title="Histogram of pc game scores")+
    labs(x="Score",y="Count")
  
  # Histogram of switch game scores...
  d <- ggplot(data=ps4Exclusives,aes(ps4Exclusives$scores))+
    geom_histogram(breaks=seq(80,100,by=2),
                   col="red",
                   fill="green",
                   alpha=.2)+
    labs(title="Histogram of ps4 game scores")+
    labs(x="Score",y="Count")
  
  # Plotting multiple graphs...
  printObject <- grid.arrange(a,b,c,d,ncol=2)
  printObject
}