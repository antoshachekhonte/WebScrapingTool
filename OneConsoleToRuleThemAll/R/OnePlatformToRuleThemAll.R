#' Collects top games for each of the main consoles - ps4, xboxone, pc and switch
#' and returns games that are exclusive to each platform and summary stats for each
#' list of games by platform
#' @return list of dataframes - 
#' element 1 is a summary statistics dataframe, 
#' element 2 is a dataframe with ps4 exclusive games, 
#' element 3 is a dataframe with pc exclusive games, 
#' element 4 is a dataframe with switch exclusive games, 
#' element 5 is a dataframe with xboxone exclusive games 
#' @import DT
#' @export
OnePlatformToRuleThemAll <- function(){
  # Comparing ps4, xboxone, pc and switch games by metacritic score...
  
  # Scraping data...
  ps4 <- scraper("http://www.metacritic.com/browse/games/score/metascore/all/ps4/filtered?sort=desc")
  xboxone <- scraper("http://www.metacritic.com/browse/games/score/metascore/all/xboxone/filtered?sort=desc")
  pc <- scraper("http://www.metacritic.com/browse/games/score/metascore/all/pc/filtered?sort=desc")
  switch <- scraper("http://www.metacritic.com/browse/games/score/metascore/all/switch/filtered?sort=desc")
  
  # Looking at games exclusive to each console by removing games that are shared by 2 or more consoles...
  ps4Exclusives <- ps4[!((ps4$games %in% xboxone$games)|(ps4$games %in% pc$games)|(ps4$games %in% switch$games)),]
  ps4Exclusives$platform <- "PS4"
  pcExclusives <- pc[!((pc$games %in% ps4$games)|(pc$games %in% xboxone$games)|(pc$games %in% switch$games)),]
  pcExclusives$platform <- "PC"
  xboxoneExclusives <- xboxone[!((xboxone$games %in% ps4$games)|(xboxone$games %in% pc$games)|(xboxone$games %in% switch$games)),]
  xboxoneExclusives$platform <- "XboxOne"
  switchExclusives <- switch[!((switch$games %in% ps4$games)|(switch$games %in% pc$games)|(switch$games %in% xboxone$games)),]
  switchExclusives$platform <- "Switch"
  # Building new table with summary stats for each of the platforms...
  
  # Putting together all datasets into one...
  dat <- rbind(ps4Exclusives,switchExclusives,pcExclusives,xboxoneExclusives)
  
  # Finding summary statistics...
  meanTable <- as.data.frame(with(dat, 
                                  tapply(scores,platform,mean))) %>%
    setDT(keep.rownames=TRUE)
  colnames(meanTable) <- c("Platform","Mean Score")
  
  maxTable <- as.data.frame(with(dat, 
                                 tapply(scores,platform,max))) %>%
    setDT(keep.rownames=TRUE)
  colnames(maxTable) <- c("Platform","Max Score")
  
  minTable <- as.data.frame(with(dat,
                                 tapply(scores,platform,min))) %>%
    setDT(keep.rownames=TRUE)
  colnames(minTable) <- c("Platform","Min Score")
  
  medianTable <- as.data.frame(with(dat,
                                    tapply(scores,platform,median))) %>%
    setDT(keep.rownames=TRUE)
  colnames(medianTable) <- c("Platform","Median Score")
  
  # Merging...
  summaryStatistics <- merge(meanTable,maxTable,by="Platform") %>%
    merge(minTable,by="Platform") %>%
    merge(medianTable,by="Platform")
  summaryStatistics <- summaryStatistics[with(summaryStatistics,order(-`Mean Score`))]
  summaryStatistics$`Mean Score` <- round(summaryStatistics$`Mean Score`)
  output <- list(summaryStatistics,
                 ps4Exclusives,
                 pcExclusives,
                 switchExclusives,
                 xboxoneExclusives)
  return(output)
}