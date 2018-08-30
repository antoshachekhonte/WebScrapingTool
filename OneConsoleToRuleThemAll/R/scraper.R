#' Scrape webpage to return table with details on top games
#' @param url metacritic top game list URL 
#' @return dataframe with top game data
#' @import rvest
#' @import httr
#' @import stringr
#' @export
scraper <- function(url="http://www.metacritic.com/browse/games/score/metascore/90day/all/filtered?sort=desc"){
  ign <- read_html(url)
  
  # Extracting game titles...
  ign_game_nodes <- html_nodes(ign,
                               ".product_item.product_title")
  games <- html_text(ign_game_nodes) %>%
    trimws() %>%
    str_extract("[^\\n]+")
  
  # Extracting game rank... 
  ign_rank_nodes <- html_nodes(ign,
                               ".product_item.row_num")
  ranks_ <- html_text(ign_rank_nodes) %>%
    trimws()
  ranks_ <- gsub("[^0-9]","",ranks_)
  ranks <- as.numeric(ranks_)
  
  # Extracting game scores...
  ign_score_nodes <- html_nodes(ign,
                                ".product_item.product_score")
  scores <- html_text(ign_score_nodes)
  scores <- str_split(scores,"\n")
  scores <- sapply(scores,"[[",5)
  scores <- trimws(scores)
  scores <- as.numeric(scores)
  
  # Extracting game user scores...
  ign_userscores_nodes <- html_nodes(ign,
                                     ".product_item.product_userscore_txt")
  userscores <- html_text(ign_userscores_nodes)
  userscores <- str_split(userscores,"\n")
  userscores <- sapply(userscores,"[[",3)
  userscores <- trimws(userscores)
  userscores <- as.numeric(userscores)
  
  # Extracting release date data...
  ign_releasedate_nodes <- html_nodes(ign,
                                      ".product_item.product_date")
  releasedate <- html_text(ign_releasedate_nodes)
  releasedate <- str_split(releasedate,"\n")
  releasedate <- sapply(releasedate,"[[",2) %>%
    trimws()
  releasedate <- gsub(",","",releasedate)
  releasedate <- as.Date(releasedate,
                         format="%B %d %Y")
  # Constructing a dataframe...
  # User scores that were classified as "to be determined" have been cerced to NA...
  
  # Checking if each of the columns of interest has 100 rows...
  if(length(ranks)==length(userscores) & length(userscores)==length(releasedate) & length(games)==length(releasedate)){
    df <- data.frame(ranks,games,scores,userscores,releasedate)
  } else {
    stop("Insufficient data...!")
  }
  return(df)
}