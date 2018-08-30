#' Scrapes brief description of top 3 games for console specified by user from metacritic,
#' and formats collected data into dataframe
#' @param url metacritic top games table url
#' @return table with descriptions of top 3 games 
#' @export
topgamesummary <- function(url="http://www.metacritic.com/browse/games/score/metascore/all/ps4/filtered"){
  meta <- read_html(url)
  
  # Extracting links from game nodes...
  meta_game_links <- html_nodes(meta,
                                ".product_item.product_title a") %>% html_attr("href")
  url.games <- meta_game_links[1:3]
  baseUrl <- "http://www.metacritic.com"
  link.1 <- paste(baseUrl,url.games[1],sep="")
  link.2 <- paste(baseUrl,url.games[2],sep="")
  link.3 <- paste(baseUrl,url.games[3],sep="")
  
  # Extracting summary for game 1
  game1 <- read_html(link.1)
  game1.name <- html_text(html_nodes(game1,".product_title h1")) %>% trimws()
  game1.summary <- html_text(html_nodes(game1,".summary_detail.product_summary span.data")) %>% trimws()
  
  
  # Extracting summary for game 2 
  game2 <- read_html(link.2)
  game2.name <- html_text(html_nodes(game2,".product_title h1")) %>% trimws()
  game2.summary <- html_text(html_nodes(game2,".summary_detail.product_summary span.data")) %>% trimws()
 
  
  # Extracting summary for game 3
  game3 <- read_html(link.3)
  game3.name <- html_text(html_nodes(game3,".product_title h1")) %>% trimws()
  game3.summary <- html_text(html_nodes(game3,".summary_detail.product_summary span.data")) %>% trimws()
  
  
  # Building table 
  df1 <- cbind(game1.name,game1.summary)
  df2 <- cbind(game2.name,game2.summary)
  df3 <- cbind(game3.name,game3.summary)
  df4 <- rbind(df1,df2,df3)
  df4 <- as.data.frame(df4)
  colnames(df4) <- c("name","summary")
  df4$name <- as.character(df4$name)
  
  return(df4)
}
