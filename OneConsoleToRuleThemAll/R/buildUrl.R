#' Build url as per user input
#' Accepts user input for console name and timeframe
#' @return url
#' @import stringr
#' @export
buildUrl <- function(){
  # Show user possible responses for console...
  cat("Possible responses for 'console' are: 1. 'all', 2.'ps4', 3.'xboxone', 4.'switch', 5.'pc', 6.'3ds', 7.'ios'")
  cat("Please input response below...")
  gameconsole <- readResponse("Console: ")
  
  # Show user possible responses for timeframe...
  cat("Possible responses for 'time frame' are: 1. '90day', 2.'all', 3. 'year', 4. 'discussed' (most discussed), 5. 'shared' (most shared)")
  cat("Don't be naughty!")
  cat("Please input response in below...")
  timeframe <- readResponse("Timeframe: ")
  
  # Building url as per inputs...
  
  # Checking if inputted values above are sensible...
  # If not, default values are assumed...
  if(gameconsole %in% c("all","ps4","xboxone","switch","pc","3ds","ios") == FALSE){gameconsole <- "all"}
  if(timeframe %in% c("90day","all","year","discussed","shared") == FALSE){timeframe <- "all"}
  
  # putting together url...
  baseUrlOne <- "http://www.metacritic.com/browse/games/score/metascore/"
  baseUrlTwo <- "/filtered?sort=desc"
  return(paste(baseUrlOne,
               timeframe,
               "/",
               gameconsole,
               baseUrlTwo,
               sep=""))
}