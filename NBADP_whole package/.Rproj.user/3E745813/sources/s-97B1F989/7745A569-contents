#' Find the best NBA players based on their salaries and score stats
#'
#' @param top Integer, input the number of top NBA players
#' @param year Integer, input the year to consider for score stats and salaries
#'
#' @return A dataframe of the top NBA players based on their salaries and score stats
#' @export
#'
#' @examples
#' best_nba_player(top = 4, year = 2021)


best_nba_player <- function(top, year){

  #require(dplyr)

  message("The data is too large, and the reading time is relatively long. Please wait patiently")

  #DataSalariesRaw <- read_csv("~/Desktop/project/archive_2/NBA Salaries(1990-2023).csv")


  ## clean data
  DataSalaries <- DataSalariesRaw
  DataSalaries$salary <- as.numeric(gsub("[$,]", "", DataSalaries$salary))
  DataSalaries$inflationAdjSalary <- as.numeric(gsub("[$,]", "", DataSalaries$inflationAdjSalary))
  ## remove row names
  DataSalaries <- DataSalaries[,-1]
  ## find recently Salaries
  DataSalaries <- DataSalaries %>%
    group_by(playerName) %>%
    filter(seasonStartYear == year)

  #DataScoreRaw <- read_csv("~/Desktop/NBA Player Box Score Stats(1950 - 2022).csv")
  DataScore <- DataScoreRaw

  ## group by player
  DataScore <- DataScoreRaw %>%
    group_by(PLAYER_NAME) %>%
    filter(Season == year) %>%
    summarise(score = sum(WL == "W"),
              .groups = 'drop')

  ## bind datasets
  DataAll <- DataSalaries %>%
    left_join(DataScore,
              by = c(playerName = "PLAYER_NAME")) %>%
    mutate(cp = inflationAdjSalary/score) %>%
    filter(!is.infinite(cp) & !is.na(cp)) %>%
    arrange(cp) %>%
    head(top)

  return(DataAll$playerName)
}

#best_nba_player(top = 3, year = 2021)
#best_nba_player(top = 4, year = 2020)

