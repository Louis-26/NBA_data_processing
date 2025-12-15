
#' Predict NBA Team Win Rate
#'
#' @param year Integer, the year to predict the win rate for, defaults to 2023
#' @param desc Logical value, Whether to sort the output by descending predicted win rate
#'
#' @return A bar chart that shows the predicted win rate and the historic win rate for each team
#' @export
#'
#' @examples
#' nba_win_rate(year = 2023)

nba_win_rate <- function(year = 2023, desc = T){

  #require(dplyr)
  #require(ggplot2)

  if (year < 2023) stop("year mast greater than 2023")

  ## read data score stats
  #DataScoreRaw <- read.csv('./archive_2/NBA Player Box Score Stats(1950 - 2022).csv')

  ## find exist team in 2022
  Team_name <- DataScoreRaw %>%
    filter(Season == 2022)
  Team_name <- unique(Team_name$Team)

  ## group by player
  DataScoreRaw <- DataScoreRaw %>%
    filter(Team %in% Team_name) %>%
    select(Season, Team, MATCHUP, WL, Game_ID) %>%
    distinct() %>%
    filter(Season <= 2022) %>%
    group_by(Season, Team) %>%
    mutate(WL = WL == 'W') %>%
    summarise(WR = sum(WL)/n(),
              .groups = 'drop') %>%
    group_by(Team) %>%
    do(model = lm(WR ~ Season,
                  data = .),
       WR = mean(.$WR, na.rm = T)) %>%
    mutate(pred = predict(model,
                          newdata = data.frame(Season = year)),
           WR = as.numeric(WR)) %>%
    ungroup()

  if (desc) {
    DataScoreRaw <- arrange(DataScoreRaw, desc(pred))
  }  else {
    DataScoreRaw <- arrange(DataScoreRaw, pred)
  }

  DataScoreRaw$Team <- factor(DataScoreRaw$Team,
                           levels = rev(DataScoreRaw$Team))

  ggplot(DataScoreRaw, aes(y = Team)) +
    geom_bar(aes(x = pred, fill = "Predicted"),
             position = position_dodge(width = 0.9),
             stat = "identity") +
    geom_bar(aes(x = -WR, fill = "Historic"),
             position = position_dodge(width = 0.9),
             stat = "identity") +
    scale_x_continuous(limits = c(-1, 1),
                       expand = c(0, 0)) +
    theme_bw()+
    theme(legend.position = "top")+
    labs(x = "Win Rate",
         y = "",
         fill = "")
}

#nba_win_rate()
