
#' Plot the field goal percentage (FGP) of NBA players for a given year and number of top players to display.
#'
#' @param index Integer, indicating the number of top players to display
#' @param year Integer, indicating the year of the NBA season to plot FGP for
#' @param desc Logical value, indicating whether to sort the players in descending order by FGP and games played (G)
#'
#' @return A plot of the top players with their FGP and G displayed
#' @export
#'
#' @examples
#' field_nba_player(index = 30, year = 2021, desc = TRUE)


field_nba_player <- function(index = 30, year = number, desc = T){

  #require(dplyr)
  #require(ggplot2)

  ## read data score stats
  #DataScore <- read.csv('./archive_2/NBA Player Box Score Stats(1950 - 2022).csv')
  DataScore <- DataScoreRaw

  ## group by player
  DataScore <- DataScore %>%
    filter(Season == year) %>%
    group_by(PLAYER_NAME) %>%
    mutate(FGA_all = sum(FGA, na.rm = T),
           FGM_in = FGA * FG_PCT,
           FGP = FGM_in/FGA_all) %>%
    summarise(FGP = sum(FGP, na.rm = T),
              G = n(),
              .groups = 'drop')

  if (desc) {
    DataScore <- arrange(DataScore, desc(FGP), desc(G))
  }  else {
    DataScore <- arrange(DataScore, FGP, G)
  }

  DataAll <- head(DataScore, index)
  DataAll$PLAYER_NAME <- factor(DataAll$PLAYER_NAME,
                                levels = rev(DataAll$PLAYER_NAME))

  ggplot(DataAll, aes(y = PLAYER_NAME, x = FGP)) +
    geom_bar(aes(fill = FGP),
             stat = "identity",
             show.legend = F) +
    geom_text(aes(label = G,
                  hjust = ifelse(FGP == 1,
                                 1.3,
                                 -0.1)))+
    scale_y_discrete()+
    scale_fill_continuous(limits = c(0, 1))+
    scale_x_continuous(limits = c(0, 1),
                       expand = c(0, 0))+
    theme_bw()+
    labs(x = "Total FGP",
         y = NULL)
}


