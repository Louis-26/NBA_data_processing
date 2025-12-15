#Prediction Function
#' Make a salary forecast for 2023-2024
#'
#' @param turn_over numeric, turnovers per game (TOV).
#' @param points_per_game numeric, points per game (PTS).
#' @param two_point_field_goal_attempts_per_game numeric, (2PA).
#' @param free_throws_per_game numeric, free throws per game (FT).
#' @param assists_per_game numeric, assists per game (AST).
#' @param games_started numeric, games started (GS).
#' @param value_over_replacement_player numeric, value over replacement player (VORP).
#' @param defensive_rebounds_per_game numeric, defensive rebounds per game (DRB).
#' @param minutes_played_per_game numeric, minutes played per game (MP).
#' @param three_point_field_goal_attempts_per_game numeric, three point field goal attempts per game (3PA).
#' @param usage_percentage numeric, usage percentage (USG\%).
#' @param offensive_box_plusorminus numeric, offensive box plus or minus (OBPM).
#' @param defensive_win_shares numeric, defensive win shares (DWS).
#'
#' @return according to the player's performance in 2023-2024 to predict 2023-2024 salary
#'
#' @import readr dplyr corrplot
#'
#' @examples
#' library(NBADP)
#' pred_salary(0.6, 3.9, 2.1, 0.4, 1.4, 6, 0, 0.8, 13.4, 1.4, 13.4, -2.9, 0.6)
#'
#' @export


pred_salary = function(turn_over, points_per_game,
                      two_point_field_goal_attempts_per_game,
                      free_throws_per_game, assists_per_game, games_started,
                      value_over_replacement_player,
                      defensive_rebounds_per_game, minutes_played_per_game,
                      three_point_field_goal_attempts_per_game, usage_percentage,
                      offensive_box_plusorminus, defensive_win_shares){
  #NBAcontract <- read_csv("Desktop/NBA/contract predict/NBAcontract.csv")
  salarya <- TOV <- PTS <- X2PA <- FT <- AST <- GS <- VORP <- DRB <- MP <-
    X3PA <- USGp <- OBPM <- DWS <- NULL
  datasets <- data.frame(datasets)
  datasets <- datasets[,-1]

  datasets_regression <- datasets %>% select(salarya, TOV, PTS, X2PA, FT, AST,
                                             GS, VORP, DRB, MP, X3PA, USGp, OBPM,
                                             DWS)
  datasets_lm <- lm(salarya ~ ., data = datasets_regression)

  pred_new <- data.frame(TOV = turn_over, PTS = points_per_game,
                         X2PA = two_point_field_goal_attempts_per_game,
                         FT = free_throws_per_game, AST = assists_per_game,
                         GS = games_started,
                         VORP = value_over_replacement_player,
                         DRB = defensive_rebounds_per_game,
                         MP = minutes_played_per_game,
                         X3PA = three_point_field_goal_attempts_per_game,
                         USGp = usage_percentage,
                         OBPM = offensive_box_plusorminus,
                         DWS = defensive_win_shares)
  predsalary = predict(datasets_lm, newdata = pred_new)
  predsalary <- data.frame(predsalary)
  predsalary <- abs(predsalary)
  predsalary <- predsalary$predsalary
  return(predsalary)
}







