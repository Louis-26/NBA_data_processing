#' Player Efficiency Rate
#'
#' We calculate player efficiency rating (PER) to boil down a player's performance.
#'
#' @param lgdata dataset, a dataset that contain the variables that have
#' league's basic information
#' @param regulardata dataset, a dataset that contain the variables that have basic
#' information of player's performance
#' @param yrs string, the seasons that we want to calculate
#' @param player1 string, input the name of the player that we want to test
#' @param team string, input the team that the player on
#'
#' @return a number
#' @import readr
#'
#' @examples
#' library(NBADP)
#' PER(lgdata = lg, regulardata = X2021to2022data, yrs = "2021-22", "LeBron James","LAL")
#' @export


PER = function(lgdata = lg, regulardata = X2021to2022data, yrs = "2021-22", player1,team) {
  #lg <- read_csv("~/Desktop/NBA/PER/lg_excel.csv")
  #official_2021to2022data <- read_csv("~/Desktop/NBA/PER/official_2021to2022data.csv")
  #data("lg",package = NULL)

  lg <- data.frame(lgdata)
  X2021_2022_Regular_Stat <- data.frame(regulardata)
  lg <- subset(lg,lg$Season == yrs)
  factor = (2/3) - (0.5 * (lg$AST / lg$FG)) / (2 * (lg$FG / lg$FT))
  VOP = lg$PTS / (lg$FGA - lg$ORB + lg$TOV + (0.44 * lg$FTA))
  DRB = (lg$TRB - lg$ORB) / lg$TRB

  team_AST_df = data.frame(tapply(X2021_2022_Regular_Stat[,"AST"], INDEX = X2021_2022_Regular_Stat$Tm, FUN = mean))
  team_AST_df$row_names <- row.names(team_AST_df)
  names(team_AST_df) = c("team_AST","Tm")
  Regular_Stat1 <- merge(X2021_2022_Regular_Stat,team_AST_df,by="Tm")
  team_FG_df = data.frame(tapply(X2021_2022_Regular_Stat[,"FG"], INDEX = X2021_2022_Regular_Stat$Tm, FUN = mean))
  team_FG_df$row_names <- row.names(team_FG_df)
  names(team_FG_df) = c("team_FG","Tm")
  Regular_Stat2 <- merge(Regular_Stat1,team_FG_df,by="Tm")
  Regular_Stat2 <- Regular_Stat2[order(Regular_Stat2$Rk),]
  Regular_Stat2$Tm[which(Regular_Stat2$Tm=="PHO")] = "PHX"
  Regular_Stat2$Tm[which(Regular_Stat2$Tm=="CHO")] = "CHA"
  Tm = data.frame(c("ATL","UTA","PHX","MIL","BOS","DEN","CHA","MEM","MIN","MIA","PHI","CHI","BKN","DAL","GSW","TOR","SAS","CLE","IND","NOP","WAS","NYK","LAL","LAC","SAC","POR","HOU","DET","OKC","ORL"))
  Tm$Pace <- c(100.0, 99.5, 102.0, 102.2, 99.0, 99.9, 102.3, 103.1, 103.5, 98.2, 98.4, 100.4, 101.6, 97.5, 100.7, 98.6, 102.4, 98.4, 100.5, 99.7, 99.3, 98.4, 102.3, 100.2, 102.1, 100.6, 103.3, 100.9, 100.8, 101.5)
  names(Tm) = c("Tm","team_Pace")
  Regular_Stat <- merge(Regular_Stat2,Tm,by="Tm")
  lg_Pace = mean(Tm$team_Pace)
  Tm$lg_pace <- c(lg_Pace)
  Tm$pace_adjust <- c(Tm$lg_pace / Tm$team_Pace)

  #Regular_Stat$MP[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)]
  uPER = (1 / Regular_Stat$MP[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)]) * (Regular_Stat$X3P[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)]
                                                                                               + ((2/3) * Regular_Stat$AST[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)])
                                                                                               + ((2 - factor * (Regular_Stat$team_AST[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)] / Regular_Stat$team_FG[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)])) * Regular_Stat$FG[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)])
                                                                                               + ((Regular_Stat$FT[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)] * 0.5 * (1 + (1 - (Regular_Stat$team_AST[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)] / Regular_Stat$team_FG[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)])) + (2/3) * (Regular_Stat$team_AST[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)] / Regular_Stat$team_FG[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)]))))
                                                                                               - (VOP * Regular_Stat$TOV[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)])
                                                                                               - (VOP * DRB * (Regular_Stat$FGA[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)] - Regular_Stat$FG[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)]))
                                                                                               - (VOP * 0.44 * (0.44 + (0.56 * DRB)) * (Regular_Stat$FTA[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)] - Regular_Stat$FT[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)]))
                                                                                               + (VOP * (1 - DRB) * (Regular_Stat$TRB[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)] - Regular_Stat$ORB[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)]))
                                                                                               + (VOP * DRB * Regular_Stat$ORB[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)])
                                                                                               + (VOP * Regular_Stat$STL[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)])
                                                                                               + (VOP * DRB * Regular_Stat$BLK[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)])
                                                                                               - (Regular_Stat$PF[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)] * ((lg$FT / lg$PF) - 0.44 * (lg$FTA / lg$PF) * VOP)))
  pace_adjustment = lg_Pace / Regular_Stat$team_Pace[which(Regular_Stat$Player==player1 & Regular_Stat$Tm==team)]
  aPER = pace_adjustment * uPER
  PER_output = aPER * (15 / 0.32)
  return(PER_output)
}


