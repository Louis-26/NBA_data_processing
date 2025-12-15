#' Visualize a player's shot percentage and distribution on a season
#'
#' @param playerName string, input the player's name
#' @param teamName string, input the team that the player on
#' @param teamdata dataset, a dataset that contain the team's all regular match data in 2022-2023 season
#'
#' @return The shot plot of the player in the whole regular season
#'
#' @import readr ggplot2
#'
#' @examples
#' library(NBADP)
#' player_visual(teamdata = GSW,"S. Curry","GSW")
#' @export

player_visual = function(teamdata = GSW, playerName, teamName) {
  #data("GSW")
  data <- x <- y <- xshot <- yshot <- scoreVal <- NULL
  GSW <- teamdata
  GSW <- data.frame(GSW)
  GSW_Curry <- subset(GSW, (GSW$playerNameI == playerName & GSW$teamTricode == teamName))
  GSW_shot <- GSW_Curry[-which(GSW_Curry$isFieldGoal == 0 & is.na(GSW_Curry$scoreVal)),] #筛除掉与投篮本身无关的数据
  GSW_shot <- GSW_shot[-which(GSW_shot$actionType == "Free Throw"),]
  GSW_shot$xshot = GSW_shot$xLegacy / 10
  GSW_shot$yshot = GSW_shot$yLegacy / 10
  GSW_shot$shot = paste(GSW_shot$xshot,GSW_shot$yshot,sep=",")

  Plot = ggplot(data=data.frame(x=1,y=1),aes(x,y))+
    #out-field:
    geom_path(data=data.frame(x=c(-25,-25,25,25,-25),y=c(0-5,47-5,47-5,0-5,0-5)))+
    #half-court line:
    geom_path(data=data.frame(x=c(-25,25),y=c(-5,-5)))+
    #half-court semicircle:
    geom_path(data=data.frame(x=c((-6000:(-1)/1000),(1:6000/1000)),y=-c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))+47-5),aes(x=x,y=y))+
    #solid FT semicircle above FT line:
    geom_path(data=data.frame(x=c((-6000:(-1)/1000),(1:6000/1000)),y=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))+47-5),aes(x=x,y=y))+
    #dashed FT semicircle below FT line:
    geom_path(data=data.frame(x=c((-6000:(-1)/1000),(1:6000/1000)),y=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))+47-5),aes(x=x,y=y),linetype='dashed')+
    #key:
    geom_path(data=data.frame(x=c(-8,-8,8,8,-8),y=c(-5,14,14,-5,-5)))+
    #box inside the key:
    geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=c(-5,14,14,-5,-5)))+
    #restricted area semicircle:
    geom_path(data=data.frame(x=c((-4000:(-1)/1000),(1:4000/1000)),y=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))+47-5),aes(x=x,y=y))+
    #rim:
    geom_path(data=data.frame(x=c((-750:(-1)/1000),(1:750/1000),(750:1/1000),(-1:-750/1000)),y=c(c(0.25+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(0.25-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
    #backboard:
    geom_path(data=data.frame(x=c(-3,3),y=c(-1,-1)),lineend='butt')+
    #three-point line:
    geom_path(data=data.frame(x=c(-22,-22,(-22000:(-1)/1000),(1:22000/1000),22,22),y=-c(47-47+5,(47-169/12)-47+5,(41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2))-47+5,(47-169/12)-47+5,47-47+5)),aes(x=x,y=y))+
    #fix aspect ratio to 1:1
    coord_fixed() +
    geom_point(data = GSW_shot, aes(x=xshot,y=yshot,color=factor(scoreVal)), alpha=0.3)

  return(Plot)
}


