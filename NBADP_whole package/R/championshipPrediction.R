#' championship prediction
#'
#' @return a vector, containing the teams with the number of series that it is expected to win, in the decreasing order
#'
#'
#' @export
#'
#' @examples
#' library(NBADP)
#' championship.Predict.2023()
#'
championship.Predict.2023=function(){
  train.data.df=data.frame(dataTeam.2021.2022)
  series.win.lm=lm(SW~PTS+FGM+FGA+FG.+X3PM+X3PA+X3P.+FTM+FTA+FT.+OR+DR
                   +AST+STL+BLK+TO+PF+OPTS+OFGM+OFGA+OFG.+O3PM+O3PA+O3P.+OFTM
                   +OFTA+OFT.+OR+ODR+OAST+OSTL+OBLK+OTO+OPF+GW,data=train.data.df)
  predict.data.2022.2023=data.frame(dataTeam.2022.2023)
  prediction.2023=predict(series.win.lm,newdata=predict.data.2022.2023)
  prediction.2023=sort(prediction.2023,decreasing = TRUE)
  prediction.2023=as.data.frame(prediction.2023)
  team.rank=as.numeric(rownames(prediction.2023))
  team.li=dataTeam.2022.2023[,"Team"]
  team.vec=team.li[team.rank]
  return(team.vec)
}


