#' predict the regular season MVP of the season 2022-2023, given the player's personal data of regular season.

#' @return a data frame representing the rank of the MVP voting,
#' so that we know the first one will be the predicted player with the highest probability to be the MVP
#' @export
#'
#' @examples
#' library(NBADP)
#' MVP.prediction()


MVP.prediction=function(){
  # remove the first five columns, because that's only the personal information of the player
  data.df.2020.2021=data.frame(dataPlayer.2020.2021)[-(1:5)]
  data.df.2022.2023=data.frame(dataPlayer.2022.2023)[-(1:5)]
  data.lm=lm(MVP~G+GS+MP+FG+FGA+FG.+X3P+X3PA+X3P.+X2P+X2PA+X2P.+eFG.+FT+FTA+FT.
             +ORB+DRB+TRB+AST+STL+BLK+TOV+PF+PTS,data = data.df.2020.2021)
  predict.2023.MVP=predict(data.lm,newdata=data.df.2022.2023)
  predict.2023.MVP=sort(predict.2023.MVP,decreasing = TRUE)
  predict.2023.MVP=as.data.frame(predict.2023.MVP)
  rank.MVP=as.numeric(rownames(predict.2023.MVP))
  top.10.MVP=head(rank.MVP,10)
  name.vec=dataPlayer.2020.2021[,"Player"]
  MVP.name.list=name.vec[top.10.MVP]
  return(MVP.name.list)
}


