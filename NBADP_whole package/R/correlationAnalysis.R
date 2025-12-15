#' analysis of different factors' influence on the result of the game


#' @return the coefficients of each factor, representing the weight of each factor
#' @export
#'
#' @examples
#' library(NBADP)
#' correlation.analysis()


correlation.analysis=function(){
  game.data.df=data.frame(game.data)
  offence.lm=lm(GW~PTS+FGM+FGA+FG.+X3PM+X3PA+X3P.+FTM+FTA+FT.+OR+DR
                +AST+TO+
                  +ODR+OSTL+OBLK+OPF,data = game.data.df)
  defence.lm=lm(GW~+DR
                +STL+BLK+PF+OPTS+OFGM+OFGA+OFG.+O3PM+O3PA+O3P.+OFTM
                +OFTA+OFT.+OOR+OAST+OTO,data = game.data.df)
  offence.coef=coef(offence.lm)
  defence.coef=coef(defence.lm)
  return(
    list(offence=offence.coef,
         defence=defence.coef)
  )
}
