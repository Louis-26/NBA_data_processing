#' analysis of state fluctuation of a superstar player: Kawhi Leonard

#' @param data.category the data category that we want to their changes, they are set as "MIN","PTS","REB","FGM", "REB" as the default
#' However, you can choose any data category in the data set.
#' They include "GP	W	L	MIN	PTS	FGM	FGA	FG\%	3PM	3PA	3P\%	FTM	FTA	FT\%	OREB	DREB	REB	AST	TOV	STL	BLK	PF	FP	DD2	TD3	BPM"
#'
#' @param month.range the month range of Kawhi Leonard. They are required to be a subset of
#' ("October","November","December","January","February","March","April") with the same order.
#'
#' @return the state change of the player, as a form of data frame,
#' column names are the month names, from October to April
#' row names are the average points, rebounds, assists, steals, blocks, turnovers of that player
#'
#' @export
#'
#' @examples
#' library(NBADP)
#' Kawhi.Leonard.state.analysis(data.category=c("GP","STL","BLK","BPM"),
#' month.range=c("December","January","February","March"))



Kawhi.Leonard.state.analysis=function(data.category=c("MIN","PTS","REB","FGM"),
                                      month.range=c("October","November","December","January","February","March","April")){

  #state.data.raw=read.csv("data/analyzePlayerState/KawhiLeonardStatePerMonth")
  state.data=data.frame(state.data.raw)
  # select the months that are in the month range
  state.data.selected=subset(state.data,Month %in% month.range)
  # plot the curve
  for (data.cate in data.category){
    #Kawhi.img=readPNG("data/analyzePlayerState/Kawhi Leonard.png")
    r=nrow(Kawhi.img)/ncol(Kawhi.img)
    start.month=month.range[1]
    end.month=month.range[length(month.range)]
    plot(1:length(month.range),state.data.selected[,data.cate],type="l",xlab="Month",ylab=data.cate,
         main=paste("Kawhi Leonard's change on",data.cate,"from",start.month,"to",end.month),xaxt="n")
    axis(1,at=1:length(month.range),labels=month.range)
    # find out the position of the image
    y.range=range(state.data.selected[,data.cate])
    x.low=1
    x.high=length(month.range)
    y.low=y.range[1]
    y.high=y.range[2]
    lam.y.1=0.75
    lam.y.2=0.95
    rasterImage(Kawhi.img,x.low,
                (1-lam.y.1)*y.low+lam.y.1*y.high,
                x.low+(x.high-x.low)/6,
                (1-lam.y.2)*y.low+lam.y.2*y.high)
  }

}




