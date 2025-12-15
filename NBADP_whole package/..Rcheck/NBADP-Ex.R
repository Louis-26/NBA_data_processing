pkgname <- "NBADP"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('NBADP')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Kawhi.Leonard.state.analysis")
### * Kawhi.Leonard.state.analysis

flush(stderr()); flush(stdout())

### Name: Kawhi.Leonard.state.analysis
### Title: analysis of state fluctuation of a superstar player: Kawhi
###   Leonard
### Aliases: Kawhi.Leonard.state.analysis

### ** Examples

library(NBADP)
Kawhi.Leonard.state.analysis(data.category=c("GP","STL","BLK","BPM"),
month.range=c("December","January","February","March"))



cleanEx()
nameEx("PER")
### * PER

flush(stderr()); flush(stdout())

### Name: PER
### Title: Player Efficiency Rate
### Aliases: PER

### ** Examples

library(NBADP)
PER(lgdata = lg, regulardata = X2021to2022data, yrs = "2021-22", "LeBron James","LAL")



cleanEx()
nameEx("best_nba_player")
### * best_nba_player

flush(stderr()); flush(stdout())

### Name: best_nba_player
### Title: Find the best NBA players based on their salaries and score
###   stats
### Aliases: best_nba_player

### ** Examples

best_nba_player(top = 4, year = 2021)



cleanEx()
nameEx("field_nba_player")
### * field_nba_player

flush(stderr()); flush(stdout())

### Name: field_nba_player
### Title: Plot the field goal percentage (FGP) of NBA players for a given
###   year and number of top players to display.
### Aliases: field_nba_player

### ** Examples

field_nba_player(index = 30, year = 2021, desc = TRUE)



cleanEx()
nameEx("nba_win_rate")
### * nba_win_rate

flush(stderr()); flush(stdout())

### Name: nba_win_rate
### Title: Predict NBA Team Win Rate
### Aliases: nba_win_rate

### ** Examples

nba_win_rate(year = 2023)



cleanEx()
nameEx("player_visual")
### * player_visual

flush(stderr()); flush(stdout())

### Name: player_visual
### Title: Visualize a player's shot percentage and distribution on a
###   season
### Aliases: player_visual

### ** Examples

library(NBADP)
player_visual(teamdata = GSW,"S. Curry","GSW")



cleanEx()
nameEx("pred_salary")
### * pred_salary

flush(stderr()); flush(stdout())

### Name: pred_salary
### Title: Make a salary forecast for 2023-2024
### Aliases: pred_salary

### ** Examples

library(NBADP)
pred_salary(0.6, 3.9, 2.1, 0.4, 1.4, 6, 0, 0.8, 13.4, 1.4, 13.4, -2.9, 0.6)




cleanEx()
nameEx("shot_visual")
### * shot_visual

flush(stderr()); flush(stdout())

### Name: shot_visual
### Title: Visualize the 2 PT percentage and 3 PT percentage of all the
###   players
### Aliases: shot_visual

### ** Examples

library(NBADP)
shot_visual(data1 = shot, team1 = "NOP",opponent1 = "NOP")




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
