#' Major League Baseball team data
#'
#' Data from all 30 Major League Baseball teams from the 2011 season.
#'
#' @format A data frame with 30 rows and 12 variables:
#' \describe{
#'   \item{team}{Team name.}
#'   \item{runs}{Number of runs.}
#'   \item{at_bats}{Number of at bats.}
#'   \item{hits}{Number of hits.}
#'   \item{homeruns}{Number of home runs.}
#'   \item{bat_avg}{Batting average.}
#'   \item{strikeouts}{Number of strikeouts.}
#'   \item{stolen_bases}{Number of stolen bases.}
#'   \item{wins}{Number of wins.}
#'   \item{new_onbase}{Newer variable: on-base percentage, a measure of
#'       how often a batter reaches base for any reason other than a fielding error,
#'       fielder's choice, dropped/uncaught third strike, fielder's obstruction, or
#'       catcher's interference.}
#'   \item{new_slug}{Newer variable: slugging percentage, popular measure of the
#'       power of a hitter calculated as the total bases divided by at bats.}
#'   \item{new_obs}{Newer variable: on-base plus slugging, calculated as the sum of the on-base and slugging percentages.}
#' }
#' @source \href{http://www.mlb.com/}{mlb.com}
"mlb11"