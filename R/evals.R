#' Teachers evaluations at the University of Texas at Austin
#'
#' The data were gathered from end of semester student evaluations for a large
#' sample of professors from the University of Texas at Austin (variables beginning
#' with \code{cls}). In addition, six students rated the professors' physical
#' appearance (variables beginning with \code{bty}). (This is a slightly modified
#' version of the original data set that was released as part of the replication
#' data for Data Analysis Using Regression and Multilevel/Hierarchical Models
#' (Gelman and Hill, 2007).
#'
#' @format A data frame with 463 rows and 21 variables:
#' \describe{
#'   \item{score}{Average professor evaluation score: (1) very unsatisfactory - (5) excellent}
#'   \item{rank}{Rank of professor: teaching, tenure track, tenure}
#'   \item{ethnicity}{Ethnicity of professor: not minority, minority}
#'   \item{gender}{Gender of professor: female, male}
#'   \item{language}{Language of school where professor received education: english or non-english}
#'   \item{age}{Age of professor}
#'   \item{cls_perc_eval}{Percent of students in class who completed evaluation}
#'   \item{cls_did_eval}{Number of students in class who completed evaluation}
#'   \item{cls_students}{Total number of students in class}
#'   \item{cls_level}{Class level: lower, upper}
#'   \item{cls_profs}{Number of professors teaching sections in course in sample: single, multiple}
#'   \item{cls_credits}{Number of credits of class: one credit (lab, PE, etc.), multi credit}
#'   \item{bty_f1lower}{Beauty rating of professor from lower level female: (1) lowest - (10) highest}
#'   \item{bty_f1upper}{Beauty rating of professor from upper level female: (1) lowest - (10) highest}
#'   \item{bty_f2upper}{Beauty rating of professor from second upper level female: (1) lowest - (10) highest}
#'   \item{bty_m1lower}{Beauty rating of professor from lower level male: (1) lowest - (10) highest}
#'   \item{bty_m1upper}{Beauty rating of professor from upper level male: (1) lowest - (10) highest}
#'   \item{bty_m2upper}{Beauty rating of professor from second upper level male: (1) lowest - (10) highest}
#'   \item{bty_avg}{Average beauty rating of professor}
#'   \item{pic_outfit}{Outfit of professor in picture: not formal, formal}
#'   \item{pic_color}{Color of professor's picture: color, black & white}
#' }
#' @source These data appear in Hamermesh DS, and Parker A. 2005. Beauty in the
#' classroom: instructors pulchritude and putative pedagogical productivity. Economics of Education Review
#'  24(4):369-376.
"evals"