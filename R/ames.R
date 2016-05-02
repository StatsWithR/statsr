#' Housing prices in Ames, Iowa
#'
#' Data set contains information from the Ames Assessor’s Office used in computing 
#' assessed values for individual residential properties sold in Ames, IA from 2006 
#' to 2010. See http://www.amstat.org/publications/jse/v19n3/decock/datadocumentation.txt 
#' for detailed variable descriptions.
#'
#' @format A tbl_df with with 2930 rows and 82 variables:
#' \describe{
#'   \item{Order}{Observation number.}
#'   \item{PID}{Parcel identification number  - can be used with city web site for parcel review.}
#'   \item{area}{Living area of the house.}
#'   \item{price}{Price of the house.}
#'   \item{MS.SubClass}{Identifies the type of dwelling involved in the sale.}
#'   \item{MS.Zoning}{Identifies the general zoning classification of the sale.}
#'   \item{Lot.Frontage}{Linear feet of street connected to property.}
#'   \item{Lot.Area}{Lot size in square feet.}
#'   \item{Street}{Type of road access to property.}
#'   \item{Alley}{Type of alley access to property.}
#'   \item{Lot.Shape}{}
#'   \item{Land.Contour}{}
#'   \item{Utilities}{}
#'   \item{Lot.Config}{}
#'   \item{Land.Slope}{}
#'   \item{Neighborhood}{}
#'   \item{Condition.1}{}
#'   \item{Condition.2}{}
#'   \item{Bldg.Type}{}
#'   \item{House.Style}{}
#'   \item{Overall.Qual}{}
#'   \item{Overall.Cond}{}
#'   \item{Year.Built}{}
#'   \item{Year.Remod.Add}{}
#'   \item{Roof.Style}{}
#'   \item{Roof.Matl}{}
#'   \item{Exterior.1st}{}
#'   \item{Exterior.2nd}{}
#'   \item{Mas.Vnr.Type}{}
#'   \item{Mas.Vnr.Area}{}
#'   \item{Exter.Qual}{}
#'   \item{Exter.Cond}{}
#'   \item{Foundation}{}
#'   \item{Bsmt.Qual}{}
#'   \item{Bsmt.Cond}{}
#'   \item{Bsmt.Exposure}{}
#'   \item{BsmtFin.Type.1}{}
#'   \item{BsmtFin.SF.1}{}
#'   \item{BsmtFin.Type.2}{}
#'   \item{BsmtFin.SF.2}{}
#'   \item{Bsmt.Unf.SF}{}
#'   \item{Total.Bsmt.SF}{}
#'   \item{Heating}{}
#'   \item{Heating.QC}{}
#'   \item{Central.Air}{}
#'   \item{Electrical}{}
#'   \item{X1st.Flr.SF}{}
#'   \item{X2nd.Flr.SF}{}
#'   \item{Low.Qual.Fin.SF}{}
#'   \item{Bsmt.Full.Bath}{}
#'   \item{Bsmt.Half.Bath}{}
#'   \item{Full.Bath}{}
#'   \item{Half.Bath}{}
#'   \item{Bedroom.AbvGr}{}
#'   \item{Kitchen.AbvGr}{}
#'   \item{Kitchen.Qual}{}
#'   \item{TotRms.AbvGrd}{}
#'   \item{Functional}{}
#'   \item{Fireplaces}{}
#'   \item{Fireplace.Qu}{}
#'   \item{Garage.Type}{}
#'   \item{Garage.Yr.Blt}{}
#'   \item{Garage.Finish}{}
#'   \item{Garage.Cars}{}
#'   \item{Garage.Area}{}
#'   \item{Garage.Qual}{}
#'   \item{Garage.Cond}{}
#'   \item{Paved.Drive}{}
#'   \item{Wood.Deck.SF}{}
#'   \item{Open.Porch.SF}{}
#'   \item{Enclosed.Porch}{}
#'   \item{X3Ssn.Porch}{}
#'   \item{Screen.Porch}{}
#'   \item{Pool.Area}{}
#'   \item{Pool.QC}{}
#'   \item{Fence}{}
#'   \item{Misc.Feature}{}
#'   \item{Misc.Val}{}
#'   \item{Mo.Sold}{}
#'   \item{Yr.Sold}{}
#'   \item{Sale.Type}{}
#'   \item{Sale.Condition}{}
#' }
#' @source De Cock, Dean. "Ames, Iowa: Alternative to the Boston housing data as 
#' an end of semester regression project." Journal of Statistics Education 19.3 (2011).
"ames"