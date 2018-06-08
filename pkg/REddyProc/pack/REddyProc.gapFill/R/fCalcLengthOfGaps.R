##############################################################################################
#' @title Calculate length of gaps (with flag NA) in specified vector

#' @author 
#' AMM

#' @description 
#' Calculate length of gaps (with flag NA) in specified vector

#' @param Data.V.n Numeric vector with gaps (missing values, NAs)

#' @references None

#' @return VA numeric value - number of missing values

#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-08)
#     original creation after fCalcLengthOfGaps function in DataFunctions.R
##############################################################################################

fCalcLengthOfGaps <- function(
  ##description<<
  ## Calculate length of gaps (with flag NA) in specified vector
  Data.V.n              ##<< Numeric vector with gaps (missing values, NAs)
)
  ##author<<
  ## AMM
  # TEST: Data.V.n <- sDATA$NEE
{
  Data.V.b <- is.na(Data.V.n)
  #Determine cumulative sums of gaps
  CumSum.V.n <- cumsum(Data.V.b)
  #Calculate sum from start of gap
  LenGaps.V.n <- CumSum.V.n-cummax((!Data.V.b)*CumSum.V.n)
  
  LenGaps.V.n
  ##value<< 
  ## An integer vector with length of gap since start of gap.
}