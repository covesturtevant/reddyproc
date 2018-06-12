##############################################################################################
#' @title Converts all gap flags to NA

#' @author 
#' AMM

#' @description 
#' Converts all gap flags to NA

#' @param Data.F Data frame
#' @param GapFlag.n Flag value used for gaps, defaults to -9999.0

#' @references None

#' @return Data.F with gap flags turned to NA

#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-12)
#     original creation after fConvertGapsToNA function in DataFunctions.R
##############################################################################################

fConvertGapsToNA <- function(
  ##description<<
  ## Converts all gap flags to NA
  Data.F                ##<< Data frame to be converted
  ,GapFlag.n=-9999.0    ##<< Flag value used for gaps, defaults to -9999.0 
  #TEST: Data.F <- ResultData.D
)
  ##author<<
  ## AMM
{
  Gaps.i <- sum(Data.F==GapFlag.n, na.rm=T)
  Data.F[Data.F==GapFlag.n] <- NA_real_
  message('Number of \'', GapFlag.n, '\' convertered to NA: ', Gaps.i)
  
  Data.F
  ##value<<
  ## Data frame with NAs instead of gaps.
}
