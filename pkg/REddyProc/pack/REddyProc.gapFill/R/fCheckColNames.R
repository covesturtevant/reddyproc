##############################################################################################
#' @title Check if specified columns exists in data frame

#' @author 
#' AMM

#' @description 
#' Check and set quality flag

#' @param Data.F Data frame
#' @param ColNames.V.s  Character Vector of variables to be checked

#' @references None

#' @return Nothing. Function stops on error


#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-08)
#     original creation after fCheckColNames function in DataFunctions.R
##############################################################################################

fCheckColNames <- function(
  ##description<<
  ## Check if specified columns exists in data frame
  Data.F                ##<< Data frame
  ,ColNames.V.s         ##<< Vector of variables to be checked
)
  ##author<<
  ## AMM
  # TEST: Data.F <- Date.F.x; ColNames.V.s <- c('Year.n', 'none', 'Month.n', 'test'); CallFunction.s <- 'Dummy'
{

  # Check variable to fill and apply quality flag
  if(!(setdiff(c(Var.s, QFVar.s),'none') %in% names(sDATA))){
    stop('Missing specified columns in dataset (or sDateTime)')
  }
  
  ##value<< 
  ## Function stops on errors.
}
