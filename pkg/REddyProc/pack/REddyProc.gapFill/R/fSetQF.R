##############################################################################################
#' @title Check and set quality flag

#' @author 
#' AMM

#' @description 
#' Check and set quality flag

#' @param Data.F Data frame
#' @param Var.s Variable to be filled
#' @param QFVar.s Quality flag of variable to be filled
#' @param QFValue.n Value of quality flag for _good_ (original) data, other data is set to missing


#' @references None

#' @return Var.s with quality flags applied

#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-08)
#     original creation after sSetQF function in DataFunctions.R
##############################################################################################

fSetQF <- function(
  ##title<<
  ## Check and set quality flag
  ##description<<
  ## Generate new vector from data and quality flag column.
  Data.F                ##<< Data frame
  ,Var.s                ##<< Variable to be filtered
  ,QFVar.s              ##<< Quality flag of variable to be filtered
  ,QFValue.n            ##<< Numeric value of quality flag for _good_ data, other data is set to missing
)
  ##author<<
  ## AMM
  # TEST: Data.F <- EddyData.F; Var.s <- 'NEE'; QFVar.s <- 'QF'; QFValue.n <- 0; CallFunction.s='dummy'
{
  # Check if specified columns exist and are numeric

  # Check variable to fill and apply quality flag
  if(!(setdiff(c(Var.s, QFVar.s),'none') %in% names(sDATA))){
    stop('Missing specified columns in dataset')
  }
  if(sum(!(unlist(lapply(Data.F[setdiff(c(Var.s, QFVar.s),'none')],is.numeric)))) != 0){
    stop('One or more Specified columns in dataset are not numeric')
  }
  
  ##details<<
  ## Quality flag will be applied to variable - unless quality flag variables is called 'none' (dummy).
  if (QFVar.s != 'none') {
    # Check quality flag value
    if( !is.numeric(QFValue.n)){
      stop(paste0('Quality flag \'', QFVar.s, '\' has a non-numeric value: ', QFValue.n))
    }
    # Only use data values when good data (quality flag is equal to flag value)
    Var.V.n <- ifelse(Data.F[,QFVar.s]==QFValue.n, Data.F[,Var.s], NA_real_)
    if( sum(!is.na(Var.V.n)) == 0 )
      warning(paste0('Variable \'', Var.s, '\' contains no data after applying quality flag \'', QFVar.s, '\' with value ', QFValue.n, '!'))
  } else {
    # Use all data
    Var.V.n <- Data.F[,Var.s]
    if( sum(!is.na(Var.V.n)) == 0 )
      warning(paste0('Variable \'', Var.s, '\' contains no data!'))
  }
  # Add units
  attr(Var.V.n, 'units') <- attr(Data.F[[Var.s]], 'units')
  attr(Var.V.n, 'varnames') <- if( QFVar.s == 'none' ) { paste(Var.s, sep='')
  } else { paste(Var.s, '.', QFVar.s, '_', round(QFValue.n, digits=3), sep='') }
  
  Var.V.n
  ##value<< 
  ## Numeric vector with _good_ data.
}
