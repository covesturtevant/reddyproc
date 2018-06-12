##############################################################################################
#' @title Check if variable is a numeric

#' @author 
#' AMM

#' @description 
#' Check if variable is a numeric

#' @param Value.n Value to be checked if numeric (but can also be NA of any type)

#' @references None

#' @return Logical value indicating variable is numeric (TRUE) or not-numeric (FALSE)

#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-08)
#     original creation after fCheckColNames function in DataFunctions.R
##############################################################################################
fCheckValNum <- function(
  ##description<<
  ## Check if variable is a numeric
  Value.n               ##<< Value to be checked if numeric (but can also be NA of any type)
)
  ##author<<
  ## AMM
  ##details<<
  ## See test_CheckValue.R for more details.
{
  if(  length(Value.n) == 0 ) {
    FALSE
  } else if( !is.na(Value.n) && !is.numeric(Value.n) ) {
    FALSE
  } else {
    TRUE
  }
  ##value<<
  ## Boolean value if true of false.
}
