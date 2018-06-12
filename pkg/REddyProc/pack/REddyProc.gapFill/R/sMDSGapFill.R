##############################################################################################
#' @title  MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.

#' @author 
#' AMM

#' @description 
#'  MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein (2005).

#' @param Var.s Character. Name of Variable to be filled
#' @param QFVar.s Character. Name of Quality flag of variable to be filled
#' @param QFValue.n Value of quality flag for _good_ (original) data, other data is set to missing
#' @param V1.s Character. Name of condition variable 1
#' @param T1.n Tolerance interval 1
#' @param V2.s Character. Name of condition variable 2
#' @param T2.n Tolerance interval 2
#' @param V3.s Character. Name of condition variable 3
#' @param T3.n Tolerance interval 3
#' @param FillAll.b Logical. Fill all values to estimate uncertainties?
#' @param Verbose.b Logical. Print status information to screen
#' @param Suffix.s String suffix needed for different processing setups on the same dataset (for explanations see below)

#' @references 
#' M. Reichstein, E. Falge, D. Baldocchi, D. Papale, M. Aubinet, P. Berbigier, C. Bernhofer, N. Buchmann, T. Gilmanov, A. Granier, T. Grunwald, K. Havrankova, H. Ilvesniemi, D. Janous, A. Knohl, T. Laurila, A. Lohila, D. Loustau, G. Matteucci, T. Meyers, F. Miglietta, J.M. Ourcival, J. Pumpanen, S. Rambal, E. Rotenberg, M. Sanz, J. Tenhunen, G. Seufert, F. Vaccari, T. Vesala, D. Yakir, R. Valentini: On the separation of net ecosystem exchange into assimilation and ecosystem respiration: review and improved algorithm, Global Change Biology 11(9) 1424–1439 (2005)

#' @return A data frame of:\cr
#' \code{'VAR_orig','VAR_f','VAR_fall','VAR_fnum','VAR_fsd','VAR_fwin'}


#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-08)
#     original creation after sMDSGapFill function in EddyGapFilling.R
##############################################################################################

sMDSGapFill = function(
  ##title<< 
  ## sEddyProc$sMDSGapFill - MDS gap filling algorithm
  ##description<<
  ## MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.
  sDATA # Named data frame with all variables indicated below as well as variable 'sDateTime' in POSIXct format
  ,Var.s                 ##<< Variable to be filled
  ,QFVar.s='none'       ##<< Quality flag of variable to be filled
  ,QFValue.n=NA_real_   ##<< Value of quality flag for _good_ (original) data, other data is set to missing
  ,V1.s='Rg'            ##<< Condition variable 1 (default: Global radiation 'Rg' in  W m-2)
  ,T1.n=50              ##<< Tolerance interval 1 (default: 50 W m-2)
  ,V2.s='VPD'           ##<< Condition variable 2 (default: Vapour pressure deficit 'VPD' in hPa)
  ,T2.n=5               ##<< Tolerance interval 2 (default: 5 hPa)
  ,V3.s='Tair'          ##<< Condition variable 3 (default: Air temperature 'Tair' in degC)
  ,T3.n=2.5             ##<< Tolerance interval 3 (default: 2.5 degC)
  ,FillAll.b=TRUE       ##<< Fill all values to estimate uncertainties
  ,Verbose.b=TRUE       ##<< Print status information to screen
  ,Suffix.s = ''	      ##<< String suffix needed for different processing setups on the same dataset (for explanations see below)
  #! ,QF.V.b = TRUE        ##<< boolean vector of length nRow(sData), to allow specifying bad data directly (those entries that are set to FALSE)
)
##author<<
## AMM, TW
##references<<
## Reichstein, M. et al. (2005) On the separation of net ecosystem exchange 
## into assimilation and ecosystem respiration: review and improved algorithm. Global Change Biology, 11, 1424-1439.
{
  'MDS gap filling algorithm adapted after the PV-Wave code and paper by Markus Reichstein.'
  
  TimeStart.p <- Sys.time()
  ##details<<
  ## Initialize temporal data frame sTEMP for newly generated gap filled data and qualifiers, see \code{\link{sFillInit}} for explanations on suffixes.
  # sTEMP <<- sTEMP[,1L,drop=FALSE]
  rpt <- sFillInit(sDATA,Var.s, QFVar.s, QFValue.n, FillAll.b )
  sTEMP <- rpt$sTEMP
  sINFO <- rpt$sINFO
  rm('rpt')

  #+++ Handling of special cases of meteo condition variables V1.s, V2.s, V3.s
  # If variables are at default values but do not exist as columns, set to 'none' (=disabled identifier).
  # This allows running MDS with less variables than prescribed in the default setting.
  # If meteo condition variable are same as variable to fill, also set to 'none'.
  # This prevents filling artificial gaps (for uncertainty estimates) with itself as meteo condition variable.
  #! Attention: Non-congruent with MR PV-Wave. There artificial gaps in Rg, VPD, Tair are filled with itself.
  if( (V1.s ==   'Rg' && !(V1.s %in% c(colnames(sDATA)))) || (V1.s == Var.s) )   V1.s <- 'none'
  if( (V2.s ==  'VPD' && !(V2.s %in% c(colnames(sDATA)))) || (V2.s == Var.s) )   V2.s <- 'none'
  if( (V3.s == 'Tair' && !(V3.s %in% c(colnames(sDATA)))) || (V3.s == Var.s) )   V3.s <- 'none'
  
  # Check column names (with 'none' as dummy)
  # (Numeric type and plausibility have been checked on initialization of sEddyProc)
  fCheckColNames(cbind(sDATA,sTEMP), c(V1.s, V2.s, V3.s), 'sMDSGapFill')
  
  # Check tolerance entries (if condition variable is not 'none')
  NoneCols.b <- c(V1.s, V2.s, V3.s) %in% 'none'
  if( !fCheckValNum(T1.n) || !fCheckValNum(T2.n) || !fCheckValNum(T3.n) ) {
    stop('sMDSGapFill::: T1.n, T2.n, T3.n, T4.n, T5.n must be numeric (if not specified, set to NA_real_)!')
  }
  if( sum(is.na(c(T1.n, T2.n, T3.n)[!NoneCols.b])) )
    stop('sMDSGapFill::: If condition variable is specified (dummy name is \'none\'), the tolerance interval must be specified!')
  
  # Run gap filling scheme depending on auxiliary meteo data availability
  ##details<<
  ## MDS gap filling algorithm calls the subroutines Look Up Table \code{\link{sFillLUT}} 
  ## and Mean Diurnal Course \code{\link{sFillMDC}} with different window sizes as described in the reference.
  ##details<<
  ## To run dataset only with MDC algorithm \code{\link{sFillMDC}}, set condition variable V1.s to 'none'.
  
  # Check availablility of meteorological data for LUT
  Met.n <- 
    if( V1.s != 'none' && V2.s != 'none' && V3.s != 'none' 
        && sum(!is.na(sDATA[,V1.s]))!=0 && sum(!is.na(sDATA[,V2.s]))!=0 && sum(!is.na(sDATA[,V3.s]))!=0 ) {  
      #All three meteo conditions are available and valid to use:
      message('Full MDS algorithm for gap filling of \'', attr(sTEMP$VAR_f,'varnames'), '\' with LUT(',V1.s,',',V2.s,',',V3.s,') and MDC.')
      3
    } else if( V1.s != 'none' && sum(!is.na(sDATA[,V1.s]))!=0 ) {
      #Only one meteo condition available for LUT
      message('Limited MDS algorithm for gap filling of \'', attr(sTEMP$VAR_f,'varnames'), '\' with LUT(',V1.s,' only) and MDC.')
      1
    } else {
      #No meteo condition available (use MDC only)
      message('Restriced MDS algorithm for gap filling of \'', attr(sTEMP$VAR_f,'varnames'), '\' with no meteo conditions and hence only MDC.')
      if (Var.s != 'Rg') warning('sMDSGapFill::: No meteo available for MDS gap filling!')
      0
    } 
  
  #+++ Full MDS algorithm
  # Step 1: Look-up table (method 1) with window size ±7 days
  if( Met.n == 3 ) sFillLUT(sTEMP, sINFO, 7, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
  # Step 2: Look-up table (method 1) with window size ±14 days
  if( Met.n == 3 ) sFillLUT(sTEMP, sINFO, 14, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
  # Step 3: Look-up table, Rg only (method 2) with window size ±7 days, 
  if( Met.n == 3 || Met.n == 1) sFillLUT(sTEMP, sINFO, 7, V1.s, T1.n, Verbose.b=Verbose.b)
  # Step 4: Mean diurnal course (method 3) with window size 0 (same day)
  sFillMDC(0, Verbose.b=Verbose.b)
  # Step 5: Mean diurnal course (method 3) with window size ±1, ±2 days
  sFillMDC(1, Verbose.b=Verbose.b)
  sFillMDC(2, Verbose.b=Verbose.b)
  # Step 6: Look-up table (method 1) with window size ±21, ±28, ..., ±70   
  if( Met.n == 3 ) for( WinDays.i in seq(21,70,7) ) sFillLUT(WinDays.i, V1.s, T1.n, V2.s, T2.n, V3.s, T3.n, Verbose.b=Verbose.b)
  # Step 7: Look-up table (method 2) with window size ±14, ±21, ..., ±70  
  if( Met.n == 3 || Met.n == 1) for( WinDays.i in seq(14,70,7) ) sFillLUT(WinDays.i, V1.s, T1.n, Verbose.b=Verbose.b)
  # Step 8: Mean diurnal course (method 3) with window size ±7, ±14, ..., ±210 days 
  for( WinDays.i in seq(7,210,7) ) sFillMDC(WinDays.i, Verbose.b=Verbose.b)
  
  # Set long gaps again to NA
  sTEMP$VAR_fall <- suppressMessages(fConvertGapsToNA(sTEMP$VAR_fall))
  
  # Message on gap filling
  TimeDiff.p <- as.numeric(Sys.time()) - as.numeric(TimeStart.p)
  message('Finished gap filling of \'', Var.s, '\' in ', floor(TimeDiff.p), ' seconds. Artificial gaps filled: ', length(sTEMP$VAR_fall) - sum(is.na(sTEMP$VAR_fall)),
          ', real gaps filled: ', sum(is.na(sTEMP$VAR_orig)), 
          ', unfilled (long) gaps: ', sum(is.na(sTEMP$VAR_fall)), '.')
  
  ##details<< \describe{\item{Different processing setups on the same dataset}{
  ## Attention: When processing the same site data set with different setups for the gap filling or flux partitioning 
  ## (e.g. due to different ustar filters),
  ## a string suffix is needed! This suffix is added to the result column names to distinguish the results of the different setups.
  ## }}
  # Rename new columns generated during gap filling
  colnames(sTEMP) <- gsub('VAR_', paste(Var.s, (if(fCheckValString(Suffix.s)) '_' else ''), Suffix.s, '_', sep=''), colnames(sTEMP))
  # Check for duplicate columns (to detect if different processing setups were executed without different suffixes)
  if( length(names(which(table(colnames(sTEMP)) > 1))) )  {                                                                                                                                 
    warning('sMDSGapFill::: Duplicated columns found! Please specify different Suffix.s when processing different setups on the same dataset!')
  }
  
  return(sTEMP)
  ##value<< 
  ## Gap filling results in sTEMP data frame (with renamed columns).
}