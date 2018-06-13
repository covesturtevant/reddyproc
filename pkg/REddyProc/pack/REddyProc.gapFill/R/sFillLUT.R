##############################################################################################
#' @title Gap-fill with Look up Table

#' @author 
#' AMM

#' @description 
#' Gap-fill using Look up Table, after Reichstein et al. (2005)

#' @param sDATA # original dataset
#' @param sTEMP # Initialized data frame sTEMP for newly generated gap filled data and qualifiers, as produced by sFillInit
#' @param sINFO # Information on gap-filling dataset, as produced by sFillInit
#' @param WinDays.i Numeric. Window size for filling [days]
#' @param V1.s Condition variable 1
#' @param T1.n Tolerance interval 1
#' @param V2.s 
#' @param T2.n 
#' @param V3.s 
#' @param T3.n 
#' @param V4.s 
#' @param T4.n 
#' @param V5.s 
#' @param T5.n 

#' @references 
#' M. Reichstein, E. Falge, D. Baldocchi, D. Papale, M. Aubinet, P. Berbigier, C. Bernhofer, N. Buchmann, T. Gilmanov, A. Granier, T. Grunwald, K. Havrankova, H. Ilvesniemi, D. Janous, A. Knohl, T. Laurila, A. Lohila, D. Loustau, G. Matteucci, T. Meyers, F. Miglietta, J.M. Ourcival, J. Pumpanen, S. Rambal, E. Rotenberg, M. Sanz, J. Tenhunen, G. Seufert, F. Vaccari, T. Vesala, D. Yakir, R. Valentini: On the separation of net ecosystem exchange into assimilation and ecosystem respiration: review and improved algorithm, Global Change Biology 11(9) 1424–1439 (2005)

#' @return A data frame of:\cr
#' \code{'VAR_orig','VAR_f','VAR_fall','VAR_fnum','VAR_fsd','VAR_fwin'}


#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-05)
#     original creation after sFillLUT function in EddyGapFilling.R
##############################################################################################

sFillLUT = function(
  ##title<<
  ## sEddyProc$sFillLUT - Gap filling with Look-Up Table (LUT)
  ##description<<
  ## Look-Up Table (LUT) algorithm of up to five conditions within prescribed window size
  sDATA # Original dataset
  ,sTEMP # Initialized data frame sTEMP for newly generated gap filled data and qualifiers, as produced by sFillInit
  ,sINFO # Information on gap-filling dataset, as produced by sFillInit
  ,WinDays.i             ##<< Window size for filling in days
  ,V1.s='none'          ##<< Condition variable 1
  ,T1.n=NA_real_        ##<< Tolerance interval 1
  ,V2.s='none'          ##<< Condition variable 2
  ,T2.n=NA_real_        ##<< Tolerance interval 2
  ,V3.s='none'          ##<< Condition variable 3
  ,T3.n=NA_real_        ##<< Tolerance interval 3
  ,V4.s='none'          ##<< Condition variable 4
  ,T4.n=NA_real_        ##<< Tolerance interval 4
  ,V5.s='none'          ##<< Condition variable 5
  ,T5.n=NA_real_        ##<< Tolerance interval 5
  ,Verbose.b=TRUE       ##<< Print status information to screen
)
##author<<
## AMM
#TEST: WinDays.i <- 7; Pos.i <- 18;
{
  'Look-Up Table (LUT) algorithm of up to five conditions within prescribed window size'
  
  #! Attention: For performance reasons, gap filled values and properties are first written to single variables and local matrix lGF.M
  #! (rather than changing single values in sTEMP which copies the data frame each time!)
  #! Improved algorithm speed by more than a factor of 10 (maybe even 100...)
  lGF.M <- matrix(NA_real_, nrow=0, ncol=7, dimnames=list(NULL,c('index','mean','fnum','fsd','fmeth','fwin','fqc')))
  
  # Check if sTEMP has been initialized with new VAR_ columns
  if( !exists('VAR_f', sTEMP) )
    stop('sFillLUT::: Temporal data frame sTEMP for processing results has not been initalized with sFillInit!')
  
  # Determine gap positions
  ToBeFilled.V.i <- which(is.na(sTEMP$VAR_fall))
  if( length(ToBeFilled.V.i) > 0 ) {
    for( Pos.i in 1:length(ToBeFilled.V.i) ) {
      # Message on progress if wanted
      NoneCols.b <- c(V1.s, V2.s, V3.s, V4.s, V5.s) %in% 'none' 
      if( Verbose.b && Pos.i ==1 )  message('Look up table with window size of ', WinDays.i, ' days with ', 
                                            paste(c(V1.s, V2.s, V3.s, V4.s, V5.s)[!NoneCols.b], collapse=' '))
      # Set window size
      Gap.i   <- ToBeFilled.V.i[Pos.i]
      if (T==T) {
        #! Non-congruent with MR PV-Wave
        Start.i <- Gap.i - (WinDays.i*sINFO$DTS)
        End.i   <- Gap.i + (WinDays.i*sINFO$DTS)
      } else {
        #! Code congruent with MR PV-Wave --> window size minus 1
        Start.i <- Gap.i - (WinDays.i*sINFO$DTS - 1)
        End.i   <- Gap.i + (WinDays.i*sINFO$DTS - 1)
      }
      
      if( Start.i <= 0 ) Start.i <- 1
      if( End.i > nrow(sTEMP) ) End.i <- nrow(sTEMP)
      
      #! Special treatment of Rg to be congruent with MR PV-Wave, in paper not mentioned 
      T1red.n <- if( grepl('Rg', V1.s) ) {
        # Reduce tolerance of radiation if variable name contains 'Rg' to [20,50] depending on measurement
        max(min(T1.n, sDATA[Gap.i,V1.s], na.rm=T), 20, na.rm=T)
      } else {
        T1.n
      }
      
      # For performance reasons, write sDATA subrange into vectors (speed up about factor of 1.5)
      V1.V.n <- sDATA[Start.i:End.i, V1.s]
      V2.V.n <- sDATA[Start.i:End.i, V2.s]
      V3.V.n <- sDATA[Start.i:End.i, V3.s]
      V4.V.n <- sDATA[Start.i:End.i, V4.s]
      V5.V.n <- sDATA[Start.i:End.i, V5.s]
      SubGap.i <- Gap.i - (Start.i-1)
      
      # Set LUT fill condition
      Rows.V.b <- !is.na(sTEMP$VAR_orig[Start.i:End.i])
      if( V1.s != 'none' )
        Rows.V.b <- Rows.V.b & abs(V1.V.n - V1.V.n[SubGap.i]) < T1red.n  & !is.na(V1.V.n)
      if( V2.s != 'none' )
        Rows.V.b <- Rows.V.b & abs(V2.V.n - V2.V.n[SubGap.i]) < T2.n  & !is.na(V2.V.n)
      if( V3.s != 'none' )
        Rows.V.b <- Rows.V.b & abs(V3.V.n - V3.V.n[SubGap.i]) < T3.n  & !is.na(V3.V.n)
      if( V4.s != 'none' )
        Rows.V.b <- Rows.V.b & abs(V4.V.n - V4.V.n[SubGap.i]) < T4.n  & !is.na(V4.V.n)
      if( V5.s != 'none' )
        Rows.V.b <- Rows.V.b & abs(V5.V.n - V5.V.n[SubGap.i]) < T5.n  & !is.na(V5.V.n)
      lLUT.V.n <- subset(sTEMP$VAR_orig[Start.i:End.i], Rows.V.b)
      
      # If enough available data, fill gap
      if( length(lLUT.V.n) > 1 ){
        lVAR_index.i <- Gap.i
        lVAR_mean.n <- mean(lLUT.V.n)
        lVAR_fnum.n <- length(lLUT.V.n)
        lVAR_fsd.n <- sd(lLUT.V.n)
        
        #Set window size and quality flag
        lVAR_fwin.n  <- 2*WinDays.i                      #! Full window length, congruent with MR PV-Wave, in paper single window sizes stated
        lVAR_fmeth.n <- NA_real_; lVAR_fqc.n <- NA_real_;
        if( V1.s != 'none' && V2.s != 'none' && V3.s != 'none') { #Three conditions
          lVAR_fmeth.n <- 1
          if( lVAR_fwin.n <= 14 ) lVAR_fqc.n <- 1        #! Limit '14' congruent with MR PV-Wave, in paper different limit of '28' (stated as single window size of 14 days)
          if( lVAR_fwin.n >  14 & lVAR_fwin.n <= 56 ) lVAR_fqc.n <- 2
          if( lVAR_fwin.n >  56 ) lVAR_fqc.n <- 3
        }
        if( V1.s != 'none' && V2.s == 'none' && V3.s == 'none') { #One condition only
          lVAR_fmeth.n <- 2
          if( lVAR_fwin.n <= 14 ) lVAR_fqc.n <- 1
          if( lVAR_fwin.n >  14 & lVAR_fwin.n <= 28 ) lVAR_fqc.n <- 2
          if( lVAR_fwin.n >  28 ) lVAR_fqc.n <- 3          
        } 
        lGF.M <- rbind(lGF.M, c(lVAR_index.i, lVAR_mean.n, lVAR_fnum.n, lVAR_fsd.n, lVAR_fmeth.n, lVAR_fwin.n, lVAR_fqc.n))
      }
      if( Verbose.b && Pos.i%%100 == 0 )  message('.', appendLF=FALSE)
      if( Verbose.b && Pos.i%%6000 == 0 ) message('\n.', appendLF=FALSE)
    }
    if( Verbose.b ) message('', nrow(lGF.M))
  }
  # Copy gap filled values and properties to sTEMP
  if( nrow(lGF.M) > 0 ) {
    # Fill all rows in VAR_fall and co
    sTEMP[lGF.M[,'index'],c('VAR_fall','VAR_fnum','VAR_fsd','VAR_fmeth','VAR_fwin','VAR_fall_qc')] <- 
      lGF.M[,c('mean','fnum','fsd','fmeth','fwin','fqc')]
    # Only fill gaps in VAR_f and VAR_fqc
    Gaps.b <- is.na(sTEMP[lGF.M[,'index'],'VAR_f'])
    sTEMP[lGF.M[,'index'],c('VAR_f','VAR_fqc')][Gaps.b,] <- as.data.frame(lGF.M[,c('mean','fqc') ,drop=FALSE])[Gaps.b,] 
  }
  
  return(sTEMP)  
  ##value<< 
  ## LUT filling results in sTEMP data frame.
}
