# Fixes recorded plots that have been serialized to disk.
# Code by JJ Allaire (r-studio) and Jeroen Ooms
# Source: https://github.com/rstudio/rstudio/blob/master/src/cpp/r/R/Tools.R
# Original: http://markmail.org/message/zat2r2pfsvhrsfqz

fixplot <- function(plot){
  # restore native symbols for R >= 3.0
  rVersion <- getRversion()
  if (rVersion >= "3.0")
  {
    for(i in 1:length(plot[[1]]))
    {
      # get the symbol then test if it's a native symbol
      symbol <- plot[[1]][[i]][[2]][[1]]
      if("NativeSymbolInfo" %in% class(symbol))
      {
        # determine the dll that the symbol lives in
        if (!is.null(symbol$package))
          name = symbol$package[["name"]]
        else
          name = symbol$dll[["name"]]
        pkgDLL <- getLoadedDLLs()[[name]]
        
        # reconstruct the native symbol and assign it into the plot
        nativeSymbol <-getNativeSymbolInfo(name = symbol$name,
          PACKAGE = pkgDLL,
          withRegistrationInfo = TRUE);
        plot[[1]][[i]][[2]][[1]] <- nativeSymbol;
      }
    }
  }
  # restore native symbols for R >= 2.14
  else if (rVersion >= "2.14")
  {
    try({
        for(i in 1:length(plot[[1]])) 
        {
          if("NativeSymbolInfo" %in% class(plot[[1]][[i]][[2]][[1]]))
          {
            nativeSymbol <-getNativeSymbolInfo(plot[[1]][[i]][[2]][[1]]$name);
            plot[[1]][[i]][[2]][[1]] <- nativeSymbol;         
          }
        }
      },
      silent = TRUE);
  }
  
  return(plot)
  
  # we suppressWarnings so that R doesnt print a warning if we restore
  # a plot saved from a previous version of R (which will occur if we 
  # do a resume after upgrading the version of R on the server)
  # suppressWarnings(grDevices::replayPlot(plot))
}