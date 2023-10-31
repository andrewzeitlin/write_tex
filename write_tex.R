write_tex <- function(
  filename,
  b,
  se=NULL,
  p=NULL,
  ci=NULL,
  xlabels=NULL,
  ylabels=NULL,
  fmt='%.2f', 
  xlabwidth='0.2\\textwidth', 
  xlabrows=2, 
  stats=NULL, 
  statsfmt = '3.3f',
  statslabels=NULL,
  centerstats=NULL,
  row.sep='1ex',
  toprow=NULL,
  topgroups=NULL,
  panel_rows=NULL, 
  panel_titles=NULL
  ){
  
  #  Package requirements
  if(is.element('%c',statsfmt)) require(scales)
  
  #  Parse inputs.  
  ##  If b is a vector, convert to matrix and transpose
  if (is.null(dim(b))){
    b <- t(as.matrix(b)) # to matrix, and transpose 
    if (!is.null(se)) se <- t(as.matrix(se))
    if (!is.null(p)) p <- t(as.matrix(p))
  }
  
  ## If Stats exists & is a vector, convert to matrix and transpose
  if ( !is.null(stats) & is.null(dim(stats))){
      stats <- t(as.matrix(stats))
  }
  
  ##  Take stock of the width of the table to be exported.
  Y <- dim(b)[2]  # Number of outcomes
  K <- dim(b)[1]  # Number of regressors
  
  ##  Row and column labels.
  if (is.null(ylabels)) ylabels <- labels(b)
  if (is.null(xlabels)) {
    if (!is.null(rownames(b))){
      xlabels <- rownames(b)  # what to do if this is empty? 
    }else{
      xlabels <- 1:K  # Default. Label with consecutive integers
    }
  } 
  
  #  Open output document
  sink(filename, split=TRUE)
  on.exit(sink())  #  Will close sink in case of error.
  
  #  Table header  ---------------------------------------------------------
  cat(sprintf('\\begin{tabular}{l *{%i}{S}} \n',Y+1))
  cat('\\toprule \n')
  if (!is.null(toprow)){
    cat(paste(toprow, ' \\\\ \n', sep=' '))
    if (!is.null(topgroups)){
      cat(paste(topgroups, ' \n', sep= ' '))
    }
  }
  cat('\t') 
  for (y in 1:Y){
    cat(sprintf(' & \\multicolumn{1}{c}{%s} ',ylabels[y]))
  }
  cat(' \\\\ \n') 
  cat('\\midrule \n') 
  
  #  Main body  ------------------------------------------------------------
  for (k in 1:K){
    
    ##  Check if this is the top of a panel, and if so, write the label
    if (k %in% panel_rows){
      cat(sprintf('\\multicolumn{%i}{c}{\\emph{%s}} \\\\[%s] \n', Y, panel_titles[match(k,panel_rows)], row.sep))
    }

    ##  Variable labels 
    cat(sprintf('\\multirow[t]{%i}{%s}{%s}',xlabrows,xlabwidth,xlabels[k]))
    
    ##  Coefficients
    for (y in 1:Y){
      cat(' & ')
      if (!is.na(b[k,y])) cat(sprintf(fmt,b[k,y]))
    }
    
    ##  Standard errors
    if (!is.null(se)){
      cat(' \\\\ \n') 
      cat(' \t ') 
      for (y in 1:Y){
        if (dim(se)[1]>= k & dim(se)[2]>=y & !is.na(se[k,y])){
          cat(sprintf(paste0(' & (', fmt, ')'),se[k,y]))
        }else{
          cat(' & ') 
        }
      }
    }
    
    ##  p values
    if (!is.null(p)){
      cat(' \\\\ \n') 
      for (y in 1:Y){
        if (dim(p)[1]>= k & dim(p)[2] >=y & !is.na(p[k,y])) {
          cat(' & [')
          cat(sprintf(fmt,p[k,y]))
          cat(']')
        } else{
          cat(' & ') 
        }
      }
    }
    
    ##  TODO:  CIs
    
    cat(sprintf(' \\\\[%s] \n',row.sep)) # ('\\\\ \n') 
    
  }
  
  #  Auxiliary statistics  -------------------------------------------------
  if (!is.null(stats)){
    
    ## Count rows of stats matrix
    S <- dim(stats)[1]
    
    ##  Stats labels -- defaults if not explicitly specified
    if (is.null(statslabels)) {
      if (!is.null(rownames(stats))){
        statslabels <- rownames(stats)  # what to do if this is empty? 
      }else{
        statslabels <- 1:S  # Default. Label with consecutive integers
      }
    } 
    
    ## Propagating stats format across all rows if specified as a single value
    if (length(statsfmt)==1 & S > 1 ) statsfmt <- rep(statsfmt,S) 

    ##  Center test statistics, or use general format for that column?
    if (is.null(centerstats)){
      centerstats <- rep(FALSE, S)
    }
    
    
    ## Loop over stats and write to file. 
    for (s in 1:S){
      cat(statslabels[s])
      for (y in 1:Y){
        cat(' & ')
        if (centerstats[s]) cat('\\multicolumn{1}{c}{')
        if (statsfmt[s] == '%c'){
          cat(sprintf('%s', comma(stats[s,y])))
        }else{
          cat(sprintf(statsfmt[s],stats[s,y]))
        }
        if (centerstats[s]) cat('}')
      }
      cat( ' \\\\ \n') 
    }
  }
  
  
  #  Table footer  ---------------------------------------------------------
  cat('\\bottomrule \n')
  cat('\\end{tabular} \n')
  
  #  Close output document
	sink() 
}