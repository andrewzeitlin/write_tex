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
  panel_titles=NULL,
  # Per-row stat columns ("colstats") that sit alongside the b matrix.
  # Each is a K x S matrix (rows aligned to rows of b) of values to print
  # as additional columns. colstats_left appears between the row-label
  # column and the b columns; colstats_right appears after the b columns.
  # Useful when the rows are the unit of observation (e.g. outcomes), so
  # per-row summaries (control mean/SD on the left, N and joint p-value on
  # the right) belong as columns rather than as footer rows. Values may be
  # numeric or character; format with colstatsfmt_{left,right}, defaulting
  # to '%s' when the matrix is character.
  colstats_left=NULL,
  colstats_right=NULL,
  colstatsfmt_left=NULL,
  colstatsfmt_right=NULL,
  colstatslabels_left=NULL,
  colstatslabels_right=NULL
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

  ##  Coerce colstats vectors to single-column matrices.
  if (!is.null(colstats_left)  && is.null(dim(colstats_left)))
    colstats_left  <- as.matrix(colstats_left)
  if (!is.null(colstats_right) && is.null(dim(colstats_right)))
    colstats_right <- as.matrix(colstats_right)

  ##  Take stock of the width of the table to be exported.
  Y <- dim(b)[2]  # Number of outcomes
  K <- dim(b)[1]  # Number of regressors
  S_left  <- if (is.null(colstats_left))  0 else ncol(colstats_left)
  S_right <- if (is.null(colstats_right)) 0 else ncol(colstats_right)

  ##  Default colstat labels and formats.
  if (S_left > 0) {
    if (is.null(colstatslabels_left))
      colstatslabels_left <- if (!is.null(colnames(colstats_left)))
        colnames(colstats_left) else rep('', S_left)
    if (is.null(colstatsfmt_left))
      colstatsfmt_left <- if (is.character(colstats_left)) '%s' else '%.3f'
    if (length(colstatsfmt_left) == 1 && S_left > 1)
      colstatsfmt_left <- rep(colstatsfmt_left, S_left)
  }
  if (S_right > 0) {
    if (is.null(colstatslabels_right))
      colstatslabels_right <- if (!is.null(colnames(colstats_right)))
        colnames(colstats_right) else rep('', S_right)
    if (is.null(colstatsfmt_right))
      colstatsfmt_right <- if (is.character(colstats_right)) '%s' else '%.3f'
    if (length(colstatsfmt_right) == 1 && S_right > 1)
      colstatsfmt_right <- rep(colstatsfmt_right, S_right)
  }

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
  cat(sprintf('\\begin{tabular}{l *{%i}{S}} \n', S_left + Y + 1 + S_right))
  cat('\\toprule \n')
  if (!is.null(toprow)){
    cat(paste(toprow, ' \\\\ \n', sep=' '))
    if (!is.null(topgroups)){
      cat(paste(topgroups, ' \n', sep= ' '))
    }
  }
  cat('\t')
  if (S_left > 0){
    for (s in 1:S_left){
      cat(sprintf(' & \\multicolumn{1}{c}{%s} ', colstatslabels_left[s]))
    }
  }
  for (y in 1:Y){
    cat(sprintf(' & \\multicolumn{1}{c}{%s} ',ylabels[y]))
  }
  if (S_right > 0){
    for (s in 1:S_right){
      cat(sprintf(' & \\multicolumn{1}{c}{%s} ', colstatslabels_right[s]))
    }
  }
  cat(' \\\\ \n')
  cat('\\midrule \n')

  #  Main body  ------------------------------------------------------------
  for (k in 1:K){

    ##  Check if this is the top of a panel, and if so, write the label
    if (k %in% panel_rows){
      cat(sprintf('\\multicolumn{%i}{c}{\\emph{%s}} \\\\[%s] \n',
                  S_left + Y + S_right,
                  panel_titles[match(k,panel_rows)], row.sep))
    }

    ##  Variable labels
    cat(sprintf('\\multirow[t]{%i}{%s}{%s}',xlabrows,xlabwidth,xlabels[k]))

    ##  Leading colstats
    if (S_left > 0){
      for (s in 1:S_left){
        cat(' & ')
        v <- colstats_left[k,s]
        if (!is.na(v) && !(is.character(v) && v == ''))
          cat(sprintf(colstatsfmt_left[s], v))
      }
    }

    ##  Coefficients
    for (y in 1:Y){
      cat(' & ')
      if (!is.na(b[k,y])) cat(sprintf(fmt,b[k,y]))
    }

    ##  Trailing colstats
    if (S_right > 0){
      for (s in 1:S_right){
        cat(' & ')
        v <- colstats_right[k,s]
        if (!is.na(v) && !(is.character(v) && v == ''))
          cat(sprintf(colstatsfmt_right[s], v))
      }
    }

    ##  Standard errors
    if (!is.null(se)){
      cat(' \\\\ \n')
      cat(' \t ')
      if (S_left > 0) for (s in 1:S_left) cat(' & ')
      for (y in 1:Y){
        if (dim(se)[1]>= k & dim(se)[2]>=y & !is.na(se[k,y])){
          cat(sprintf(paste0(' & (', fmt, ')'),se[k,y]))
        }else{
          cat(' & ')
        }
      }
      if (S_right > 0) for (s in 1:S_right) cat(' & ')
    }
    
    ##  p values
    if (!is.null(p)){
      cat(' \\\\ \n')
      if (S_left > 0) for (s in 1:S_left) cat(' & ')
      for (y in 1:Y){
        if (dim(p)[1]>= k & dim(p)[2] >=y & !is.na(p[k,y])) {
          cat(' & [')
          cat(sprintf(fmt,p[k,y]))
          cat(']')
        } else{
          cat(' & ')
        }
      }
      if (S_right > 0) for (s in 1:S_right) cat(' & ')
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
      if (S_left > 0) for (sl in 1:S_left) cat(' & ')
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
      if (S_right > 0) for (sr in 1:S_right) cat(' & ')
      cat( ' \\\\ \n')
    }
  }
  
  
  #  Table footer  ---------------------------------------------------------
  cat('\\bottomrule \n')
  cat('\\end{tabular} \n')
  
  #  Close output document
	sink() 
}