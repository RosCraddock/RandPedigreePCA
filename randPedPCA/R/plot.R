


#' @method plot rppca
#' @export
plot.rppca <- function(x, dims=c(1,2), to=10000, col=NULL, xlab=NULL, ylab=NULL, ...){
  # Summary throws a warning if there are no variance proportions
  # We'll suppress this as we handle both cases, with and without variance components.
  ss <- suppressWarnings(summary(x)$importance) # get variance components

  if(is.null(x$ds)) x <- dspc(x, to) # add index for downsampling (if not present)

  if(!is.null(col)) {
    if(length(col)>1){
      # warn if col has different length to number of individuals
      if(length(col) != nrow(x$x)) warning(paste0("The length of col was ", length(col), " but there were ", nrow(x$x), " individuals."))

      message("Downsampling colours.")
      cols <- col[x$ds] # downsample colours if any
    } else {
      cols <- col
    }
  } else { # if no colours set
    cols <- 1
  }
  if(dim(ss)[1] == 3){ # add var proportions if any
    plot(
      x$x[x$ds,dims],
      xlab=ifelse(is.null(xlab), paste0("PC", dims[1], " (", signif(100*ss[2,dims[1]], 3), "%)"), xlab),
      ylab=ifelse(is.null(ylab), paste0("PC", dims[2], " (", signif(100*ss[2,dims[2]], 3), "%)"), ylab),
      col = cols,
      ...
    )
  } else {
    plot(
      x$x[x$ds,dims],
      col = cols,
      xlab=ifelse(is.null(xlab), paste0("PC", dims[1]), xlab),
      ylab=ifelse(is.null(ylab), paste0("PC", dims[2]), ylab),
      ...
    )
  }
}
