#' @title Create a combined smooth scatterplot
#' @description  Like smoothScatter, it creates scatterplots with smoothed densities color representation
#' @param x y characters, names of the  x and y coordinates for the plot.
#' If x and y are dataframe columns they must be specifiedn with the dollar sign notation ('DF$col')
#' @param npoints number of equally spaced grid points in both directions (same as nbin in smoothScatter)
#' @param shades number of shades per plot (default 8)
#' @param draw.image boolean specifying if a plot should be drawn. In either case a bitmap is returned
#' that can be plotted subsequently with a call too image(). Default is FALSE.
#' @param labels a boolean, should labels (the names of the 2 vectors) be printed in the plot?
#' @param axes a boolean, should axes be drawn?
#' @param categories a character the name of a factor. If provided, the plot will be split by the levels of this factor
#' @param colour.pool vector of colours that will be picked sequentially, one for each category. The default contains 8 values, if
#' there are more levels, the colours will be recycled.
#' @param emphasize_level a number, which level should be emphasized (stronger colour, more opaque, layered on top)
#' @param type a character, "combine" (the default) or "split"
#' @param async a logical, see datashield.aggregate
#' @param wait a logical, see datashield.aggregate
#' @param  datasources a list of opal objects obtained after logging into the opal servers (see datashield.login)
#' @return  a list with the format (lims = <calculated or specified limits>, img = bitmap of the image to be drawn, legend = the colours used for each category)
#' @export
#'

dssSmooth2d <-  function (x, y, npoints = 128, shades = 8, draw.image = FALSE, labels = TRUE, axes = TRUE,
                           categories = NULL, colour.pool = c('#377eb8','#e41a1c','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#999999'),
                           emphasize_level = 0, type = "combine", async = FALSE, wait = TRUE, datasources = NULL)
{
  if (is.null(datasources)) {
    datasources <- dsBaseClient:::findLoginObjects()
  }
  bandwidth <- list(x = .bw.args(x, datasources), y = .bw.args(y,
                                                               datasources))
  lims <- dssRange(x, y, type = type, datasources = datasources)
  holder <- NULL
  if (!is.null(categories)) {
    keep.cols <- NULL
    holder <- "tempSmooth"
    dssSubsetByClass(c(x, y), subsets = holder, variables = categories,
                      keep.cols = keep.cols, async = async, wait = wait,
                      datasources = datasources)
  }
  bandwidth.arg <- .encode.arg(bandwidth)
  lims.arg <- .encode.arg(lims)
  holder <- .encode.arg(holder)
  expr <- paste0("partKde2d('", holder, "', '", x, "', '",
                 y, "','", bandwidth.arg, "','", lims.arg, "',", npoints,
                 ")")
  res <- datashield.aggregate(datasources, as.symbol(expr),
                                    async = FALSE, wait = wait)
  res <- res[!sapply(res, is.null)]
  if (length(res) == 0) {
    return(NULL)
  }
  if (draw.image) {
    xlab <- ""
    ylab <- ""
    if (labels) {
      xlab <- x
      ylab <- y
    }
  }
  sw <- dssSwapKeys(res)
  if (type == "combine") {
    img <- list(Combined = sapply(sw, function(this.level) {
      list(x = this.level[[1]]$x, y = this.level[[1]]$y,
           z = Reduce("+", lapply(this.level, function(a) {
             a$len * a$z
           }))/Reduce("+", lapply(this.level, function(a) a$len)))
    }, simplify = FALSE))
  }
  else {
    img <- sapply(res, function(this.node) {
      sapply(this.node, function(this.level) {
        this.level[c("x", "y", "z")]
      }, simplify = FALSE)
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  my.legend <- NULL
  if (draw.image) {
    startcols <- list()
    endcols <- list()
    levs <- names(sw)

    while(length(colour.pool) < length(levs)){
      colour.pool <- rep(colour.pool,2)
    }
    endcols[levs] <- colour.pool[1:length(levs)]
    startcols[levs] <- rep('white', length(levs))

    my.legend <- sapply(img, function(this.set) {
      #ret <- list()
      categories <- names(this.set)
      if(emphasize_level >0){
        categories <- c(categories[-emphasize_level], categories[emphasize_level]) # move the important one in the upper layer
      }
      sapply(categories, function(this.name) {
        #ret[[this.name]] <<- endcols[[this.name]]
        if(emphasize_level > 0){
          if(levs[emphasize_level] == this.name){
            this.alpha <- 1
          } else {
            this.alpha <- 0.4

          }

          image(this.set[[this.name]], col = .add.alpha(colorRampPalette(c(startcols[[this.name]],
                                                                                           endcols[[this.name]]))(shades), c(0,rep(this.alpha, length.out = shades-1 ))),
                xlab = xlab, ylab = ylab, axes = axes)

        } else {
          image(this.set[[this.name]], col = .add.alpha(colorRampPalette(c(startcols[[this.name]],
                                                                                           endcols[[this.name]]))(shades), c(0, seq(0.2,
                                                                                                                                    0.8, length.out = shades - 1))), xlab = xlab,
                ylab = ylab, axes = axes)
        }
        par(new = T)


      })
      if(length(levs) > 1){
        plot.new()
        legend(1,1,legend = levs, col = unlist(endcols[levs]),lwd=2, xjust=1, yjust=1 )
      }
      par(new = F)
      #return(ret)
    }, simplify = FALSE)
  }
  ret <- list(lims = lims, img = img)
  #if (!is.null(my.legend)) {
  #  ret[["legend"]] <- unlist(unname(my.legend), recursive = FALSE)
  #}
  invisible(ret)
}
