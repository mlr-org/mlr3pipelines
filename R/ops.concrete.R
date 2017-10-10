cpoNull = PipeNode$new(
  id = "null",
  in.format = "task",
  out.format = "task",
  train = function(inlist, par.vals) {
    list(
      control = list(),
      task = task
    )
  },

  predict = function(inlist, control) {
    list(
      task = inlist$task
    )
  },

  par.set = ParamSetFlat$new()
)

cpoDropConst = PipeNode$new(
  id = "dropconst",
  in.format = "data-target",
  out.format = "data",
  
  train = function(inlist, par.vals) {

  # perc = 0, na.ignore = FALSE, tol = .Machine$double.eps^.5
    
    #FIXME: where to put these asserts?
    # can phng handle this now automatically?

    # assertNumber(perc, lower = 0, upper = 1)
    # assertSubset(dont.rm, choices = names(data))
    # assertFlag(na.ignore)
    # assertNumber(tol, lower = 0)
    # assertFlag(show.info)

    isEqual = function(x, y) {
      res = (x == y) | (is.na(x) & is.na(y))
      replace(res, is.na(res), FALSE)
    }
    #FIXME: this function was really slow due to computeMode in BBmisc. what to do?
    # sorting the vector first is probably a good idea. then iterate in C
    digits = ceiling(log10(1 / tol))
    cns = setdiff(colnames(data), dont.rm)
    ratio = vnapply(data[cns], function(x) {
      if (allMissing(x))
        return(0)
      if (is.double(x))
        x = round(x, digits = digits)
      m = computeMode(x, na.rm = na.ignore, ties.method = "first")
      if (na.ignore) {
        mean(m != x, na.rm = TRUE)
      } else {
        mean(!isEqual(x, m))
      }
    }, use.names = FALSE)

    dropped.cols = cns[ratio <= perc]
    # FIXME: do we show such messages in verbose mode?
    # if (show.info && length(dropcols))
      # messagef("Removing %i columns: %s", length(dropcols), collapse(dropcols))
    dropNamed(data, dropcols)
    list(
      data = dropNamed(data, drop.cols),
      drop.cols = drop.cols
    )
  },

  predict = function(inlist, control) {
    data[control$dropped.cols]
  },
  
  par.set = ParamSetFlat$new(params = list(
    ParamReal$new("perc", default = 0.005, lower = 0, upper = 1),
    ParamReal$new("tol", default = .Machine$double.eps^.5, lower = 0, upper = 1),
    ParamFlag$new(na.ignore, default = FALSE)
  ))
)

# simple feature transform, no hyperpars
cpoPca = PipeNode$new(
  id = "pca",
  in.format = "data-target",
  out.format = "data",
  train = function(inlist, par.vals) {
    pcr = prcomp(as.matrix(inlist$data), center = FALSE, scale. = FALSE)
    list(
      control = list(rotation = pcr$rotation),
      data = as.data.frame(pcr$x)
    )
  },

  predict = function(inlist, control) {
    list(
      data = as.data.frame(as.matrix(inlist$data) %*% control$rotation)
    )
  },

  par.set = ParamSetFlat$new()
)

# simple feature transform, but hyperpars
cpoScale = PipeNode$new(
  id = "scale",
  in.format = "data-target",
  out.format = "data",
  
  train = function(inlist, par.vals) {
    sc = scale(as.matrix(inlist$data), center = inlist$par.vals$center, scale = inlist$par.vals$scale)
    
    list(
      control = list(
        center = attr(sc, "scaled:center") %??% FALSE, 
        scale = attr(sc, "scaled:scale") %??% FALSE
      ),
      data = as.data.frame(sc)
    )
  },

  predict = function(inlist, control) {
    list(
      data = as.data.frame(scale(as.matrix(inlist$data), center = control$center, scale = control$scale))
    )
  },
  
  par.set = ParamSetFlat$new(params = list(
    ParamFlag$new("center", default = TRUE),
    ParamFlag$new("scale", default = TRUE)
  ))
)
