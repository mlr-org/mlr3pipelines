% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert_graph.R
\name{as_graph}
\alias{as_graph}
\title{Conversion to mlr3pipelines Graph}
\usage{
as_graph(x, clone = FALSE)
}
\arguments{
\item{x}{(\code{any}) \cr
Object to convert.}

\item{clone}{(\code{logical(1)}) \cr
Whether to return a (deep copied) clone if \code{x} is a Graph.}
}
\value{
\code{\link{Graph}} \code{x} or a deep clone of it.
}
\description{
The argument is turned into a \code{\link{Graph}} if possible.
If \code{clone} is \code{TRUE}, a deep copy is made
if the incoming object is a \code{\link{Graph}} to ensure the resulting
object is a different reference from the incoming object.

\code{\link[=as_graph]{as_graph()}} is an S3 method and can therefore be implemented
by other packages that may add objects that can naturally be converted to \code{\link{Graph}}s.

By default, \code{\link[=as_graph]{as_graph()}} tries to
\itemize{
\item apply \code{\link[=gunion]{gunion()}} to \code{x} if it is a \code{list}, which recursively applies \code{\link[=as_graph]{as_graph()}} to all list elements first
\item create a \code{\link{Graph}} with only one element if \code{x} is a \code{\link{PipeOp}} or can be converted to one using \code{\link[=as_pipeop]{as_pipeop()}}.
}
}
\seealso{
Other Graph operators: 
\code{\link{\%>>\%}()},
\code{\link{as_pipeop}()},
\code{\link{assert_graph}()},
\code{\link{assert_pipeop}()},
\code{\link{chain_graphs}()},
\code{\link{greplicate}()},
\code{\link{gunion}()},
\code{\link{mlr_graphs_greplicate}}
}
\concept{Graph operators}
