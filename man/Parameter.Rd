% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Parameter-class.R
\name{Parameter}
\alias{Parameter}
\title{Parameter}
\description{
Parameter

Parameter
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{Name of the parameter}

\item{\code{description}}{Short description of the parameter}

\item{\code{schema}}{Type and subtype of the parameter}

\item{\code{optional}}{Is this parameter required for the process}

\item{\code{value}}{Value of the parameter}

\item{\code{parameters}}{Included parameters}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Parameter$new()}}
\item \href{#method-parameterInfo}{\code{Parameter$parameterInfo()}}
\item \href{#method-clone}{\code{Parameter$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{

\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$new(name = NA, description = NA, schema = NA, optional = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{Name of the parameter}

\item{\code{description}}{Short description of the parameter}

\item{\code{schema}}{Type and subtype of the parameter}

\item{\code{optional}}{Is this parameter required for the process}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-parameterInfo"></a>}}
\if{latex}{\out{\hypertarget{method-parameterInfo}{}}}
\subsection{Method \code{parameterInfo()}}{
Get the information of the parameter
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$parameterInfo()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
list of information
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Parameter$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
