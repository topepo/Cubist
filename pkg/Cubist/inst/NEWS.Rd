\name{NEWS}
\title{News for Package \pkg{Cubist}}
\newcommand{\cpkg}{\href{https://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\newcommand{\issue}{\href{https://github.com/topepo/cubist/issues/#1}{(issue #1)}}

\section{Changes in version 0.0.19}{
\itemize{ 
\item A bug was fixed, found by Duncan Murdoch. 
\item A bug in predictions when sampling was used was fixed by Laurae (a random data witch/magician) \issue{1}
 }  
}


\section{Changes in version 0.0.17}{
\itemize{ 

\item Memory allocation bugs in the R version of the C code, which lead to segmentation faults, were fixed.

 }  
}

\section{Changes in version 0.0.15}{
\itemize{ 

\item A bug was fixed where the model failed if no predictors were
included in the model (thanks to Gabe Gershenfeld for the find). 

 }  
}

\section{Changes in version 0.0.14}{
\itemize{ 

\item The Windows build configuration was updated.

\item Some files were synced with the \cpkg{C50} package. 

 }  
}

\section{Changes in version 0.0.13}{
\itemize{ 

\item More efficient code was borrowed from the C50 package to write
the data file to a string.

\item A bug related to column names found by David Clifford was fixed. 

 }  
}


\section{Changes in version 0.0.12}{
\itemize{ 

\item A bug was fixed that occurred when splits were determined in
models with a categorical variable involving more than two categories
(eg. "if X4 in {a, b}")

\item Also, a bug was found in the variable importances calculations
 when a variable was used only in the conditions and not the model.

\item The 'doc' directory was moved into the "vignettes" directory  

 }  
}

\section{Changes in version 0.0.11}{
\itemize{ 

\item cubist uses a string buffer to capture the output files that
  would normally get saved to external files. The package was failing
  R CMD check only on windows because the output files were getting
  garbled. \code{s*printf} functions work differently on Windows. On
  Linux, they return the size that is needed to correctly print the
  string.  On Windows, if there isn't enough space, they return -1.

\item Also, work-arounds were added to the C sources because of R CMD
check issues. \code{printf()}, \code{assert()} and \code{exist()}
statements were removed or redefined.
 } 
}

\section{Changes in version 0.0.8}{
\itemize{ 
\item A bug was fixed for the \code{coef} method.
 } 
}


\section{Changes in version 0.0.6}{
\itemize{ 
\item The GPL version was revised to be version 3 (thanks to Ross
Quinlan for the note).
\item The variable usage statistics were saved to a data frame in
called \code{object$usage}. The package vignette and help files were
updated accordingly.
\item When using \code{cubist}, if \code{x} was a vector or a single
variable, an error occurred. This was fixed.
\item The vignette was also expanded to show how \cpkg{caret} can be
used to tune the model and a technical error was corrected about the
boosting algorithm (thanks again to Ross Quinlan).
 } 
}


