\name{find.gs}
\alias{find.gs}
\title{Find the ghostscript executable}
\usage{
find.gs(mustexist = TRUE, gsexe = NULL)
}
\arguments{
  \item{mustexist}{stop with an error if the executable
  does not exist}

  \item{gsexe}{full path or program name (env vars not used
  if this is supplied)}
}
\value{
the full pathname of the ghostscript executable
}
\description{
Find the ghostscript executable
}
\details{
If env var R_GSCMD is set (or GSC under Windows), return
that value (or stop with an error if it does not exist.)
If these env vars are not set, look for an executable on
the path or in likely places.  R_GSCMD or GSC can be a full
path or just a program name, in the latter case, it is
looked for on the system PATH.
}

