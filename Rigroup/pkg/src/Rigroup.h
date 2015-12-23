#include <R.h>
#include <Rinternals.h>  
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("Rigroup", String)
#else
#define _(String) (String)
#endif

#define R_IGFCOUNTS 0
#define R_IGFSUMS   1
#define R_IGFMAXS   2
#define R_IGFMINS   3
#define R_IGFPRODS  4

SEXP igroupFuns(SEXP);

