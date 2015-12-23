/*
 *  R: A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copuright (C) 2006 The R Core Development Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version or the LGPL version.  
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

#include "Rigroup.h"

SEXP igroupFuns(SEXP args)
{
    SEXP x, f, res;
    int i, j, nobs, nlevs, nfac;
    int itmp, fcn;
    double rtmp;
    Rboolean  narm, ltmp;

    /* collect arguments */ 
    x = CADR(args);
    f = CADDR(args);
    fcn = asInteger(CADDDR(args));
    narm = asLogical(CAD4R(args));

    /* initialize some things to prevent warnings */
    rtmp = 0.0;
    res = (SEXP) 0;

    /* do basic sanity checking */
    if (!isVector(x))
        error(_("first argument must be a vector"));
    nfac = LENGTH(f);
    nlevs = 0;
    for (i=0; i < nfac; i++) if (INTEGER(f)[i] > nlevs) nlevs = INTEGER(f)[i];
    nobs = LENGTH(x);
    if (nobs <= 0) return R_NilValue;
    if (nobs != nfac)
        error(_("data length is not the same as igroup length"));
    if ((TYPEOF(x) != LGLSXP) && (TYPEOF(x) != INTSXP) && (TYPEOF(x) != REALSXP))
        error(_("data must be of type logical, integer, or real"));

    /* initialize the result arrays */

    switch (fcn) {
    case R_IGFCOUNTS:
        PROTECT(res = allocVector(INTSXP, nlevs));
        for (i=0; i < nlevs; i++) INTEGER(res)[i] = 0;
        break;
    case R_IGFSUMS:
        PROTECT(res = allocVector(REALSXP, nlevs));
        for (i=0; i < nlevs; i++) REAL(res)[i] = 0.0;
        break;
    case R_IGFMAXS:
        if (TYPEOF(x) == LGLSXP) {
            PROTECT(res = allocVector(LGLSXP, nlevs));
            for (i=0; i < nlevs; i++) LOGICAL(res)[i] = 0;
        } else {
            PROTECT(res = allocVector(REALSXP, nlevs));
            for (i=0; i < nlevs; i++) REAL(res)[i] = R_NegInf;
	}
        break;
    case R_IGFMINS:
        if (TYPEOF(x) == LGLSXP) {
            PROTECT(res = allocVector(LGLSXP, nlevs));
            for (i=0; i < nlevs; i++) LOGICAL(res)[i] = 1;
        } else {
            PROTECT(res = allocVector(REALSXP, nlevs));
            for (i=0; i < nlevs; i++) REAL(res)[i] = R_PosInf;
	}
        break;
    case R_IGFPRODS:
        if (TYPEOF(x) == LGLSXP) {
            PROTECT(res = allocVector(LGLSXP, nlevs));
            for (i=0; i < nlevs; i++) LOGICAL(res)[i] = 1;
        } else {
            PROTECT(res = allocVector(REALSXP, nlevs));
            for (i=0; i < nlevs; i++) REAL(res)[i] = 1.0;
	}
        break;
    default:
        error(_("Rigroup unimplemented function"));
    }

    /* now perform the main loop and calculations */
    switch (fcn) {
    case R_IGFCOUNTS:
        for (i = 0;  i < nobs; i++) {
	    j = INTEGER(f)[i];
            if ((j == NA_INTEGER) || (j < 1) || (j > nlevs)) continue; 
	    j--;
            switch (TYPEOF(x)) {
	    case LGLSXP:
	        if (!((LOGICAL(x)[i] == NA_LOGICAL) && narm)) INTEGER(res)[j] += 1;
                break;
	    case INTSXP:
	        if (!((INTEGER(x)[i] == NA_INTEGER) && narm)) INTEGER(res)[j] += 1;
                break;
	    case REALSXP:
	        if (!(ISNA(REAL(x)[i]) && narm)) INTEGER(res)[j] += 1;
                break;
	    }
	}
        break;

    case R_IGFSUMS:
        for (i = 0;  i < nobs; i++) {
	    j = INTEGER(f)[i];
            if ((j == NA_INTEGER) || (j < 1) || (j > nlevs)) continue; 
	    j--;
            switch (TYPEOF(x)) {
	    case LGLSXP:
            case INTSXP:
	        itmp = INTEGER(x)[i];
                if (itmp == NA_INTEGER) rtmp = NA_REAL;
                else rtmp = (double) itmp;
                break;
	    case REALSXP:
	        rtmp = REAL(x)[i];
                break;
	    }
            if (ISNA(rtmp)) {
	        if (!narm) {
                   REAL(res)[j] = NA_REAL;
	        } 
                continue;
            }
            if (!ISNA(REAL(res)[j])) REAL(res)[j] += rtmp;
	}
        break;

    case R_IGFMAXS:
        if (TYPEOF(x) == LGLSXP) {
            for (i = 0;  i < nobs; i++) {
	        j = INTEGER(f)[i];
                if ((j == NA_INTEGER) || (j < 1) || (j > nlevs)) continue; 
	        j--;
	        ltmp = LOGICAL(x)[i];
                if (ltmp == NA_LOGICAL) {
		   if (narm) continue;
                   if (LOGICAL(res)[j] == NA_LOGICAL) continue; 
		   if (LOGICAL(res)[j]) continue;
                   LOGICAL(res)[j] = NA_LOGICAL;
                   continue;
                }
                if (! ltmp) continue;
                LOGICAL(res)[j] = 1;
	    }
        } else {
            for (i = 0;  i < nobs; i++) {
	        j = INTEGER(f)[i];
                if ((j == NA_INTEGER) || (j < 1) || (j > nlevs)) continue;
                j--;
                if (TYPEOF(x) == INTSXP) {
	            itmp = INTEGER(x)[i];
                    if (itmp == NA_INTEGER) rtmp = NA_REAL;
                    else rtmp = (double) itmp;
                } else rtmp = REAL(x)[i];
                if (ISNA(rtmp)) {
	            if (!narm) REAL(res)[j] = NA_REAL;
                    continue;
                }
                if (!ISNA(REAL(res)[j]))
	            if (rtmp >  REAL(res)[j]) REAL(res)[j] = rtmp;
	    }
	}
        break;


    case R_IGFMINS:
        if (TYPEOF(x) == LGLSXP) {
            for (i = 0;  i < nobs; i++) {
	        j = INTEGER(f)[i];
                if ((j == NA_INTEGER) || (j < 1) || (j > nlevs)) continue; 
	        j--;
	        ltmp = LOGICAL(x)[i];
                if (ltmp == NA_LOGICAL) {
		   if (narm) continue;
                   if (LOGICAL(res)[j] == NA_LOGICAL) continue; 
                   if (!(LOGICAL(res)[j])) continue; 
                   LOGICAL(res)[j] = NA_LOGICAL;
                   continue;
                }
                if (ltmp) continue;
                LOGICAL(res)[j] = 0;
	    }
	} else {
            for (i = 0;  i < nobs; i++) {
	        j = INTEGER(f)[i];
                if ((j == NA_INTEGER) || (j < 1) || (j > nlevs)) continue; 
                j--;
                if (TYPEOF(x) == INTSXP) {
	            itmp = INTEGER(x)[i];
                    if (itmp == NA_INTEGER) rtmp = NA_REAL;
                    else rtmp = (double) itmp;
                } else rtmp = REAL(x)[i];
                if (ISNA(rtmp)) {
	            if (!narm) REAL(res)[j] = NA_REAL;
                    continue;
                }
                if (!ISNA(REAL(res)[j]))
	            if (rtmp <  REAL(res)[j]) REAL(res)[j] = rtmp;
	    }
	}
        break;

    case R_IGFPRODS:
        for (i = 0;  i < nobs; i++) {
	    j = INTEGER(f)[i];
            if ((j == NA_INTEGER) || (j < 1) || (j > nlevs)) continue; 
	    j--;
            if (TYPEOF(x) == REALSXP) {
	        rtmp = REAL(x)[i];
            } else {
	        itmp = INTEGER(x)[i];
                if (itmp == NA_INTEGER) rtmp = NA_REAL;
                else rtmp = (double) itmp;
            }
            if (ISNA(rtmp)) {
	        if (!narm) REAL(res)[j] = NA_REAL;
                continue;
            }
	    if (!ISNA(REAL(res)[j])) REAL(res)[j] *= rtmp;
	}
	break;
    }
    UNPROTECT(1);
    return res;
}

