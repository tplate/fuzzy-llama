// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// wia
int wia(const int i);
RcppExport SEXP sparsetsmat_wia(SEXP iSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const int >::type i(iSEXP );
        int __result = wia(i);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stsm_xt_sqi
Rcpp::IntegerVector stsm_xt_sqi(const Rcpp::IntegerVector x_date, const Rcpp::IntegerVector i_idx, const Rcpp::IntegerVector j_idx, const Rcpp::IntegerVector id_idx, const Rcpp::IntegerVector id_noc, const int backfill);
RcppExport SEXP sparsetsmat_stsm_xt_sqi(SEXP x_dateSEXP, SEXP i_idxSEXP, SEXP j_idxSEXP, SEXP id_idxSEXP, SEXP id_nocSEXP, SEXP backfillSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type x_date(x_dateSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type i_idx(i_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type j_idx(j_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type id_idx(id_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type id_noc(id_nocSEXP );
        Rcpp::traits::input_parameter< const int >::type backfill(backfillSEXP );
        Rcpp::IntegerVector __result = stsm_xt_sqi(x_date, i_idx, j_idx, id_idx, id_noc, backfill);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stsm_xt_sqd
Rcpp::IntegerVector stsm_xt_sqd(const Rcpp::DoubleVector x_date, const Rcpp::DoubleVector i_idx, const Rcpp::IntegerVector j_idx, const Rcpp::IntegerVector id_idx, const Rcpp::IntegerVector id_noc, const int backfill);
RcppExport SEXP sparsetsmat_stsm_xt_sqd(SEXP x_dateSEXP, SEXP i_idxSEXP, SEXP j_idxSEXP, SEXP id_idxSEXP, SEXP id_nocSEXP, SEXP backfillSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const Rcpp::DoubleVector >::type x_date(x_dateSEXP );
        Rcpp::traits::input_parameter< const Rcpp::DoubleVector >::type i_idx(i_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type j_idx(j_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type id_idx(id_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type id_noc(id_nocSEXP );
        Rcpp::traits::input_parameter< const int >::type backfill(backfillSEXP );
        Rcpp::IntegerVector __result = stsm_xt_sqd(x_date, i_idx, j_idx, id_idx, id_noc, backfill);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stsm_xt_mii
Rcpp::IntegerVector stsm_xt_mii(const Rcpp::IntegerVector i_idx, const Rcpp::IntegerVector j_idx, const Rcpp::IntegerVector x_date, const Rcpp::IntegerVector id_idx, const Rcpp::IntegerVector id_noc, const int backfill);
RcppExport SEXP sparsetsmat_stsm_xt_mii(SEXP i_idxSEXP, SEXP j_idxSEXP, SEXP x_dateSEXP, SEXP id_idxSEXP, SEXP id_nocSEXP, SEXP backfillSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type i_idx(i_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type j_idx(j_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type x_date(x_dateSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type id_idx(id_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type id_noc(id_nocSEXP );
        Rcpp::traits::input_parameter< const int >::type backfill(backfillSEXP );
        Rcpp::IntegerVector __result = stsm_xt_mii(i_idx, j_idx, x_date, id_idx, id_noc, backfill);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// stsm_xt_mid
Rcpp::IntegerVector stsm_xt_mid(const Rcpp::DoubleVector i_idx, const Rcpp::IntegerVector j_idx, const Rcpp::DoubleVector x_date, const Rcpp::IntegerVector id_idx, const Rcpp::IntegerVector id_noc, const int backfill);
RcppExport SEXP sparsetsmat_stsm_xt_mid(SEXP i_idxSEXP, SEXP j_idxSEXP, SEXP x_dateSEXP, SEXP id_idxSEXP, SEXP id_nocSEXP, SEXP backfillSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const Rcpp::DoubleVector >::type i_idx(i_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type j_idx(j_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::DoubleVector >::type x_date(x_dateSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type id_idx(id_idxSEXP );
        Rcpp::traits::input_parameter< const Rcpp::IntegerVector >::type id_noc(id_nocSEXP );
        Rcpp::traits::input_parameter< const int >::type backfill(backfillSEXP );
        Rcpp::IntegerVector __result = stsm_xt_mid(i_idx, j_idx, x_date, id_idx, id_noc, backfill);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
