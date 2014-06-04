#include <Rcpp.h>
#include <Rmath.h>
// [[Rcpp::export]]
int wia(const int i) {
    if (i==0) return(0);
    if (i>0) return(42);
    else return(42);
}

// Returns indices into x$value for x[i,j] indexing
// i_idx and x_date are integer
// x_date
// i_idx
// j_idx
// id_idx
// id_noc

// [[Rcpp::export]]
Rcpp::IntegerVector stsm_xt_sqi_ij(
		   const Rcpp::IntegerVector x_date,
		   const Rcpp::IntegerVector i_idx,
		   const Rcpp::IntegerVector j_idx,
		   const Rcpp::IntegerVector id_idx,
		   const Rcpp::IntegerVector id_noc,
		   const int backfill) {
    Rcpp::IntegerVector val_idx(i_idx.size() * j_idx.size());
    int i, j, ii, s, e, k;
    int d;
    // i_idx indexes into x_date
    // j_idx indexes into id_idx
    for (ii=0; ii < i_idx.size() * j_idx.size(); ii++)
	val_idx[ii] = NA_INTEGER;
    for (j=0; j < j_idx.size(); j++) {
	s = id_idx[j_idx[j] - 1] - 1;
	e = s + id_noc[j_idx[j] - 1];
	// data for j_idx[j] lies in [s,e) indices of x_date
	if (s >=0 && e <= x_date.size() & s < e) {
	    ii = j * i_idx.size();
	    for (i=0; i < i_idx.size(); i++) {
		d = i_idx[i];
		// find the index k in [s,e) st x_date[k] <= d < x_date[k+1]
		k = s;
		while (k < (e-1) && x_date[k+1] <= d)
		    k++;
		if (d >= x_date[k] || (backfill && k == s))
		    val_idx[ii] = k + 1; // translate to base-1 index
		ii++;
	    }
	}
    }
    return(val_idx);
}

// Returns indices into x$value for x[i,j] indexing
// i_idx and x_date are double
// x_date
// i_idx
// j_idx
// id_idx
// id_noc

// [[Rcpp::export]]
Rcpp::IntegerVector stsm_xt_sqd_ij(
		   const Rcpp::DoubleVector x_date,
		   const Rcpp::DoubleVector i_idx,
		   const Rcpp::IntegerVector j_idx,
		   const Rcpp::IntegerVector id_idx,
		   const Rcpp::IntegerVector id_noc,
		   const int backfill) {
    Rcpp::IntegerVector val_idx(i_idx.size() * j_idx.size());
    int i, j, ii, s, e, k;
    double d;
    // i_idx indexes into x_date
    // j_idx indexes into id_idx
    for (ii=0; ii < i_idx.size() * j_idx.size(); ii++)
	val_idx[ii] = NA_INTEGER;
    for (j=0; j < j_idx.size(); j++) {
	s = id_idx[j_idx[j] - 1] - 1;
	e = s + id_noc[j_idx[j] - 1];
	// data for j_idx[j] lies in [s,e) indices of x_date
	if (s >=0 && e <= x_date.size() && s < e) {
	    ii = j * i_idx.size();
	    for (i=0; i < i_idx.size(); i++) {
		d = i_idx[i];
		// find the index k in [s,e) st x_date[k] <= d < x_date[k+1]
		k = s;
		while (k < (e-1) && x_date[k+1] <= d)
		    k++;
		if (d >= x_date[k] || (backfill && k==s))
		    val_idx[ii] = k + 1; // translate to base-1 index
		ii++;
	    }
	}
    }
    return(val_idx);
}

// stsm_xt_mati_ij
// Returns indices into x$value for x[cbind(i,j)] indexing
// i_idx and x_date are integer
// x_date
// i_idx
// j_idx
// id_idx
// id_noc

// [[Rcpp::export]]
int stsm_xt_mati_ij(const int x_date) {
    return(-1);
}

// stsm_xt_matd_ij
// Returns indices into x$value for x[cbind(i,j)] indexing
// i_idx and x_date are double
// x_date
// i_idx
// j_idx
// id_idx
// id_noc

// [[Rcpp::export]]
int stsm_xt_matd_ij(const double x_date) {
    return(-1);
}

