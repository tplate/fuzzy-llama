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
Rcpp::IntegerVector stsm_xt_sqi(
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
	if (Rcpp::IntegerVector::is_na(j_idx[j]))
	    continue;
	s = id_idx[j_idx[j] - 1] - 1;
	e = s + id_noc[j_idx[j] - 1];
	// data for j_idx[j] lies in [s,e) indices of x_date
	if (s >=0 && e <= x_date.size() && s < e) {
	    ii = j * i_idx.size();
	    for (i=0; i < i_idx.size(); i++) {
		d = i_idx[i];
		if (d == NA_INTEGER)
		    continue;
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
Rcpp::IntegerVector stsm_xt_sqd(
		   const Rcpp::DoubleVector x_date,
		   const Rcpp::DoubleVector i_idx,
		   const Rcpp::IntegerVector j_idx,
		   const Rcpp::IntegerVector id_idx,
		   const Rcpp::IntegerVector id_noc,
		   const int backfill) {
    Rcpp::IntegerVector val_idx(i_idx.size() * j_idx.size());
    int i, ii, j, s, e, k;
    double d;
    // i_idx indexes into x_date
    // j_idx indexes into id_idx
    for (ii=0; ii < i_idx.size() * j_idx.size(); ii++)
	val_idx[ii] = NA_INTEGER;
    for (j=0; j < j_idx.size(); j++) {
	if (Rcpp::IntegerVector::is_na(j_idx[j]))
	    continue;
	s = id_idx[j_idx[j] - 1] - 1;
	e = s + id_noc[j_idx[j] - 1];
	// data for j_idx[j] lies in [s,e) indices of x_date
	if (s >=0 && e <= x_date.size() && s < e) {
	    ii = j * i_idx.size();
	    for (i=0; i < i_idx.size(); i++) {
		d = i_idx[i];
		if (ISNA(d))
		    continue;
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
// j is sorted, i is sorted within j

// [[Rcpp::export]]
Rcpp::IntegerVector stsm_xt_mii(
		   const Rcpp::IntegerVector i_idx,
		   const Rcpp::IntegerVector j_idx,
		   const Rcpp::IntegerVector x_dates,
		   const Rcpp::IntegerVector id_idx,
		   const Rcpp::IntegerVector id_noc,
		   const int backfill) {
    Rcpp::IntegerVector val_idx(i_idx.size());
    int i, j, s, e, k, d;
    i = 0; // index into i.idx and j.idx
    while (i < i_idx.size()) {
        j = j_idx[i] - 1;
	// Process all of this id group together
        if (j_idx[i] == NA_INTEGER || id_noc[j] == 0) {
            // No data for this id at all
            while (i < i_idx.size() && j_idx[i] == j+1) {
		val_idx[i] = NA_INTEGER;
		i++;
	    }
        } else {
            k = s = id_idx[j] - 1;
            e = s + id_noc[j] - 1;
            // d = x_dates[k]; not used
            while (i < i_idx.size() && j_idx[i] == j+1) {
		// increase k until x_dates[k] is the largest still
		// smaller than i.idx[i]
		if (i_idx[i] == NA_INTEGER) {
		    val_idx[i] = NA_INTEGER;
		} else {
		    while (k < e && x_dates[k+1] <= i_idx[i])
			k = k + 1;
		    if (backfill || i_idx[i] >= x_dates[k])
			val_idx[i] = k + 1; // for 1-based indexing
		    else
			val_idx[i] = NA_INTEGER;
		}
		i++;
	    }
        }
    }
    return(val_idx);
}

// stsm_xt_mid
// Returns indices into x$value for x[cbind(i,j)] indexing
// i_idx and x_dates are double
// x_dates
// i_idx
// j_idx
// id_idx
// id_noc

// [[Rcpp::export]]
Rcpp::IntegerVector stsm_xt_mid(
		   const Rcpp::DoubleVector i_idx,
		   const Rcpp::IntegerVector j_idx,
		   const Rcpp::DoubleVector x_dates,
		   const Rcpp::IntegerVector id_idx,
		   const Rcpp::IntegerVector id_noc,
		   const int backfill) {
    Rcpp::IntegerVector val_idx(i_idx.size());
    int i, j;
    double k, e, s, d;
    i = 0; // index into i.idx and j.idx
    while (i < i_idx.size()) {
        j = j_idx[i] - 1;
	// Process all of this id group together
        if (j_idx[i] == NA_INTEGER || id_noc[j] == 0) {
            // No data for this id at all
            while (i < i_idx.size() && j_idx[i] == j+1) {
		val_idx[i] = NA_INTEGER;
		i++;
	    }
        } else {
            k = s = id_idx[j] - 1;
            e = s + id_noc[j] - 1;
            // d = x_dates[k]; not used
            while (i < i_idx.size() && j_idx[i] == j+1) {
		// increase k until x_dates[k] is the largest still
		// smaller than i.idx[i]
		if (Rcpp::DoubleVector::is_na(i_idx[i])) {
		    val_idx[i] = NA_INTEGER;
		} else {
		    while (k < e && x_dates[k+1] <= i_idx[i])
			k = k + 1;
		    if (backfill || i_idx[i] >= x_dates[k])
			val_idx[i] = k + 1; // for 1-based indexing
		    else
			val_idx[i] = NA_INTEGER;
		}
		i++;
	    }
        }
    }
    return(val_idx);
}

