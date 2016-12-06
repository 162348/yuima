// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// W1
double W1(NumericMatrix crossdx, NumericMatrix b, NumericMatrix A, double h);
RcppExport SEXP yuima_W1(SEXP crossdxSEXP, SEXP bSEXP, SEXP ASEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type crossdx(crossdxSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type b(bSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    Rcpp::traits::input_parameter< double >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(W1(crossdx, b, A, h));
    return rcpp_result_gen;
END_RCPP
}
// W2
double W2(NumericMatrix dx, NumericMatrix b, double h);
RcppExport SEXP yuima_W2(SEXP dxSEXP, SEXP bSEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dx(dxSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(W2(dx, b, h));
    return rcpp_result_gen;
END_RCPP
}
// sqnorm
double sqnorm(NumericVector x);
RcppExport SEXP yuima_sqnorm(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sqnorm(x));
    return rcpp_result_gen;
END_RCPP
}
// makeprop
NumericVector makeprop(NumericVector mu, NumericVector sample, NumericVector low, NumericVector up);
RcppExport SEXP yuima_makeprop(SEXP muSEXP, SEXP sampleSEXP, SEXP lowSEXP, SEXP upSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sample(sampleSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type low(lowSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type up(upSEXP);
    rcpp_result_gen = Rcpp::wrap(makeprop(mu, sample, low, up));
    return rcpp_result_gen;
END_RCPP
}
// detcpp
double detcpp(NumericMatrix A);
RcppExport SEXP yuima_detcpp(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(detcpp(A));
    return rcpp_result_gen;
END_RCPP
}
// Smake
NumericMatrix Smake(NumericVector b, int d);
RcppExport SEXP yuima_Smake(SEXP bSEXP, SEXP dSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< int >::type d(dSEXP);
    rcpp_result_gen = Rcpp::wrap(Smake(b, d));
    return rcpp_result_gen;
END_RCPP
}
// solvecpp
NumericMatrix solvecpp(NumericMatrix A);
RcppExport SEXP yuima_solvecpp(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(solvecpp(A));
    return rcpp_result_gen;
END_RCPP
}
// sub_f
double sub_f(NumericMatrix S, NumericVector b);
RcppExport SEXP yuima_sub_f(SEXP SSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type S(SSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(sub_f(S, b));
    return rcpp_result_gen;
END_RCPP
}
// likndim
double likndim(NumericMatrix dx, NumericMatrix b, NumericMatrix A, double h);
RcppExport SEXP yuima_likndim(SEXP dxSEXP, SEXP bSEXP, SEXP ASEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dx(dxSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type b(bSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type A(ASEXP);
    Rcpp::traits::input_parameter< double >::type h(hSEXP);
    rcpp_result_gen = Rcpp::wrap(likndim(dx, b, A, h));
    return rcpp_result_gen;
END_RCPP
}
