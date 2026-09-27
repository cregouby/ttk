#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List merge_tree_cpp(const Rcpp::List& mesh,
                          const std::string& scalar_field_name,
                          const std::string& tree_type,
                          bool simplify_tree) {
  return Rcpp::List::create(
    Rcpp::Named("nodes") = Rcpp::List::create(
      Rcpp::Named("ids") = Rcpp::IntegerVector(0),
      Rcpp::Named("values") = Rcpp::NumericVector(0)
    ),
    Rcpp::Named("edges") = Rcpp::List::create(
      Rcpp::Named("source") = Rcpp::IntegerVector(0),
      Rcpp::Named("target") = Rcpp::IntegerVector(0)
    ),
    Rcpp::Named("n_nodes") = 0,
    Rcpp::Named("n_edges") = 0,
    Rcpp::Named("scalar_field") = scalar_field_name,
    Rcpp::Named("tree_type") = tree_type,
    Rcpp::Named("simplify_tree") = simplify_tree,
    Rcpp::Named("implemented") = false
  );
}
