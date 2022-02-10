#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericVector missing_below(Rcpp::NumericVector row_number,
                            Rcpp::CharacterVector column_name,
                            Rcpp::NumericVector train,
                            Rcpp::NumericVector below,
                            Rcpp::String var) {
  
  int n = row_number.length();
  int counter;
  
  for(int i = 0; i <= n - 1; i++) {
    
    if(column_name[i] == var) {
      
      counter = 0;
      while(Rcpp::NumericVector::is_na(train[row_number[i] - counter - 1])) {
        
        counter = counter + 1;
        if(counter > 100) {
          
          break;
          
        }
        
      }
    
    below[i] = counter - 1;
      
    }
    
  }
  
  return(below);
}


// [[Rcpp::export]]
NumericVector missing_above(Rcpp::NumericVector row_number,
                            Rcpp::CharacterVector column_name,
                            Rcpp::NumericVector train,
                            Rcpp::NumericVector above,
                            Rcpp::String var) {
  
  int n = row_number.length();
  int counter;
  
  for(int i = 0; i <= n - 1; i++) {
    
    if(column_name[i] == var) {
      
      counter = 0;
      while(Rcpp::NumericVector::is_na(train[row_number[i] + counter - 1])) {
        
        counter = counter + 1;
        if(counter > 100) {
          
          break;
          
        }
        
      }
      
      above[i] = counter - 1;
      
    }
    
  }
  
  return(above);
}