// CODE TO SPEED UP MODELS //


#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector do_add_alphas_to_arr(NumericVector alpha_values,
                                   IntegerVector alpha_dims, //indexed from 1, as in R
                                   IntegerVector alpha_indices, //indexed from 1, as in R
                                   IntegerVector n_before_dim,
                                   IntegerVector n_in_dim,
                                   IntegerVector n_after_dim)
{
    int n_alphas = alpha_values.length();
    int len = n_before_dim[0] * n_in_dim[0] * n_after_dim[0];
    
    // re-index alpha_dims and alpha_indices from zero
    for (int i=0; i<n_alphas; i++)
    {
        alpha_dims[i]--;
        alpha_indices[i]--;
    }
    
    // set up the rv
    NumericVector rv(len);
    for (int i=0; i<len; i++)
        rv[i] = 0;
    
    int dim;

    // iterate through the array
    for (int i=0; i<n_alphas; i++)
    {
        dim = alpha_dims[i];
        for (int i_before=0; i_before<n_before_dim[dim]; i_before++)
        {
            for (int i_after=0; i_after<n_after_dim[dim]; i_after++)
            {
                rv[i_before +//* n_after_dim[dim] +
                    alpha_indices[i] * n_before_dim[dim] +
                    i_after * n_before_dim[dim] * n_in_dim[dim] ] += alpha_values[i];
            }
        }
    }
    
    return (rv);
}