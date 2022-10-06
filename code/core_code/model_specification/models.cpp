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


// [[Rcpp::export]]
void do_add_interaction_alphas_to_arr(NumericVector arr,
                                      IntegerVector dims,
                                      int dim1,
                                      IntegerVector dim1_values,
                                      int dim2,
                                      IntegerVector dim2_values,
                                      NumericVector values
                                      )
{
    // make sure dim 1 is less than dim 2
    int* dv1;
    int *dv2;
    if (dim1<dim2)
    {
        dv1 = dim1_values.begin();
        dv2 = dim2_values.begin();
    }
    else
    {
        int temp = dim2;
        dim1 = dim2;
        dim2 = temp;
        
        dv1 = dim2_values.begin();
        dv2 = dim1_values.begin();
    }
    
    int n_dim = dims.length();
    int n_interactions = values.length();
    
    double *a = arr.begin();
    
    // move from R indexing (from 1) to C++ indexing (from 0)
    dim1--;
    dim2--;
    for (int i_interaction=0; i_interaction<n_dim; i_interaction++)
    {
        dv1[i_interaction]--;
        dv2[i_interaction]--;
    }
    
    // Calculate n for each section we'll iterate through
    int n_before_1 = 1;
    for (int i=0; i<dim1; i++)
        n_before_1 *= dims[i];
    int n_dim1 = dims[dim1];
    int n_between_1_2 = 1;
    for (int i=(dim1+1); i<dim2; i++)
        n_between_1_2 *= dims[i];
    int n_dim2 = dims[dim2];
    int n_after_2 = 1;
    for (int i=(dim2+1); i<n_dim; i++)
        n_after_2 *= dims[i];
    
    int n_before_between = n_before_1 * n_dim1;
    int n_before_2 = n_before_between * n_between_1_2;
    int n_before_after = n_before_2 * n_dim2;
    
    // Iterate through the array and add
    
    for (int i_after = 0; i_after<n_after_2; i_after++)
    {
        for (int i_between = 0; i_between<n_between_1_2; i_between++)
        {
            for (int i_interaction = 0; i_interaction<n_interactions; i_interaction++)
            {
                for (int i_before=0; i_before<n_before_1; i_before++)
                {
                    a[i_after*n_before_after + 
                        dv2[i_interaction]*n_before_2 + 
                        i_between*n_before_between +
                        dv1[i_interaction]*n_before_1 +
                        i_before] += values[i_interaction];
                }
            }
        }
    }
}

// [[Rcpp::export]]
NumericVector do_multiply_alphas_into_arr(NumericVector alpha_values,
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
        rv[i] = 1;
    
    int dim;
    
    // iterate through the array
    for (int i=0; i<n_alphas; i++)
    {
        dim = alpha_dims[i];
        for (int i_before=0; i_before<n_before_dim[dim]; i_before++)
        {
            for (int i_after=0; i_after<n_after_dim[dim]; i_after++)
            {
                rv[i_before +
                    alpha_indices[i] * n_before_dim[dim] +
                    i_after * n_before_dim[dim] * n_in_dim[dim] ] *= alpha_values[i];
            }
        }
    }
    
    return (rv);
}