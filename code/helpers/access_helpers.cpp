
// CODE TO SPEED UP ACCESS HELPERS //


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector do_get_two_dim_access_indices(IntegerVector dims,
                                            int dim1,
                                            int dim1_value,
                                            int dim2,
                                            int dim2_value)
{
    // make sure 1 is before 2
    if (dim1 > dim2)
    {
        int tmp = dim1;
        dim1 = dim2;
        dim2 = tmp;
        
        tmp = dim1_value;
        dim1_value = dim2_value;
        dim2_value = tmp;
    }
    
    // index from 0 (not 1 as in R)
    dim1--;
    dim1_value--;
    dim2--;
    dim2_value--;
    
    // count before and after
    int n_before_1 = 1;
    for (int i=0; i<dim1; i++)
        n_before_1 *= dims[i];

    int n_between_12 = 1;
    for (int i=dim1+1; i<dim2; i++)
        n_between_12 *= dims[i];
        
    int n_after_2 = 1;
    for (int i=dim2+1; i<dims.length(); i++)
        n_after_2 *= dims[i];
    
    int n_src_before_between = n_before_1 * dims[dim1];
    int n_src_before_2 = n_src_before_between * n_between_12;
    int n_src_before_after = n_src_before_2 * dims[dim2];
    
    int n_dst_before_after = n_before_1 * n_between_12;
            
    // set up the rv
    int len = n_before_1 * n_between_12 * n_after_2;
    IntegerVector rv(len);
    
    // iterate and fill
    for (int i_before=0; i_before<n_before_1; i_before++)
    {
        for (int i_between=0; i_between<n_between_12; i_between++)
        {
            for (int i_after=0; i_after<n_after_2; i_after++)
            {
                rv[i_before + i_between * n_before_1 + i_after * n_dst_before_after] =
                    i_before + 
                        dim1_value * n_before_1 + 
                        i_between * n_src_before_between +
                        dim2_value * n_src_before_2 +
                        i_after * n_src_before_after;
            }
        }
    }
    
    // Return
    return (rv);
}
