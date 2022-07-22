#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericVector union_sorted_vectors(List vectors)
{
    int n_vectors = vectors.length();
    
    double expected_size_factor = 2;
    int max_v_size = 0;
    for (int i=0; i<n_vectors; i++)
    {
        int i_length = ((NumericVector) vectors[i]).length();
        if (i_length > max_v_size)
            max_v_size = i_length;
    }
    
    // pull the first vector
    NumericVector raw_vector = (NumericVector) vectors[0];
    
    // set up the scratch vectors
    int scratch_capacity = max_v_size * expected_size_factor;

    double *scratch1 = (double *) malloc(scratch_capacity * sizeof(double));
    double *scratch2 = scratch1; //we init to scratch1 to avoid a warning. If we will really use it, we will allocate below
    if (n_vectors > 2)
        scratch2 = (double *) malloc(scratch_capacity * sizeof(double));
    
    // set up indices for loop
    int index1;
    int index2;
    
    int n1 = raw_vector.length();
    int n2;
    int n_dst;
    
    int capacity1 = 0; //the init value doesn't actually matter - we just do it to avoid an error
    int capacity2;
    int capacity_dst = scratch_capacity;
    
    double *v1 = raw_vector.begin();
    double *v2;
    double *v_dst = scratch1;

    for (int v_index=1; v_index<n_vectors; v_index++)
    {
        raw_vector = (NumericVector) vectors[v_index];
        n2 = raw_vector.length();
        v2 = raw_vector.begin();
        
        index1 = 0;
        index2 = 0;
        n_dst = 0;
        
      
        while(index1 < n1 && index2 < n2)
        {
            //expand the array
            if (n_dst == capacity_dst) 
            {
                capacity_dst += max_v_size * (expected_size_factor-1);
                double *new_dst = (double *) malloc(capacity_dst * sizeof(double));
                for (int j=0; j<n_dst; j++)
                    new_dst[j] = v_dst[j];
                free(v_dst);
                v_dst = new_dst;
            }

            // copy and advance the indices
            if (v1[index1] < v2[index2]) // copy from 1 and advance 1
            {
                v_dst[n_dst] = v1[index1];
                index1++;
                n_dst++;
            }
            else if (v2[index2] < v1[index1]) // copy from 2 and advance 2
            {
                v_dst[n_dst] = v2[index2];
                index2++;
                n_dst++;
            }
            else //v1[index1] == v2[index2] - copy from 1 and advance both
            {
                v_dst[n_dst] = v1[index1];
                index1++;
                index2++;
                n_dst++;
            }
        }
        
        // finish out what's left from 1 or 2
        while (index1 < n1)
        {
            if (n_dst == capacity_dst) 
            {
                capacity_dst += max_v_size * (expected_size_factor-1);
                double *new_dst = (double *) malloc(capacity_dst * sizeof(double));
                for (int j=0; j<n_dst; j++)
                    new_dst[j] = v_dst[j];
                free(v_dst);
                v_dst = new_dst;
            }
            
            v_dst[n_dst] = v1[index1];
            index1++;
            n_dst++;
        }
        while (index2 < n2)
        {
            if (n_dst == capacity_dst) 
            {
                capacity_dst += max_v_size * (expected_size_factor-1);
                double *new_dst = (double *) malloc(capacity_dst * sizeof(double));
                for (int j=0; j<n_dst; j++)
                    new_dst[j] = v_dst[j];
                free(v_dst);
                v_dst = new_dst;
            }
            
            v_dst[n_dst] = v2[index2];
            index2++;
            n_dst++;
        }
        
        // swap v1 and v_dst - use v2 as the holder
        v2 = v1;
        capacity2 = capacity1;
        
        v1 = v_dst;
        n1 = n_dst;
        capacity1 = capacity_dst;
        if (v_index==1)
        {
            v_dst = scratch2;
            capacity_dst = scratch_capacity;
        }
        else
        {
            v_dst = v2;
            capacity_dst = capacity2;
        }
    }
    
    // Package it up into a numeric vector
    NumericVector rv(n1);
    for (int i=0; i<n1; i++)
        rv[i] = v1[i];
    
    free(v1);
    if (n_vectors>2)
        free(v_dst);
    
    return (rv);
}