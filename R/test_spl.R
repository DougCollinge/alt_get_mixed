## The goal here is the split ni into two elements of a list 

## Using the same data as merge.R
ni = 42 + 0:(20-1)
i = 5

## Reflecting my naivety with FORTRAN, I'm not sure where nr comes in here.
nr = 10


spl <- function(ni, i) {
  k1=ni[i]
  k2=ni[i+1]
  
  jsplit_cond=(k1+k2)/2
  
    ## Using double condition in an attempt to be defensive
  jsplit <- ifelse(jsplit_cond >= k2-1, k2-2,
                   ifelse(jsplit_cond <= k1+1, k1+1, 
                          "Condition Not Satisfied")
                   )
  
  ## Built in R function
  split(ni, ifelse(ni>jsplit,"Group1","Group2"))
}

spl(ni, i)
