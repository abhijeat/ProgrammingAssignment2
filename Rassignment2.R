# Function to create Matrix variable

makeCacheMatrix <- function(x)
{  inv_matx <- NULL

 
   # Following function sets a square matrix in cache



   set_sqr_matrix <-  function( x)
    { 

	if(det(x) != 0 )
  	 
 	 { 
    	 sqr_matrix <- x

	 
          
        
   	 }
	else {
		
		sqr_matrix <- NULL
	message("determinant is zero")

         }
	
	matx <<- sqr_matrix
     }

    set_sqr_matrix(x)


    # Function to return the matrix saved in cache

    get_matrix <- function() matx



    #Function set inverse matrix

    set_inverse <- function(fixed_inv_mat) inv_matx <<- fixed_inv_mat

    get_inverse <- function() inv_matx

    

    list(set_sqr_matrix = set_sqr_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
    

 }


# CacheSolve checks if the matrix is available in cache before calling solve function



cacheSolve <- function(x,...)
 {
   inv_mat1 <- x$get_inverse()

   
   if( is.matrix(inv_mat1)  )
   
   
   
     {
	message("getting cached data")
	return(inv_mat1)
     }

    sqr_matx <- x$get_matrix()

    

    if ( det(sqr_matx) == 0)
    
    { message("Matrix is not invertible")
      inv_mat1 = NULL
     }
     else
     {

      inv_mat1 <- solve(sqr_matx)
      }
	
    x$set_inverse(inv_mat1)
    inv_mat1
  }
