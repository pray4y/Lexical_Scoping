## The first function will cache the inverse of an n by n matrix, 
## while the second function will compute the inverse of a matrix.

## makeCacheMatrix, similar to cachemean, will creates a a list containing a 
## function to set/get the matrix and set/get its inverse matrix.

makeCacheMatrix <- function(x = mat()) {
        m <- NULL    
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setmat <- function(mat) m <<- mat
        getmat <- function() m
        list(set = set, get = get,
             setmat = setmat, getmat = getmat)
}


## cacheSolve first detects whether the the inverse of a given n by n invertible
## matrix 'x' has been calculated. If the inverse has already been calculated, 
## cacheSolve will get the cached inverse and print it. If the inverse has not 
## already been calculated, cacheSolve will then calculate it and print it.

cacheSolve <- function(x = matrix, ...) {
        n <- nrow(x)
        id <- diag(nrow = n)
        xid <- cbind(x, id)

        for (i in 1:n) {
                for (j in (1:n)[-i]) {
                    
                    k <- 1
                    while (k <= n) {
                            if (identical(0, xid[i,i]) == TRUE) {
                                    if (i + k <= n) {
                                            xid[i,] <- xid[i,] + xid[i+k,]
                                            k <- k + 1
                                    } else {
                                            k <- n + 1
                                    }  
                            } else {
                                    k <- n + 1    
                            } 
                    }
                    
                    xid[i,] <- xid[i,] / xid[i, i]
                    
                    k <- 1
                    while (k <= n) {              
                            if (identical(0, xid[j,i]) == TRUE) {
                                    if (i + k <= n) {
                                            xid[j,] <- xid[j,] + xid[k,]
                                            k <- k + 1
                                    } else {
                                            k <- n + 1
                                    }  
                            } else {
                                    k <- n + 1    
                            } 
                    }
                    
                    xid[j,] <- xid[j,] - xid[i,] * xid[j, i]
                }
        }
        inv <- xid[, 4:6]
        inv
}
