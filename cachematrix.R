## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     ## matrizInversa vai guardar a matriz inversa
     ## atribui o valor NULO à matrizInversa
     matrizInversa <- NULL 
     
     ## Definições da matriz
     defMatriz <- function(y)
       { 
         x <<- y 
         matrizInversa <<- NULL 
       } 
     ##--------------------------     
     ## Pegar os dados da matriz 
     pegMatriz <- function() x 

     ## Definições para a matriz inversa 
     defMatrizInversa <- function(inverse) matrizInversa <<- inverse 
     ## Pegar dados para a matriz inversa 
     pegMatrizInversa <- function() matrizInversa 
     ## Retornar lista para as matrizes com as novas definições 
     list(set = defMatriz, get = pegMatriz, setinv = defMatrizInversa, getinv = pegMatrizInversa) 
}



##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache


cacheSolve <- function(x, ...) {
     matrizInversa <- x$getinv() 
     ## Se a matriz inversa foi calculada, ou seja, é diferente de nula
     ## retorna a matriz 
     if (!is.null(matrizInversa)) { 
         return(matrizInversa) 
     } 
     ## Caso ainda não tenha sido calculada procede o cálculo 
     dados <- x$get() 
     matrizInversa <- solve(dados, ...) 
     ## Persiste a matriz inversa 
     x$setinv(matrizInversa) 
     ## Retorna a matriz
     matrizInversa 
 }



##Testing the function
x <- matrix(rnorm(16), nrow = 4)           
y <- makeCacheMatrix(x)                   
y$get()                                   
cacheSolve(y)
