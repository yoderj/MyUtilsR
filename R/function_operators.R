# Predicate Function Operators
#' @export
And <- function(f1, f2){
    force(f1); force(f2)
    function(...){
        f1(...) && f2(...)
    }
}
#' @export
Not <- function(f1){
    force(f1)
    function(...){!f1(...)}
}
#' @export
Or <- function(f1, f2){
    force(f1); force(f2)
    function(...){
        f1(...) || f2(...)
    }
}

# Behavioral side effect FOs
#' @export
dot_every <- function(n, f){
    i <- 1
    function(...){
        if (i %% n == 0) cat('.')
        i <<- i + 1
        f(...)
    }
}

