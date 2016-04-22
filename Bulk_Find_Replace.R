#find and replace function
findandreplace <- function(x, search, replace, default = NULL) {
  # build a nested ifelse function by recursion
  findandreplace.fun <- function (search, replace, default = NULL)
    if (length(search) == 0L) {
      function(x) if (is.null(default)) x else rep(default, length(x))
    } else {
      function(x) ifelse(x == search[1L], replace[1L],
                         findandreplace.fun(tail(search,  -1L),
                                            tail(replace, -1L),
                                            default)(x))
    }
  
  return(findandreplace.fun(search, replace, default)(x))
}
