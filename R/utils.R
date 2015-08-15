# Internal helpers - export none of these

# strtovec
str_to_vec <- function(string, char) {
     # simplifies the list semantics
     unlist(stri_split_fixed(string, char))
}

# dispatch function based on input length
select.func <- function(..., args, funcs) {
     funcs[[length(args)]](..., args)
}
