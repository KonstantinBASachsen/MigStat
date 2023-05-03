fit_models <- function(age_groups, years, data, formula) {
    ### would be better as more general function that fits models for
### subsets of data. Maybe loop explicitly over subsets?
    age_gr <- NULL ## why no cmd note for "year"
    n_age <- length(age_groups)
    n_y <- length(years)
    fits <- vector(mode = "list", 
                   length = length(n_age * n_y))
    for (y in years) {
        for (g in age_groups) {
            n <- paste(y, g, sep = "_")
            fits[n] <- lapply(formula, function(x) 
                lm(x, data = data[age_gr == g & year == y]))
        }
    }
    fits <- fits[-1] ## first elements always NULL. Looks dangerous!
    return(fits)
}
