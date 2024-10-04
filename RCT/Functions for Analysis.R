descriptives <- function(...)
  















# Examples
or_lrm <- function(model) {
  
  coef <- model$coefficients
  
  se <- model$var %>%
    diag() %>%
    sqrt()
  
  out <- tibble(
    var = names(coef),
    or = exp(coef),
    high = exp(coef + (se * qnorm(.975))),
    low = exp(coef + (se * qnorm(.025)))
  )
  
  return(out)
  
}

tidy_lrm <- function(model) {
  
  r2 <- tibble(
    var = "R2",
    or = model$stats[["R2"]] %>%
      round(3) %>%
      as.character()
  )
  
  model %>%
    or_lrm() %>%
    mutate(
      or = paste0(
        round(or, 2),
        " (",
        round(low, 2),
        ", ",
        round(high, 2),
        ")"
      )
    ) %>%
    select(var, or) %>%
    bind_rows(r2) %>%
    return()
  
}