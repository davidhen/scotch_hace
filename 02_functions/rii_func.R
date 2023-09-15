rii_func <- function(df, group = c("year")){
  df %>%
    group_by(across({{group}})) %>%
    mutate(se = sd(pct)/sqrt(length(pct)),
           decile = case_when(
             decile == "1 - Most Deprived" ~"1",
             decile == "10 - Least Deprived" ~ "10",
             TRUE ~ decile),
           decile = as.integer(decile)) %>%
    select(value = pct,
           se, decile,
           population = n,
           year) %>%
    phe_sii(data = ., quantile = decile, population = population,
            value = value, se = se, rii = TRUE) %>%
    select(year, 
           rii,
           rii_lo = rii_lower95_0cl,
           rii_hi = rii_upper95_0cl,
           sii, 
           sii_low = sii_lower95_0cl,
           sii_hi = sii_upper95_0cl)
}






