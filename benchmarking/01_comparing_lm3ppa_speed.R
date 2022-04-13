library(rsofun)

leuning <- bench::mark(
  runread_lm3ppa_f(
    lm3ppa_gs_leuning_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
)

p_model <- bench::mark(
  runread_lm3ppa_f(
    lm3ppa_p_model_drivers,
    makecheck = TRUE,
    parallel = FALSE
  )
)

print(p_model)
print(leuning)
