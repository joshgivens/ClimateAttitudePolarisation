library(lavaan)
library(dplyr)
library(tidyr)
library(xlsx)
# A function to return the approprioate symbol for given p-value
p_signif <- function(x) {
  case_when(
    x < 0.001 ~ "***",
    x < 0.01 ~ "**",
    x < 0.05 ~ "*",
    TRUE ~ ""
  )
}

# A function to create the string required for an RI-CLPM in lavaan
create_RICLPM <- function(var_1, var_2, waves, verbose = FALSE, fixed_lag = FALSE,
                          fix_var = FALSE, new_lag_form = FALSE, constr_vars = FALSE, constr_cov = FALSE) {
  nwave <- length(waves)
  var_1s <- paste0(var_1, "_", waves)
  var_2s <- paste0(var_2, "_", waves)
  wvar_1s <- paste0("w", var_1s)
  wvar_2s <- paste0("w", var_2s)
  RI_var_1 <- paste0("RI", var_1)
  RI_var_2 <- paste0("RI", var_2)

  create_RIs <- paste0(
    "# Create between components (random intercepts)\n",
    RI_var_1, " =~ ", paste0("1*", var_1s, collapse = " + "), "\n\n",
    RI_var_2, " =~ ", paste0("1*", var_2s, collapse = " + "), "\n\n"
  )

  create_withins <- paste0(
    "# Create within-person centered variables\n",
    paste0(wvar_1s, " =~ 1*", var_1s, collapse = "\n"), "\n\n",
    paste0(wvar_2s, " =~ 1*", var_2s, collapse = "\n"), "\n\n"
  )

  if (fix_var) {
    zero_vars <- paste0(
      "# Fix observed variances to 0\n",
      paste0(c(var_1s, var_2s), " ~~ 0*", c(var_1s, var_2s), collapse = "\n"), "\n"
    )
  } else {
    zero_vars <- ""
  }

  if (fixed_lag) {
    a_1 <- "c(a_1)*"
    b_1 <- "c(b_1)*"
    c_1 <- "c(c_1)*"
    d_1 <- "c(d_1)*"
  } else {
    a_1 <- b_1 <- c_1 <- d_1 <- ""
  }

  if (new_lag_form) {
    lagged_effects <- paste0(
      "# Estimate the lagged effects between the within-person centered variables.\n",
      paste0(wvar_1s[-1], " + ", wvar_2s[-1], " ~ ", wvar_1s[-nwave], " + ", wvar_2s[-nwave],
        collapse = "\n"
      ), "\n\n"
    )
  } else {
    lagged_effects <- paste0(
      "# Estimate the lagged effects between the within-person centered variables.\n",
      paste0(wvar_1s[-1], " ~ ", a_1, wvar_1s[-nwave], " + ", c_1, wvar_2s[-nwave], collapse = "\n"), "\n\n",
      paste0(wvar_2s[-1], " ~ ", b_1, wvar_1s[-nwave], " + ", d_1, wvar_2s[-nwave], collapse = "\n"), "\n\n"
    )
  }

  if (constr_cov) {
    covxy <- "cov*"
  } else {
    covxy <- ""
  }

  within_covariances <- paste0(
    "# Estimate the covariance between the within-person centered variables at the first wave.\n",
    wvar_1s[1], " ~~ ", wvar_2s[1], "\n",
    "# Estimate the covariances between the residuals of the within-person centered variables. \n",
    paste0(wvar_1s[-1], " ~~ ", covxy, wvar_2s[-1], collapse = "\n"), "\n\n"
  )

  if (constr_vars) {
    varx <- "varx*"
    vary <- "vary*"
  } else {
    varx <- vary <- ""
  }
  within_variances <- paste0(
    "# Estimate the (residual) variance of the within-person centered variables.\n",
    paste0(wvar_1s[1], " ~~ ", wvar_1s[1], collapse = "\n"), "\n",
    paste0(wvar_1s[-1], " ~~ ", varx, wvar_1s[-1], collapse = "\n"), "\n\n",
    paste0(wvar_2s[1], " ~~ ", wvar_2s[1], collapse = "\n"), "\n",
    paste0(wvar_2s[-1], " ~~ ", vary, wvar_2s[-1], collapse = "\n"), "\n\n"
  )

  RI_variances <- paste0(
    "# Estimate the variance and covariance of the random intercepts.\n",
    RI_var_1, " ~~ ", RI_var_1, "\n", RI_var_2, " ~~ ", RI_var_2, "\n",
    RI_var_1, " ~~ ", RI_var_2
  )

  out <- paste0(
    create_RIs, create_withins, zero_vars, lagged_effects, within_covariances,
    within_variances, RI_variances
  )

  if (verbose) {
    writeLines(out)
  }
  return(out)
}

# A function to create the string required for an RI-CLPM in lavaan
create_CLPM <- function(var_1, var_2, waves, verbose = FALSE, fixed_lag = FALSE,
                        nlags = 1) {
  nwave <- length(waves)
  var_1s <- paste0(var_1, "_", waves)
  var_2s <- paste0(var_2, "_", waves)

  if (fixed_lag) {
    a_1 <- paste0("c(a_", 1:nlags, ")*")
    b_1 <- paste0("c(b_", 1:nlags, ")*")
    c_1 <- paste0("c(c_", 1:nlags, ")*")
    d_1 <- paste0("c(d_", 1:nlags, ")*")
  } else {
    a_1 <- b_1 <- c_1 <- d_1 <- ""
  }

  lag_var_1s <- matrix(ncol = nlags, nrow = nwave - nlags)
  lag_var_2s <- matrix(ncol = nlags, nrow = nwave - nlags)
  for (i in 1:nlags) {
    lag_var_1s[, i] <- var_1s[-c((nwave - (i - 1)):nwave, 0:(nlags - i))]
    lag_var_2s[, i] <- var_2s[-c((nwave - (i - 1)):nwave, 0:(nlags - i))]
  }

  auto_reg_1s <- apply(lag_var_1s, 1, function(x) {
    paste0(a_1, x, collapse = " + ")
  })
  auto_reg_2s <- apply(lag_var_2s, 1, function(x) {
    paste0(b_1, x, collapse = " + ")
  })
  cross_1s <- apply(lag_var_2s, 1, function(x) {
    paste0(c_1, x, collapse = " + ")
  })
  cross_2s <- apply(lag_var_1s, 1, function(x) {
    paste0(d_1, x, collapse = " + ")
  })

  lagged_effects <- paste0(
    "# Estimate the lagged effects between the within-person centered variables.\n",
    paste0(var_1s[-(1:nlags)], " ~ ", auto_reg_1s, " + ", cross_1s, collapse = "\n"), "\n\n",
    paste0(var_2s[-(1:nlags)], " ~ ", auto_reg_2s, " + ", cross_2s, collapse = "\n"), "\n\n"
  )

  within_covariances <- paste0(
    "# Estimate the covariance between the within-person centered variables at the first wave.\n",
    var_1s[1], " ~~ ", var_2s[1], "\n",
    "# Estimate the covariances between the residuals of the within-person centered variables. \n",
    paste0(var_1s[-1], " ~~ ", var_2s[-1], collapse = "\n"), "\n\n"
  )

  within_variances <- paste0(
    "# Estimate the (residual) variance of the within-person centered variables.\n",
    paste0(var_1s, " ~~ ", var_1s, collapse = "\n"), "\n\n",
    paste0(var_2s, " ~~ ", var_2s, collapse = "\n"), "\n\n"
  )

  out <- paste0(lagged_effects, within_covariances, within_variances)

  if (verbose) {
    writeLines(out)
  }
  return(out)
}

tidy_data <- function(df, waves, response_cols) {
  df_out <- df %>%
    select(lfdn, wave, all_of(response_cols)) %>%
    # Normalise responses
    mutate(across(
      response_cols,
      ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))
    )) %>%
    # Filter relevant waves
    filter(wave %in% waves) %>%
    group_by(lfdn) %>%
    # Get number of responses per person
    mutate(across(response_cols, ~ sum(!is.na(.x)), .names = "{.col}_responses")) %>%
    # Transpose to wide format for RI-CLPM
    pivot_wider(names_from = "wave", values_from = response_cols) %>%
    filter(if_any(paste0(response_cols, "_responses"), ~ .x != 0))

  return(df_out)
}

present_coeffs <- function(fit, var_names) {
  newnames <- names(var_names)
  parameterEstimates(fit) %>%
    filter(op == "~") %>%
    mutate(
      wave = paste(gsub(".*?_(\\d+)$", "\\1", rhs), "->", gsub(".*?_(\\d+)$", "\\1", lhs)),
      op = gsub("w|_\\d+", "", paste0(rhs, "->", lhs)),
      op = gsub(var_names[1], newnames[1], op),
      op = gsub(var_names[2], newnames[2], op),
      est = paste0(round(est, 2), p_signif(pvalue))
    ) %>%
    select(-c(se, z, ci.lower, ci.upper, lhs, rhs, pvalue)) %>%
    arrange(op) %>%
    pivot_wider(id_cols = wave, names_from = op, values_from = est) %>%
    select(1, 2, 5, 3, 4)
}
########################### AfD ################################
df <- readRDS("Data/full_tidied.rds")
df <- df %>%
  mutate(
    party_split1 = case_when(
      party_split1 == "GRUNE" ~ 1,
      party_split1 == "Other" ~ 0,
      party_split1 == "AfD" ~ -1
    ), party_split1 = 3 * party_split1,
    party_split2 = if_else(party == "AfD", 1, 0)
  )

party_waves <- c(1, 2, 4, 7, 8, 10:20, 22:23)

afd_response_cols <- c("econ_vs_clim", "party_split2")
names(afd_response_cols) <- c("Clim", "AfD")
df_AfD <- df %>% tidy_data(party_waves, unname(afd_response_cols))



############ RI-CLPM ###########
RICLPM_AfD <- create_RICLPM("econ_vs_clim", "party_split2", party_waves)
RICLPM.fit_AfD <- lavaan(RICLPM_AfD,
  data = df_AfD, missing = "ML",
  meanstructure = TRUE, int.ov.free = TRUE
)

write.xlsx2(
  present_coeffs(RICLPM.fit_AfD, afd_response_cols) %>% as.data.frame() %>%
    wave(), `Afd->1`,
  "Tables/riclpm_AfD.xlsx",
  row.names = FALSE
)

############ CLPM ###########
CLPM_AfD <- create_CLPM("econ_vs_clim", "party_split2", party_waves)
CLPM.fit_AfD <- lavaan(CLPM_AfD,
  data = df_AfD, missing = "ML",
  meanstructure = TRUE, int.ov.free = TRUE
)

write.xlsx2(
  present_coeffs(CLPM.fit_AfD, afd_response_cols) %>% as.data.frame(),
  "Tables/clpm_AfD.xlsx",
  row.names = FALSE
)

############ CL2PM ###########
lag_1s <- paste0(party_waves[-c(1, 18)], " -> ", party_waves[-c(1, 2)])
lag_2s <- paste0(party_waves[-c(17, 18)], " -> ", party_waves[-c(1, 2)])

CL2PM_AfD <- create_CLPM("econ_vs_clim", "party_split2", party_waves, nlags = 2)
CL2PM.fit_AfD <- lavaan(CL2PM_AfD,
  data = df_AfD, missing = "ML",
  meanstructure = TRUE, int.ov.free = TRUE
)
Table <- present_coeffs(CL2PM.fit_AfD, afd_response_cols) %>%
  as.data.frame() %>%
  mutate(Lag = if_else(wave %in% lag_1s, 1, 2)) %>%
  arrange(Lag)
write.xlsx2(Table,
  "Tables/cl2pm_AfD.xlsx",
  row.names = FALSE
)

############# Summary Statistics ######
fitMeasures(RICLPM.fit_AfD, fit.measures = c("CFI", "RMSEA", "SRMR", "TLI"))
fitMeasures(CLPM.fit_AfD, fit.measures = c("CFI", "RMSEA", "SRMR", "TLI"))
fitMeasures(CL2PM.fit_AfD, fit.measures = c("CFI", "RMSEA", "SRMR", "TLI"))
