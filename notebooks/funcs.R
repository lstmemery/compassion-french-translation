library(lavaan)
library(tidyr)
library(dplyr)
library(stringr)
library(huxtable)
library(ggcorrplot)
library(jtools)
library(semPlot)


fit_cfa <- function(model, df) {
  cfa(
    model, 
    data=df,
    estimator="MLR",
    std.lv=TRUE
  )
}

format_cfa_parameters <- function(model) {
  parameterestimates(
    model, 
    standardized = TRUE,
    remove.eq = TRUE,
    zstat = FALSE,
    remove.system.eq = TRUE,
    remove.nonfree = TRUE
  ) %>% 
    mutate(across(where(is.numeric), ~round(., 2)))
}

lav_fit_measures <- function(fit_model, df, item_scale, model_name) {
  round_amount <- 2
  size <- nrow(df)

  model_fit_model <- fitmeasures(fit_model, fit.measures = "all") %>% 
    stack() %>% 
    pivot_wider(names_from = "ind", values_from = "values")
  model_fit_model %>% 
    transmute(
      Scale = item_scale,
      Model = model_name,
      Size = size,
      CFI = round(cfi, round_amount),
      `RMSEA [90% C.I.]` = str_c(
        round(rmsea, round_amount), 
        " [", 
        round(rmsea.ci.lower, round_amount), 
        ", ", 
        round(rmsea.ci.upper, round_amount), "]"),
      NNFI = round(nnfi, round_amount),
      SRMR = round(srmr, round_amount),
      `χ² (df)` = str_c(round(chisq), " (", df, ")"),
      AIC = round(aic)
    )
}

format_sem_paths <- function(fit_model, item_scale, model_name) {
  semPaths(
    fit_model, 
    what = "std", 
    edge.label.cex = 0.7, 
    edge.color = 1, 
    esize = 1, 
    sizeMan = 4.5, 
    asize = 2.5, 
    intercepts = FALSE, 
    rotation = 4, 
    thresholdColor = "red", 
    mar = c(1, 5, 1.5, 5), 
    fade = FALSE, 
    nCharNodes = 10, 
  )
  title(str_c(item_scale, model_name, sep=" "))
}

format_huxtable <- function(df) {
  df %>% 
    as_huxtable(autoformat=FALSE) %>% 
    theme_article() %>% 
    set_number_format(value=fmt_pretty(big.mark = ""))
}

format_huxtable2 <- function(df, output) {
  df %>% 
    as_huxtable(autoformat=FALSE) %>% 
    theme_article() %>% 
    set_number_format(value=fmt_pretty(big.mark = "")) %>% 
    huxtable::quick_docx(file=output, open = FALSE)
}

format_correlation <- function(corr, title) {
  ggcorrplot(
    corr, 
    ggtheme=theme_apa(), 
    type="lower", 
    title = title,
    legend.title = "Correlation",
    lab =TRUE,
  )
}