library(lavaan)
library(tidyr)
library(dplyr)
library(stringr)
library(huxtable)
library(ggcorrplot)
library(jtools)
library(semPlot)
library(effectsize)
library(psych)
library(jtools)
library(viridis)
library(semTools)
library(knitr)
library(report)


get_omegas <- function(cfa_fit) {
  bind_rows(
    compRelSEM(cfa_fit, return.total = TRUE),
    compRelSEM(cfa_fit, return.total = TRUE, tau.eq = TRUE)
  ) %>% 
    mutate_all(~round(., 3)) %>% 
    mutate(measurement = c("Omega", "Alpha"), .before=everything())
}

remove_nas <- function(df) {
  no_nas <- df %>% 
    pivot_wider(values_from = response, id_cols = ResponseId, names_from = item) %>% 
    drop_na()
  
  df %>% 
    semi_join(no_nas, by = "ResponseId")
}

plot_response_distribution <- function(df) {
  
  if ("sub_scale" %in% colnames(df)) {
    ggplot(df, aes(x =as.factor(response), fill=sub_scale)) +
      geom_bar() +
      facet_wrap(~item) +
      labs(x="Response", fill="Sub Scale", y="Count") +
      theme_apa() +
      scale_fill_viridis(discrete=TRUE, option = "H") +
      theme(legend.position = "bottom") +
      guides(fill=guide_legend(ncol = 2))
  } else {
    ggplot(df, aes(x =as.ordered(response))) +
      geom_bar() +
      facet_wrap(~item) +
      labs(x="Response", y="Count") +
      theme_apa()
  }
  
}


get_alpha <- function(df) {
  alpha_df <- df %>%
    pivot_wider(values_from = response, id_cols = ResponseId, names_from = item) %>% 
    drop_na() %>% 
    select(-ResponseId) %>% 
    alpha() 
  
  alpha_df[["total"]][["raw_alpha"]]
}

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

interpret_fit <- function(cfa_fit) {
  interpret(cfa_fit) %>% 
    filter(Name %in% c("CFI", "NNFI", "RMSEA", "SRMR")) %>% 
    mutate(Interpretation = case_when(
      Name == "RMSEA" & Value > 0.05 & Value < 0.08 ~ "satisfactory",
      Name == "RMSEA" & Value < 0.05 ~ "good",
      TRUE ~ as.character(Interpretation)
    ))
}

report_fit_less_chi <- function(cfa_fit) {
  report_performance(
    cfa_fit, 
    metrics=c(
      "NNFI", 
      "CFI",
      "RMSEA", 
      "SRMR",
      "AIC")
    )
}

report_fit_chi <- function(cfa_fit) {
  report_performance(
    cfa_fit, 
    metrics=c("Chi2", "Chi2_df", "p_Chi2", "NNFI", "CFI"))
}