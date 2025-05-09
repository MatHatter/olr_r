## ----html-style, results='asis', echo=FALSE-----------------------------------
htmltools::HTML("
pre code {
  white-space: pre-wrap;
  word-wrap: break-word;
  overflow-x: auto;
  font-size: 90%;
}
")

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(olr)
library(ggplot2)

## -----------------------------------------------------------------------------
# Load data
crudeoildata <- read.csv(system.file("extdata", "crudeoildata.csv", package = "olr"))
dataset <- crudeoildata[, -1]

# Define variables
responseName <- 'CrudeOil'
predictorNames <- c('RigCount', 'API', 'FieldProduction', 'RefinerNetInput',
                    'OperableCapacity', 'Imports', 'StocksExcludingSPR',
                    'NonCommercialLong', 'NonCommercialShort',
                    'CommercialLong', 'CommercialShort', 'OpenInterest')

## -----------------------------------------------------------------------------
# Full model using R-squared
model_r2 <- olr(dataset, responseName, predictorNames, adjr2 = FALSE)

# Adjusted R-squared model
model_adjr2 <- olr(dataset, responseName, predictorNames, adjr2 = TRUE)

## ----plot-r2-line, fig.align="center", fig.width=6.3, fig.height=4.5, out.width='99%'----
# Actual values
actual <- dataset[[responseName]]
fitted_r2 <- model_r2$fitted.values
fitted_adjr2 <- model_adjr2$fitted.values

# Data frames for ggplot
plot_data <- data.frame(
  Index = 1:length(actual),
  Actual = actual,
  R2_Fitted = fitted_r2,
  AdjR2_Fitted = fitted_adjr2
)

# Plot both fits
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Actual), color = "black", size = 1, linetype = "dashed") +
  geom_line(aes(y = R2_Fitted), color = "steelblue", size = 1) +
  labs(
    title = "Full Model (R-squared): Actual vs Fitted Values",
    subtitle = "Observation Index used in place of dates (parsed from original dataset)",
    x = "Observation Index",
    y = "CrudeOil % Change"
  ) +
  theme_minimal()

## ----plot-adjr2-line, fig.align="center", fig.width=6.3, fig.height=4.5, out.width='99%'----
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Actual), color = "black", size = 1, linetype = "dashed") +
  geom_line(aes(y = AdjR2_Fitted), color = "limegreen", size = 1.1) +
  labs(
    title = "Optimal Model (Adjusted R-squared): Actual vs Fitted Values",
    subtitle = "Observation Index used in place of dates (parsed from original dataset)",
    x = "Observation Index",
    y = "CrudeOil % Change"
  )+
  theme_minimal() +
  theme(plot.background = element_rect(color = "limegreen", size = 2))

