## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)
library(dplyr)

## -----------------------------------------------------------------------------
library(lares)

## -----------------------------------------------------------------------------
data(dft)
head(dft, 3)

## -----------------------------------------------------------------------------
# How many survived?
freqs(dft, Survived)

## -----------------------------------------------------------------------------
# Survival by passenger class
freqs(dft, Pclass, Survived)

## ----fig.width=7, fig.height=4------------------------------------------------
# Visualize survival by class
freqs(dft, Pclass, Survived, plot = TRUE)

## ----fig.width=7, fig.height=5------------------------------------------------
freqs_df(dft, plot = TRUE, top = 10)

## -----------------------------------------------------------------------------
# Correlation matrix of numeric variables
cors <- corr(dft[, 2:5], method = "pearson")
head(cors, 3)

## ----fig.width=7, fig.height=5------------------------------------------------
# Which variables correlate most with Survival?
corr_var(dft, Survived, top = 10)

## ----fig.width=7, fig.height=5------------------------------------------------
# Top cross-correlations
corr_cross(dft[, 2:6], top = 8)

## -----------------------------------------------------------------------------
# Reduce ticket categories (keep top 5, group rest as "other")
dft_reduced <- categ_reducer(dft, Ticket, top = 5)
freqs(dft_reduced, Ticket, top = 10)

## -----------------------------------------------------------------------------
# Normalize age
dft$Age_norm <- normalize(dft$Age)
head(dft[, c("Age", "Age_norm")], 5)

## -----------------------------------------------------------------------------
# One-hot encode passenger class
dft_encoded <- ohse(dft[, c("Pclass", "Survived")], limit = 5)
colnames(dft_encoded)

## -----------------------------------------------------------------------------
# Create sample dates
dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")

# Extract year-month
ym <- year_month(dates[1:5])
ym

# Extract year-quarter
yq <- year_quarter(dates[1:5])
yq

# Cut dates into quarters
quarters <- date_cuts(dates[c(1, 100, 200, 300)], type = "Q")
quarters

## ----fig.width=7, fig.height=4------------------------------------------------
library(ggplot2)

ggplot(dft, aes(x = Age, y = Fare * 1000, color = Survived)) +
  geom_point(alpha = 0.6) +
  labs(title = "Age vs Fare by Survival") +
  # Customize theme with several available options
  theme_lares(legend = "top", grid = "Yy", pal = 2, background = "#f2f2f2") +
  # Customize axis scales to look nicer
  scale_y_abbr()

## ----fig.width=7, fig.height=5------------------------------------------------
# Analyze Fare distribution
distr(dft, Fare, breaks = 20)

## -----------------------------------------------------------------------------
# Format large numbers
formatNum(c(1234567, 987654.321), decimals = 2)

# Abbreviate numbers
num_abbr(c(1500, 2500000, 1.5e9))

# Convert abbreviations back to numbers
num_abbr(c("1.5K", "2.5M", "1.5B"), numeric = TRUE)

## ----fig.width=7, fig.height=4------------------------------------------------
df_summary <- dft %>%
  group_by(Pclass) %>%
  summarize(avg_fare = mean(Fare, na.rm = TRUE), .groups = "drop")

ggplot(df_summary, aes(x = factor(Pclass), y = avg_fare)) +
  geom_col(fill = "#00B1DA") +
  labs(title = "Average Fare by Class", x = "Class", y = NULL) +
  scale_y_dollar() + # Format as currency
  theme_lares()

## -----------------------------------------------------------------------------
# Simple comma-separated
vector2text(c("apple", "banana", "cherry"))

# With "and" before last item
vector2text(c("red", "green", "blue"), and = "and")

# Shorter alias
v2t(LETTERS[1:5])

## ----fig.width=7, fig.height=5------------------------------------------------
library(dplyr)

# 1. Load and prepare data
data(dft)

# 2. Clean and transform
dft_clean <- dft %>%
  mutate(Age_Group = cut(Age,
    breaks = c(0, 18, 35, 60, 100),
    labels = c("Child", "Young", "Adult", "Senior")
  ))

# 3. Analyze frequencies
freqs(dft_clean, Age_Group, Survived, plot = TRUE)

# 4. Check correlations
corr_var(dft_clean, Survived_TRUE, top = 8, max_pvalue = 0.05)

