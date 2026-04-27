library(gtsummary)
library(gt)
library(ggplot2)
library(dplyr)

df <- read.csv("cohort.csv")

#Make a descriptive table of the variables

var_desc <- data.frame(
  Variable = c("smoke", "female", "age", "cardiac", "cost"),
  Type     = c("Binary", "Binary", "Continuous", "Binary", "Continuous"),
  Description = c(
    "Current Smoker (0 = No, 1 = Yes)",
    "Sex (0 = Male, 1 = Female)",
    "Age at Baseline (Years)",
    "Type of Hospitalization (0 = Non-Cardiac, 1 = Cardiac Event-Related)",
    "Cost of Hospitalization (USD)"
  )
)

var_desc %>%
  gt() %>%
  tab_header(title = "Variable Descriptions") %>%
  cols_label(
    Variable    = "Variable",
    Type        = "Type",
    Description = "Description"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

#Table 1! 

df$smoke   <- factor(df$smoke,   levels = c(0,1), labels = c("No", "Yes"))
df$female  <- factor(df$female,  levels = c(0,1), labels = c("Male", "Female"))
df$cardiac <- factor(df$cardiac, levels = c(0,1), 
                     labels = c("Non-Cardiac Event-Related Hospitalization",
                                "Cardiac Event-Related Hospitalization"))

tbl_summary(
  df,
  by = cardiac,
  label = list(
    smoke  ~ "Current Smoker",
    female ~ "Sex",
    age    ~ "Age (Years)",
    cost   ~ "Cost of Hospitalization (USD)"
  ),
  statistic = list(
    all_continuous()  ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 1
) %>%
  add_overall() %>%
  add_p() %>%
  bold_labels()


#Make a figure comparing mean cost by type of hospitalization type

df_summary <- df %>%
  group_by(cardiac) %>%
  summarise(
    mean_cost = mean(cost),
    se_cost   = sd(cost) / sqrt(n())
  )

ggplot(df_summary, aes(x = cardiac, y = mean_cost, fill = cardiac)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_cost - 1.96 * se_cost,
                    ymax = mean_cost + 1.96 * se_cost),
                width = 0.15) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    x     = NULL,
    y     = "Mean Cost of Hospitalization (USD)",
    title = "Hospitalization Cost by Type",
    fill  = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#Linear regression model predicting cost of hospitalization

fit_lm <- lm(cost ~ age + female + smoke + cardiac, data = df)
summary(fit_lm)


fit_lm %>%
  tbl_regression(
    label = list(
      age     ~ "Age (years)",
      female  ~ "Sex",
      smoke   ~ "Current smoker",
      cardiac ~ "Hospitalization type"
    ),
    estimate_fun = ~ style_number(.x, digits = 1)
  ) %>%
  bold_labels() %>%
  bold_p()
