library(dplyr)
library(tidyr)
library(ggplot2)


###########
## PLOTS ##
###########

########################################
## Plots for Ordinal Regression model ##
########################################

library(ggplot2)
library(dplyr)

# Ensure both vectors use the same factor levels
ordinal_observed <- factor(
  ordinal_observed,
  levels = c(1, 2, 3)
)

ordinal_predicted <- factor(
  ordinal_predicted,
  levels = c(1, 2, 3)
)

# Ensure the vectors correspond to the same individuals
stopifnot(length(ordinal_observed) == length(ordinal_predicted))

# Remove observations with missing values
comparison_df <- data.frame(
  observed = ordinal_observed,
  predicted = ordinal_predicted
) |>
  filter(!is.na(observed), !is.na(predicted))

# Confusion matrix
ordinal_cm <- table(
  Observed = comparison_df$observed,
  Predicted = comparison_df$predicted
)

# Row percentages
ordinal_row_percentages <- prop.table(
  ordinal_cm,
  margin = 1
) * 100

# Data for the overlapping bars
plot_df <- data.frame(
  Health_Category = factor(
    c(1, 2, 3),
    levels = c(1, 2, 3),
    labels = c(
      "Very Poor/Poor/Fair",
      "Good",
      "Very Good"
    )
  ),
  Observed = 100,
  Correctly_Predicted = diag(ordinal_row_percentages)
)

# Plot
ggplot(plot_df, aes(x = Health_Category)) +
  
  # All observed individuals in each category
  geom_col(
    aes(y = Observed, fill = "Observed SRH"),
    width = 0.75,
    alpha = 0.55
  ) +
  
  # Correctly predicted individuals within each observed category
  geom_col(
    aes(y = Correctly_Predicted, fill = "Correctly predicted SRH"),
    width = 0.75,
    alpha = 0.55
  ) +
  
  scale_fill_manual(
    values = c(
      "Observed SRH" = "#7B3F8C",
      "Correctly predicted SRH" = "#E5C44F"
    ),
    name = NULL
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    x = NULL,
    y = "Percentage of observed category"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    panel.grid.minor = element_blank()
  )












library(ggplot2)
library(dplyr)

# Make sure levels are in the correct order
ordinal_observed <- factor(ordinal_observed, levels = c(1, 2, 3))
ordinal_predicted <- factor(ordinal_predicted, levels = c(1, 2, 3))

# Check same length
stopifnot(length(ordinal_observed) == length(ordinal_predicted))

# Remove missing values if needed
df <- data.frame(
  observed  = ordinal_observed,
  predicted = ordinal_predicted
) |>
  filter(!is.na(observed), !is.na(predicted))

# Confusion matrix
cm <- table(
  Observed  = df$observed,
  Predicted = df$predicted
)

# Raw counts for plotting
plot_df <- data.frame(
  Health_Category = factor(
    c(1, 2, 3),
    levels = c(1, 2, 3),
    labels = c("Very Poor/Poor/Fair", "Good", "Very Good")
  ),
  Observed_Total = rowSums(cm),
  Correctly_Predicted = diag(cm)
)

# Plot
ggplot(plot_df, aes(x = Health_Category)) +
  geom_col(
    aes(y = Observed_Total, fill = "Observed SRH"),
    width = 0.75,
    alpha = 0.55,
    position = "identity"
  ) +
  geom_col(
    aes(y = Correctly_Predicted, fill = "Correctly Predicted SRH"),
    width = 0.75,
    alpha = 0.55,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c(
      "Observed SRH" = "#7B3F8C",
      "Correctly Predicted SRH" = "#E5C44F"
    ),
    name = NULL
  ) +
  labs(
    x = NULL,
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.grid.minor = element_blank()
  )







#####################################
## Plots for Ordinal Random Forest ##
#####################################

# Ensure both vectors use the same factor levels
health_levels <- c("PoorFair", "Good", "VeryGood")

ordinal_rf_observed_class <- factor(
  ordinal_rf_observed_class,
  levels = health_levels
)

ordinal_rf_predicted_class <- factor(
  ordinal_rf_predicted_class,
  levels = health_levels
)

# Confusion matrix
ordinal_rf_cm <- table(
  Observed = ordinal_rf_observed_class,
  Predicted = ordinal_rf_predicted_class
)

ordinal_rf_cm







# Row percentages from the confusion matrix
ordinal_rf_row_percent <- prop.table(
  ordinal_rf_cm,
  margin = 1
) * 100

# Data for plotting
ordinal_rf_percent_plot_df <- data.frame(
  Health_Category = factor(
    health_levels,
    levels = health_levels
  ),
  Observed = 100,
  Predicted = diag(ordinal_rf_row_percent)
) %>%
  pivot_longer(
    cols = c(Observed, Predicted),
    names_to = "Classification",
    values_to = "Percentage"
  )

# Percentage plot
ordinal_rf_percentage_plot <- ggplot(
  ordinal_rf_percent_plot_df,
  aes(
    x = Health_Category,
    y = Percentage,
    fill = Classification
  )
) +
  geom_col(
    data = subset(
      ordinal_rf_percent_plot_df,
      Classification == "Observed"
    ),
    position = "identity",
    alpha = 0.55,
    width = 0.7
  ) +
  geom_col(
    data = subset(
      ordinal_rf_percent_plot_df,
      Classification == "Predicted"
    ),
    position = "identity",
    alpha = 0.55,
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Observed" = "purple",
      "Predicted" = "gold"
    ),
    labels = c(
      "Observed cases",
      "Correctly predicted"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "PoorFair" = "Very Poor/Poor/Fair",
      "Good" = "Good",
      "VeryGood" = "Very Good"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = "Observed Health Category",
    y = "Percentage",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

ordinal_rf_percentage_plot






ordinal_rf_count_plot_df <- data.frame(
  Health_Category = factor(
    health_levels,
    levels = health_levels
  ),
  Observed = rowSums(ordinal_rf_cm),
  Predicted = diag(ordinal_rf_cm)
) %>%
  pivot_longer(
    cols = c(Observed, Predicted),
    names_to = "Classification",
    values_to = "Count"
  )

# Raw-number plot
ordinal_rf_count_plot <- ggplot(
  ordinal_rf_count_plot_df,
  aes(
    x = Health_Category,
    y = Count,
    fill = Classification
  )
) +
  geom_col(
    data = subset(
      ordinal_rf_count_plot_df,
      Classification == "Observed"
    ),
    position = "identity",
    alpha = 0.55,
    width = 0.7
  ) +
  geom_col(
    data = subset(
      ordinal_rf_count_plot_df,
      Classification == "Predicted"
    ),
    position = "identity",
    alpha = 0.55,
    width = 0.7
  ) +
  scale_fill_manual(
    values = c(
      "Observed" = "purple",
      "Predicted" = "gold"
    ),
    labels = c(
      "Observed cases",
      "Correctly predicted"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "PoorFair" = "Very Poor/Poor/Fair",
      "Good" = "Good",
      "VeryGood" = "Very Good"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Observed Health Category",
    y = "Number of Cases",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank()
  )

ordinal_rf_count_plot
