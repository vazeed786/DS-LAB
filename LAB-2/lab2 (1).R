# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Install required packages if not already installed
if (!require(tidyverse, quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require(lubridate, quietly = TRUE)) {
  install.packages("lubridate")
  library(lubridate)
}

if (!require(scales, quietly = TRUE)) {
  install.packages("scales")
  library(scales)
}

if (!require(ggpubr, quietly = TRUE)) {
  install.packages("ggpubr")
  library(ggpubr)
}

if (!require(moments, quietly = TRUE)) {
  install.packages("moments")
  library(moments)
}

if (!require(corrplot, quietly = TRUE)) {
  install.packages("corrplot")
  library(corrplot)
}

cat("Loading dataset...\n")
df <- read_csv('yellow_tripdata_sample.csv')

df <- df %>%
  mutate(across(where(is.character), str_trim))

datetime_cols <- c('tpep_pickup_datetime', 'tpep_dropoff_datetime')
for(col in datetime_cols) {
  if(col %in% names(df)) {
    df[[col]] <- as_datetime(df[[col]])
  }
}

cat("Dataset Shape:", dim(df), "\n")
cat("\nMissing Values:\n")
print(colSums(is.na(df)))

cat("\n==================================================\n")
cat("PART A: DESCRIPTIVE STATISTICS\n")
cat("==================================================\n")

target_columns <- c('passenger_count', 'trip_distance', 'fare_amount', 
                   'total_amount', 'tip_amount', 'extra')

for(col in target_columns) {
  if(col %in% names(df)) {
    cat("\n========================================\n")
    cat("Analysis for:", toupper(col), "\n")
    cat("========================================\n")
    
    data_vec <- df[[col]]
    data_vec <- data_vec[!is.na(data_vec)]
    
    cat("Count:", length(data_vec), "\n")
    cat("Missing values:", sum(is.na(df[[col]])), "\n")
    cat("Mean:", round(mean(data_vec, na.rm = TRUE), 2), "\n")
    cat("Median:", round(median(data_vec, na.rm = TRUE), 2), "\n")
    cat("Min:", round(min(data_vec, na.rm = TRUE), 2), "\n")
    cat("Max:", round(max(data_vec, na.rm = TRUE), 2), "\n")
    cat("Std Dev:", round(sd(data_vec, na.rm = TRUE), 2), "\n")
    cat("Variance:", round(var(data_vec, na.rm = TRUE), 2), "\n")
    cat("Skewness:", round(skewness(data_vec, na.rm = TRUE), 2), "\n")
    cat("Kurtosis:", round(kurtosis(data_vec, na.rm = TRUE), 2), "\n")
  }
}

cat("\n==================================================\n")
cat("VISUALIZATIONS\n")
cat("==================================================\n")

plots <- list()

if('trip_distance' %in% names(df)) {
  plots$p1 <- ggplot(df, aes(x = trip_distance)) +
    geom_histogram(fill = "skyblue", color = "black", alpha = 0.7, bins = 50) +
    labs(title = "Trip Distance Distribution", x = "Trip Distance (miles)", y = "Frequency")
}

if('fare_amount' %in% names(df)) {
  plots$p2 <- ggplot(df, aes(y = fare_amount)) +
    geom_boxplot() +
    labs(title = "Fare Amount Box Plot", y = "Fare Amount ($)")
}

if('tip_amount' %in% names(df)) {
  plots$p3 <- ggplot(df, aes(x = tip_amount)) +
    geom_density(fill = "orange", alpha = 0.7) +
    labs(title = "Tip Amount Density Plot", x = "Tip Amount ($)")
}

if('passenger_count' %in% names(df)) {
  plots$p4 <- ggplot(df, aes(x = "", y = passenger_count)) +
    geom_violin(fill = "lightgreen", alpha = 0.7) +
    labs(title = "Passenger Count Violin Plot", x = "", y = "Passenger Count")
}

if('payment_type' %in% names(df)) {
  payment_counts <- df %>% count(payment_type)
  plots$p5 <- ggplot(payment_counts, aes(x = as.factor(payment_type), y = n)) +
    geom_col(fill = "lightblue") +
    labs(title = "Payment Type Distribution", x = "Payment Type", y = "Count")
}

if('VendorID' %in% names(df)) {
  vendor_counts <- df %>% count(VendorID)
  plots$p6 <- ggplot(vendor_counts, aes(x = "", y = n, fill = as.factor(VendorID))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Vendor ID Distribution") +
    theme_void()
}

# Create only the plots that exist
existing_plots <- plots[names(plots) %in% paste0("p", 1:6)]
if(length(existing_plots) > 0) {
  # Print individual plots instead of using ggarrange to avoid errors
  for(plot_name in names(existing_plots)) {
    print(existing_plots[[plot_name]])
  }
}

if('trip_distance' %in% names(df)) {
  print(ggplot(df, aes(x = trip_distance)) +
    geom_freqpoly(bins = 50, color = "blue", size = 1) +
    labs(title = "Frequency Polygon - Trip Distance", x = "Trip Distance (miles)", y = "Frequency") +
    theme_minimal())
}

cat("\n==================================================\n")
cat("PART B: INFERENTIAL STATISTICS\n")
cat("==================================================\n")

confidence_interval <- function(data, confidence = 0.95) {
  n <- length(data)
  mean_val <- mean(data, na.rm = TRUE)
  se <- sd(data, na.rm = TRUE) / sqrt(n)
  margin <- qt((1 + confidence) / 2, df = n - 1) * se
  c(mean = mean_val, lower = mean_val - margin, upper = mean_val + margin)
}

cat("\n95% Confidence Intervals:\n")
for(col in c('trip_distance', 'fare_amount', 'tip_amount')) {
  if(col %in% names(df)) {
    ci <- confidence_interval(df[[col]])
    cat(col, ": Mean =", round(ci['mean'], 2), ", CI = (", round(ci['lower'], 2), ",", round(ci['upper'], 2), ")\n")
  }
}

cat("\n------------------------------\n")
cat("HYPOTHESIS TESTING\n")
cat("------------------------------\n")

if('tip_amount' %in% names(df)) {
  tip_data <- na.omit(df$tip_amount)
  t_test <- t.test(tip_data, mu = 2)
  cat("One-sample t-test for tip amount = $2:\n")
  cat("T-statistic:", round(t_test$statistic, 3), ", P-value:", round(t_test$p.value, 4), "\n")
  if(t_test$p.value < 0.05) {
    cat("Reject H0: Average tip amount is significantly different from $2\n")
  } else {
    cat("Fail to reject H0: No significant evidence that average tip amount differs from $2\n")
  }
}

if('payment_type' %in% names(df) && 'fare_amount' %in% names(df)) {
  credit_card_fares <- df %>% filter(payment_type == 1) %>% pull(fare_amount) %>% na.omit()
  cash_fares <- df %>% filter(payment_type == 2) %>% pull(fare_amount) %>% na.omit()
  
  if(length(credit_card_fares) > 0 && length(cash_fares) > 0) {
    t_test2 <- t.test(credit_card_fares, cash_fares, var.equal = FALSE)
    cat("\nTwo-sample t-test (Credit Card vs Cash fares):\n")
    cat("T-statistic:", round(t_test2$statistic, 3), ", P-value:", round(t_test2$p.value, 4), "\n")
    cat("Mean fare (Credit Card): $", round(mean(credit_card_fares), 2), "\n")
    cat("Mean fare (Cash): $", round(mean(cash_fares), 2), "\n")
    if(t_test2$p.value < 0.05) {
      cat("Reject H0: Significant difference in fare amounts between payment types\n")
    } else {
      cat("Fail to reject H0: No significant difference in fare amounts\n")
    }
  }
}

if('payment_type' %in% names(df) && 'RatecodeID' %in% names(df)) {
  contingency_table <- table(df$payment_type, df$RatecodeID)
  chi_test <- chisq.test(contingency_table)
  cat("\nChi-square test for Payment_type and RateCodeID:\n")
  cat("Chi2 statistic:", round(chi_test$statistic, 3), ", P-value:", round(chi_test$p.value, 4), ", Degrees of freedom:", chi_test$parameter, "\n")
  if(chi_test$p.value < 0.05) {
    cat("Reject H0: Payment_type and RateCodeID are not independent\n")
  } else {
    cat("Fail to reject H0: No evidence against independence\n")
  }
}

cat("\n------------------------------\n")
cat("CORRELATION ANALYSIS\n")
cat("------------------------------\n")

corr_columns <- c('trip_distance', 'fare_amount', 'tip_amount', 'total_amount', 'passenger_count')
corr_columns <- corr_columns[corr_columns %in% names(df)]

if(length(corr_columns) > 0) {
  corr_df <- df %>% select(all_of(corr_columns)) %>% na.omit()
  
  if(nrow(corr_df) > 0) {
    pearson_corr <- cor(corr_df, method = "pearson")
    cat("Pearson Correlation Matrix:\n")
    print(round(pearson_corr, 3))
    
    if('trip_distance' %in% corr_columns && 'fare_amount' %in% corr_columns) {
      cat("\nTrip_distance vs Fare_amount: r =", round(pearson_corr['trip_distance', 'fare_amount'], 3), "\n")
    }
    if('fare_amount' %in% corr_columns && 'tip_amount' %in% corr_columns) {
      cat("Fare_amount vs Tip_amount: r =", round(pearson_corr['fare_amount', 'tip_amount'], 3), "\n")
    }
    
    corrplot(pearson_corr, method = "color", type = "upper", 
             tl.col = "black", tl.srt = 45, addCoef.col = "black")
  }
}

cat("\n==================================================\n")
cat("BONUS TASKS\n")
cat("==================================================\n")

if('tpep_pickup_datetime' %in% names(df)) {
  df <- df %>%
    mutate(pickup_date = as_date(tpep_pickup_datetime),
           pickup_hour = hour(tpep_pickup_datetime))
  
  # Daily trip count
  daily_trips <- df %>% count(pickup_date)
  
  p1 <- ggplot(daily_trips, aes(x = pickup_date, y = n)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(title = "Daily Trip Count", x = "Date", y = "Number of Trips") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p1)
  
  # Save the plot
  ggsave("daily_trip_count.png", p1, width = 10, height = 6, dpi = 300)
  cat("Daily trip count plot saved as 'daily_trip_count.png'\n")
  
  # Hourly average fare
  hourly_fare <- df %>%
    group_by(pickup_hour) %>%
    summarise(avg_fare = mean(fare_amount, na.rm = TRUE))
  
  p2 <- ggplot(hourly_fare, aes(x = pickup_hour, y = avg_fare)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    labs(title = "Average Fare Amount by Hour of Day", 
         x = "Hour of Day", 
         y = "Average Fare Amount ($)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = 0:23)
  
  print(p2)
  
  # Save the plot
  ggsave("hourly_fare.png", p2, width = 10, height = 6, dpi = 300)
  cat("Hourly fare plot saved as 'hourly_fare.png'\n")
  
  # Additional analysis: Rush hour analysis
  rush_hours <- c(7, 8, 9, 16, 17, 18) # Morning and evening rush hours
  df <- df %>%
    mutate(is_rush_hour = pickup_hour %in% rush_hours)
  
  rush_hour_fare <- df %>%
    group_by(is_rush_hour) %>%
    summarise(avg_fare = mean(fare_amount, na.rm = TRUE),
              avg_tip = mean(tip_amount, na.rm = TRUE),
              n_trips = n())
  
  cat("\nRush Hour Analysis:\n")
  print(rush_hour_fare)
  
  # Most common pickup locations
  if('PULocationID' %in% names(df)) {
    top_pickup_locations <- df %>%
      count(PULocationID) %>%
      arrange(desc(n)) %>%
      head(10)
    
    cat("\nTop 10 Pickup Locations:\n")
    print(top_pickup_locations)
    
    p3 <- ggplot(top_pickup_locations, aes(x = reorder(as.factor(PULocationID), n), y = n)) +
      geom_col(fill = "coral", alpha = 0.8) +
      labs(title = "Top 10 Pickup Locations", 
           x = "Location ID", 
           y = "Number of Trips") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_flip()
    
    print(p3)
    ggsave("top_pickup_locations.png", p3, width = 10, height = 6, dpi = 300)
    cat("Top pickup locations plot saved as 'top_pickup_locations.png'\n")
  }
  
  # Most common dropoff locations
  if('DOLocationID' %in% names(df)) {
    top_dropoff_locations <- df %>%
      count(DOLocationID) %>%
      arrange(desc(n)) %>%
      head(10)
    
    cat("\nTop 10 Dropoff Locations:\n")
    print(top_dropoff_locations)
    
    p4 <- ggplot(top_dropoff_locations, aes(x = reorder(as.factor(DOLocationID), n), y = n)) +
      geom_col(fill = "lightgreen", alpha = 0.8) +
      labs(title = "Top 10 Dropoff Locations", 
           x = "Location ID", 
           y = "Number of Trips") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_flip()
    
    print(p4)
    ggsave("top_dropoff_locations.png", p4, width = 10, height = 6, dpi = 300)
    cat("Top dropoff locations plot saved as 'top_dropoff_locations.png'\n")
  }
}

cat("\nBonus tasks analysis completed successfully!\n")
cat("All plots have been saved to your working directory.\n")