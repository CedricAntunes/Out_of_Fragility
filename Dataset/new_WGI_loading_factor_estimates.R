estimating_new_WGI_loading_factor_missing_values <- function(final_clean_percentiles_data) {
  # Looping through each country
  for (country in unique(final_clean_percentiles_data$COUNTRY_NAME)) {
    # Subsetting the data for each country
    country_data <- final_clean_percentiles_data[final_clean_percentiles_data$COUNTRY_NAME == country, ]
    
    # Filling missing values for 1997
    if (any(is.na(country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 1997]))) {
      value_1996 <- country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 1996]
      value_1998 <- country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 1998]
      if (length(value_1996) == 1 && length(value_1998) == 1) {
        final_clean_percentiles_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[final_clean_percentiles_data$COUNTRY_NAME == country & final_clean_percentiles_data$YEAR == 1997] <- (value_1996 + value_1998) / 2
      }
    }
    
    # Filling missing values for 1999
    if (any(is.na(country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 1999]))) {
      value_1998 <- country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 1998]
      value_2000 <- country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 2000]
      if (length(value_1998) == 1 && length(value_2000) == 1) {
        final_clean_percentiles_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[final_clean_percentiles_data$COUNTRY_NAME == country & final_clean_percentiles_data$YEAR == 1999] <- (value_1998 + value_2000) / 2
      }
    }
  }
  
  # Filling missing values for 2001 after filling 1999
  for (country in unique(final_clean_percentiles_data$COUNTRY_NAME)) {
    # Subsetting the data frame for the current country
    country_data <- final_clean_percentiles_data[final_clean_percentiles_data$COUNTRY_NAME == country, ]
    
    # Filling missing values for 2001
    if (any(is.na(country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 2001]))) {
      value_2000 <- country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 2000]
      value_2002 <- country_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[country_data$YEAR == 2002]
      if (length(value_2000) == 1 && length(value_2002) == 1) {
        final_clean_percentiles_data$NEW_WGI_LOADING_FACTOR_1_NORMALIZED[final_clean_percentiles_data$COUNTRY_NAME == country & final_clean_percentiles_data$YEAR == 2001] <- (value_2000 + value_2002) / 2
      }
    }
  }
  
  return(final_clean_percentiles_data)
}