library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(reshape2)
library(viridis)
library(tidyr)
library(gridExtra)
library(kableExtra)
library(tseries)
library(car)
library(MASS)
library(nnet)
library(lmtest)
library(openxlsx)
library(RColorBrewer)
library(olsrr)
library(readxl)
library(segmented)
library(mgcv)
library(rpart)
library(car)
library(coin)
library(ggh4x)
library(ggnewscale)



#HL Variable
categorize_hearing_loss <- function(PTA) {
  if (is.na(PTA)) {
    return(NA)  # Handle NA values if present
  } else if (PTA <= 15) {
    return('Normal')
  } else if (PTA <= 25) {
    return('Slight')
  } else if (PTA <= 40) {
    return('Mild')
  } else if (PTA <= 55) {
    return('Moderate')
  } else if (PTA <= 70) {
    return('Moderately severe')
  } else if (PTA <= 90) {
    return('Severe')
  } else {
    return('Profound')
  }
}



# Function to perform Wilcoxon Rank-Sum Test for each Q1 category
perform_wilcoxon_test <- function(df, question, variable) {
  results <- df %>%
    group_by(.data[[question]]) %>%
    summarise(p_value = ifelse(length(unique(Sex)) > 1, wilcox.test(get(variable) ~ Sex)$p.value, NA)) %>%
    filter(!is.na(p_value)) %>%
    mutate(question = question, variable = variable)
  return(results)
}



get_segment_age <- function(sex, question, q_level, age, female_breakpoints, male_breakpoints) {
  # Handle NA values
  if (is.na(sex) | is.na(q_level) | is.na(age)) {
    return(NA)
  }
  
  # Determine the breakpoints based on gender
  if (sex == 'Female') {
    breakpoints <- female_breakpoints
  } else {
    breakpoints <- male_breakpoints
  }
  
  # Get the appropriate breakpoint
  key <- paste(question, q_level, sep = ".")
  if (key %in% names(breakpoints)) {
    age_thresholds <- breakpoints[[key]]
    # Handle empty thresholds
    if (length(age_thresholds) == 0) {
      return('Unknown')
    }
    # Assign segment_age based on the age
    if (age < age_thresholds[1]) {
      return(paste0('<', age_thresholds[1]))
    } else if (age >= age_thresholds[1] && age <= age_thresholds[2]) {
      return(paste0(age_thresholds[1], '-', age_thresholds[2]))
    } else {
      return(paste0('>', age_thresholds[2]))
    }
  } else {
    return('Unknown')
  }
}




# Function to collect p-values with sex differentiation
#This function use kruskal.test
collect_p_values_with_sex_kruskal <- function(df, question_col, segmented_age_col) {
  unique_levels <- unique(df[[question_col]])
  p_values <- data.frame(
    Question = character(),
    Sex = character(),
    Level = character(),
    P_Value = numeric(),
    Value_Col = character(),
    stringsAsFactors = FALSE
  )
  
  for (level in unique_levels) {
    for (sex in unique(df$Sex)) {
      sub_df <- df %>% filter(!!sym(question_col) == level & Sex == sex)
      if (nrow(sub_df) > 1) { # Ensure there are enough data points for the test
        if (length(unique(sub_df[[segmented_age_col]])) > 1) { # Ensure there are multiple groups
          kruskal_test_snr <- kruskal.test(SNR ~ get(segmented_age_col), 
                                           data = sub_df)
          kruskal_test_srt <- kruskal.test(SRT ~ get(segmented_age_col), 
                                           data = sub_df)
          
          p_values <- rbind(p_values, data.frame(
            Question = question_col,
            Sex = sex,
            Level = level,
            P_Value = kruskal_test_snr$p.value,
            Value_Col = "SNR"
          ))
          
          p_values <- rbind(p_values, data.frame(
            Question = question_col,
            Sex = sex,
            Level = level,
            P_Value = kruskal_test_srt$p.value,
            Value_Col = "SRT"
          ))
        }
      }
    }
  }
  
  return(p_values)
}



#This function use permutation test
collect_p_values_with_sex_perm1 <- function(df, question_col, segmented_age_col) {
  unique_levels <- unique(df[[question_col]])
  p_values <- data.frame(
    Question = character(),
    Sex = character(),
    Level = character(),
    P_Value = numeric(),
    Value_Col = character(),
    stringsAsFactors = FALSE
  )
  
  for (level in unique_levels) {
    for (sex in unique(df$Sex)) {
      sub_df <- df %>% filter(!!sym(question_col) == level & Sex == sex)
      if (nrow(sub_df) > 1) { # Ensure there are enough data points for the test
        # Convert segmented age column to factor
        sub_df[[segmented_age_col]] <- as.factor(sub_df[[segmented_age_col]])
        
        if (length(unique(sub_df[[segmented_age_col]])) > 1) { # Ensure there are multiple groups
          perm_test_snr <- oneway_test(SNR ~ get(segmented_age_col), data = sub_df, 
                                       distribution = approximate(nresample = 9999))
          perm_test_srt <- oneway_test(SRT ~ get(segmented_age_col), 
                                       data = sub_df, distribution = approximate(nresample = 9999))
          
          p_values <- rbind(p_values, data.frame(
            Question = question_col,
            Sex = sex,
            Level = level,
            P_Value = pvalue(perm_test_snr),
            Value_Col = "SNR"
          ))
          
          p_values <- rbind(p_values, data.frame(
            Question = question_col,
            Sex = sex,
            Level = level,
            P_Value = pvalue(perm_test_srt),
            Value_Col = "SRT"
          ))
        }
      }
    }
  }
  
  return(p_values)
}


# Function to collect p-values with sex differentiation using permutation test
collect_p_values_with_sex_perm2 <- function(df, question_col, segmented_age_col, 
                                      nresample = 9999, adjustment_method = "holm") {
  unique_levels <- unique(df[[question_col]])
  p_values <- data.frame(
    Question = character(),
    Sex = character(),
    Level = character(),
    P_Value = numeric(),
    Value_Col = character(),
    stringsAsFactors = FALSE
  )
  
  for (level in unique_levels) {
    for (sex in unique(df$Sex)) {
      sub_df <- df %>% filter(!!sym(question_col) == level & Sex == sex)
      if (nrow(sub_df) > 1) { # Ensure there are enough data points for the test
        # Convert segmented age column to factor
        sub_df[[segmented_age_col]] <- as.factor(sub_df[[segmented_age_col]])
        
        if (length(unique(sub_df[[segmented_age_col]])) > 1) { # Ensure there are multiple groups
          perm_test_snr <- oneway_test(SNR ~ get(segmented_age_col), data = sub_df, 
                                       distribution = approximate(nresample = nresample))
          perm_test_srt <- oneway_test(SRT ~ get(segmented_age_col), 
                                       data = sub_df, distribution = approximate(nresample = nresample))
          
          p_values <- rbind(p_values, data.frame(
            Question = question_col,
            Sex = sex,
            Level = level,
            P_Value = pvalue(perm_test_snr),
            Value_Col = "SNR"
          ))
          
          p_values <- rbind(p_values, data.frame(
            Question = question_col,
            Sex = sex,
            Level = level,
            P_Value = pvalue(perm_test_srt),
            Value_Col = "SRT"
          ))
        }
      }
    }
  }
  
  # Adjust for multiple comparisons
  p_values$Adjusted_P_Value <- p.adjust(p_values$P_Value, method = adjustment_method)
  
  return(p_values)
}



# Function to collect p-values with sex differentiation using Mood's Median Test
collect_p_values_with_sex_median <- function(df, question_col, segmented_age_col) {
  unique_levels <- unique(df[[question_col]])
  p_values <- data.frame(
    Question = character(),
    Sex = character(),
    Level = character(),
    P_Value = numeric(),
    Value_Col = character(),
    stringsAsFactors = FALSE
  )
  
  for (level in unique_levels) {
    for (sex in unique(df$Sex)) {
      sub_df <- df %>% filter(!!sym(question_col) == level & Sex == sex)
      if (nrow(sub_df) > 1) { # Ensure there are enough data points for the test
        # Convert segmented age column to factor
        sub_df[[segmented_age_col]] <- as.factor(sub_df[[segmented_age_col]])
        
        if (length(unique(sub_df[[segmented_age_col]])) > 1) { # Ensure there are multiple groups
          median_test_snr <- median_test(SNR ~ get(segmented_age_col), data = sub_df)
          median_test_srt <- median_test(SRT ~ get(segmented_age_col), data = sub_df)
          
          p_values <- rbind(p_values, data.frame(
            Question = question_col,
            Sex = sex,
            Level = level,
            P_Value = pvalue(median_test_snr),
            Value_Col = "SNR"
          ))
          
          p_values <- rbind(p_values, data.frame(
            Question = question_col,
            Sex = sex,
            Level = level,
            P_Value = pvalue(median_test_srt),
            Value_Col = "SRT"
          ))
        }
      }
    }
  }
  
  return(p_values)
}







#SAMPLE SIZES
check_sample_sizes <- function(df, question_col, segmented_age_col) {
  df %>%
    group_by(!!sym(question_col), !!sym(segmented_age_col), Sex) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
}

check_sample_sizes_HL <- function(df, question_col, segmented_age_col) {
  df %>%
    group_by(!!sym(question_col), !!sym(segmented_age_col), Sex, HL) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count))
}


# Function to plot sample sizes
plot_sample_sizes <- function(sample_sizes, question_col, segmented_age_col) {
  # Filter out the "Unknown3" category
  sample_sizes <- sample_sizes %>%
    filter(get(segmented_age_col) != "Unknown")
  
  # Custom sorting function
  sort_age_levels <- function(levels) {
    # Separate out "<", ">" and ranges
    less_than <- levels[grep("^<", levels)]
    greater_than <- levels[grep("^>", levels)]
    ranges <- levels[grep("^[0-9]+-[0-9]+$", levels)]
    
    # Sort numerically
    less_than <- less_than[order(as.numeric(gsub("<", "", less_than)))]
    greater_than <- greater_than[order(as.numeric(gsub(">", "", greater_than)))]
    ranges <- ranges[order(as.numeric(sub("-.*", "", ranges)))]
    
    sorted_levels <- c(less_than, ranges, greater_than)
    return(sorted_levels)
  }
  
  age_levels <- unique(sample_sizes[[segmented_age_col]])
  sorted_levels <- sort_age_levels(age_levels)
  sample_sizes[[segmented_age_col]] <- factor(sample_sizes[[segmented_age_col]], 
                                              levels = sorted_levels)
  
  
  ggplot(sample_sizes, aes(x = get(segmented_age_col), 
                           y = count, 
                           fill = get(question_col))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(#title = paste("Sample Sizes by", 
         #              segmented_age_col, "for", 
         #              question_col),
         x = "Age Break Points",
         y = "Sample Size",
         fill = "COSI Need") +
    theme_bw() + 
    facet_grid2(Sex  ~ get(question_col), scales = "free_x", independent = "x") +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          strip.text = element_text(color = 'black', size = 7),
          strip.background = element_rect(fill = 'lightblue', color = 'black'),
          legend.position = "none") +
    guides(color = guide_legend(ncol = 4))
}

plot_sample_sizes_HL <- function(sample_sizes, question_col, segmented_age_col) {
  # Filter out the "Unknown" category
  sample_sizes <- sample_sizes %>%
    filter(get(segmented_age_col) != "Unknown")
  
  # Custom sorting function for age segments
  sort_age_levels <- function(levels) {
    # Separate out "<", ">" and ranges
    less_than <- levels[grep("^<", levels)]
    greater_than <- levels[grep("^>", levels)]
    ranges <- levels[grep("^[0-9]+-[0-9]+$", levels)]
    
    # Sort numerically
    less_than <- less_than[order(as.numeric(gsub("<", "", less_than)))]
    greater_than <- greater_than[order(as.numeric(gsub(">", "", greater_than)))]
    ranges <- ranges[order(as.numeric(sub("-.*", "", ranges)))]
    
    sorted_levels <- c(less_than, ranges, greater_than)
    return(sorted_levels)
  }
  
  age_levels <- unique(sample_sizes[[segmented_age_col]])
  sorted_levels <- sort_age_levels(age_levels)
  sample_sizes[[segmented_age_col]] <- factor(sample_sizes[[segmented_age_col]], 
                                              levels = sorted_levels)
  
  ggplot(sample_sizes, aes(x = get(segmented_age_col), 
                           y = count, 
                           fill = HL)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Age Break Points",
         y = "Sample Size",
         fill = "Hearing Loss") +
    theme_bw() + 
    facet_grid2(Sex ~ get(question_col), scales = "free_x", independent = "x") +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          strip.text = element_text(color = 'black', size = 8),
          strip.background = element_rect(fill = 'lightblue', color = 'black'),
          legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 4))
}



plot_distributions <- function(df, question_col, segmented_age_col) {
  
  # Custom sorting function
  sort_age_levels <- function(levels) {
    # Separate out "<", ">" and ranges
    less_than <- levels[grep("^<", levels)]
    greater_than <- levels[grep("^>", levels)]
    ranges <- levels[grep("^[0-9]+-[0-9]+$", levels)]
    
    # Sort numerically
    less_than <- less_than[order(as.numeric(gsub("<", "", less_than)))]
    greater_than <- greater_than[order(as.numeric(gsub(">", "", greater_than)))]
    ranges <- ranges[order(as.numeric(sub("-.*", "", ranges)))]
    
    sorted_levels <- c(less_than, ranges, greater_than)
    return(sorted_levels)
  }
  
  age_levels <- unique(df[[segmented_age_col]])
  sorted_levels <- sort_age_levels(age_levels)
  df[[segmented_age_col]] <- factor(df[[segmented_age_col]], 
                                              levels = sorted_levels)
  
  
  df %>%  
    filter(get(segmented_age_col) != "Unknown")%>%
    ggplot(aes(x = get(segmented_age_col),
               y = SNR,
               color = get(question_col))) +
    geom_boxplot(fill = "white") +
    stat_summary(fun = median, geom = "point", shape = 95, size = 4, 
                 color = "red") + 
    facet_grid2(Sex  ~ get(question_col), scales = "free_x", independent = "x") +
    labs(#title = paste("Distribution of SNR by", 
      #              question_col),
      x = "Age Break Points",
      y = "SNR",
      color = "COSI Needs") +  # Renaming the legend for color
    theme_bw() + 
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 12, 
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          strip.text = element_text(color = 'black', size = 7),
          strip.background = element_rect(fill = 'lightblue', color = 'black'),
          legend.position = "none") +
    guides(color = guide_legend(ncol = 4))
}


plot_distributions_SRT <- function(df, question_col, segmented_age_col) {
  
  # Custom sorting function
  sort_age_levels <- function(levels) {
    # Separate out "<", ">" and ranges
    less_than <- levels[grep("^<", levels)]
    greater_than <- levels[grep("^>", levels)]
    ranges <- levels[grep("^[0-9]+-[0-9]+$", levels)]
    
    # Sort numerically
    less_than <- less_than[order(as.numeric(gsub("<", "", less_than)))]
    greater_than <- greater_than[order(as.numeric(gsub(">", "", greater_than)))]
    ranges <- ranges[order(as.numeric(sub("-.*", "", ranges)))]
    
    sorted_levels <- c(less_than, ranges, greater_than)
    return(sorted_levels)
  }
  
  age_levels <- unique(df[[segmented_age_col]])
  sorted_levels <- sort_age_levels(age_levels)
  df[[segmented_age_col]] <- factor(df[[segmented_age_col]], 
                                    levels = sorted_levels)
  
  
  df %>%  
    filter(get(segmented_age_col) != "Unknown")%>%
    ggplot(aes(x = get(segmented_age_col),
               y = SRT,
               color = get(question_col))) +
    geom_boxplot(fill = "white") +
    stat_summary(fun = median, geom = "point", shape = 95, size = 4, 
                 color = "red") + 
    facet_grid2(Sex  ~ get(question_col), scales = "free_x", independent = "x") +
    labs(#title = paste("Distribution of SRT by", 
      #              question_col),
      x = "Age Break Points",
      y = "SRT",
      color = "COSI Needs") +  # Renaming the legend for color
    theme_bw() + 
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 12, 
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          strip.text = element_text(color = 'black', size = 7),
          strip.background = element_rect(fill = 'lightblue', color = 'black'),
          legend.position = "none") +
    guides(color = guide_legend(ncol = 4))
}


plot_distributions_small_multiples <- function(df, question_col, segmented_age_col) {
  
  # Custom sorting function for age segments
  sort_age_levels <- function(levels) {
    less_than <- levels[grep("^<", levels)]
    greater_than <- levels[grep("^>", levels)]
    ranges <- levels[grep("^[0-9]+-[0-9]+$", levels)]
    
    less_than <- less_than[order(as.numeric(gsub("<", "", less_than)))]
    greater_than <- greater_than[order(as.numeric(gsub(">", "", greater_than)))]
    ranges <- ranges[order(as.numeric(sub("-.*", "", ranges)))]
    
    sorted_levels <- c(less_than, ranges, greater_than)
    return(sorted_levels)
  }
  
  age_levels <- unique(df[[segmented_age_col]])
  sorted_levels <- sort_age_levels(age_levels)
  df[[segmented_age_col]] <- factor(df[[segmented_age_col]], levels = sorted_levels)
  
  # Remove NA values
  df_clean <- df %>%
    drop_na(SNR, SRT, !!sym(segmented_age_col), HL, !!sym(question_col))
  
  df_long <- df_clean %>%
    pivot_longer(cols = c(SNR, SRT), names_to = "Metric", values_to = "Value")
  
  ggplot() +
    # Plot for SNR
    geom_boxplot(data = df_long %>% filter(Metric == "SNR"),
                 aes(x = .data[[segmented_age_col]], y = Value, color = HL),
                 fill = "white", alpha = 0.5) +
    scale_color_viridis_d(name = "Hearing Loss (SNR)") +
    new_scale_color() +
    # Plot for SRT
    geom_boxplot(data = df_long %>% filter(Metric == "SRT"),
                 aes(x = .data[[segmented_age_col]], y = Value, color = HL),
                 fill = "white", alpha = 0.5) +
    scale_color_viridis_d(name = "Hearing Loss (SRT)", option = "plasma") +
    facet_grid2(Metric + Sex ~ .data[[question_col]], scales = "free",
                independent = "all") +
    labs(x = "Age Break Points",
         y = "Value") +
    theme_bw() +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 7),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          strip.text = element_text(color = 'black', size = 11),
          strip.background = element_rect(fill = 'lightblue', color = 'black'),
          legend.position = "bottom") +
    guides(color = guide_legend(ncol = 4))
}





plot_distributions_heatmap <- function(df, question_col, segmented_age_col) {
  
  # Custom sorting function for age segments
  sort_age_levels <- function(levels) {
    less_than <- levels[grep("^<", levels)]
    greater_than <- levels[grep("^>", levels)]
    ranges <- levels[grep("^[0-9]+-[0-9]+$", levels)]
    
    less_than <- less_than[order(as.numeric(gsub("<", "", less_than)))]
    greater_than <- greater_than[order(as.numeric(gsub(">", "", greater_than)))]
    ranges <- ranges[order(as.numeric(sub("-.*", "", ranges)))]
    
    sorted_levels <- c(less_than, ranges, greater_than)
    return(sorted_levels)
  }
  
  age_levels <- unique(df[[segmented_age_col]])
  sorted_levels <- sort_age_levels(age_levels)
  df[[segmented_age_col]] <- factor(df[[segmented_age_col]], levels = sorted_levels)
  
  # Remove NA values
  df_clean <- df %>%
    drop_na(SNR, SRT, !!sym(segmented_age_col), HL, !!sym(question_col))  # Adjust columns as needed
  
  
  # Aggregate data to calculate mean values for SNR and SRT
  df_aggregated <- df_clean %>%
    pivot_longer(cols = c(SNR, SRT), names_to = "Metric", values_to = "Value") %>%
    group_by(across(all_of(c(question_col, segmented_age_col, "Sex", "HL", "Metric")))) %>%
    summarise(mean_value = mean(Value, na.rm = TRUE), .groups = 'drop')
  
  ggplot() +
    # SNR Layer
    geom_tile(data = df_aggregated %>% filter(Metric == "SNR"), 
              aes(x = .data[[segmented_age_col]], y = HL, fill = mean_value)) +
    scale_fill_viridis_c(name = "SNR Mean Value") +
    new_scale_fill() +
    # SRT Layer
    geom_tile(data = df_aggregated %>% filter(Metric == "SRT"), 
              aes(x = .data[[segmented_age_col]], y = HL, fill = mean_value)) +
    scale_fill_viridis_c(name = "SRT Mean Value", option = "plasma") +
    facet_grid2(Metric + Sex ~ .data[[question_col]], 
                scales = "free_x", independent = "x",
                labeller = label_wrap_gen(width=10)) +
    labs(x = "Age Break Points",
         y = "Hearing Loss") +
    theme_light() +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          strip.text = element_text(color = 'black', size = 15),
          strip.background = element_rect(fill = 'lightblue', 
                                          color = 'black'),
          legend.position = "bottom")
}
