## font_import()
## loadfonts(device = "win")

# Standard Normal Table Functions

normal_table <- data.frame(x = seq(-4, 4, 0.05), y = dnorm(seq(-4, 4, 0.05)))


#' @export
test_stat <- function(x, mu = 0, sd = 1, n = 1) {
  stat <- (x - mu) / (sd / sqrt(n))
  return(stat)
}

#' Convert area to corresponding Z-score
#'
#' This function returns the Z-score associated with probability. You can have 
#' it as lower or upper tail. You can also change the distribution. It returns 
#' a beautiful plot as well
#'
#' @param area The probability
#' @param mu mean of the distribution
#' @param sd standard dev of the distribution
#' @param lower.tail whether to look at the positive or negative side
#' @return The corresponding Z-score and a plot
#' @export
prob_to_z_score <- function(area, mu = 0, sd = 1, lower.tail = T, plot = T) {
  x <- round(qnorm(area, mean = mu, sd = sd, lower.tail = lower.tail), 3)
  z <- round(test_stat(x, mu, sd), 3)
  
  if (lower.tail) {
    shaded_area = normal_table[normal_table$x <= z, ]
  } else if (lower.tail == F) {
    shaded_area = normal_table[normal_table$x >= z, ]
  }
  
  if (plot) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() + 
      geom_ribbon(data = shaded_area, aes(x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 0.75) +
      annotate("segment", x = z, xend = 2.4, y = 0, yend = 0.1, size = 1.3,
               arrow = arrow(type = "closed", length = unit(0.25, "inches")), color = "darkorchid") +
      annotate("text", x = 2.4, y = 0.1, label = paste("Z-score:", z), vjust = -1, color = "darkgreen", size = 5) + 
      annotate("text", x = -2.5, y = 0.2, label = paste("Area:", area), vjust = -1, color = "darkgreen", size = 5) +
      labs(y = "f(x)") +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5))
    print(plot)
  }
  return(z)
}


#' @export
find_prob <- function(x = inf, mu = 0, sd = 1, n = 1, lower.bound = F,  lower.tail = T, plot = T) {
  z <- round(test_stat(x, mu, sd, n), 3)
  
  if (lower.bound) {
    lower.bound <- round(test_stat(lower.bound, mu, sd, n), 3)
    area <- round(pnorm(q = z) - pnorm(q = lower.bound), 5)
  } else if (lower.tail == F) {
    lower.bound <- z
    z <- Inf
    area <- round(1 - pnorm(q = lower.bound), 5)
  } else {
    lower.bound = -Inf
    area <- round(pnorm(q = z), 5)
  }
  
  if (plot) {
    shaded_area = normal_table[normal_table$x <= z & normal_table$x >= lower.bound, ]
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() + 
      geom_ribbon(data = shaded_area, aes(x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      
      labs(title = "Shaded Area Probability", x = "Z-score", y = "Density") +
      
      annotate("text", x = 3, y = 0.25, label = area, hjust = 0.5, vjust = -0.5, color = "skyblue", size = 7) +
      
      annotate("segment", x = lower.bound, xend = -3, y = 0, yend = 0.1, linewidth = 1.3,
               arrow = arrow(type = "closed", length = unit(0.25, "inches")), color = "darkorchid") + 
      
      annotate("segment", x = z, xend = 3, y = 0, yend = 0.1, linewidth = 1.3,
               arrow = arrow(type = "closed", length = unit(0.25, "inches")), color = "darkorchid") +
      
      annotate("text", x = -3, y = 0.1, label = lower.bound, vjust = -1, color = "darkgreen", size = 5) +
      
      annotate("text", x = 3, y = 0.1, label = z, vjust = -1, color = "darkgreen", size = 5) +
      
      theme(text = element_text(size = 14))
    
    print(plot)
  }
  return(area)
}



# Hypothesis Testing Functions
#' Perform a Z-test for a given sample mean
#'
#' This function conducts a Z-test based on a sample mean. You can specify the population mean,
#' standard deviation, sample size, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param x The sample mean.
#' @param mu The population mean (default is 0).
#' @param sd The standard deviation of the population (default is 1).
#' @param n Sample size (default is 1).
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
z.test <- function(x, mu = 0, sd = 1, n = 1, alternative = c("two-sided", "less", "greater"),
                   alpha = 0.05, confint = F) {
  z <- round(test_stat(x, mu, sd, n), 3)
  
  
  if (alternative == "two-sided") {
    crit <- round(prob_to_z_score(1 - (alpha / 2), plot = F), 2)
    upper_crit <- crit; lower_crit <- -crit
    
    prob <- 2 * round(1 - pnorm(abs(z)), 5)
    reject <- 1 - pnorm(abs(z)) < alpha / 2
    shaded_area <- normal_table[normal_table$x > upper_crit | normal_table$x < lower_crit, ]
    
  } else if (alternative == "less") {
    crit <- round(prob_to_z_score(1 - alpha, plot = F), 2)
    lower_crit <- -Inf
    upper_crit <- crit
    prob <- round(1 - pnorm(abs(z)), 5)
    
    reject <- pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x < -upper_crit, ]
    
  } else if (alternative == "greater") {
    crit <- round(prob_to_z_score(1 - alpha, plot = F), 2)
    lower_crit <- -crit
    upper_crit <- Inf
    prob <- round(1 - pnorm(z), 5)
    
    reject <- 1 - pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x > -lower_crit, ]
    
  } else {
    stop(paste("Did not recognize alternative", alternative))
  }
  if (confint) {
    confint = round(x + c(lower_crit, upper_crit) * (sd / sqrt(n)), 3)
  }
  
  if (reject) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") + 
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = z, y = 0.01), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop mean:", mu, "Samp mean:", x, "\nAlpha:", alpha, "\nCritical Value: b", crit,
              "\nTest Statistic:", z, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", prob, 5),
              "\nWe have sufficient evidence to reject Null Hyp."))
  } 
  else if (reject == F) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") +
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting") +
      geom_point(aes(x = z, y = 0.01), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop mean:", mu, "Samp mean:", x, "\nAlpha:", alpha, "\nCritical Value: b", crit,
              "\nTest Statistic:", z, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", prob, 5),
              "\nWe don't have sufficient evidence to reject Null Hyp."))
  }
  
}

# Hypothesis Testing Functions
#' Perform a T-test for a given sample mean
#'
#' This function conducts a T-test based on a sample mean. You can specify the population mean,
#' standard deviation, sample size, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param x The sample mean.
#' @param mu The population mean (default is 0).
#' @param sd The standard deviation of the population (default is 1).
#' @param n Sample size (default is 1).
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
t.test <- function(x, mu = 0, sd = 1, n = 2, alternative = c("two-sided", "less", "greater"),
                   alpha = 0.05, confint = F) {
  t <- round(test_stat(x, mu, sd, n), 3)
  se <- sd / sqrt(n)
  
  t_table <- data.frame(x = seq(-4, 4, 0.05), y = dt(seq(-4, 4, 0.05), df = n-1))
  
  if (alternative == "two-sided") {
    crit <- round(qt(1 - (alpha / 2), df = n-1), 2)
    upper_crit <- crit; lower_crit <- -crit
    prob <- 2 * (1 - round(pt(abs(t), df = n-1), 5))
    
    reject <- 1 - pt(abs(t), df = n-1) < alpha / 2
    shaded_area <- t_table[t_table$x > upper_crit | t_table$x < lower_crit, ]
    
    confint <- round(c(x + lower_crit * se, x + upper_crit * se), 3)
  } else if (alternative == "less") {
    crit <- round(qt(alpha, df = n-1), 2)
    lower_crit <- -Inf
    upper_crit <- crit
    prob <- round(pt(t, df = n-1), 5)
    
    reject <- prob < alpha
    shaded_area <- t_table[t_table$x < upper_crit, ]
    
    confint <- round(c(-Inf, x - upper_crit * se), 3)
  } else if (alternative == "greater") {
    crit <- round(qt(1 - alpha, df = n-1), 2)
    lower_crit <- -crit
    upper_crit <- Inf
    prob <- 1 - round(pt(t, df = n-1), 5)
    
    reject <- prob < alpha
    shaded_area <- t_table[t_table$x > lower_crit, ]
    
    confint <- round(c(x + lower_crit * se, Inf), 3)
  } else {
    stop(paste("Did not recognize alternative", alternative))
  }
  
  if (reject) {
    plot <- ggplot(t_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "darkred", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > crit, ], aes(x = x, ymin = 0, ymax = y), fill = "darkred", alpha = 0.7) +
      annotate("text", label = t, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = t - (t/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") + 
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = t, y = 0.01), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop mean:", mu, "Samp mean:", x, "\nDF:", n-1, "\nAlpha:", alpha, "\nCritical Value: b", crit,
              "\nTest Statistic:", t, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", prob,
              "\nWe have sufficient evidence to reject Null Hyp."))
  } 
  else if (reject == F) {
    plot <- ggplot(t_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < lower_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > upper_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = t, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = t - (t/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") + 
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting") +
      geom_point(aes(x = t, y = 0.01), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop mean:", mu, "Samp mean:", x, "\nDF:", n-1, "\nAlpha:", alpha, "\nCritical Value: b", crit,
              "\nTest Statistic:", t, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", prob,
              "\nWe Don't have sufficient evidence to reject Null Hyp."))
  }
}

# Hypothesis Testing Functions
#' Perform a Z-test for a given proportion
#'
#' This function conducts a Z-test based on a sample prop. You can specify the population mean,
#' standard deviation, sample size, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param x The number of favorable observations.
#' @param n Sample size.
#' @param pi population proportion to be tested
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
prop.test <- function(x, n, pi, alternative = c("two-sided", "greater", "less"),
                      confint = F, alpha = 0.05) {
  pi_hat <- x / n
  sd <- sqrt(pi * (1 - pi) / n)
  pi_hat_sd <- sqrt(pi_hat * (1 - pi_hat) / n)
  test_stat <- round((pi_hat - pi) / sd, 3)
  
  if (alternative == "two-sided") {
    reject <- 1 - pnorm(abs(test_stat)) < alpha / 2
    
    lower_crit <- round(prob_to_z_score(alpha / 2, plot = F), 2)
    upper_crit <- round(prob_to_z_score(1 - (alpha / 2), plot = F), 2)
  } else if (alternative == "less") {
    reject <- pnorm(test_stat) < alpha
    
    lower_crit <- round(prob_to_z_score(alpha, plot = F), 2)
    upper_crit <- Inf
  } else if (alternative == "greater") {
    reject <- 1 - pnorm(test_stat) < alpha
    
    lower_crit <- -Inf
    upper_crit <- round(prob_to_z_score(1 - (alpha), plot = F), 2)
  } else {
    stop(paste("Did not recognize alternative", alternative))
  }
  
  if (confint) {
    confint = round(c(pi_hat + lower_crit * pi_hat_sd, pi_hat + upper_crit * pi_hat_sd), 3)
  }
  
  if (reject) {  
    cat(paste("Hyp Test:", alternative, "\nPop mean:", pi, "Samp mean:", pi_hat, "\nAlpha:", alpha, "\nCritical Value: b", upper_crit,
              "\nTest Statistic:", test_stat, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(test_stat)), 5),
              "\nWe have sufficient evidence to reject Null Hyp."))
  } else if (reject == F) {
    cat(paste("Hyp Test:", alternative, "\nPop mean:", pi, "Samp mean:", pi_hat, "\nAlpha:", alpha, "\nCritical Value: b", upper_crit,
              "\nTest Statistic:", test_stat, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(test_stat)), 5),
              "\nWe Don't have sufficient evidence to reject Null Hyp."))
  }
}

# Hypothesis Testing Functions
#' Perform a Chi-square test for a given sample variance
#'
#' This function conducts a Chi-square test based on a sample variance You can specify the
#' population variance, sample size, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param var The population variance
#' @param sigma2 The sample variance
#' @param n Sample size (default is 1).
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
chi_square.test <- function(var, sigma2, n, alternative = c("two-sided", "greater", "less"),
                            confint = F, alpha = 0.05) {
  # I need to specify x values based on n
  x_values <- seq(round((n-1)^2 / 100, 0), round(5 + ((n-1) * 20)^(2/3), 0) , 0.1)
  x_range <- max(x_values) - min(x_values)
  chi_table <- data.frame(x = x_values, y = dchisq(x_values, df = n-1))
  chi_test <- round((n-1) * var / sigma2, 3)
  
  if (alternative == "two-sided") { 
    lower_chi_crit <- round(qchisq(alpha / 2, n-1), 3) 
    upper_chi_crit <- round(qchisq(1 - alpha / 2, n-1), 3)
    confint <- round((n-1) * var / c(upper_chi_crit, lower_chi_crit), 3)
    
    reject <- chi_test < lower_chi_crit | chi_test > upper_chi_crit
    shaded_area <- chi_table[chi_table$x < lower_chi_crit | chi_table$x > upper_chi_crit, ]
  }
  else if (alternative == "greater") {
    lower_chi_crit <- qchisq(alpha, n-1) 
    upper_chi_crit <- qchisq(1 - alpha, n-1)
    confint <- round(c((n-1) * var / upper_chi_crit, Inf), 3)
    
    reject <- chi_test > upper_chi_crit
    shaded_area <- chi_table[chi_table$x > upper_chi_crit, ]
  }
  else if (alternative == "less") {
    lower_chi_crit <- round(qchisq(alpha, n-1), 3) 
    upper_chi_crit <- round(qchisq(1 - alpha, n-1), 3)
    confint <- round(c(-Inf, (n-1) * var / lower_chi_crit), 3)
    
    reject <- chi_test < lower_chi_crit
    shaded_area <- chi_table[chi_table$x < lower_chi_crit, ]
  } 
  else {
    stop("Did not recognize alternative hypothesis")
  }
  
  if (reject) {
    plot <- ggplot(chi_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < lower_chi_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > upper_chi_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = chi_test, x = x_range / 2, y = 0.01, size = 4, color = "darkgreen") +
      geom_curve(aes(x = x_range / 2, xend = chi_test - (chi_test/20), y = 0.015, yend = 0.005), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") + 
      annotate("text", label = paste("Alpha:", alpha), x = min(x_values) + 3 / 5 * x_range, y = 0.06, color = "darkgrey", size = 4, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = min(x_values) + 3 / 5 * x_range, y = 0.04, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = chi_test, y = 0.005), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop var:", sigma2, "Samp var:", var, "\nDF:", n-1, "\nAlpha:", alpha, "\nCritical Values: ",
              lower_chi_crit, upper_chi_crit, "\nTest Statistic:", chi_test, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")",
              "\nWe have sufficient evidence to reject Null Hyp."))
  } 
  else if (reject == F) {
    plot <- ggplot(chi_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < lower_chi_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "darkred", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > upper_chi_crit, ], aes(x = x, ymin = 0, ymax = y), fill = "darkred", alpha = 0.7) +
      annotate("text", label = chi_test, x = x_range / 2, y = 0.01, size = 4, color = "darkgreen") +
      geom_curve(aes(x = x_range / 2, xend = chi_test - (chi_test/20), y = 0.015, yend = 0.005), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") + 
      annotate("text", label = paste("Alpha:", alpha), x = min(x_values) + x_range / 5, y = 0.06, color = "darkgrey", size = 4, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = min(x_values) + 3 / 5 * x_range, y = 0.06, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = chi_test, y = 0.005), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nPop var:", sigma2, "Samp var:", var, "\nDF:", n-1, "\nAlpha:", alpha, "\nCritical Values: ",
              lower_chi_crit, upper_chi_crit, "\nTest Statistic:", chi_test, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")",
              "\nWe Don't have sufficient evidence to reject Null Hyp."))
  }
}

# Hypothesis Testing Functions
#' Perform a two-sample Z-test for given sample means
#'
#' This function conducts a Z-test based on sample means. You can specify the sample means,
#' standard deviations, sample sizes, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param x1 The mean of sample 1.
#' @param x2 The mean of sample 2.
#' @param s1 The standard deviation of the sample 1.
#' @param s2 The standard deviation of the sample 2.
#' @param n1 Size of sample 1.
#' @param n2 Size of sample 2.
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
two_sample.z_test <- function(x1, x2, s1, s2, n1, n2, alternative = c("two-sided", "greater", "less"),
                              confint = F, alpha = 0.05, equal_var = F) {
  if (equal_var == F) {
    stand_error <- sqrt(s1^2 / n1 + s2^2 / n2)
    z <- round((x1 - x2) / stand_error, 3)
  } 
  else if (equal_var) {
    pooled_var <- ((n-1) * s1^2 + (n2-1) * s2^2) / (n1 + n2 - 2)
    stand_error <- sqrt(pooled_var / n1 + pooled_var / n2)
    z <- round((x1 - x2) / stand_error, 3)
  }
  
  # z.test(z, alternative = alternative, alpha = alpha, confint = confint) # The confidence int is wrong 
  
  if (alternative == "two-sided") {
    crit <- round(prob_to_z_score(1 - (alpha / 2), plot = F), 2)
    upper_crit <- crit; lower_crit <- -crit
    
    reject <- 1 - pnorm(abs(z)) < alpha / 2
    shaded_area <- normal_table[normal_table$x > upper_crit | normal_table$x < lower_crit, ]
  }
  else if (alternative == "less") {
    crit <- round(prob_to_z_score(1 - alpha, plot = F), 2)
    lower_crit <- -Inf
    upper_crit <- crit
    
    reject <- pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x < -upper_crit, ]
  }
  else if (alternative == "greater") {
    crit <- round(prob_to_z_score(1 - alpha, plot = F), 2)
    lower_crit <- -crit
    upper_crit <- Inf
    
    reject <- 1 - pnorm(z) < alpha
    shaded_area <- normal_table[normal_table$x > -lower_crit, ]
  } 
  else {
    stop("Did not recognize alternative hypothesis")
  }
  
  if (confint) {
    confint = round((x1 - x2) + c(lower_crit, upper_crit) * stand_error, 2)
  }
  
  if (reject) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") +
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = z, y = 0.01), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "Samp means:", x1, x2, "\nAlpha:", alpha, "\nCritical Value: b", crit,
              "\nTest Statistic:", z, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(z)), 3),
              "\nWe have sufficient evidence to reject Null Hyp."))
  }
  else if (reject == F) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = z, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = z - (z/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") +
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting") +
      geom_point(aes(x = z, y = 0.01), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative,"Samp means:", x1, x2, "\nAlpha:", alpha, "\nCritical Value: b", crit,
              "\nTest Statistic:", z, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pnorm(abs(z)), 3),
              "\nWe don't have sufficient evidence to reject Null Hyp."))
  }
}

# Hypothesis Testing Functions
#' Perform a two-sample T-test for given sample means
#'
#' This function conducts a T-test based on sample means. You can specify the sample means,
#' standard deviations, sample sizes, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param x1 The mean of sample 1.
#' @param x2 The mean of sample 2.
#' @param s1 The standard deviation of the sample 1.
#' @param s2 The standard deviation of the sample 2.
#' @param n1 Size of sample 1.
#' @param n2 Size of sample 2.
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
two_sample.t_test <- function(x1, x2, s1, s2, n1, n2, alternative = c("two-sided", "greater", "less"),
                              confint = F, alpha = 0.05, equal_var = F) {
  if (equal_var == F) {
    stand_error <- sqrt(s1^2 / n1 + s2^2 / n2)
    t <- round((x1 - x2) / stand_error, 3)
    df <- round((s1^2 / n1 + s2^2 / n2) ^ 2 / ((s1^2 / n1)^2 / (n1 - 1) + (s2^2 / n2)^2 / (n2 - 1)), 1)
  } 
  else if (equal_var) {
    pooled_var <- ((n1-1) * s1^2 + (n2-1) * s2^2) / (n1 + n2 - 2)
    stand_error <- sqrt(pooled_var / n1 + pooled_var / n2)
    t <- round((x1 - x2) / stand_error, 3)
    df <- n1 + n2 - 2
  }
  
  
  if (alternative == "two-sided") {
    crit <- round(qt(1 - (alpha / 2), df), 2)
    upper_crit <- crit; lower_crit <- -crit
    
    reject <- 1 - pt(abs(t), df) < alpha / 2
    shaded_area <- normal_table[normal_table$x > upper_crit | normal_table$x < lower_crit, ]
  } 
  else if (alternative == "less") {
    crit <- round(qt(1 - alpha, df), 2)
    lower_crit <- -Inf
    upper_crit <- crit
    
    reject <- pt(t, df) < alpha
    shaded_area <- normal_table[normal_table$x < -upper_crit, ]
  } 
  else if (alternative == "greater") {
    crit <- round(qt(1 - alpha, df), 2)
    lower_crit <- -crit
    upper_crit <- Inf
    
    reject <- 1 - pt(t, df) < alpha
    shaded_area <- normal_table[normal_table$x > -lower_crit, ]
  }
  else {
    stop("Did not recognize alternative hypothesis")
  }
  
  if (confint) {
    confint = round((x1 - x2) + c(lower_crit, upper_crit) * stand_error, 2)
  }
  
  if (reject) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = t, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = t - (t/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") + 
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 5, family = "Lucida Handwriting") +
      geom_point(aes(x = t, y = 0.01), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative, "\nSamp means:", x1, x2, "\nDF:", df, "\nAlpha:", alpha, "\nCritical Value: b", crit,
              "\nTest Statistic:", t, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pt(abs(t), df), 3),
              "\nWe have sufficient evidence to reject Null Hyp."))
  }
  else if (reject == F) {
    plot <- ggplot(normal_table, aes(x, y)) + geom_line() +
      geom_ribbon(data = shaded_area[shaded_area$x < crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      geom_ribbon(data = shaded_area[shaded_area$x > -crit, ], aes(x = x, ymin = 0, ymax = y), fill = "red", alpha = 0.7) +
      annotate("text", label = t, x = 0, y = 0.1, size = 6, color = "darkgreen") +
      geom_curve(aes(x = 0, xend = t - (t/20), y = 0.08, yend = 0.01), linewidth = 1,
                 arrow = arrow(type = "open", length = unit(0.15, "inches")), color = "darkorchid") +
      annotate("text", label = paste("Alpha:", alpha), x = 2.25, y = 0.325, color = "darkgrey", size = 5, family = "Lucida Handwriting") +
      annotate("text", label = "CAN'T REJECT NULL", x = -2.25, y = 0.325, color = "blue", size = 4, family = "Lucida Handwriting") +
      geom_point(aes(x = t, y = 0.01), color = "darkolivegreen1", size = 2, alpha = 0.5)
    
    print(plot)
    cat(paste("Hyp Test:", alternative,"\nSamp means:", x1, x2, "\nDF:", df, "\nAlpha:", alpha, "\nCritical Value: b", crit,
              "\nTest Statistic:", t, "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")", "\nProb:", 1 - round(pt(abs(t), df), 3),
              "\nWe don't have sufficient evidence to reject Null Hyp."))
  }
}

# Hypothesis Testing Functions
#' Perform a two-sample Z-test for given sample proportions
#'
#' This function conducts a Z-test based on sample proportions. You can specify
#' the sample props, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param x1 The number of favorable observations in sample 1.
#' @param n1 Size of sample 1.
#' @param x2 The number of favorable observations in sample 2.
#' @param n2 Size of sample 2.
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
two_sample.prop_test <- function(x1, x2, n1, n2, alternative = c("two-sided", "greater", "less"),
                                 alpha = 0.05, confint = F) {
  p1_hat <- round(x1 / n1, 5)
  p2_hat <- round(x2 / n2, 5)
  
  p_hat <- round((x1 + x2) / (n1 + n2), 5)
  p_hat_sd <- sqrt(p_hat * (1-p_hat) * (1/n1 + 1/n2))
  
  test_stat <- round((p1_hat - p2_hat) / p_hat_sd, 3)
  
  cat(p1_hat, p2_hat, p_hat, test_stat)
  
  z.test(test_stat, alternative = alternative, alpha = alpha, confint = confint)
}

# Hypothesis Testing Functions
#' Perform a Z-test for a given sample mean
#'
#' This function conducts a Z-test based on a sample mean. You can specify the population mean,
#' standard deviation, sample size, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param diff_vec The difference vector.
#' @param vec1 First set of data.
#' @param vec2 Second set of data.
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
paired_diff.test <- function(diff_vec = F, vec1 = F, vec2 = F, alternative = c("two-sided", "greater", "less"),
                             confint = F, alpha = 0.05) {
  if (diff_vec == F) {
    diff_vec <- vec1-vec2
    diff_mean <- mean(diff_vec)
    diff_sd <- sd(diff_vec)
    n <- length(diff_vec)
  }
  
  if (n < 30) {
    t.test(diff_mean, 0, sd = diff_sd, n = n, alternative = alternative, alpha = alpha, confint = confint)
  } 
  else if (n >= 30) {
    z.test(diff_mean, 0, sd = diff_sd, n = n, alternative = alternative, alpha = alpha, confint = confint)
  }
}

# Hypothesis Testing Functions
#' Perform an F-test for two sample variances
#'
#' This function conducts an F-test based on sample variances. You can specify the sample variances,
#' sample sizes, alternative hypothesis, significance level, and whether to
#' calculate a confidence interval.
#'
#' @param v1 The variance of sample 1.
#' @param v2 The variance of sample 2.
#' @param n1 Size of sample 1.
#' @param n2 Size of sample 2.
#' @param alternative Type of alternative hypothesis ("two-sided," "less," or "greater").
#' @param alpha Significance level (default is 0.05).
#' @param confint Whether to calculate a confidence interval (default is FALSE).
#' @return The test result (reject or fail to reject null hypothesis) and an optional confidence interval.
#' @export
two_sample.f_test <- function(v1, v2, n1, n2, alternative = c("two-sided", "greater", "less"),
                              confint = F, alpha = 0.05) {
  test_stat <- round(v1 / v2, 4)
  if (alternative == "two-sided") {
    lower_crit <- round(qf(alpha / 2, n1-1, n2-1), 3)
    upper_crit <- round(qf(1 - alpha / 2, n1-1, n2-1), 3)
    
    reject <- test_stat < lower_crit | test_stat > upper_crit
    confint <- c(v1 / v2 * lower_crit, v1 / v2 * upper_crit)
  }
  else if (alternative == "greater") {
    lower_crit <- round(qf(alpha, n1-1, n2-1), 3)
    upper_crit <- round(qf(1 - alpha, n1-1, n2-1), 3)
    
    reject <- test_stat > upper_crit
    confint(v1 / v2 * lower_crit, Inf)
  }
  else if (alternative == "less") {
    lower_crit <- round(qf(alpha, n1-1, n2-1), 3)
    upper_crit <- round(qf(1 - alpha, n1-1, n2-1), 3)
    
    reject <- test_stat < lower_crit
    confint <- c(0, v1 / v2 * upper_crit)
  }
  else {
    stop("Did not recognize alternative")
  }
  
  if (reject) {
    cat(paste("Hyp Test:", alternative, "\nF-Test:", "\nSamp1 var:", v2, "DF1:", n1-1,
              "\nSamp2 var:", v2, "DF2:", n2-1, "\nAlpha:", alpha, "\nCritical Values: ",
              lower_crit, upper_crit, "\nTest Statistic:", test_stat,
              "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")",
              "\nWe have sufficient evidence to reject Null Hyp."))
  }
  else if (reject == F) {
    cat(paste("Hyp Test:", alternative, "\nF-Test:", "\nSamp1 var:", v1, "DF1:", n1-1,
              "\nSamp2 var:", v2, "DF2:", n2-1, "\nAlpha:", alpha, "\nCritical Values: ",
              lower_crit, upper_crit, "\nTest Statistic:", test_stat,
              "\nConfidence Int:", "(", paste0(confint, collapse = ","), ")",
              "\nWe do not have sufficient evidence to reject Null Hyp."))
  }
}

