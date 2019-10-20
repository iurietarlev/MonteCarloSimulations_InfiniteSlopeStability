# --- RENEW RStudio MEMORY --- #
#rm(list=ls())

# --- SET CURRENT WORKING DIRECTORY --- #
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()

# --- LOADING RELEVANT LIBRARIES --- #
library(ggplot2)      #ggplot
library(evd)          #for extreme value distributions (gumbel)
library(ggpubr)       #used for ggarrange
library(pracma)       #practical math for deg2rad
library(expss)        #used for count_if

# --- LOCATION AND SHAPE FUNCITONS FOR LOGNORMAL DIST --- #
location <- function(m, sd){
  loc <- log(m^2 / sqrt(sd^2 + m^2))
}

shape <- function(m, sd){
  shp <- sqrt(log(1 + (sd^2 / m^2)))
}


# --- FIND MAX AND MIN FOR UNIFORM DIST --- #
min_max <- function(e_x, sd){
  a <- rbind(c(1, 1), c(1, -1))
  b <- c(2*e_x, sqrt(4*sd^2)) 
  min_max = solve(a, b)
  min <- min(min_max)
  max <- max(min_max)
  newList <- list("min" = min, "max" = max)
  return(newList)
}


# --- SCALING THE AXIS TO BE ALL OF THE SAME UNITS --- #
scaleFUN <- function(x) sprintf("%.1f", x)


# --- DISPLAYS 4 PROBABILITY PAPER PLOTS --- #
goodness_of_fit <- function(original_df, line_col){
  themes <- theme(axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        axis.text = element_text(size = 12), 
        plot.title = element_text(size=18))
  
  
  
  x_label <- expression(phi^-1*(p[r]))
  x_label_unif <- expression(p[r])
  y_label <- "numerical value"
  # ---  variable  distribution  ----
  # beta was the first variable tested, but this function works for any variable (regardless the name)
  beta_mean <- mean(original_df)
  beta_sd <- sd(original_df)
  
  original_df <- sort(original_df, decreasing = TRUE)
  
  result <- data.frame(matrix(nrow = length(original_df), ncol = 3))
  colnames(result) <- c("observation", "p_r", "phi-1(pr)")
  
  result[1] <- original_df
  
  for (i in 1:length(original_df)){
    p_r <- 1-i/(length(original_df)+1)
    inv_norm <- qnorm(p_r, 0, 1)
    result[i, 2] <- p_r
    result[i, 3] <- inv_norm
  }
  
  #----------- log normal -------------
  #calculate location and shape parameters
  a <- location(beta_mean, beta_sd) 
  b <- shape(beta_mean, beta_sd) 
  
  lnorm_func <- function(x) exp(a + b*x)
  
  log_norm_plot <- ggplot(result, aes(x = result$`phi-1(pr)`, y = result$observation)) + 
    geom_point() + stat_function(fun = lnorm_func, col = line_col) +
    coord_trans(y = "log10") + ggtitle("Lognormal") +
    xlab(label = x_label) + ylab(label = y_label) + themes +
    scale_y_continuous(labels=scaleFUN) +
    scale_x_continuous(labels=scaleFUN)

  #-----------normal-------------
  norm_func <- function(x) beta_mean + beta_sd*x

  norm_plot <- ggplot(result, aes(x = result$`phi-1(pr)`, y = result$observation)) +
    geom_point() + stat_function(fun = norm_func, col = line_col) +
    ggtitle("Normal") +
    xlab(label = x_label) + ylab(label = y_label) +themes +
    scale_y_continuous(labels=scaleFUN) +
    scale_x_continuous(labels=scaleFUN)

  #---------gumbel---------------
  alpha <- sqrt(6)*beta_sd/pi
  nu <- beta_mean - 0.5772*alpha
  gumb_func <- function(x) nu + alpha*x

  gumb_res <- result
  for (i in 1:length(gumb_res$observation)){
    x <- (i-0.44)/(length(gumb_res$observation)+0.12)
    gumb_res[i,2] <- x
    gumb_res[i,3] <- -log(-log(1-x))
  }

  gumb_plot <- ggplot(gumb_res, aes(x = gumb_res$`phi-1(pr)`, y = gumb_res$observation)) +
    geom_point() + stat_function(fun = gumb_func, col = line_col) +
    ggtitle("Gumbel") +
    xlab(label = x_label) + ylab(label = y_label) +themes +
    scale_y_continuous(labels=scaleFUN) +
    scale_x_continuous(labels=scaleFUN)

  #---------uniform---------------

  unif_res <- result
  unif_res <- unif_res[seq(dim(unif_res)[1],1),]
  
  for (i in 1:length(gumb_res$observation)){
    x <- (i)/(length(gumb_res$observation)+1)
    unif_res[i,2] <- x
    unif_res[i,3] <- 0
  }
  
  
  mini_maxi <- min_max(beta_mean, beta_sd)
  a_unif <-  min(unlist(mini_maxi))
  b_unif <- max(unlist(mini_maxi))
  
  unif_func <- function(x) (b_unif - a_unif)*x + a_unif
  uniform_plot <- ggplot(unif_res, aes(x = unif_res$p_r, y = unif_res$observation))+
  geom_point() + stat_function(fun = unif_func, col = line_col) +
  ggtitle("Uniform") +
  xlab(label = x_label_unif) + ylab(label = y_label) + themes +
    scale_y_continuous(labels=scaleFUN) +
    scale_x_continuous(labels=scaleFUN)
  
  four_plots <- ggarrange(norm_plot, log_norm_plot, gumb_plot, uniform_plot, nrow=2, ncol = 2, align = "hv", legend = "top")

  newList <- list("n_mean" = beta_mean, "n_sd" = beta_sd,
                  "ln_location" = a , "ln_shape" =  b,
                  "gum_location" = nu , "gum_scale" = alpha,
                  "unif_min"= a_unif, "unif_max" =b_unif, 
                  "four_plots" = four_plots)
  return(newList)
  
}


# --- FUNCTION FOR INPUT TYPE DISPLAY DEPENDING ON THE SELECTED RADIO BUTTON --- #
# it shows csv input when csv button is selected and mean and sd input when other type of data is selected
# and it shows a single value constant input when constant vaue input radio button is selected
input_type_selector <- function(input_choice, csv, m_sd, csv_tag, 
                                csv_dist_tag, m_tag, sd_tag, m_val, sd_val, m_sd_dist_tag, const_tag, const_value, 
                                dist_choices, selected_m_sd_distr, selected_csv_distr){
  if (input_choice == csv) {
    list(
      fileInput(inputId = csv_tag, label = "Upload parameter data", accept = ".csv"),
      selectInput(inputId = csv_dist_tag, label = "Select Distribution Type",
                  choices = dist_choices, selected = selected_csv_distr)
    )
  }
  
  else if (input_choice == m_sd) {
    list(
      numericInput(inputId = m_tag, label = "Mean", value = m_val, min = 0.05, step = 0.05),
      numericInput(inputId = sd_tag, label = "Standard Deviation", value = sd_val, min = 0.05, step = 0.05),
      selectInput(inputId = m_sd_dist_tag, label = "Select Distribution Type",
                  choices = dist_choices, selected = selected_m_sd_distr)
    )}
  else{
    numericInput(inputId = const_tag, label = "Select a constant value", value = const_value, min = 0.05, step = 0.05)
  }
}

# --- FUNCTION FOR INPUT TYPE DISPLAY FOR GROUND WATER HEIGHT DEPDENDING ON SELECTED METHOD BY RADIO BUTTONS --- #
input_type_selector_gwh <- function(input_choice, csv, m_sd, csv_tag, 
                                csv_dist_tag, m_tag, sd_tag, m_val, sd_val, m_sd_dist_tag, const_tag, const_value, 
                                dist_choices, selected_m_sd_distr, selected_csv_distr){
  if (input_choice == csv) {
    list(
      fileInput(inputId = csv_tag, label = "Upload parameter data", accept = ".csv"),
      selectInput(inputId = csv_dist_tag, label = "Select Distribution Type",
                  choices = dist_choices, selected = selected_csv_distr)
    )
  }
  
  else if (input_choice == m_sd) {
    list(
      sliderInput(inputId = m_tag, label = "Mean", value = m_val, min = 0, max = 1, step = 0.05),
      numericInput(inputId = sd_tag, label = "Standard Deviation", min = 0.05, value = sd_val, step = 0.05),
      selectInput(inputId = m_sd_dist_tag, label = "Select Distribution Type",
                  choices = dist_choices, selected = selected_m_sd_distr)
    )}
  else{
    numericInput(inputId = const_tag, label = "Select a constant value", min = 0.05, value = const_value, step = 0.05)
  }
}


# --- FUNCTION FOR INPUT TYPE DISPLAY FOR ITERATIONS NUMBER DEPDENDING ON SELECTED METHOD BY RADIO BUTTONS --- #
input_type_selector_iter <- function(input_choice, slid, num, value){
  if (input_choice == slid) {
      sliderInput(inputId = value, label = "", value = 10000, min = 0, max = 100000, step = 5000)
  }
  else{
    numericInput(inputId = value, label = "", min = 0, value = 10000, step = 5)
  }
}

# --- RETURN RANDOM VARIABLE FROM CHOSEN DISTRIBUTION --- #
dist_choice_final <- function(n, df, dist_type_csv, m, sd, dist_type_m_sd, constant_value, input_choice, csv, m_sd, constant){
  
  # actions to be taken based on csv input choice 
  if (input_choice == csv){
    infile <- df$datapath
    
    # if csv input type selected, load up the sample.csv file (for display purpose only)
    # another csv can be loaded and this one will be overwritten
    if (is.null(infile)) {
      dpath <- "./sample.csv"
    } else {
      dpath <- infile
    }
    
    df <- read.csv(dpath, header = F)$V1
    
    req(df, dist_type_csv)
    
    df_mean <- mean(df)
    df_sd <- sd(df)
    dist_type <- dist_type_csv
    
    #parameters for log normal
    a <- location(df_mean, df_sd) 
    b <- shape(df_mean, df_sd) 
    
    #parameters for uniform distribution
    mini_maxi <- min_max(df_mean, df_sd)
    a_unif <-  min(unlist(mini_maxi))
    b_unif <- max(unlist(mini_maxi))
    
    #parameters for gumbel distribution
    alpha <- sqrt(6)*df_sd/pi
    mu <- df_mean - 0.5772*alpha
    
    if (dist_type == "Normal"){
      rand_var <- rnorm(n, mean = df_mean, sd = df_sd)
      return(rand_var)
    }
    else if (dist_type == "Lognormal"){
      rand_var <- rlnorm(n, meanlog = a, sdlog = b)
      return(rand_var)
    }
    else if (dist_type == "Uniform"){
      rand_var <- runif(n, min = a_unif, max = b_unif)
      return(rand_var)
      
    }
    else if (dist_type == "Gumbel"){
      rand_var <- rgumbel(n, loc = mu, scale = alpha)
      return(rand_var)
    }
    
  }
  # actions to be taken based on the mean and sd input choice
  else if (input_choice == m_sd){
    req(m, sd, dist_type_m_sd)
    df_mean <- m
    df_sd <- sd
    dist_type <- dist_type_m_sd
    
    #parameters for log normal
    a <- location(df_mean, df_sd) 
    b <- shape(df_mean, df_sd) 
    
    #parameters for uniform distribution
    mini_maxi <- min_max(df_mean, df_sd)
    a_unif <-  min(unlist(mini_maxi))
    b_unif <- max(unlist(mini_maxi))
    
    #parameters for gumbel distribution
    alpha <- sqrt(6)*df_sd/pi
    mu <- df_mean - 0.5772*alpha
    
    if (dist_type == "Normal"){
      rand_var <- rnorm(n, mean = df_mean, sd = df_sd)
      return(rand_var)
    }
    else if (dist_type == "Lognormal"){
      rand_var <- rlnorm(n, meanlog = a, sdlog = b)
      return(rand_var)
    }
    else if (dist_type == "Uniform"){
      rand_var <- runif(n, min = a_unif, max = b_unif)
      return(rand_var)
      
    }
    else if (dist_type == "Gumbel"){
      rand_var <- rgumbel(n, loc = mu, scale = alpha)
      return(rand_var)
    }
    
    
  }
  # actions to take if input choice is a constant
  else if (input_choice == constant){
    req(constant_value)
    return(constant_value)
  }
}

# --- FUNCTION FOR PLOTTING HISTOGRAM DISTRIBUTIONS FOR VARIABLES --- #
hist_plot <- function(param_df, input_choice, dist_type_csv, dist_type_m_sd, csv, m_sd, constant, x_axis){
  req(param_df)
  param <- data.frame(x=param_df)
  
  if (input_choice == csv){
    req(dist_type_csv)
    dist_type = dist_type_csv
    
    p <- ggplot(param, aes(x = x)) +
      geom_histogram(color = "black", bins = 50) + xlab(paste(x_axis, " - ", dist_type)) + ylab("Count")
    
    
    return(p)
  }
  
  else if (input_choice == m_sd){
    req(dist_type_m_sd)
    dist_type = dist_type_m_sd
    
    p <- ggplot(param, aes(x = x)) +
      geom_histogram(color = "black", bins = 50) + xlab(paste(x_axis, " - ", dist_type)) + ylab("Count")#+ coord_fixed(ratio = 2)
    
    
    return(p)
  }
}
    

    
  
  
  





