library(shiny)
library(shinydashboard)
library(DT)
library(lattice)
library(retimes)

response.times <- read.csv("www/data/responsetimes.csv",
                           colClasses = c("character", "character", "character",
                                          "integer", "numeric", 
                                          "character", "character"))
response.times[,"RT"] <- response.times[,"RT"] * 1000

dtrtvalfun <- function(x){
  if (!is.null(x)) {
    data.col <- x + 1
    var.name <- names(response.times)[data.col]
    var.class <- class(response.times[[data.col]])
    cur.var <- response.times[,var.name]
    
    if (var.name == "RT") {
      temp.var <- rep("NA", times = length(cur.var))
      for (i in 1:length(cur.var)) {
        if (!is.na(cur.var[i])) {
          if (cur.var[i] < 0) {
            temp.var[i] <- "< 0" 
          } else if (cur.var[i] < 100) {
            temp.var[i] <- "< 100"
          } else if (cur.var[i] < 200) {
            temp.var[i] <- "< 200"
          } else if (cur.var[i] < 300) {
            temp.var[i] <- "< 300"
          } else if (cur.var[i] < 400) {
            temp.var[i] <- "< 400"
          } else if (cur.var[i] >= 400) {
            temp.var[i] <- "> 400"
          }
        }
      }
      cur.var <- temp.var
    }
    
    unique.values <- c(na.omit(unique(cur.var)))
    table.output <- matrix(0, nrow = length(unique.values), ncol = 3)
    colnames(table.output) <- c("Values", "Count", "%")
    table.output <- as.data.frame(table.output)
    table.output[,1] <- unique.values
    for (i in 1:length(unique.values)) {
      table.output[i,2] <- length(which(cur.var == unique.values[i]))
    }
    table.output[,3] <- round(table.output[,2]*100/nrow(response.times), digits = 2)
    table.output <- table.output[order(table.output$Values),]
    return(table.output)
  }
}


# Exponentially modified Gaussian CDF
expgauss.cdf <- function(x, mu, sigma, tau) {
  part1 <- -exp(-x/tau + mu/tau + (sigma ^ 2)/(2*(tau ^ 2)))
  part2 <- pnorm((x - mu - (sigma ^ 2/tau))/sigma)
  part3 <- pnorm((x - mu)/sigma)
  f <- part1 * part2 + part3
  return(unname(f))
}

# Parameters
expgauss.fit <- function(x) {
  fit <- timefit(x = subset(x, x >= 0))
  mu <- fit@par[1]
  sig <- fit@par[2]
  tau <- fit@par[3]
  
  output <- list(mu = unname(mu), sig = unname(sig), tau = unname(tau))
  return(output)
}

# Probability of Superiority
expgauss.ps <- function(low.fit, upp.fit) {
  
  # Calculate the median of the lower population - optimisation routine
  cur.cdf.val <- -1
  low.med <- low.fit$mu + low.fit$tau
  low.sd <- sqrt(low.fit$tau^2 + low.fit$sig^2)
  while (abs(cur.cdf.val) > 1e-6) {
    cur.cdf.val <- -0.5 + expgauss.cdf(x = low.med, mu = low.fit$mu,
                                       sigma = low.fit$sig,
                                       tau = low.fit$tau)
    if (cur.cdf.val < 0) {
      low.med <- low.med + low.sd * cur.cdf.val
    } else {
      low.med <- low.med - low.sd * cur.cdf.val
    }
  } 
  
  # Calculate the probability of the upper population above median of lower
  prob.super <- 1 - expgauss.cdf(x = low.med, mu = upp.fit$mu, 
                                 sigma = upp.fit$sig, tau = upp.fit$tau)
  
  output <- structure(list(ps = prob.super, low.med = low.med))
  return(output)
}

