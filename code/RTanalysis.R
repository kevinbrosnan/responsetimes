#------------------------------------------------------------------------------#
#               Effects of false-start disqualification rules on               #
#                  response-times of elite standard sprinters                  #
#                                                                              #
# Authors:  Mr. Kevin Brosnan, Maths & Stats, University of Limerick           #
#           Dr. Kevin Hayes, Maths & Stats, University of Limerick             #
#           Prof. Drew Harrison, Sports Science, University of Limerick        #
#                                                                              #
# The Journal of Sports Sciences:                                              #
#           Submitted: 14/04/2016                                              #
#           Accepted:  08/06/2016                                              #
#           Published: 28/06/2016                                              #
#                                                                              #
#------------------------------------------------------------------------------#

#### Data and Libraries ----------------------------------------------------####

  install.packages(c("lattice", "retimes", "pracma"))  
  library(lattice)
  library(retimes)
  library(grid)
  library(pracma)
  
  # Load data from Github Repository and define type of data
  data.file <- "https://goo.gl/xYuQXH"
  sprint.starts <- read.csv(data.file, header = TRUE, 
                            colClasses = c("character", "character", 
                                           "character", "integer", "character", 
                                           "integer", "integer", "numeric",
                                           "character", "numeric", "numeric", 
                                           "character", "character"))

  # Put times in milliseconds rather than seconds
  sprint.starts$FT <- sprint.starts$FT * 1000     
  sprint.starts$RT <- sprint.starts$RT * 1000 
  
  # Subset data to have mens and womens for each ruling period
  mens <- subset(x = sprint.starts, Sex == "Mens")
  womens <- subset(x = sprint.starts, Sex == "Womens")
  
  # Individual Warning Groups 1999-2003
  ind.men <- subset(x = mens, RuleChange == "1999-2003")
  ind.women <- subset(x = womens, RuleChange == "1999-2003")
  
  # Group Warning Groups 2004-2009  
  grp.men <- subset(x = mens, RuleChange == "2004-2009")
  grp.women <- subset(x = womens, RuleChange == "2004-2009")
  
  # Automatic DQs 2010-2014  
  auto.men <- subset(x = mens, RuleChange == "2010-2014")
  auto.women <- subset(x = womens, RuleChange == "2010-2014")
  
  # European Championships
  euro.men <- subset(x = mens, Competition == "Europeans")
  euro.women <- subset(x = womens, Competition == "Europeans")
  
  # World Championships
  world.men <- subset(x = mens, Competition == "Worlds")
  world.women <- subset(x = womens, Competition == "Worlds")

  # Championship Finalists
  finalists <- c("Gold", "Silver", "Bronze", "Finalist")
  fin.men <- fin.women <- data.frame()
  for (i in 1:length(finalists)) {
    fin.men <- rbind(fin.men, subset(x = mens, Result == finalists[i]))
    fin.women <- rbind(fin.women, subset(x = womens, Result == finalists[i]))
  }
  
  # First Round Competitors
  fir.men <- subset(x = mens, Round == 1)
  fir.women <- subset(x = womens, Round == 1)
  
#### Methods ---------------------------------------------------------------####

  # Total male responses and number of unique male athletes
  unq.men <- length(unique(mens$Sprinter)) 
  total.men <- nrow(mens)
    
  # Total female responses and number of unique female athletes
  unq.women <- length(unique(womens$Sprinter))
  total.women <- nrow(womens)

#### Results ---------------------------------------------------------------####
    
  # Mens European & Worlds Medians
  median(subset(world.men, RT >= 100)$RT, na.rm = TRUE)
  median(subset(euro.men, RT >= 100)$RT, na.rm = TRUE)
  
  # Womens European & Worlds Medians
  median(subset(world.women, RT >= 100)$RT, na.rm = TRUE)
  median(subset(euro.women, RT >= 100)$RT, na.rm = TRUE)
  
# Table 1: Descriptive Statistics for elite sprinters response times (ms)
  
  descstats <- function(x) {
    X <- na.omit(x$RT)
    
    N <- length(x$RT)
    M <- median(X)
    cover <- quantile(X, probs = c(0.025, 0.975))
    fs <- length(which(X < 100))
    
    output <- structure(list(N = N, M = M, cover = cover, FS = fs))
    return(output)
  }

  descstats(x = ind.men)
  descstats(x = grp.men)
  descstats(x = auto.men)
  descstats(x = ind.women)
  descstats(x = grp.women)
  descstats(x = auto.women)
  descstats(x = sprint.starts)  
  
# Figure 1: RTs across ruling periods
  fss <- matrix(c(1,0,8,0,0,1), nrow = 2, ncol = 3, byrow = TRUE)
  bwplot(RT ~ factor(RuleChange) | Sex, xlab = "Ruling Period",
         ylab = "Reaction Time (ms)", ylim = c(0, 600), aspect = 2, 
         strip = strip.custom(factor.levels = c("Men", "Women")),	
         scales = list(x = list(cex = 1), y = list(cex = 1)),
         data = sprint.starts[which(sprint.starts$RT >= 100),],
         panel = function(x, y, ...){
           panel.abline(h = 100, col = "black", lty = 2, lwd = 2)
           grid.circle(x = c(1:3), y = 50, r = 20, draw = TRUE, 
                       default.units = "native")
           panel.text(x = c(1:3), y = 50, labels = fss[panel.number(),])
           panel.bwplot(x,y, ...)
         },
         par.settings = list(box.umbrella = list(col = "black"),
                             box.dot = list(col = "black"),
                             box.rectangle = list(col = "black"),
                             plot.symbol = list(col = "lightgray"),
                             strip.background = list(col = "lightgrey")
         )
  )
    
# Table 2: ex-Gaussian Fit to the Reaction Time Data
  
  # Exponentially modified Gaussian CDF
  expgauss.cdf <- function(x, mu, sigma, tau) {
    part1 <- -exp(-x/tau + mu/tau + (sigma ^ 2)/(2*(tau ^ 2)))
    part2 <- pnorm((x - mu - (sigma ^ 2/tau))/sigma)
    part3 <- pnorm((x - mu)/sigma)
    f <- part1 * part2 + part3
    return(unname(f))
  }
  
  # Chi-Square Goodness-of-Fit for exponentially modified Gaussian
  expgauss.gof <- function(x, mu, sigma, tau) {
    x <- na.omit(x)
    x <- x[x != 0]
    x <- sort(x)
    N <- length(x)
    r <- 10
    cum_prob <- seq(from = 1/r, to = (r - 1)/r, by = 1/r)
    binedges <- unname(quantile(x = x, probs = cum_prob, type = 5))
    bincount <- histc(x = x, edges = c(-Inf, binedges, +Inf))$cnt
    bincount <- bincount[-length(bincount)]
    probmass_obs <- bincount/sum(bincount)
    
    temp <- expgauss.cdf(binedges, mu, sigma, tau)
    probmass_exp <- diff(x = c(0, temp, 1))
    
    chiSquare <- N * sum(((probmass_obs - probmass_exp) ^ 2)/(probmass_exp))
    return(chiSquare)
  }
  
  # Parameters, Properties and Chi-Squared Statistic
  expgauss.fit <- function(x) {
    fit <- timefit(x = subset(x, RT >= 0)$RT)
    mu <- fit@par[1]
    sig <- fit@par[2]
    tau <- fit@par[3]
    
    exp.val <- mu + tau
    s <- sqrt(sig^2 + tau^2)
    
    chi <- expgauss.gof(x = x$RT, mu = mu, sigma = sig, tau = tau)
    
    output <- list(mu = unname(mu), sig = unname(sig), tau = unname(tau),
                   exp.val = unname(exp.val), s = unname(s), chi = chi)
    return(output)
  }
  
  ind.men.fit <- expgauss.fit(ind.men)
  grp.men.fit <- expgauss.fit(grp.men)
  auto.men.fit <- expgauss.fit(auto.men)
  fir.men.fit <- expgauss.fit(fir.men)
  fin.men.fit <- expgauss.fit(fin.men)
  
  ind.women.fit <- expgauss.fit(ind.women)
  grp.women.fit <- expgauss.fit(grp.women)
  auto.women.fit <- expgauss.fit(auto.women)
  fir.women.fit <- expgauss.fit(fir.women)
  fin.women.fit <- expgauss.fit(fin.women)
  
# Figure 2: EMGD fits to RT data
  times <- seq(100, 450, by = 1)
  ind.men.dis <- dexgauss(q = times, mu = ind.men.fit$mu,
                          sigma = ind.men.fit$sig, tau = ind.men.fit$tau)
  grp.men.dis <- dexgauss(q = times, mu = grp.men.fit$mu,
                          sigma = grp.men.fit$sig, tau = grp.men.fit$tau)
  auto.men.dis <- dexgauss(q = times, mu = auto.men.fit$mu,
                           sigma = auto.men.fit$sig, tau = auto.men.fit$tau)
  
  ind.women.dis <- dexgauss(q = times, mu = ind.women.fit$mu,
                            sigma = ind.women.fit$sig, tau = ind.women.fit$tau)
  grp.women.dis <- dexgauss(q = times, mu = grp.women.fit$mu,
                            sigma = grp.women.fit$sig, tau = grp.women.fit$tau)
  auto.women.dis <- dexgauss(q = times, mu = auto.women.fit$mu,
                             sigma = auto.women.fit$sig, 
                             tau = auto.women.fit$tau)
  
  fir.men.dis <- dexgauss(q = times, mu = fir.men.fit$mu,
                          sigma = fir.men.fit$sig, tau = fir.men.fit$tau)
  fin.men.dis <- dexgauss(q = times, mu = fin.men.fit$mu,
                          sigma = fin.men.fit$sig, tau = fin.men.fit$tau)
  
  fir.women.dis <- dexgauss(q = times, mu = fir.women.fit$mu,
                            sigma = fir.women.fit$sig, tau = fir.women.fit$tau)
  fin.women.dis <- dexgauss(q = times, mu = fin.women.fit$mu,
                            sigma = fin.women.fit$sig, tau = fin.women.fit$tau)
  
  plot(x = times, y = ind.men.dis, col = "black", type = "l", lty = 3, 
       lwd = 2, xlab = "Reaction Time (ms)", ylab = "Probability")
  lines(x = times, y = grp.men.dis, col = "black", lty = 2, lwd = 2)
  lines(x = times, y = auto.men.dis, col = "black", lty = 1, lwd = 2)
  title("A: EMGD fits for men by ruling period")    
  
  plot(x = times, y = ind.women.dis, col = "black", type = "l", lty = 3, 
       lwd = 2, xlab = "Reaction Time (ms)", ylab = "Probability",
       ylim = c(0, max(max(ind.women.dis), max(grp.women.dis))))
  lines(x = times, y = grp.women.dis, col = "black", lty = 2, lwd = 2)
  lines(x = times, y = auto.women.dis, col = "black", lty = 1, lwd = 2)
  title("B: EMGD fits for women by ruling period")
  
  plot(x = times, y = fir.men.dis, col = "black", type = "l", lty = 2, 
       lwd = 2, xlab = "Reaction Time (ms)", ylab = "Probability",
       ylim = c(0, max(max(fir.men.dis), max(fin.men.dis))))
  lines(x = times, y = fin.men.dis, col = "black", lty = 1, lwd = 2)
  title("C: EMGD fits for men by round")
  
  plot(x = times, y = fir.women.dis, col = "black", type = "l", lty = 2, 
       lwd = 2, xlab = "Reaction Time (ms)", ylab = "Probability",
       ylim = c(0, max(max(fir.women.dis), max(fin.women.dis))))
  lines(x = times, y = fin.women.dis, col = "black", lty = 1, lwd = 2)
  title("D: EMGD fits for women by round")

# Inline Values: Pr(X > 120 ms)
  # Men across ruling periods
  1 - expgauss.cdf(x = 120, mu = ind.men.fit$mu, 
                   sigma = ind.men.fit$sig, tau = ind.men.fit$tau)
  1 - expgauss.cdf(x = 120, mu = grp.men.fit$mu, 
                   sigma = grp.men.fit$sig, tau = grp.men.fit$tau)
  1 - expgauss.cdf(x = 120, mu = auto.men.fit$mu, 
                   sigma = auto.men.fit$sig, tau = auto.men.fit$tau)
  
  # Women across ruling periods
  1 - expgauss.cdf(x = 120, mu = ind.women.fit$mu, 
                   sigma = ind.women.fit$sig, tau = ind.women.fit$tau)
  1 - expgauss.cdf(x = 120, mu = grp.women.fit$mu, 
                   sigma = grp.women.fit$sig, tau = grp.women.fit$tau)
  1 - expgauss.cdf(x = 120, mu = auto.women.fit$mu, 
                   sigma = auto.women.fit$sig, tau = auto.women.fit$tau)
  
  # Mens and Womens First Rounds
  1 - expgauss.cdf(x = 120, mu = fir.men.fit$mu, 
                   sigma = fir.men.fit$sig, tau = fir.men.fit$tau)
  1 - expgauss.cdf(x = 120, mu = fir.women.fit$mu, 
                   sigma = fir.women.fit$sig, tau = fir.women.fit$tau)
  
  # Mens and Womens Finals
  1 - expgauss.cdf(x = 120, mu = fin.men.fit$mu,
                   sigma = fin.men.fit$sig, tau = fin.men.fit$tau)
  1 - expgauss.cdf(x = 120, mu = fin.women.fit$mu,
                   sigma = fin.women.fit$sig, tau = fin.women.fit$tau)
  
# RT revised thresholds - Male and Female
  men.fit <- timefit(x = subset(mens, RT >= 0)$RT)
  women.fit <- timefit(x = subset(womens, RT >= 0)$RT)
  
  men.cdf.cur <- 0
  men.thresh <- -1
  while (men.cdf.cur <= 0.01) {
     men.thresh <- men.thresh + 1
     men.cdf.cur <- expgauss.cdf(x = men.thresh, mu = men.fit@par[1],
                                 sigma = men.fit@par[2],
                                 tau = men.fit@par[3])
  }
  men.thresh
  
  women.cdf.cur <- 0
  women.thresh <- -1
  while (women.cdf.cur <= 0.01) {
     women.thresh <- women.thresh + 1
     women.cdf.cur <- expgauss.cdf(x = women.thresh, mu = women.fit@par[1],
                                   sigma = women.fit@par[2],
                                   tau = women.fit@par[3])
  }
  women.thresh
  
# Table 3: Probability of Superiority
    
  expgauss.ps <- function(low.fit, upp.fit) {
    
    # Calculate the median of the lower population - optimisation routine
    cur.cdf.val <- -1
    low.med <- low.fit$exp.val
    while (abs(cur.cdf.val) > 1e-6) {
      cur.cdf.val <- -0.5 + expgauss.cdf(x = low.med, mu = low.fit$mu,
                                         sigma = low.fit$sig,
                                         tau = low.fit$tau)
      if (cur.cdf.val < 0) {
        low.med <- low.med + low.fit$s * cur.cdf.val
      } else {
        low.med <- low.med - low.fit$s * cur.cdf.val
      }
    } 
    
    # Calculate the probability of the upper population above median of lower
    prob.super <- 1 - expgauss.cdf(x = low.med, mu = upp.fit$mu, 
                                   sigma = upp.fit$sig, tau = upp.fit$tau)
    
    output <- structure(list(ps = prob.super, low.med = low.med))
    return(output)
  }
  
  # Rule 1 v Rule 2 - Men & Women
  expgauss.ps(ind.men.fit, grp.men.fit)
  expgauss.ps(ind.women.fit, grp.women.fit)
  
  # Rule 2 v Rule 3 - Men & Women
  expgauss.ps(grp.men.fit, auto.men.fit)
  expgauss.ps(grp.women.fit, auto.women.fit)
  
  # Rule 1 v Rule 3 - Men & Women
  expgauss.ps(ind.men.fit, auto.men.fit)
  expgauss.ps(ind.women.fit, auto.women.fit)
    
  # Finals v First Rounds - Men & Women
  expgauss.ps(fin.men.fit, fir.men.fit)
  expgauss.ps(fin.women.fit, fir.women.fit)
    
#### End Script ------------------------------------------------------------####