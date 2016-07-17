

library(shiny)
library(shinydashboard)
library(DT)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$dtrt <- DT::renderDataTable(response.times, 
                 rownames = FALSE, server = FALSE,
                 options = list(pageLength = 5, dom = 'tp'),
                 selection = list(mode = "single", target = "column",
                                  selected = 4)
                 )
  
  output$dtrtval <- DT::renderDataTable(
                    dtrtvalfun(input$dtrt_columns_selected), 
                    rownames = FALSE, options = list(dom = "t")
                    )
  
  output$plotrtval <- renderPlot({
    if (!is.null(input$dtrt_columns_selected)) {
      data.col <- input$dtrt_columns_selected + 1
      var.name <- names(response.times)[data.col]
      var.class <- class(response.times[[data.col]])
  
      switch(var.class, 
             character = {
               # Graphical Barchart of the character data
               barplot(table(response.times[,var.name]),
                       main = paste0("Barchart of ", var.name),
                       xlab = var.name, las = 1)
             },
             numeric = {
               # Graphical Histogram of the numerical data 
               hist(response.times[which(response.times[,var.name] >= 0),var.name], 
                    main = paste0("Histogram of ", var.name),
                    xlab = var.name, las = 1)
             },
             integer = {
               # Graphical Barchart of the integer data
               barplot(table(response.times[,var.name]), 
                    main = paste0("Barchart of ", var.name),
                    xlab = var.name, las = 1)
             }
        )
    }
  })
  
  output$analdesc <- renderPlot({
    if (!is.null(input$subsex) && !is.null(input$comparison)) {
      
      cur.subset <- subset(response.times, RT >= 100)
      
      if (input$comparison == "Sex") {
        if (input$subsex != "All") {
          updateRadioButtons(session = session, inputId = "subsex", 
                             selected = "All")
          cond.factor <- cur.subset$Sex
          cond.factor.name <- "Sex"
        } else {
          cond.factor <- cur.subset$Sex
          cond.factor.name <- "Sex"
        }
      }
      
      if (input$subsex != "All") {
        cur.subset <- subset(cur.subset, Sex == input$subsex)
      }
      
      if (input$comparison == "Rounds") {
        finals <- subset(cur.subset, Result == "Finalist")
        first <- subset(cur.subset, Round == 1)
        cur.subset <- rbind(finals, first)
        cond.factor <- c(rep("Finals", times = nrow(finals)),
                         rep("First", times = nrow(first)))
        cond.factor.name <- "Rounds"
      }
      
      if (input$comparison == "Rule Period") {
        cond.factor <- cur.subset$RuleChange
        cond.factor.name <- "Rule Period"
      }
      
      bwplot(cur.subset$RT ~ cond.factor, xlab = cond.factor.name,
             ylab = "Reaction Time (ms)", ylim = c(0, 600), 
             panel = function(x, y, ...){
               panel.abline(h = 100, col = "green", lty = 2, lwd = 1)
               panel.bwplot(x,y, ...)
             },
             par.settings = list(box.umbrella = list(col = "black"),
                                 box.dot = list(col = "black"),
                                 box.rectangle = list(col = "black"),
                                 plot.symbol = list(col = "lightgray")
             )
      )
    }
  })
  
  model.fit <- reactive({
    cur.subset <- response.times
    
    if (input$subsex != "All") {
      cur.subset <- subset(cur.subset, Sex == input$subsex)
    }
    
    if (input$comparison == "Sex") {
      cond.var <- cur.subset$Sex
      cond.var.unq <- sort(unique(cond.var), decreasing = TRUE)
    } else if (input$comparison == "Rounds") {
      finals <- subset(cur.subset, Result == "Finalist")
      first <- subset(cur.subset, Round == 1)
      cur.subset <- rbind(finals, first)
      cond.var <- c(rep("Finals", times = nrow(finals)), 
                    rep("First", times = nrow(first)))
      cond.var.unq <- sort(unique(cond.var), decreasing = TRUE)
    } else {
      cond.var <- cur.subset$RuleChange
      cond.var.unq <- unique(cond.var)
    }
    
    mod <- list(mu = c(), sig = c(), tau = c(), cond.var.unq = cond.var.unq)
    
    for (i in 1:length(cond.var.unq)) {
      temp <- expgauss.fit(subset(cur.subset, cond.var == cond.var.unq[i])$RT)
      mod$mu[i] <- temp$mu
      mod$sig[i] <- temp$sig
      mod$tau[i] <- temp$tau
    }
    
    return(mod)
  })
  
  output$modeltab <- DT::renderDataTable({
    mod.fit <- model.fit()
    df <- cbind(mod.fit$mu, mod.fit$sig, mod.fit$tau)
    df <- round(df, digits = 2)
    df <- as.data.frame(df)
    df <- cbind(mod.fit$cond.var.unq, df)
    
    ps <- rep(NA, times = nrow(df))
    for (i in 2:nrow(df)) {
      low.fit <- list(mu = mod.fit$mu[i], sig = mod.fit$sig[i], 
                      tau = mod.fit$tau[i])
      upp.fit <- list(mu = mod.fit$mu[i-1], sig = mod.fit$sig[i-1], 
                      tau = mod.fit$tau[i-1])
      ps[i] <- round(expgauss.ps(low.fit, upp.fit)$ps , digits = 2)
    }
    df <- cbind(df, ps)
    colnames(df) <- c("Factor Level", "mu", "sigma", "tau", "Pr(Superiority)")
    df
  }, rownames = FALSE, options = list(dom = "t"))
  
  output$modelplot <- renderPlot({
    times <- seq(100, 450, by = 1)
    m.fit <- model.fit()
    par(mar = c(5.1,4.1,2,2))
    temp <- dexgauss(q = times, mu = m.fit$mu[1],
                     sigma = m.fit$sig[1], tau = m.fit$tau[1])
    plot(x = times, y = temp, col = 1, type = "l", lty = 1, ylim = c(0, 0.025),
         lwd = 2, xlab = "Reaction Time (ms)", ylab = "Probability", las = 1)
    
    if(length(m.fit$mu) > 1) {
      for (i in 2:length(m.fit$mu)) {
        temp <- dexgauss(q = times, mu = m.fit$mu[i],
                         sigma = m.fit$sig[i], tau = m.fit$tau[i])
        lines(x = times, y = temp, col = i, lty = i, lwd = 2)
      }
    }
    legend("topright", legend = m.fit$cond.var.unq, col = 1:length(m.fit$mu),
           lty = 1:length(m.fit$mu))
    par(mar = c(5.1,4.1,4.1,2))
  })
})
