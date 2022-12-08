activate.packages.if.missing <- function(packageNames) {
  packages <- rownames(installed.packages())
  
  for (packageName in packageNames) {
    if (!packageName %in% packages) {
      install.packages(packageName)
    }
    
    require(packageName, character.only=T)
  }
}

return_min_cvm_lambda <- function (data, y.name, depth=5, verbose=T){
  form <- as.formula(paste(y.name, "~", "."))
  X.model <- model.matrix(form, data)[,-1]
  alphas <- seq(0, 1, by = 0.1)
  results <- data.frame(matrix(0, nrow = 11, ncol = 3))
  colnames(results) <- c("CVM", "Best.Lambda", "Alpha")
  best.models <- list()
  best.models.data <- results[0,]
  lambdas <- NULL
  
  for (k in 1:depth) {
    
    print(paste("Beginning iteration at depth", k))
    models <- list()
    
    for (j in 1:length(alphas)) {
      cv.lasso <- cv.glmnet(X.model, 
                            data[[y.name]], 
                            family = binomial,
                            lambda = lambdas,
                            alpha = alphas[j])
      
      models[[j]] <- cv.lasso
      i <- which(cv.lasso$lambda == cv.lasso$lambda.min) 
      mse.min <- cv.lasso$cvm[i]
      results[j,1]<-mse.min
      results[j,2]<-cv.lasso$lambda.min 
      results[j,3]<-alphas[j]
    }
    
    # Get best model parameters
    best.cvm <- min(results$CVM)
    best.model.idx <- which(results$CVM == best.cvm)
    best.model.data <- results[best.model.idx,]
    best.models.data <- rbind(best.models.data, best.model.data)
    best.models[[k]] <- models[[best.model.idx]]
    
    if (verbose)
      print(best.model.data)
    
    best.alpha <- best.model.data$Alpha
    best.lambda <- best.model.data$Best.Lambda
    
    # Get next set of search hyperparameters
    alphas <- get.centred.set(best.alpha, lims=c(0,1))
    
    if (verbose) {
      print(paste("Best alpha:", best.alpha))
      print(paste("New alpha list:", paste(alphas, collapse = ",")))
    }
    
    lambdas <- get.centred.set(best.lambda, lims=c(0,Inf))
    
    if (verbose) {
      print(paste("Best lambda:", best.lambda))
      print(paste("New lambda list:", paste(lambdas, collapse = ",")))
    }    
  }
  
  list(results = best.models.data,
       best.models = best.models)
}

activate.packages.if.missing("caret")
activate.packages.if.missing("pROC")

get.y.prob <- function(model, test.data) {
  
  if (length(intersect(c("glmnet", "cv.glmnet"), class(model))) > 0) {
    test.data.X <- model.matrix(Churn ~ ., cbind(test.data, Churn=y))[,-1]
    y.prob <- predict(model,
                      test.data.X,
                      type="response",
                      s = "lambda.min")[,1]
  }
  else {
    y.prob <- predict(model,
                      test.data, 
                      type="response")
  }
  
  y.prob
}

get.model.accuracy.metrics <- function(models, 
                                       test.data,
                                       mfrow=c(2,4)) {
  results <- list()
  model.names <- names(models)
  y <- test.data$Churn
  levels(y) <- list("FALSE" = "Non-churn", "TRUE" = "Churn")
  test.data <- test.data %>% dplyr::select( - Churn)
  par(mfrow=mfrow)
  
  for (i in 1:length(model.names)) {
    model.name <- model.names[i]
    model <- models[[i]]
    y.prob <- get.y.prob(model, test.data)
    ROC.prob <- roc(y, y.prob)
    p <- plot.roc(ROC.prob,
                  col = "royalblue",
                  main = model.name,
                  print.auc = TRUE,
                  max.auc.polygon = TRUE,
                  print.thres = TRUE, print.thres.pch = 19, print.thres.col = "red",
                  auc.polygon = TRUE, auc.polygon.col = "#D1F2EB"
    )
    
    threshold <- coords(p, "best", ret="threshold")[1,1]
    print(paste("Model", model.name, "performs best with threshold of", round(threshold, 4)))
    y.hat <- factor(y.prob > threshold)
    conf.mat <- confusionMatrix(y, y.hat)
    results[[model.name]] <- list(confusion.matrix = conf.mat, optimal.threshold = threshold)
  }
  
  results
}

measure.model.profit <- function(models, test.data, classification.thresholds) {
  model.names <- names(models)
  y <- test.data$Churn
  levels(y) <- list("FALSE" = "Non-churn", "TRUE" = "Churn")
  CustomerValue <- test.data$CustomerValue
  avg.customer.value <- mean(CustomerValue)
  test.data <- test.data %>% dplyr::select(-c(Churn, CustomerValue))
  results <- list()
  
  for (i in 1:length(model.names)) {
    model.name <- model.names[i]
    
    classification.threshold <- classification.thresholds[i]
    model <- models[[i]]
    y.prob <- get.y.prob(model, test.data)
    y.hat <- factor(y.prob > classification.threshold)
    comparison <- data.frame(
      Churn = y,
      Churn_Pred = y.hat, 
      CustomerValue = CustomerValue) %>%
      
      mutate(
        CustomerProfit = case_when(
          Churn == TRUE & Churn_Pred == TRUE ~ 0.5 * CustomerValue - 500,
          Churn == FALSE & Churn_Pred == TRUE ~ CustomerValue - 500,
          Churn == TRUE & Churn_Pred == FALSE ~ 0,
          Churn == FALSE & Churn_Pred == FALSE ~ CustomerValue
        )
      )
    results[[model.name]] <- comparison
  }
  
  results
}

get.centred.set <- function(value, lims = c(-Inf,Inf)) {
  if (length(lims) == 0)
    lims <- c(-Inf, Inf)
  
  if (is.null(lims[1]) | is.na(lims[1]))
    lims[1] <- -Inf
  
  if (is.null(lims[2]) | is.na(lims[2]))
    lims[2] <- Inf
  
  sig.digit.exp <- floor(log(value, 10))
  sig.digit <- floor(value * 10 ^ (-sig.digit.exp))
  
  start <- (sig.digit - 1) * 10 ^ sig.digit.exp
  start <- max(start, lims[1])
  
  end <- (sig.digit + 1) * 10 ^ sig.digit.exp
  end <- min(end, lims[2])
  
  seq(start, end, by = 0.05 * 10 ^ sig.digit.exp)
}

get.interaction.terms <- function(data) {
  non.churn.cov <- cov(data %>%
                         filter(!!sym(yvar.name) == "Non-churn"))
  churn.cov <- cov(data %>%
                     filter(!!sym(yvar.name) == "Churn"))
  diff.cov <- non.churn.cov - churn.cov
  var.names <- colnames(diff.cov)
  n <- nrow(diff.cov)
  combinations <- data.frame(matrix(nrow = n*(n-1), ncol = 2))
  names(combinations) <- c("var1", "var2")
  k <- 1
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      cov.value <- abs(diff.cov[i, j])
      
      if (cov.value > 1)
        combinations[k, ] <- c(var.names[i], var.names[j])
      
      k <- k + 1
    }
  }
  
  combinations %>% 
    filter(!is.na(var1)) %>% 
    filter(!is.na(var2))
}

get.variable.significance.levels <- function(model, test, alpha = 0.05) {
  #c("deviance", "Wald", "likelihood")
  
  if (test == "Wald") {
    coefs <- data.frame(coef(summary(model))) %>% 
      rename(P.Value = Pr...z..) %>%
      mutate(P.Value = round(P.Value, 4),
             Is.Significant = P.Value <= alpha)
    vars <- coefs %>% arrange(desc(P.Value))
  }
  else if (test == "deviance") {
    full.deviance <- deviance(model)
    vars <- data.frame(anova(model))[-1,]
    var.names <- rownames(vars)
    vars$Variable.Name <- var.names
    rownames(vars) <- NULL
    var.count <- nrow(vars)
    vars$Deviance.Without <- rep(NA, var.count)
    
    for (i in 1:var.count) {
      var.name <- var.names[i]
      remove.form = as.formula(paste("~ . -", var.name))
      reg.reduced <- update(model, remove.form)
      vars[i,]$Deviance.Without <- deviance(reg.reduced)
    }
    
    # List of model deviances with variables removed
    # and test p-values
    vars %>% mutate(
      Relative.Deviance =
        round(Deviance.Without - full.deviance, 4),
      P.Value = round(pchisq(Relative.Deviance,
                             Df,
                             lower.tail = F), 4)) %>%
      mutate(
        Is.Significant = P.Value <= alpha) %>%
      arrange(desc(P.Value))
  }
  else if (test == "likelihood") {
    print("lol")
  }
}

my.backward.stepwise <- function(
    model, test,  sig.level = 0.05, print.results = T) {
  
  removed.variables <- c()
  
  repeat {
    full.deviance <- deviance(model)
    ordered.deviances <- get.variable.significance.levels(model, test)
    
    if (print.results)
      print(ordered.deviances)
    
    # Get insignificant variables at the 5% level
    insignificant.vars <- ordered.deviances %>% filter(!Is.Significant)
    
    if (nrow(insignificant.vars) == 0)
      break
    
    # Get least significant variable
    least.significant <- insignificant.vars[1,]
    least.significant.name <- least.significant$Variable.Name
    remove.form <- as.formula(
      paste("~ . -", paste(least.significant.name, collapse = " -")))
    
    # Pop from model
    model <- update(model, remove.form)
    removed.variables <- append(removed.variables, least.significant.name)
    
    if (print.results)
      print(paste("Removed", 
                  least.significant.name, 
                  "from model with P.Value of", 
                  least.significant$P.Value))
  }
  
  list(model = model, removed.variables = removed.variables)
}

#Empirical logit Function
myemplogit <- function(
    yvar=y, xvar=x, maxbins=10, sc=1, line=TRUE, ...) {
  breaks  <<- unique(quantile(xvar, probs=0:maxbins/maxbins))
  levs  <<- (cut(xvar, breaks, include.lowest=FALSE))
  nums <<- as.numeric(levs)
  c.tab <- plyr::count(nums,'levs')
  c.tab$levs <- factor(
    c.tab$levs, 
    levels = levels(addNA(c.tab$levs)), 
    labels = c(levels(c.tab$levs),
               paste("[",min(xvar),"]",sep="")), 
    exclude = NULL)
  
  c.tab <- c.tab[c(nrow(c.tab),1:nrow(c.tab)-1),]
  sc <- (max(c.tab$freq)/min(c.tab$freq)/sc)^2
  zcex <<- sqrt(c.tab$freq/pi)/sc
  print(c.tab);print(zcex);print(sc)
  emplogitplot1(yvar~xvar,
                breaks=breaks,
                cex=zcex,
                showline=line,
                ...)
}

emplogit <- function(bins=30, yvar, xvar, name) {
  breaks <- unique(
    quantile(
      xvar, 
      probs=0:bins/bins, 
      na.rm = TRUE))
  
  emplogitplot1(
    yvar ~ xvar , breaks = breaks , xlab = name) 
}

plot.emplogits <- function(
    yvar, xvars, scs, mfrow = c(2,3),new.window = T, ...) {
  
  if (new.window)
    x11()
  
  
  par(mfrow=mfrow)
  n <- length(xvars)
  x.names <- names(xvars)
  
  for (i in 1:n) {
    myemplogit(
      yvar, xvars[[i]], 30, sc=scs[i], xlab=x.names[i])
  }
}

plot.boxes <- function(
    xvar, yvars, mfrow = c(2,3), new.window = T, ...) {
  
  if (new.window)
    x11()
  
  par(mfrow=mfrow)
  n <- length(yvars)
  y.names <- names(yvars)
  
  for (i in 1:n) {
    boxplot(
      yvars[[i]] ~ xvar,
      ylab = y.names[i],
      ...)
  }
}

plot.conditional.densities <- function(
    yvar, xvars, mfrow = c(2,4), new.window = T, ...) {
  
  if (new.window)
    x11()
  
  par(mfrow = mfrow)
  y.levels <- levels(yvar)
  colfill<-c(2:(2+length(y.levels)))
  n <- length(xvars)
  x.names <- names(xvars)
  
  for (i in 1:n) {
    sm.density.compare(
      xvars[[i]], yvar, xlab=x.names[i], ...)
    
    legend(x = "topright", y.levels, fill=colfill)
  }
}

plot.hists <- function(
    vars, freq = F, mfrow = c(2,3), new.window = T, 
    bw.bin.multiplier = 1.5, ...) {
  
  n <- length(vars)
  names <- names(vars)
  
  if (new.window)
    x11()  
  
  par(mfrow = mfrow)
  
  for (i in 1:n) {
    p <- hist(
      vars[[i]], plot = F, ...)
    bw <- get.density.bw(p$breaks, bw.bin.multiplier)
    plot(p, xlab = names[i], main="", freq = freq, ...)
    lines(density(vars[[i]], bw = bw), col = 'red')
  }
}

get.density.bw <- function(breaks, mult) {
  
  n <- length(breaks)
  diffs <- rep(NA, n - 1)
  
  for (i in 1:(n - 1)) {
    diffs[i] <- breaks[i + 1] - breaks[i]
  }
  
  mean(diffs) * mult
}
