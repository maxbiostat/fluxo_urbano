Data <- read.csv(file = "data/FlowDataSet.csv", header = TRUE)

make_dt <- function(Flow, Distance, Dt){
  dt <- data.frame(
    Y =  Dt[, Flow],
    Pi = Dt$Pi,
    Pj = Dt$Pj,
    D = Dt[, Distance]
  )
  nz_dt <- subset(dt, Y > 0 & D > 0)
  nz_dt$D <- -log(nz_dt$D)
  return(nz_dt)
}
#
FitModels <- function(dataset){
  log_gravity <- lm(log(Y) ~ D, offset = log(Pi) + log(Pj), data = dataset)
  log_gravity_extra <- lm(log(Y) ~ D + log(Pi) + log(Pj), data = dataset)
  return(list(gravity = log_gravity, gravity_plus = log_gravity_extra)) 
}
#
StudyModels <- function(fit){
  cat("Standard Gravity Model \n")
  print(summary(fit$gravity))
  cat("K: ", exp(coef(fit$gravity)[1]), exp(confint(fit$gravity)[1, ]), "\n")
  cat("sigma: ", coef(fit$gravity)[2],   confint(fit$gravity)[2, ], "\n")  
  cat("Sum of Squared Residuals:", sum(fit$gravity$residuals^2), "\n")
  cat("\n -------------------- \n Extended Gravity Model \n")
  print(summary(fit$gravity_plus))
  cat("K:",  exp(coef(fit$gravity_plus)[1]), exp(confint(fit$gravity_plus)[1, ]), "\n")
  cat("sigma:", coef(fit$gravity_plus)[2], confint(fit$gravity_plus)[2, ], "\n")  
  cat("alpha: ", coef(fit$gravity_plus)[3],  confint(fit$gravity_plus)[3, ], "\n")
  cat("beta: ", coef(fit$gravity_plus)[4],  confint(fit$gravity_plus)[4, ], "\n")
  cat("Sum of Squared Residuals:", sum(fit$gravity_plus$residuals^2), "\n")
}
##############
##############
Analyses <- data.frame(
  flow = c("flow.total", "flow.dr", "flow.wk", "flow.dr", "flow.wk"),
  distance = c("dist.euc", "dist.euc", "dist.euc", "dist.dr", "dist.wk")   
)
DataSets <- lapply(1:nrow(Analyses), 
                   function(row){
                     make_dt(Flow = paste(Analyses[row, 1]), Distance = paste(Analyses[row, 2]), Dt = Data)
                   }
)
Names <- rep(NA, nrow(Analyses))
for(row in  1:nrow(Analyses)) Names[row] <- paste(Analyses[row, 1], "vs", Analyses[row, 2], sep = "_")
names(DataSets) <- Names
Fitted <- lapply(DataSets, FitModels)
Results <- lapply(Fitted, function(x) capture.output(StudyModels(x)))
out <- file("results/fitted_gravity_models.txt")
sink(out, append = FALSE)
Results
sink() 

plotObs_vs_Fitted <- function(fit, title = ""){
  plot(fit$gravity$fitted.values ~fit$gravity$model$`log(Y)`, main = title,
       xlab = "Observed flow", ylab = "Predicted flow")
  points(fit$gravity_plus$fitted.values ~ fit$gravity_plus$model$`log(Y)`, col = "red")
  legend(x = "topleft", legend = c("standard gravity model", " four parameter gravity model"),
         col = 1:2, pch = 16, bty = "n")
  abline(a = 0, b = 1, lwd = 3, col = "green")
}
for(i in 1:length(Fitted)) plotObs_vs_Fitted(fit = Fitted[[i]], title = Names[i])