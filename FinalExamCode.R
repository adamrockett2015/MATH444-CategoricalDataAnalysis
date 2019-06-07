# R code for problem 1
challenger.data <- read.table("challenger.txt", header=TRUE);
challenger.data

# a
challenger.model <- glm(TD ~ temp, family = binomial(link = logit), data = challenger.data);
summary(challenger.model)

# g
challenger.probit <- glm(TD ~ temp, family = binomial(link = probit), data = challenger.data);
summary(challenger.probit)

# h
plot(challenger.data$TD~challenger.data$temp, main="Thermal Distress vs. Temperature", xlab="Temperature (Celsius)",
     ylab="Esimated Probability");
curve(expr = plogis(challenger.model$coefficients[1] + challenger.model$coefficients[2]*x),
      col = "red", add = TRUE);
curve(expr = plogis(challenger.probit$coefficients[1] + challenger.probit$coefficients[2]*x),
      col = "blue", add = TRUE);

#C.I. for beta
alpha <- 0.05
lower_beta <- challenger.model$coefficients[2] - qnorm(1 - alpha/2) * sqrt(vcov(challenger.model) [2, 2])
upper_beta <- challenger.model$coefficients[2] + qnorm(1 - alpha/2) * sqrt(vcov(challenger.model) [2, 2])

#Print in nice frame
data.frame(beta.hat = challenger.model$coefficients[2], lower_beta, upper_beta)

#COmpute OR for c=1
OR1_hat <- exp(challenger.model$coefficients[2])
OR1_hat

#pearson residuals
pearson <-residuals(object=challenger.model, type="pearson")
head(pearson)

challenger.res <- resid(challenger.model);

plot(challenger.data$temp, challenger.res, ylab = "Residuals", xlab = "Temperature", 
     main = "Thermal Distress on O-rings");
abline(0, 0)

######################################
######################################

# R code for Problem 2
LBW.data <- read.table("low_bwt.txt", header = TRUE);
LBW.data

# Full and Null models
LBW.full <- glm(formula = LOW ~ AGE + RACE + SMOKE + PTL + HT + UI + FTV, data=LBW.data, 
                family=binomial(link=logit));
LBW.null <- glm(formula = LOW ~ 1, data=LBW.data, family=binomial(link=logit));

# Forward Selection
step(LBW.null, scope = list(lower=~1, upper = LBW.full), direction="forward");

# Backward Selection
step(LBW.full, scope = list(lower=~1, upper = LBW.full), direction="backward");

# Stepwise Selection
step(LBW.null, scope = list(lower=~1, upper = LBW.full), direction="both");


######################################
######################################

# R code for problem 4

DrVisit.data <- read.table("Dr+office+visit+data.txt", header=TRUE);
DrVisit.data

DrVisit.poisson <- glm(formula = ofp ~ hosp + numchron + gender + school + privins + health.excellent
                       + health.poor, family = poisson(link = log), data = DrVisit.data);
summary(DrVisit.poisson)

library(MASS);
DrVisit.NB <- glm.nb(formula = ofp ~ hosp + numchron + gender + school + privins + health.excellent
                       + health.poor, link = log, data = DrVisit.data);
summary(DrVisit.NB)


