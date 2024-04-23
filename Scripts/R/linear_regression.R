library(rgl)
library(scatterplot3d)
library(ggplot2)
library(e1071)
library(nortest)
library(MASS)
library(car)
library(stats)

data_sporco1 <- `cbb.(1)`
data <- data_sporco1 [, 3:21]

risultato <- as.factor(data_sporco1[,22])

#nuova variabile
win_ratio <- data[,2]*100/data[,1]
min(win_ratio)
max(win_ratio)

data_new <- cbind(data, win_ratio)

dati_2015 <- data_new[data_sporco1$YEAR == 2015, ]
dati_2016 <- data_new[data_sporco1$YEAR == 2016, ]
dati_2017 <- data_new[data_sporco1$YEAR == 2017, ]
dati_2018 <- data_new[data_sporco1$YEAR == 2018, ]
dati_2019 <- data_new[data_sporco1$YEAR == 2019, ]


# Calcola la matrice delle correlazioni
correlation_matrix <- cor(dati_2015)

# Visualizza la matrice delle correlazioni
print(correlation_matrix)

# Puoi anche visualizzare la matrice delle correlazioni con una heatmap per una migliore visualizzazione grafica
library(ggplot2)
library(reshape2)

# Trasforma la matrice delle correlazioni in un formato adatto per ggplot2
correlation_melted <- melt(correlation_matrix)

# Crea la heatmap
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()




# Seleziona una riga dalla matrice delle correlazioni
row_index <- 3  # Sostituisci con l'indice della riga desiderata

# Estrai i dati della riga selezionata
row_data <- correlation_melted[correlation_melted$Var1 == rownames(correlation_matrix)[row_index], ]

# Crea il plot
ggplot(row_data, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 4) +  # Aggiungi il testo con il valore della correlazione
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()





lm_model_2015 <- lm(win_ratio ~ ADJOE, data = dati_2015)
# Creazione del plot dello scatterplot con retta di regressione
plot(dati_2015$ADJOE, dati_2015$win_ratio, 
     xlab = "Adjusted Offensive Efficiency (ADJOE)",
     ylab = "Win Ratio",
     main = "Scatterplot of Win Ratio vs ADJOE",
     col = "blue", # colore dei punti
     pch = 16)     # forma dei punti

# Aggiunta della retta di regressione
abline(lm_model_2015, col = "red",lwd=4)  # retta di regressione in rosso



lm_model_2016 <- lm(win_ratio ~ ADJOE, data = dati_2016)
lm_model_2017 <- lm(win_ratio ~ ADJOE, data = dati_2017)
lm_model_2018 <- lm(win_ratio ~ ADJOE, data = dati_2018)
lm_model_2019 <- lm(win_ratio ~ ADJOE, data = dati_2019)

summary(lm_model_2015)
summary(lm_model_2016)
summary(lm_model_2017)
summary(lm_model_2018)
summary(lm_model_2019)

par(mfrow=c(1,1))
plot(lm_model_2015)
residuals<-residuals(lm_model_2015)
skew <- skewness(residuals)
print(skew)

# Get standardized residuals
standardized_residuals <- rstandard(lm_model_2015)

# Generate a normal probability plot
par(mfrow=c(1,1))
qqnorm(standardized_residuals, main = "Normal Probability Plot of Standardized Residuals")
qqline(standardized_residuals)

hist(standardized_residuals, breaks=9)
hist(standardized_residuals, breaks = 9, col = "green", border = "black", main = "Histogram of Standardized Residuals")


# Extract residuals from the linear model
residuals_lm <- residuals(lm_model_2015)

# Extract fitted values from the linear model
fitted_values <- predict(lm_model_2015)

# Plot residuals vs. predictor (ADJOE)
plot(dati_2015$ADJOE, residuals_lm, 
     xlab = "ADJOE", ylab = "Residuals",
     main = "Residuals vs. Predictor Plot",
     pch = 16, col = "blue")


# Perform Durbin-Watson test
durbin_watson <- durbinWatsonTest(lm_model_2015)

# Print the Durbin-Watson statistic and its p-value
print(durbin_watson)

# Perform Ljung-Box test
ljung_box <- Box.test(residuals(lm_model_2015), lag = 1, type = "Ljung-Box")

# Print the Ljung-Box test statistic and its p-value
print(ljung_box)


# Check for skewness
skewness <- skewness(standardized_residuals)
if(skewness > 0) {
  cat("The normal probability plot indicates right skewness.\n")
} else if(skewness < 0) {
  cat("The normal probability plot indicates left skewness.\n")
} else {
  cat("The normal probability plot indicates no skewness.\n")
}

par(mfrow=c(2,2))
plot(lm_model_2016)

par(mfrow=c(2,2))
plot(lm_model_2017)

par(mfrow=c(2,2))
plot(lm_model_2018)

par(mfrow=c(2,2))
plot(lm_model_2019)


shapiro.test(residuals(lm_model_2015))
shapiro.test(residuals(lm_model_2016))
shapiro.test(residuals(lm_model_2017))
shapiro.test(residuals(lm_model_2018))
shapiro.test(residuals(lm_model_2019))

sh_2015<-shapiro.test(residuals(lm_model_2015))$p.value
sh_2016<-shapiro.test(residuals(lm_model_2016))$p.value
sh_2017<-shapiro.test(residuals(lm_model_2017))$p.value
sh_2018<-shapiro.test(residuals(lm_model_2018))$p.value
sh_2019<-shapiro.test(residuals(lm_model_2019))$p.value

x<- c((summary(lm_model_2015))$r.squared, (summary(lm_model_2016))$r.squared, 
(summary(lm_model_2017))$r.squared, (summary(lm_model_2018))$r.squared, (summary(lm_model_2019))$r.squared)

y<- c(sh_2015, sh_2016, sh_2017, sh_2018, sh_2019)

plot(x, y)


subset_data_2015 <- dati_2015[dati_2015$win_ratio != 0, ]
log_win_ratio <- log(subset_data_2015$win_ratio)
log_ADJOE <- log (subset_data_2015[, 3])
log_data_set_2015 <- cbind(subset_data_2015, log_win_ratio, log_ADJOE)



##now let's do the linear model but with the log variables (both or one of them)
lm_model_2015_log1 <- lm(win_ratio ~ log_ADJOE, data = log_data_set_2015)
lm_model_2015_log2 <- lm(log_win_ratio ~ ADJOE, data = log_data_set_2015)
lm_model_2015_log3 <- lm(log_win_ratio ~ log_ADJOE, data = log_data_set_2015)
# we cannot use this models because log of 0 is -inf and it gives problems


summary(lm_model_2015_log1)
shapiro.test(residuals(lm_model_2015_log1))
par(mfrow=c(2,2))
plot(lm_model_2015_log1)     


summary(lm_model_2015_log2)
shapiro.test(residuals(lm_model_2015_log2))
par(mfrow=c(2,2))
plot(lm_model_2015_log2)


summary(lm_model_2015_log3)
shapiro.test(residuals(lm_model_2015_log3))
par(mfrow=c(2,2))
plot(lm_model_2015_log3)


## torniamo al modello usuale

lm_model_2015 <- lm(win_ratio ~ ADJOE, data = dati_2015)
summary(lm_model_2015)
par(mfrow=c(2,2))
plot(lm_model_2015)

# confidence interval for beta1
conf_interval <- confint(lm_model_2015, level = 0.95)
print(conf_interval)

#confidence interval for r
cor_test_result <- cor.test(dati_2015$ADJOE, dati_2015$win_ratio, conf.level = 0.95)
print(cor_test_result$conf.int)

# > print(cor_test_result$conf.int)
# [1] 0.7376465 0.8195440


data_due_colonne<- data.frame(cbind(dati_2015$ADJOE, dati_2015$win_ratio))
lm_model_2015_due_colonne <- lm(X2 ~ X1, data = data_due_colonne)
summary(lm_model_2015_due_colonne)

#punto 2.i
# Create a new data frame with the fixed value of 'x'
new_data <- data.frame(X1=c(120, 110, 100, 90,80))


# Predict the mean of 'y' for the fixed value of 'x' with a 95% confidence interval
pred <- predict(lm_model_2015_due_colonne, newdata = new_data, interval = "confidence", level = 0.95)

print(pred)

#punto 2.j
# Create a new data frame with the fixed value of 'x'
new_data <- data.frame(X1=c(115,105,95,85))

# Predict the value 'y' for the fixed value of 'x' with a 95% confidence interval
pred <- predict(lm_model_2015_due_colonne, newdata = new_data, interval = "prediction", level = 0.95)

print(pred)




## plot della retta di regressione con confidence e prediction interval


library(ggplot2)

# Fit the linear model
lm_model_2015_due_colonne <- lm(X2 ~ X1, data = data_due_colonne)









##correzione del dataset


# Sostituzione del valore 2 a tutte le righe che hanno valore 0 nella colonna specifica
dati_2015$W[dati_2015$W == 0] <- 2

dati_2015$win_ratio<- (dati_2015$W/dati_2015$G)



# Fit linear regression model
lm_model_2015 <- lm(win_ratio ~ ADJOE, data = dati_2015)

# Check the current distribution of the response variable (win_ratio)
ggplot(dati_2015, aes(x = win_ratio)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black") +
  labs(title = "Histogram of win_ratio") +
  theme_minimal()

# Perform Box-Cox transformation
boxcox_lambda <- boxcox(lm_model_2015)

# Plot the profile likelihood for lambda
plot(boxcox_lambda, xlab = "lambda", ylab = "Profile log-likelihood")

# Determine the optimal lambda
lambda_opt <- boxcox_lambda$x[which.max(boxcox_lambda$y)]
cat("Optimal lambda:", lambda_opt, "\n")

# Transform the response variable using the optimal lambda
dati_2015$win_ratio_transformed <- ifelse(lambda_opt == 0, log(dati_2015$win_ratio), (dati_2015$win_ratio^lambda_opt - 1) / lambda_opt)

# Check the transformed distribution
ggplot(dati_2015, aes(x = win_ratio_transformed)) +
  geom_histogram(binwidth = 0.0005, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Box-Cox transformed win_ratio") +
  theme_minimal()

# Update the linear regression model with the transformed response variable
lm_model_2015_transformed <- lm(win_ratio_transformed ~ ADJOE, data = dati_2015)
residual_boxcox<- rstandard((lm_model_2015_transformed))
hist(residual_boxcox,breaks=50)
shapiro.test((residual_boxcox))

# Visualize the regression line
ggplot(dati_2015, aes(x = ADJOE, y = win_ratio_transformed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatterplot with Regression Line (Transformed)") +
  theme_minimal()

# Optionally, you can back-transform the predictions to the original scale
predictions_backtransformed <- ifelse(lambda_opt == 0, exp(predict(lm_model_2015_transformed)), ((lambda_opt * predict(lm_model_2015_transformed) + 1)^(1 / lambda_opt)))

# You can then compare the back-transformed predictions with the original response variable to assess the adequacy of the transformation.








