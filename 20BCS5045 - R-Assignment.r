# Load the dataset
data(mtcars)

# Perform multiple linear regression
model <- lm(mpg ~ cyl + disp + hp + wt + qsec, data = mtcars)

# Summary of the regression model
summary(model)

# Predicting using the regression model
predictions <- predict(model, newdata = mtcars)

# Visualizing the results
plot(predictions, mtcars$mpg)
abline(lm(mtcars$mpg ~ predictions), col = "red")

# Evaluating the model
rmse <- sqrt(mean((predictions - mtcars$mpg)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))
