
# Change your working directory to your folder
setwd("/Users/darrenkarlsapalo/school/business-insights")
getwd()

# Read the CSV file
laptop_prices <- read.csv("dataset.csv")

# View just the top 5 rows of data
head(laptop_prices)

# Construct a lm or 'Linear Model' which is a regression
model <- lm(laptop_prices$price~laptop_prices$spec_rating+laptop_prices$Ram+laptop_prices$ROM+laptop_prices$display_size+laptop_prices$resolution_width+laptop_prices$resolution_height+laptop_prices$warranty)

# Display a summary of the model
summary(model)

# Scope
laptopprices_scope = list(
  lower=lm(laptop_prices$price ~ 1, data=laptop_prices), 
  upper=lm(laptop_prices$price~laptop_prices$spec_rating+laptop_prices$Ram+laptop_prices$ROM+laptop_prices$display_size+laptop_prices$resolution_width+laptop_prices$resolution_height+laptop_prices$warranty, data=laptop_prices)
  )

# Multiple linear regression
laptopprices_model_all <- lm(laptop_prices$price~laptop_prices$spec_rating+laptop_prices$Ram+laptop_prices$ROM+laptop_prices$display_size+laptop_prices$resolution_width+laptop_prices$resolution_height+laptop_prices$warranty, data=cars)

# Capture the summary report and store it into a file
summary_output_laptopprices_model_all <- summary(laptopprices_model_all)
beta_coefficients <- lm.beta(laptopprices_model_all)
sink("summary_output_laptopprices_model_all.txt")
print(summary_output_laptopprices_model_all)
print(beta_coefficients)
sink()

# Use step-wise algorithm to tweak the regression model and store performance results into a file
laptopprices_stepwise_model <- step(laptopprices_model_all, direction="both", scope=laptopprices_scope)
summary_output_laptopprices_stepwise_model <- summary(laptopprices_stepwise_model)
beta_coefficients <- lm.beta(laptopprices_stepwise_model)
sink("summary_output_laptopprices_stepwise_model.txt")
print(summary_output_laptopprices_stepwise_model)
print(beta_coefficients)
sink()
