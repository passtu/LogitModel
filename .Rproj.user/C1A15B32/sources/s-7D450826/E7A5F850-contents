churn_prob_function <- function(x, y){
  merged_data <- x
  CustID <- y
  if(CustID %in% merged_data$CustomerId){
    logistic_regression <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, data = merged_data, family = "binomial")
    merged_data$churn_prob <- predict(logistic_regression,merged_data,type = "response")
    result <- merged_data[CustomerId==CustID,list(CustomerId,churn_prob)]
    return(result)
  } else {
    print("Error")
  }
}
