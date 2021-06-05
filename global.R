

model_cardio<-function(hd_data, age, sex, thalach){
  
  hd_data %>% mutate(hd = ifelse(class > 0, 1, 0))-> hd_data
  
  # recode sex using mutate function and save as hd_data
  hd_data %>% mutate(sex = factor(sex, levels = 0:1, labels = c("Female","Male")))-> hd_data
  
  
  
  model <- glm(data = hd_data, hd~age+sex+thalach, family="binomial")
  
  
  # tidy up the coefficient table
  tidy_m <- model %>% tidy()
  tidy_m
  
  # calculate OR
  tidy_m$OR <- exp(tidy_m$estimate)
  
  # calculate 95% CI and save as lower CI and upper CI
  tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96 * tidy_m$std.error)
  tidy_m$upper_CI <- exp(tidy_m$estimate + 1.96 * tidy_m$std.error)
  
  
  
  # get the predicted probability in our dataset using the predict() function
  pred_prob <- predict(model, hd_data, type = "response")
  
  # create a decision rule using probability 0.5 as cutoff and save the predicted decision into the main data frame
  hd_data$pred_hd <- ifelse(pred_prob>=.5,1,0)
  
  # create a newdata data frame to save a new case information
  newdata <- data.frame(age, sex, thalach)
  
  # predict probability for this new case and print out the predicted value
  p_new <- predict(model, newdata, type = "response")
  
  
  return(p_new)
  
  
  
}