library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset
my_data <- read.csv("austin_weather.csv")
View(my_data)
# This will replace the missing values in the Events column with "clearday"
my_data$Events[is.na(my_data$Events)] <- "clearday"

my_data <- na.omit(my_data)
print(nrow(my_data))
# this prints all the column names in the dataset
variables <- names(my_data)
print(variables)

# Split Events into separate rows for multi-events
my_data_split <- my_data %>%
  # Replace " , " with a simpler delimiter (e.g., comma) if needed
  mutate(Events = gsub(" , ", ",", Events)) %>%
  # Separate rows by comma
  separate_rows(Events, sep = ",") %>%
  # Add a column of 1s to mark presence
  mutate(value = 1)

# Create dummy columns with 1s and 0s
my_data <- my_data_split %>%
  # Pivot to wide format, filling missing with 0
  pivot_wider(
    names_from = Events,
    values_from = value,
    values_fill = 0,
    names_prefix = "Event_"
  )



# List of columns to convert (all except Date and Events)
cols_to_convert <- setdiff(names(my_data), c("Date", "Events"))
# Convert specified columns to numeric
my_data <- my_data %>%
  mutate(across(all_of(cols_to_convert), as.numeric))



test_data <- data.frame(
                        TempHighF = my_data$TempHighF,
                        SeaLevelPressureLowInches = 
                          sqrt(my_data$SeaLevelPressureLowInches))

View(test_data)



# Create the ggplot
plot <- ggplot(my_data, aes(x = DewPointHighF, y = TempLowF))+
  geom_point()+
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste("TempHighF vs HumidityHighPercent"), x = "HumidityHighPercent", y = "TempHighF")+
    theme_classic()+ theme(text = element_text(size = 30))
plot



lm_linear <- lm(TempHighF ~ TempLowF, data = my_data)
plot(lm_linear$fitted.values, residuals(lm_linear))


# Fit the linear model
my_lm <-  lm(TempHighF ~
                DewPointLowF +
               HumidityLowPercent +
               SeaLevelPressureLowInches +
               VisibilityLowMiles +WindGustMPH + 
               PrecipitationSumInches + Event_Rain,
            data = my_data)

# Summary of the model
summary(my_lm)

# Fit the linear model
my_dp<-  lm(DewPointAvgF ~HumidityAvgPercent
                , 
             data = my_data)
summary(my_dp)

# Summary of the model
summary(my_lm)








# Fit the linear model
my_lm <-  lm(TempAvgF ~
              Events    , 
             data = my_data)

# Summary of the model
summary(my_lm)
poly





my_lm <-  lm(TempHighF ~
               TempLowF+DewPointHighF+HumidityHighPercent+
               SeaLevelPressureLowInches+ Event_Rain *Event_Thunderstorm , 
             data = my_data)

# Summary of the model
summary(my_lm)

lm_linear <- lm(TempHighF ~ DewPointHighF, data = my_data)
lm_quad <- lm(TempHighF ~ poly(DewPointHighF, 2), data = my_data)

summary(lm_linear)
summary(lm_quad)

plot(lm_linear$fitted.values, residuals(lm_linear))
plot(lm_quad$fitted.values, residuals(lm_quad))
