#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter

install.packages("flextable")
library(flextable)

yrbss$Grade <- yrbss$grade
yrbss$Gender <- yrbss$gender
yrbss$Age <- yrbss$age
yrbss$Hispanic <- yrbss$hispanic 
yrbss$Race <- yrbss$race
yrbss$Height <- yrbss$height
yrbss$Weight <- yrbss$weight
yrbss$Helmet_12m <- yrbss$helmet_12m
yrbss$Text_While_Driving_30d <- yrbss$text_while_driving_30d
yrbss$Physically_Active_7d <- yrbss$physically_active_7d
yrbss$Hours_Tv_Per_School_Day <- yrbss$hours_tv_per_school_day
yrbss$Strength_Training_7d <- yrbss$strength_training_7d
yrbss$School_Night_Hours_Sleep <- yrbss$school_night_hours_sleep

yrbss <- yrbss[order(yrbss$Grade), ]

yrbss <- yrbss[, c("Grade", "Gender", "Age", "Hispanic", "Race", "Height", "Weight", "Helmet_12m", "Text_While_Driving_30d",
          "Physically_Active_7d", "Hours_Tv_Per_School_Day", "Strength_Training_7d", "School_Night_Hours_Sleep")]

z <- summarizor(
  yrbss[c("Grade", "Gender", "Age", "Hispanic", "Race", "Height", "Weight", "Helmet_12m", "Text_While_Driving_30d",
          "Physically_Active_7d", "Hours_Tv_Per_School_Day", "Strength_Training_7d", "School_Night_Hours_Sleep")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

yrbss$Grade <-factor(yrbss$Grade, levels=c('9', '10', '1', '12', 'other'))

aggregate(Physically_Active_7d ~ Grade + Gender, data=yrbss, mean, na.rm=TRUE) |>
  ggplot(aes(x=Grade, y=Physically_Active_7d, color=Gender, group=Gender)) +
  geom_line() +
  labs(x='Grade', y='Mean Number of Physically Active Days',
       color = 'Gender', title = 'Mean Number of Physically Active Days by Grade and Gender')
...


# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

yrbss$BMI <- yrbss$Weight / (yrbss$Height^2) 

g12f <- yrbss |>
  filter(Grade=='12', Gender=='female')

ggplot(g12f, aes(x=BMI, y=Physically_Active_7d)) + 
  geom_point(colour='red') + 
  labs(x='BMI', y='Physically Active Days in the Last 7 Days', 
       title='Physical Activity and BMI Among Grade 12 Female Students')

# Push your completed code to your GitHub repository
