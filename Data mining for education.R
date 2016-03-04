# Load necessary packages for analysis
library(readr)
library(dplyr)
library(ggplot2)

# Read in data sets. Convert appropriate variables to factors. 
## Test scores
test <- read_csv('test_data.csv', na = c('', 'NULL'))

## Course enrollment
course <- read_csv('course.csv')

## Current student enrollment
student <- read_csv('student.csv')

## Longitudinal attendance data
attendance <- read_csv('attendance.csv')

# Coerce correct data types for each table
## test
str(test)
test$school <- as.factor(test$school)
test$grade <- as.factor(test$grade)
test$term <- as.factor(test$term)
test$subject <- as.factor(test$subject)

## course
str(course)
course$school <- as.factor(course$school)
course$course_name <- as.factor(course$course_name)
course$course_number <- as.factor(course$course_number)

## student
str(student)
student$school <- as.factor(student$school)

## Attendance: Holidays and non-student days were removed from the calendar. 
## Binary variables for "absent" and "tardy" were inserted to simplify later analysis. 
str(attendance)
attendance$school <- as.factor(attendance$school)
attendance$absent <- ifelse(attendance$AttendanceCode == 'UA' | 
                              attendance$AttendanceCode == 'EA' |
                              attendance$AttendanceCode == 'N/A' |
                              attendance$AttendanceCode == 'S', 1, 0)

attendance$tardy <- ifelse(attendance$AttendanceCode == 'ET' |
                             attendance$AttendanceCode == 'UT', 1, 0)

attendance <- attendance[!((attendance$date >= '2015-08-03' & 
                              attendance$date <= '2015-08-07') |
                             (attendance$date >= '2015-10-08' &
                                attendance$date <= '2015-10-12') |
                             (attendance$date >= '2015-11-23' & 
                                attendance$date <= '2015-11-27') |
                             (attendance$date >= '2015-12-21' &
                                attendance$date <= '2016-01-06') |
                             attendance$date == '2015-09-07' |
                             attendance$date == '2015-09-25' |
                             attendance$date == '2015-11-13' |
                             attendance$date == '2016-01-18'), ]

# avg_scores -- Average test scores by school, year, term, subject, and grade
avg_scores <- test %>%
  group_by(school, year, grade, subject, term) %>%
  summarize(mean(score))

# Write avg_scores to csv
write_csv(avg_scores, 'avg_scores.csv')

# Longitudinal -- Percent gain in test for each student group from Fall 2015 - Winter 2016
## Convert "grade" column to integer
avg_scores$grade <- as.integer(avg_scores$grade)

## Create last_grade column
avg_scores$last_grade <- avg_scores$grade - 1  

## Subset all Fall 2015 and Winter 2016 records from avg_scores
growth <- avg_scores[with(avg_scores, (year == 2015 & term == 'Fall') 
                       | (year == 2016 & term == 'Winter')), ]

## Initialize avg_growth column
growth$avg_growth <- NA
names(growth)[names(growth)=='mean(score)'] <- 'avg_score'

## Iterate over data sets to create percentage growth metric
i <- seq(1:8)
for (num in i) {
  growth$avg_growth[growth$grade == num & growth$year == 2016 & growth$subject == 'Mathematics'] <-
    (growth$avg_score[growth$grade == num & growth$year == 2016 & growth$subject == 'Mathematics'] - 
       growth$avg_score[growth$grade == num - 1 & growth$year == 2015 & growth$subject == 'Mathematics']) / 
    growth$avg_score[growth$grade == num - 1 & growth$year == 2015 & growth$subject == 'Mathematics'] * 100
}

for (num in i) {
  growth$avg_growth[growth$grade == num & growth$year == 2016 & growth$subject == 'Reading'] <-
    (growth$avg_score[growth$grade == num & growth$year == 2016 & growth$subject == 'Reading'] - 
       growth$avg_score[growth$grade == num - 1 & growth$year == 2015 & growth$subject == 'Reading']) / 
    growth$avg_score[growth$grade == num - 1 & growth$year == 2015 & growth$subject == 'Reading'] * 100
}

# Write growth to csv
write_csv(growth, 'growth.csv')

# Create data frame of each students' number of absences and tardies
absent_tardy <- attendance %>%
  group_by(studentId) %>%
  summarize(sum(absent) , sum(tardy))

# Rename the necessary columns for later data manipulation
names(absent_tardy)[names(absent_tardy)== 'sum(absent)'] <- 'total_absences'
names(absent_tardy)[names(absent_tardy)== 'sum(tardy)'] <- 'total_tardies'

# Merge attendance and test data
attendance_test <- merge(absent_tardy, test, by = 'studentId')

# Coerce to proper data type
amp$typical_growth <- as.integer(amp$typical_growth)

# Subset only students who have year-to-year data.
# Created a column for the difference in performance between 2015 and 2016
amp_full <- amp %>%
  group_by(studentId, subject, term) %>%
  filter(n_distinct(year) == 2) %>%
  arrange(year) %>%
  mutate(diff = lead(score) - score)

# Create a binary column for whether a student passed his or her target
amp_full$goal_met <- ifelse(amp_full$diff >= amp_full$typical_growth, 1, 0)

# Remove records that have incomplete information. Convert binary variable from previous
# step to proper data type
amp_full <- amp_full[is.na(amp_full$typical_growth) == FALSE, ]
amp_full$goal_met <- as.integer(amp_full$goal_met)

# Create join column in course using a regular expression
course$subject <- ifelse(grepl('Math', enrollment$course_name), 'Mathematics', 'Reading')

# Merge final data set
final_merge <- merge(amp_full, enrollment, by = c('studentId', 'subject'))

# Append to final data set the percentage of students meeting goals by teacher
final_merge <- final_merge %>%
  group_by(teacherId) %>%
  mutate(met_goal_by_teacher = as.numeric(sum(goal_met) / n() * 100))

# Create quintile columns for tardiness, absences, and student achievement by teacher
final_merge$met_goal_quintile <- ntile(final_merge$met_goal_by_teacher, 5)
final_merge$absence_quintile <- ntile(final_merge$total_absences, 5)
final_merge$tardy_quintile <- ntile(final_merge$total_tardies, 5)

# Write final_merge to csv
write_csv(final_merge, 'C:/Users/sackettj/Google Drive/Job Interviews/atest_full_teacher.csv')

# Create "exceeed" column as amount a student exceeded projected growth
final_merge$teacherId <- as.factor(final_merge$teacherId)
final_merge$exceed <- final_merge$diff - final_merge$typical_growth

# Visualizations for impact of teachers on growth
## Boxplot of percentage meeting growth by quintile
qplot(factor(met_goal_quintile), met_goal_by_teacher, data = final_merge, geom = 'boxplot', 
      xlab = 'Teacher quintiles', ylab = 'Percentage of students meeting growth',
      main = 'Teachers by percentage of students meeting growth goals')

## Boxplot of raw student scores by teacher quintile
qplot(factor(met_goal_quintile), score, data = final_merge, geom = 'boxplot', 
      xlab = 'Teacher quintiles', ylab = 'Average 2015 test scores',
      main = 'Teachers by average 2015 test score')

## Graph of growth versus last year's test scores. The color of each point represents the 
## quintile that student's teacher was in. 
qplot(score, diff, data = final_merge, geom = 'point',
      ylab = 'Gain/loss between 2015 and 2016', xlab = 'Average 2015 baseline test scores',
      main = 'Relationship between growth, achievement, and teachers', colour = as.factor(met_goal_quintile),
      alpha = I(1), size = I(4)) +
  facet_grid(subject ~ school)

## ANOVA and plots for absences/tardiness question
## Relationship between absolute growth and absense/tardiness
summary(aov(exceed ~ tardy_quintile, data = final_merge))
summary(aov(exceed ~ absence_quintile, data = final_merge))

## Boxplots
qplot(factor(tardy_quintile), exceed, data = final_merge, geom = 'boxplot',
      xlab = 'Tardy quintile', ylab = 'Exceeded projected test growth by',
      main = 'Tardiness and performance') + 
  facet_grid(subject ~ school)

qplot(factor(tardy_quintile), exceed, data = final_merge, geom = 'boxplot',
      xlab = 'Tardy quintile', ylab = 'Exceeded projected test growth by',
      main = 'Tardiness and performance') + 
  facet_grid(subject ~ school)

## Relationship between meeting goals and absence/tardiness
summary(aov(goal_met ~ total_absences, data = final_merge))
summary(aov(goal_met ~ total_tardies, data = final_merge))

## Boxplots
qplot(goal_met, total_absences, data = final_merge, geom = 'boxplot', 
      xlab = 'Met projected test growth', ylab = 'Total absences', main = 'Absences and Growth') + 
  facet_grid(subject ~ school)

qplot(goal_met, total_tardies, data = final_merge, geom = 'boxplot', 
      xlab = 'Met projected test growth', ylab = 'Total tardies', main = 'Tardiness and Growth') + 
  facet_grid(subject ~ school)


