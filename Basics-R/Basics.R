heart <- read.table("processed.va.data", sep=",", col.names = c("age", "sex", "chest_pain_type", "resting_blod_pressure", "serum_cholestoral", "fasting_blood_sugar_greater_than_120", "resting_electrocardiograph", "maximum_heart_rate_achieved", "exercise_induced_angina", "stdepression_induced_by_exercise_related_to_rest", "slope_of_peak_exercise", "flourosopy_colored_vessels", "thal", "heart_disease_diagnosis"), na.strings=c("?"), colClasses="numeric")

sum(is.na(df$attribute))

sum(is.na(df$age))
sum(is.na(df$sex))
sum(is.na(df$chest_pain_type))
sum(is.na(df$resting_blood_pressure))
sum(is.na(df$serum_cholestrol))
sum(is.na(df$fating_blood_sugar_greater_than_120))
sum(is.na(df$resting_electrocardiograph))
sum(is.na(df$maximum_heart_rate_achieved))
sum(is.na(df$exercise_induced_angina))
sum(is.na(df$stdepression_induced_by_exercise_related_to_rest))
sum(is.na(df$slope_of_peak_exercise))
sum(is.na(df$flourosopy_colored_vessels))
sum(is.na(df$thal))
sum(is.na(df$heart_disease_diagnosis))
	
x <- data.frame(heart$age, heart$sex, heart$chest_pain_type,
heart$resting_electrocardiograph, heart$heart_disease_diagnosis)
y <- data.frame(heart$age, heart$sex, heart$chest_pain_type,
heart$resting_electrocardiograph, heart$heart_disease_diagnosis)
cor(x,y)


