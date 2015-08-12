bodyfat <- read.table("/media/rahul/18DAA206DAA1E06C/My stuffs/NUS/Semester 2/BIS/Assignment 2/bodyfat.data", sep=",", col.names = c("bodyfatpercent","age", "weight", "height", "neck_circumference", "chest_circumference", "abdomen_2_circumference", "hip_circumference", "thigh_circumference", "knee_circumference", "ankle_circumference", "biceps_circumference","forearm_circumference", "wrist_circumference"), na.strings=c("?"), colClasses="numeric")

model <- lm(bodyfat$bodyfatpercent~bodyfat$age+bodyfat$weight+bodyfat$height+bodyfat$neck_circumference+bodyfat$chest_circumference+bodyfat$abdomen_2_circumference+bodyfat$hip_circumference+bodyfat$thigh_circumference+bodyfat$knee_circumference+bodyfat$ankle_circumference+bodyfat$biceps_circumference+bodyfat$forearm_circumference+bodyfat$wrist_circumference)	

sum <-summary(model)

model1 <- lm(formula = bodyfat$bodyfatpercent ~ bodyfat$neck_circumference + bodyfat$abdomen_2_circumference + bodyfat$forearm_circumference + bodyfat$wrist_circumference)

summary(model1)

mse <- function(sum) { 
    mse <- mean(sum$residuals^2)
    return(mse)
}

mse(sum)

mse(sum1)

