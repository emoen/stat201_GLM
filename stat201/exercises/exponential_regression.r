#https://www.statology.org/exponential-regression-in-r/

x=1:20
y=c(1, 3, 5, 7, 9, 12, 15, 19, 23, 28, 33, 38, 44, 50, 56, 64, 73, 84, 97, 113)

plot(x, y)

#fit the model
model <- lm(log(y)~ x)

#view the output of the model
summary(model)



#The overall F-value of the model is 204 and the corresponding p-value is extremely small (2.917e-11), which indicates that the model as a whole is useful.
#ln(y) = 0.9817 + 0.2041(x)
#y = 2.6689 * 1.2264^x
pred = 2.6689 * 1.2264^x

pred = 2.9689 * 1.2254^x

plot(x, y)
lines(pred)