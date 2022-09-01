data <- cars
summary(data)
str(data)
plot(data, xlab = "Speed (mph)", ylab = "Stopping distance (ft)", las = 1)
lines(lowess(data$speed, data$dist, f = 2/3, iter = 3), col = "red")
T1=Sys.time()
reg=lm(speed~dist,data)
T2=Sys.time()
T=T2-T1
print(T)
summary(reg)
plot(data, xlab = "Speed (mph)", ylab = "Stopping distance (ft)", las = 1, log = "xy")
lines(lowess(data$speed, data$dist, f = 2/3, iter = 3), col = "red")
T1=Sys.time()
reg=lm(log(speed)~log(dist),data)
T2=Sys.time()
T=T2-T1
print(T)
summary(reg)
data <- cars
scale (data)
T1=Sys.time()
reg=lm(speed~dist,data)
T2=Sys.time()
T=T2-T1
print(T)
summary(reg)

data_2<- cars
min_speed = min(data_2$speed)
max_speed = max(data_2$speed)
min_dist = min(data_2$dist)
max_dist = max(data_2$dist)
data_5 = transform(data_2, speed = (speed - min_speed)/(max_speed - min_speed), dist = (dist - min_dist)/(max_dist - min_dist))
data_5
T1=Sys.time()
reg=lm(speed~dist,data_5)
T2=Sys.time()
T=T2-T1
print(T)
summary(reg)
