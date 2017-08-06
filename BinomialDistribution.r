# Load ggplot2 for the graphs
library(ggplot2)

# Set the main parameters, the two probabilities you're interested
# in (p=0.5 and p=0.1), and the number of trials (n=30)
p1 <- 0.3
p2 <- 0.5
p3 <- 0.8 
n<-60

# Generate out x, which in this case it's the numbers 1 to 20
#x <- seq.int(60)
x<-0:60
# Calculate the probability density given for each of the two
# probability values, with x and n as defined above
y1 <- dbinom(x, n, p1)
y2 <- dbinom(x, n, p2)
y3 <- dbinom(x, n, p3)

#summary(y1)
#summary(y2)

# Put the data in a data frame in order to plot it with ggplot2
df <- data.frame(x, y1, y2, y3)

# Create the graph with both probability densities

# get the binomial distribution for p = 0.3, and plot
y1 <- dbinom(x,n,p1)

ggplot(df) +
  geom_point(aes(x, y1, color = "Green")) +
  geom_smooth()+ scale_x_continuous(name = "Trial n=60") +
  scale_y_continuous(name = "Probabilities")+
  scale_color_manual("Color", values = "darkgreen") + ggtitle("Distribution when p=0.3")

#gets the first quartile of the binomial distribution for p = 0.3
q1_1 <- qbinom(0.25,n,p1)
print(q1_1)
# mean value of the binomial distribution for p = 0.3
mean1 <- n*p1
print(mean1)
# since mean = median
median1 <- mean1
print(median1)
# standard deviation value of the binomial distribution for p = 0.3
std1 <- sqrt((mean1*(1-p1)))
print(std1)
# gets the third quartile of the binomial distribution for p = 0.3
q3_1 <- qbinom(0.75,n,p1)
print(q3_1)

# get the binomial distribution for p = 0.5, and plot
y2 <- dbinom(x,n,p2)

ggplot(df) +
  geom_point(aes(x, y2, color = 'purple')) +
  geom_smooth()+ scale_x_continuous(name = "Trial n=60") +
  scale_y_continuous(name = "Probabilities")  +
  scale_color_manual("Color", values = "purple") + ggtitle("Distribution when p=0.5")

#gets the first quartile of the binomial distribution for p = 0.5
q1_2 <- qbinom(0.25,n,p2)
print(q1_2)
# mean value of the binomial distribution for p = 0.5
mean2 <- n*p2
print(mean2)
# since mean = median
median2 <- mean2
print(median2)
# standard deviation value of the binomial distribution for p = 0.5
std2 <- sqrt((mean2*(1-p2)))
print(std2)
# gets the third quartile of the binomial distribution for p = 0.5
q3_2 <- qbinom(0.75,n,p2)
print(q3_2)


# get the binomial distribution for p = 0.8, and plot
y3 <- dbinom(x,n,p3)

ggplot(df) +
  geom_point(aes(x, y3, color = 'blue')) +
  geom_smooth()+ scale_x_continuous(name = "Trial n=60") +
  scale_y_continuous(name = "Probabilities")+
    scale_color_manual("Color", values = "black") + ggtitle("Distribution when p=0.8")

#gets the first quartile of the binomial distribution for p = 0.8
q1_3 <- qbinom(0.25,n,p3)
print(q1_3)
# mean value of the binomial distribution for p = 0.8
mean3 <- n*p3
print(mean3)
# since mean = median
median3 <- mean3
print(median3)
# standard deviation value of the binomial distribution for p = 0.8
std3 <- sqrt((mean3*(1-p3)))
print(std3)
# gets the third quartile of the binomial distribution for p = 0.8
q3_3 <- qbinom(0.75,n,p3)
print(q3_3)


#Plot with all probabilities
ggplot(df) +
  geom_point(aes(x, y1, color = "blue")) +
  geom_point(aes(x, y2, color = 'green')) +
  geom_point(aes(x, y3, color = 'red'))+ geom_smooth()+ scale_x_continuous(name = "Trial n=60") +
  scale_y_continuous(name = "Probabilities")+
  scale_color_manual("Color", values = c("darkgreen","purple","black")) 


# groups the values into probabilities
values1 <- c(q1_1,mean1,median1,std1,q3_1)
probs1 <- c(p1,p1,p1,p1,p1)
values2 <- c(q1_2,mean2,median2,std2,q3_2)
probs2 <- c(p2,p2,p2,p2,p2)
values3 <- c(q1_3,mean3,median3,std3,q3_3)
probs3 <- c(p3,p3,p3,p3,p3)
# regroup into one set of values, and one set of probabilities
values <- c(values1, values2, values3)
probs <- c(probs1,probs2,probs3)
# creates a data frame for these values and probabilities
data_frame <- data.frame(values,probs)
# print(data_frame)

# vertical box plotting of the data_frame


qplot(as.factor(probs), values, data=data_frame, geom=c("boxplot", "jitter"), 
      fill=probs, main="Q1, Mean, Median, SD, Q3 vs P",
      xlab="Probabilities", ylab="Values (Q1, Mean, Median, SD, Q3)")
