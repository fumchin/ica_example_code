# package install  ---------------------------------
install.packages("gsignal")
install.packages("fastICA")
library(gsignal)
library(fastICA)

# create source  ---------------------------------
time <- seq(0, 8, length.out=1000)
s1 = sin(2 * time)  # Signal 1 : sinusoidal signal
s2 = sawtooth(2 * pi * time)  # Signal 3: saw tooth signal
s3 = sign(sin(3 * time))  # Signal 2 : square signal

S <- cbind(s1,s2,s3)
noise <- 0.2 * matrix( rnorm(3000,mean=0,sd=1),nrow = 1000)
# S_noise = S
S_noise <- S + noise
par(mfrow = c(3, 3))
plot(1:1000, S_noise[,1], type = "l",xlab = "S1", ylab = "")  
plot(1:1000, S_noise[,2], type = "l", xlab = "S2", ylab = "", main = "Source") 
plot(1:1000, S_noise[,3], type = "l", xlab = "S2", ylab = "") 

# create mixture matirx  --------------------------
A = rbind(c(1, 1, 1), c(0.5, 2.0, 1.0), c(1.5, 1.0, 2.0))
X = S_noise %*% t(A)
# par(mfcol = c(1, 3))
plot(1:1000, X[,1], type = "l",xlab = "X1", ylab = "")
plot(1:1000, X[,2], type = "l", xlab = "X2", ylab = "", main = "Mix")
plot(1:1000, X[,3], type = "l", xlab = "X3", ylab = "")

# Independent Component Analysis  --------------------------
ICA_result = fastICA(X, n.comp = 3)
S1_extracted = ICA_result$S[, 1]
S2_extracted = ICA_result$S[, 2]
S3_extracted = ICA_result$S[, 3]
# par(mfcol = c(1, 3))
plot(1:1000, S1_extracted, type = "l", xlab = "S'1", ylab = "")
plot(1:1000, S2_extracted, type = "l", xlab = "S'2", ylab = "", main = "ICA")
plot(1:1000, S3_extracted, type = "l", xlab = "S'3", ylab = "")

# Combine  --------------------------
par(mfrow = c(3, 1))
plot(1:1000, S_noise[,1], type = "l",xlab = "S", ylab = "", col='red') 
lines(S_noise[,2], col='blue')
lines(S_noise[,3], col='green')

plot(1:1000, X[,1], type = "l",xlab = "X", ylab = "", col='red') 
lines(X[,2], col='blue')
lines(X[,3], col='green')

plot(1:1000, S1_extracted, type = "l",xlab = "ICA", ylab = "", col='red') 
lines(S2_extracted, col='blue')
lines(S3_extracted, col='green')
