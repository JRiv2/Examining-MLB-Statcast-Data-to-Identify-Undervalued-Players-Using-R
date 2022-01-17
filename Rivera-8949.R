###################################
# Stat 102B   Jordan Rivera    ID: 604988949
# Data Analysis Project
####################################

library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)

# Read in data
mlb <- read_csv("./Rivera-8949.csv")
attach(mlb)

##########################################################################
# Part II: The Data Set
##########################################################################

# Identify numeric variables
num_cols <- unlist(lapply(mlb, is.numeric))
numeric_mlb <- mlb[,num_cols]
numeric_mlb
dim(numeric_mlb)

mlb_means <- colMeans(numeric_mlb)
mlb_sds <- apply(numeric_mlb, 2, sd)
mlb_medians <- apply(numeric_mlb, 2, median)
mlb_iqrs <- apply(numeric_mlb, 2, IQR)
rbind(mlb_means, mlb_sds, mlb_medians, mlb_iqrs) # use this to fill out table in write-up

# Include Bar Plots for some variables
p1 <- ggplot(mlb, aes(x = Bats))+geom_bar()+ggtitle("Bats")
p1
p2 <- ggplot(mlb, aes(x = `All-Star`))+geom_bar()+ggtitle("All-Star")
p2
p3 <- ggplot(mlb, aes(x = Value))+geom_bar(orientation = "x")+ggtitle("Value")
p3
p4 <- ggplot(mlb, aes(x = Acquired))+geom_bar(orientation = "x")+ggtitle("Acquired")+theme(axis.text.x = element_text(angle = 90, size = 10))
p4

# Include histograms for some variables
p1 <- ggplot(mlb, aes(x = Salary))+geom_histogram(bins = 21)+ggtitle("Salary")
p2 <- ggplot(mlb, aes(x = WAR))+geom_histogram(bins = 21)+ggtitle("WAR")
p3 <- ggplot(mlb, aes(x = Hits))+geom_histogram(bins = 21)+ggtitle("Hits")
p4 <- ggplot(mlb, aes(x = Home_runs))+geom_histogram(bins = 21)+ggtitle("Home Runs")
p5 <- ggplot(mlb, aes(x = RBI))+geom_histogram(bins = 21)+ggtitle("RBI")
p6 <- ggplot(mlb, aes(x = SB))+geom_histogram(bins = 21)+ggtitle("Stolen Bases")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2) # Figure 2

p1 <- ggplot(mlb, aes(x = xBA))+geom_histogram(bins = 21)+ggtitle("Expected BA")
p2 <- ggplot(mlb, aes(x = xOBP))+geom_histogram(bins = 21)+ggtitle("Expected OBP")
p3 <- ggplot(mlb, aes(x = xwOBA))+geom_histogram(bins = 21)+ggtitle("Expected wOBA")
p4 <- ggplot(mlb, aes(x = Exit_velocity))+geom_histogram(bins = 21)+ggtitle("Average Exit Velocity")
p5 <- ggplot(mlb, aes(x = Launch_angle))+geom_histogram(bins = 21)+ggtitle("Average Launch Angle")
p6 <- ggplot(mlb, aes(x = Whiff_percent))+geom_histogram(bins = 21)+ggtitle("Whiff Percentage")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2) # Figure 3

# Include some boxplots for Figure 4
p1 <- boxplot(WAR, main = "WAR")
p2 <- boxplot(Salary, main = "Salary")
p3 <- boxplot(Age, main = "Age")


##########################################################################
# Part III: Cluster Analysis
##########################################################################
# Using kmeans algorithm

mlb_forcluster <- select(numeric_mlb,
                         "BA", "SLG", "OBP", "OPS", "ISO", 
                         "xBA", "xSLG", "wOBA", "xwOBA", "xOBP", "xISO", "Exit_velocity", 
                         "Launch_angle", "Sweet_spot_percent", "Barrel_percent", "Hard_hit_percent","GB_percent", "FB_percent", "LD_percent"
)

mlb_kmeans <- kmeans(mlb_forcluster,4,nstart=20)
#### NOTE: The clusters are exactly the same but the label numbers are different when I ran this code again, 
# so the naming convention doesn't match my paper. For example, Cluster 4 is called Cluster 1 in the paper.
mlb_kmeans 
cluster <- mlb_kmeans$cluster

mlb_kmeans$size # size of each cluster
mlb_kmeans$centers # cluster means

# cluster medians
apply(mlb_forcluster[which(cluster == 1),], 2, median)
apply(mlb_forcluster[which(cluster == 2),], 2, median)
apply(mlb_forcluster[which(cluster == 3),], 2, median)
apply(mlb_forcluster[which(cluster == 4),], 2, median)

# cluster sd
round(apply(mlb_forcluster[which(cluster == 1),], 2, sd), digits = 3)
round(apply(mlb_forcluster[which(cluster == 2),], 2, sd), digits = 3)
round(apply(mlb_forcluster[which(cluster == 3),], 2, sd), digits = 3)
round(apply(mlb_forcluster[which(cluster == 4),], 2, sd), digits = 3)


table(cluster, Value) #Table5
table(cluster, `All-Star`)
table(cluster, Bats)


#Plot Launch Angle vs Barrel Percentage
values <- factor(mlb$Value)

plot(mlb$Barrel_percent, mlb$Launch_angle,
     col=c("red","blue", "green", "orange")[unclass(cluster)], 
     pch=c(21,22,23,24,25)[unclass(values)], main="K-means of mlb data",xlab="Barrel Percentage", ylab="Launch Angle", xlim = c(0, 33))
legend("topright",c("All-Star","MVP","Replacement","Starter", "Sub"),pch=c(21,22,23,24,25), cex = .7)
legend("bottomright",c("cluster 1","cluster 2","cluster 3", "cluster 4"),pch=c
       ("R","B","G","O"),col=c("red","blue","green","orange"))

#Plot GB% vs Exit Velocity
plot(mlb$GB_percent, mlb$Exit_velocity,
     col=c("red","blue", "green", "orange", "black")[unclass(cluster)], 
     pch=c(21,22,23,24,25)[unclass(values)], main="K-means of mlb data",xlab="GB Percentage", ylab="Exit Velocity", xlim = c(25, 70))
legend("topright",c("All-Star","MVP","Replacement","Starter", "Sub"),pch=c(21,22,23,24,25), cex = .7)
legend("bottomleft",c("cluster 1","cluster 2","cluster 3", "cluster 4"),pch=c
       ("R","B","G","O"),col=c("red","blue","green","orange"), cex = .8)



# Nearest neighbor
mlb_cs <- scale(mlb_forcluster, scale = T) # first scale and center
mlb_dist <- dist(mlb_cs)
nn <- hclust(mlb_dist,method="single",members=NULL)
plot(nn)
nn_cluster <- cutree(nn, k = 4)
nn_cluster

table(nn_cluster)


# Furthest neighbor
fn <- hclust(mlb_dist,method="complete",members=NULL)
plot(fn)
fn_cluster <- cutree(fn, k = 4)
fn_cluster
table(fn_cluster)

##########################################################################
# Part IV: Principal Components
##########################################################################

head(mlb)
head(numeric_mlb)

mlb_forPC <- numeric_mlb[,-c(2,3)] # take out irrelevant variables
head(mlb_forPC)

X.cs <- scale(mlb_forPC,scale=T)
head(X.cs)
dim(X.cs)

# Check correlation between variables
cor(X.cs)
round(cor(X.cs[1:10, 1:10]), 3)
round(cor(X.cs[32:41, 32:41]), 3)

pairs(X.cs[, c(3:6, 8:10)],lower.panel=NULL, main="pairs plot of mlb data\n (standard deviation units)")

# Sample Variance-Covariance Matrix
S <- var(X.cs)
S

# Obtain eigenvalues of Variance-Cov Matrix
eigen(S)
eigenvalues <- eigen(S)$values
eigenvalues

## compute what percentage of the sum of eigenvalues is each eigenvalue
sum(eigenvalues)
100*eigenvalues/sum(eigenvalues)

cumsum(100*eigenvalues/sum(eigenvalues))

# Eigenvectors of Variancce-Cov Matrix
V <- eigen(S)$vectors
V

## check that eigenvectors are orthonormal
## should be 0 in the off diagonals and 1 in the diagonals when you do cross-product 
# Off-diagonals represent pairwise inner products and diagonals represent norm^2. 

t(V)%*%V
# alternatively
round(crossprod(V,V))

#Eigenvectors are orthonormal

# Display eigenvectors in relation to data variables
V.r <- round(V,3)
rownames(V.r)=colnames(mlb_forPC)
colnames(V.r)=c("V1", "V2", "V3","V4", "V5", "V6", "V7", "V8", "V9", "V10",
                "V11", "V12", "V13","V14", "V15", "V16", "V17", "V18", "V19", "V20",
                "V21", "V22", "V23","V24", "V25", "V26", "V27", "V28", "V29", "V30",
                "V31", "V32", "V33","V34", "V35", "V36", "V37", "V38", "V39", "V40", "V41")
V.r

# Obtain the principal components 

## Note: X.tilde is the PC matrix 
X.tilde <- X.cs %*% V  # X.tilde are the PC
colnames(X.tilde)=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10",
                    "PC11","PC12","PC13","PC14","PC15","PC16","PC17","PC18","PC19","PC20",
                    "PC21","PC22","PC23","PC24","PC25","PC26","PC27","PC28","PC29","PC30",
                    "PC31","PC32","PC33","PC34","PC35","PC36","PC37","PC38","PC39","PC40","PC41")
head(X.tilde)
head(mlb)
#######Check that the PC are orthogonal

round(t(X.tilde)%*%X.tilde,3)

#### Check the variance-covariance of the PC 
round(var(X.tilde),3)

# Pairwise plot of the PCs
pairs(X.tilde[, 1:7],lower.panel=NULL,main="Pairs plot of PC of mlb data.\n Same scale in axes to see variability")

# Biplot
biplot(princomp(X.cs),
       main="Biplot of mlb data w.r.t. first two principal components" )

# Study correlation of PCs with data variables
transform.matrix=diag(sqrt(eigenvalues))
loading.like.coefficients=V%*%transform.matrix
loading.like.coefficients

# Notice that this is the same as 

t(cor(X.tilde,X.cs))

cors <- as.data.frame(round(t(cor(X.tilde[,1:10], X.cs)), 3)) # for presentation purposes
cors


##########################################################################
# Part V: Principal Components Regression
##########################################################################

# Normalize PCs

norms <- apply(X.tilde, 2, function(x){ # find the norm for each PC
  norm(x, type = "2")
})
PC_normalized <- scale(X.tilde, center = F, scale = norms) # divide each PC by its norm
head(PC_normalized)

# Make new data matrix with DV scaled WAR and IVs that are normalized PCs
data_new <- as.data.frame(cbind(scale(mlb$WAR), PC_normalized))
colnames(data_new)[1] <- "WAR"
head(data_new)

# Split data_new into training and testing data
set.seed(22)
test_coords <- sample(nrow(data_new), 10)
#mlb[test_coords,]

test_data <- data_new[test_coords, ]
train_data <- data_new[-test_coords,]
dim(data_new)
dim(test_data)
dim(train_data)

# Check correlation with dependent variable
correlations <- cor(data_new$WAR, PC_normalized)
correlations
round(correlations, 5)

order(abs(correlations), decreasing = T) # order PCs by correlation with DV

# Regression using Train data
m <- lm(WAR ~ PC1 + PC5 + PC7 + PC2 + PC21 + PC26 + PC20 - 1, data = train_data)
summary(m)

rmse <- sqrt(sum((m$residuals)^2) / nrow(train_data))
rmse

plot(train_data$WAR, resid(m),main="Residual plot\n Principal Component Regression")
abline(h=0)

hist(resid(m), main="Histogram of Residuals", breaks = 12)

# Predict values for Test data
y.hat <- predict(m, data = train_data, newdata = test_data)
y.hat
test_data$WAR
test_data$WAR - y.hat

plot(1:10, test_data$WAR - y.hat, ylim = c(-1,1), main = "Residual plot for Testing Data", xlab = "Observation", ylab = "Residual")
abline(h=0)

# Calculate root mean square error
rmse <- sqrt(sum((test_data$WAR - y.hat)^2) / nrow(test_data))
rmse



##########################################################################
# Part VI: MLE
##########################################################################
# Make data > 0

data_new$WARadj <- data_new$WAR - min(data_new$WAR) + 0.01

x <- c(seq(0,max(data_new$WARadj),by=0.1))

hist(data_new$WAR, probability = T, xlab = "WAR_cs", main = "Histogram of WAR (Centered and Scaled)")

hist(data_new$WARadj, probability = T, xlab = "WAR (adjusted)", main = "Histogram of WAR with Log Normal Distribution\n mu = 0.45, sigma = 0.6")
points(x,dlnorm(x, meanlog=0.45, sdlog=0.6, log=FALSE), col="brown", type="o", pch=25, bg="brown")

loglike <- function(p, y, n){
  a= -sum(log(y)) -n*log(p[2])-(1/(2*(p[2])^2 ))*sum((log(y)-p[1])^2)
  a = -a 
}

n <- nrow(data_new)
minim <- nlm(loglike, p=c(0.45, 0.6), data_new$WARadj, n, hessian=TRUE) # p = initial values of maroon curve
minim
minim$estimate 
mle.estimate=minim$estimate

gradient= minim$gradient #gradient at the optimum 
gradient
I=minim$hessian  # I= negative of the Hessian matrix
I
Big.Sigma = solve(I)   #variance covariance matrix 
s.e= sqrt(diag(Big.Sigma)) #standard error of MLE estimate
s.e
hist(data_new$WARadj, prob=T, main="Histogram of WAR with Log Normal Distribution\n mu = 0.402, sigma = 0.622", xlab = "WAR_adj", ylim = c(0, 0.6))
points(x,dlnorm(x, meanlog=minim$estimate[1],
                sdlog=minim$estimate[2], log=FALSE), col="red", type="o", pch=21, bg="red")

### Confidence intervals ###
CI_mu <- minim$estimate[1] + c(-1,1)*qnorm(0.975)*sqrt(diag(solve(minim$hessian)))[1]
CI_mu
CI_sigma <- minim$estimate[2] + c(-1,1)*qnorm(0.975)*sqrt(diag(solve(minim$hessian)))[2]
CI_sigma

# Probabilities for Predicted Values
adj_preds <- y.hat - min(data_new$WAR) + 0.01
adj_preds
plnorm(adj_preds, meanlog = minim$estimate[1], sdlog=minim$estimate[2])
plnorm(adj_preds, meanlog = minim$estimate[1], sdlog=minim$estimate[2], lower.tail = F)

# Probs for Actual Values
plnorm(test_data$WAR - min(data_new$WAR) + 0.01, meanlog = minim$estimate[1], sdlog=minim$estimate[2])

##########################################################################
# Part VII: Conclusions
##########################################################################

# find undervalued players using cluster analysis results
best_cluster <- mlb[cluster==4, 1:10] # all MLB players in top tier cluster
head(best_cluster)
undervalued <- best_cluster[best_cluster$Salary <1000000 & best_cluster$`All-Star` == "No", ] # include players making less than 1 mil and not all-star
head(undervalued)


# find undervalued players using PC Regression results
mlb[as.numeric(rownames(train_data[m$residuals < -0.9, ])), 1:10]

