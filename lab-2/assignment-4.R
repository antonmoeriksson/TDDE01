# Lab 2 assignment IV
# Anton Mo Eriksson
# anter491
# 2017-12-13

library(fastICA)


set.seed(12345)
# ---------------------- I ----------------------- 

setwd("/home/ame/git/tdde01/TDDE01/lab-2")

NITspectra = read.csv2("NIRSpectra.csv")

result = prcomp(NITspectra[, -ncol(NITspectra)])

lambda = result$sdev^2

sprintf("%2.3f", lambda / sum(lambda) * 100)

screeplot(result)
barplot(lambda / sum(lambda) * 100, xlab = "Component", ylab = "% of of variation",
        xlim = c(0,10), ylim = c(0,100))

plot(result$x[, 1], result$x[, 2], xlab = "PC1", ylab = "PC2",
     col = "darkblue", main = "Score of PC1 vs PC2")

# ---------------------- II ----------------------

U = loadings(result)
plot(U[, 1], main = "Trace Plote of PC1", ylab = "PC1", col = "green")
plot(U[, 2], main = "Trace Plote of PC1", ylab = "PC2", col = "red")


# ---------------------- III ---------------------
# ---------------------- a) ---------------------

ICA = fastICA::fastICA(data.frame(NITspectra[, -ncol(NITspectra)]), n.comp = 2)

w_prime = ICA$K %*% ICA$W

plot(w_prime[,1], main = "Trace plot of W'", col = "blue", ylab = "First column of W'")
plot(w_prime[,2], main = "Trace plot of W'", col = "blue", ylab = "First column of W'")


# ---------------------- III ---------------------
# ---------------------- b) ----------------------

plot(ICA$S[, 2], ICA$S[, 1], main = "Trace Plote of PC1", col = "darkblue")


# ---------------------- IV ----------------------
library(pls)

PCR = pcr(Viscosity ~., data= NITspectra, validation="CV")
summary(PCR)
validationplot(PCR, val.type="MSEP")
