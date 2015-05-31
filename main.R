# Principal Component Analysis and Factor Analysis in R
library(dplyr)

# Read the data set
data.source <- read.csv("pca_gsp.csv")

# Define variables
data.pca <- select(mydata, -State)

# Descriptive statistics
summary(data.pca)
cor(data.pca)

# Principal component analysis
# We use cor = True because the data set hs different min and max
pca.result = princomp(data.pca, scores = TRUE, cor = TRUE)
summary(pca.result)

# Loadings of principal components
loadings(pca.result)
#pca1$loadings

# Scree plot of eigenvalues
plot(pca.result)
screeplot(pca.result, type="line", main="Scree Plot")

# Biplot of score variables
biplot(pca.result)

# Scores of the components
pca.result$scores[1:10,]

# Rotation
#varimax(pca.result$rotation)
#promax(pca.result$rotation)

# Factor analysis - different results from other softwares and no rotation
fa1 <- factanal(data.pca, factor=3)
fa1

fa2 <- factanal(data.pca, factor=3, rotation="varimax")
fa2

fa3 <- factanal(data.pca, factors=3, rotation="varimax", scores="regression")
fa3
