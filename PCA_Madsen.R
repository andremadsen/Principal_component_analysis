###############
# Principal Component Analysis [PCA] pipeline
###############


# Importing the 'iris' data set
library(datasets)
data(iris)      #load
head(iris)      #inspect
str(iris)       #note 'Species' factor variable is column 5


# PCA cannot handle NA values: a real-world dataset will require this cleaning
df1 <- iris[complete.cases(iris$Sepal.Length),]
df2 <- df1[complete.cases(df1$Sepal.Width),]
df3 <- df2[complete.cases(df2$Petal.Length),]
df4 <- df3[complete.cases(df3$Petal.Width),]


# Apply principal component analysis (PCA)
PCA.model <- prcomp(df4[, 1:4], scale = TRUE, center = TRUE, retx = TRUE)

summary(PCA.model)                       #PCA result [variance explained]
plot(PCA.model, type = "l")              #SCREE PLOT [discard PCs with Eigenvalue < 1.0]
PCA.model$sdev^2                         #EIGENVALUES [calculate numerical Eigenvalues for each PC]
biplot(PCA.model, scale = 0)             #BIPLOT [crude plot of the captured dataset variance]
PCA.model$rotation                       #LOADINGS [description of variable rotations/contribution for each PC]
df.pca <- cbind(df4, PCA.model$x[,1:4])  #NEW DATAFRAME WITH PC SCORES [for all 4 PCs]


# ggbiplot graphical representation of PCA
library(devtools)
install_github("vqv/ggbiplot", force = TRUE)
library(ggplot2)
library(ggbiplot)

# Basic ggbiplot model - notice horizontal variable vectors will determine PC1 score
ggbiplot(PCA.model, choices = c(1,2), obs.scale = 1, var.scale = 1, groups = df.pca$Species)

# Customized ggbiplot model
ggbiplot(PCA.model, choices = c(1,2), obs.scale = 1, var.scale = 1, groups = df.pca$Species, 
         varname.size = 5.3, varname.adjust = 0.5, ellipse = TRUE, ellipse.prob = 0.82, alpha = 1) +
    theme_bw() + scale_color_manual(values = c("red", "green", "darkblue"), labels = c("setosa", "versicolor", "virginica")) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + labs(title="") +
    guides(col = guide_legend(title = "Species")) + theme(legend.position = "top") +
    theme(text = element_text(size=12)) + theme(legend.text = element_text(size=12)) +
    theme(aspect.ratio=0.8) + theme(legend.margin=margin(t=0, r=0, b=0, l=0, unit="cm"))

str(iris)
a <- data.frame(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width, iris$Species)

# APPLY CENTER=TRUE & SCALE=TRUE TO NEW DATA
require(graphics)
c.fun<-function(a, center, scale) {
    return((a-center)/scale ) }

#Apply centering and scaling to transform "NewData" according to previous PCAmodel
TransformedNewData <- apply(a[, 1:4], MARGIN=1, FUN=c.fun, PCA.model$center, PCA.model$scale)
NewDataPCscores <- t(PCA.model$rotation) %*% TransformedNewData
NewDataPCscores2 <- data.frame(t(NewDataPCscores))
head(NewDataPCscores2)

df <- cbind(df.pca, NewDataPCscores2) # confirm PC-scores [from PCA] match the manually calculated PC-scores



# Write dataframe containing PC scores 
write.table(df.pca, "PCA_results.csv",
            na = " ",
            row.names = FALSE,
            col.names = TRUE,
            append = FALSE,
            sep = ",",
            dec = ".")





# ROC analyses to evaluate PCA clustering: ability to distinguish "versicolor" vs "virginica"
library(pROC)
df.pca$SpeciesVar <- as.factor(ifelse(df.pca$Species == "versicolor",1, ifelse(df.pca$Species == "virginica",2,NA)))

class <- ifelse(c$SpeciesVar == "1", "2", "NA")
score <- c$PC1  #try all PCs and petal/sepal width/length in sequence     
PROC1 <- roc(class,score)
plot(PROC1)
auc(PROC1)
coords(PROC1, "best", transpose=TRUE, ret=c("ppv", "npv", "accuracy", "threshold", "tp", "fp", "tn", "fn"))



# ppv = positive predictive value
# npv = negative predictive value
# tp  = % true positive classifications
# fp  = % false positive classifications
# tn  = % true negative classifications
# fn  = % false negative classifications
# accuracy = proportion of (TP+TN / TP+TN+FP+FN)
# threshold = THE VARIABLE VALUE CORRESPONDING TO THE OPTIMAL CUTOFF ON THE ROC CURVE (Youden index)

# ROC AUC score table: Petal.Length > Petal.Width > PC1 > Sepal.Length > PC2 > Sepal.Width > PC3 > PC4
