library(knitr)
library(ggplot2)
library(datasets)

# clear environment variable
rm(list=ls(all=TRUE))

# Keeping the original data
mtcars.orig <- mtcars

# Convert the categorical variables into factors
# We don't transform vs and am as factor because they only have a 2-state value
mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)


# Take a look at what the datasets consists of
head(mtcars)

# exploring the relationship between variables

cor_mtcars <- round(cor(mtcars.orig), 2)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cor_mtcars)

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri)
melted_cormat <- na.omit(melted_cormat)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value*100), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# relationship between by exploring miles per gallon (MPG)
abs_cor_MPG <- abs(cor_mtcars)[1,2:11]
abs_cor_MPG <- abs_cor_MPG[order(abs_cor_MPG,decreasing = TRUE)]
abs_cor_MPG


# consumption distribution (or MPG distribution) by transmission system 
par(mfrow = c(1, 1))
boxplot(mpg~am, data = mtcars,
col = c("blue", "red"),
xlab = "Transmission",
ylab = "Miles per Gallon",
main = "MPG by Transmission Type",
names= c("automatic","manual"))

# Two samples t-test on the MPG parameter distinguishing auto vs manual systems.
auto <-mtcars.orig[mtcars.orig$am==0,c("mpg")] 
manual <- mtcars.orig[mtcars.orig$am==1,c("mpg")]
t.test(mtcars$mpg ~ mtcars$am)

# model only based on the am parameter
mpg_am_model <- lm(mpg~am, mtcars)
sum_mpg_am_model <- summary(mpg_am_model)

# R-squared (mpg~am)
round(100*sum_mpg_am_model$r.squared,2)
# R-squared adjusted (mpg~am)
round(100*sum_mpg_am_model$adj.r.squared,2)

# Looking for the best predictive model
mpg_all_model <- lm(mpg~., mtcars)
best_model <- step(mpg_all_model, trace=0, k=2)
sum_best <- summary(best_model)
summary(best_model)

# R-squared adjusted of our best model
round(100*sum_best$adj.r.squared,2)

# Exploring the predictors using box plot
# Plotting the box-plot into 2 * 2 matrix
par(mfrow=c(2, 2))
# Impact of Transmission on the Fuel consumption
boxplot(mtcars$mpg ~ mtcars$am, xlab="Transmission")
# Impact of weight on the Fuel consumption
boxplot(mtcars$mpg ~ mtcars$wt, xlab="Weight")
# Impact of Horsepower on the Fuel consumption
boxplot(mtcars$mpg ~ mtcars$hp, xlab = "Horesepower")
# Impact of Cylinder on the Fuel consumption
boxplot(mtcars$mpg ~ mtcars$cyl, xlab="Cylinder")

# Residual analysis
par(mfrow=c(2,2))
plot(best_model)
