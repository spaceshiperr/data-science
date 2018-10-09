# iris sepal scatter plot
pdf("scatter_plot1.pdf", height=6, width=6)
plot(iris$Sepal.Length, iris$Sepal.Width, col = iris$Species, xlab = 'Sepal Length', ylab = 'Sepal Width', main = 'Scatter plot for iris sepal data', pch = 20)
text(5, 4.2, "setosa", col = 'black')
text(5.3, 2.1, "versicolor", col = 'red')
text(7, 3.7, "virginica", col = 'green')
dev.off() 

# iris petal scatter plot
pdf("scatter_plot2.pdf", height=6, width=6)
plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species, xlab = 'Petal Length', ylab = 'Petal Width', main = 'Scatter plot for iris petal data', pch = 20)
text(1.5, 0.8, "setosa", col = 'black')
text(4, 0.8, "versicolor", col = 'red')
text(6, 1.3, "virginica", col = 'green')
dev.off()

# iris sepal length box-whisker plot
pdf("box_whisker_plot1.pdf", height=6, width=6)
boxplot(Sepal.Length ~ Species, col = 'coral', xlab = 'Species', ylab = 'Sepal Length', main = 'Box-whisker plot for iris sepal length', data = iris)
dev.off()

# iris sepal width box-whisker plot
pdf("box_whisker_plot2.pdf", height=6, width=6)
boxplot(Sepal.Width ~ Species, col = 'coral', xlab = 'Species', ylab = 'Sepal Width', main = 'Box-whisker plot for iris sepal width', data = iris)
dev.off()

# iris petal length box-whisker plot
pdf("box_whisker_plot3.pdf", height=6, width=6)
boxplot(Petal.Length ~ Species, col = 'coral', xlab = 'Species', ylab = 'Petal Length', main = 'Box-whisker plot for iris petal length', data = iris)
dev.off()

# iris petal width box-whisker plot
pdf("box_whisker_plot4.pdf", height=6, width=6)
boxplot(Petal.Width ~ Species, col = 'coral', xlab = 'Species', ylab = 'Petal Width', main = 'Box-whisker plot for iris petal width', data = iris)
dev.off()

# generate latex table from iris dataset
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

data(iris)
xtable(head(iris, 16))