# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

#install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wine_scaled <- wine[, -1]
wine_scaled <- scale(wine, scale = TRUE)



# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(wine_scaled)


# Exercise 2:
#   * How many clusters does this method suggest? 
#  3 clusters

#   * Why does this method work? What's the intuition behind it?
# With K-means clusterings the aim to minimize the  within cluster sum of squares 
# while keeping the number of clusters as small and manageable as possible. The ideal 
# number of clusters can be chosen as the the within group sum of squares levels off,
# as the there should be differences between clusters at this point.  

#   * Look at the code for wssplot() and figure out how it works

#The code calculates and plots the sum of squares within the number of clusters


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_scaled, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#This method also suggests using 3 clusters


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km


fit.km <- kmeans(wine_scaled , centers = 3)


# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(fit.km$cluster, wine$Type)

#It predcits well on the second cluster but not so well on the 1st and 3rd clusters.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?


library(cluster)

clusplot(wine_scaled, fit.km$cluster)


#  There are three relatively distinct groups, although I have some
# concerns given the problem just above with the accuracy of prediciting both the 1st and
# 3rd clusters accurately.

