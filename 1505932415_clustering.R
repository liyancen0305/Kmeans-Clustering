# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
  df <- scale(wine[-1])  
  
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <b- function(data, nc=15, seed=1234) {
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	for (i in 2:nc) {
		              set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)
	             }

		           plot(1:nc, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
	          }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

  # Because we want to have Low model complexity (lower number of clusters) but also high within-group homogeneity (lower within-group sum of squares)
  # the method suggest 3 or 4 clusters.
  # The plot indicates that there is a distinct drop in within groups sum of squares when moving from 1 to 3 clusters. After three clusters, this decrease 
  # drops off, suggesting that a 3-cluster solution may be a good fit to the data.
  # Hence, within groups sums of squares vs. the number of clusters extracted. The sharp decreases from 1 to 3 clusters (with little decrease after) suggest 
  # a 3-cluster solution.



# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

    library(NbClust)
    set.seed(1234)
    nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
    barplot(table(nc$Best.n[1,]),
    	          xlab="Numer of Clusters", ylab="Number of Criteria",
    		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
    # 14 of 24 criteria provided by the NbClust package suggest a 3-cluster solution. Note that not all 30 criteria can be calculated for every dataset.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )
    set.seed(1234)
    fit.km <- kmeans(df, 3, nstart=100) 
    fit.km$size
    fit.km$centers  
    aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
    
    ct.km <- table(wine$Type, fit.km$cluster)
    library(flexclust)
    randIndex (ct.km)
 # The adjusted Rand index provides a measure of the agreement between two partitions, adjusted for chance. It ranges from -1 (no agreement) to 1 (perfect agreement). Agreement between the wine varietal type and the cluster solution is 0.9. Not bad.

    
# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering.
#clusplot( ... )
    
    library("cluster")
  
    clusplot(pam(wine, k = 3))
    
    # In general, the higher is the percent, the less "information" about the data remain hidden from the graph. The point variabliyt is 57% precent,
    # so it should not be a bad clustering.


