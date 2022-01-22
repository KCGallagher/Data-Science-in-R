# k-means clustering algorithm

choose_random_centroids <- function(data, k=3) {
  # Samples k data points (without replacement) to be the initial centroids
  indices <- 1:nrow(data)
  sample_i <- sample(indices, k)
  return(data[sample_i, ])
}

euclidean_dist <- function(x1, x2) {
  # Returns Euclidean distance between two points.
  return(sqrt(sum((x1 - x2)^2)))
}

euclidean_dist_row <- function(x, centroid) {
  # Distance between each row (coresponding to a data point) and a centroid
  return(map_dbl(seq(1, nrow(x), 1), ~euclidean_dist(centroid, x[., ])))
}

euclidean_dist_centroids <- function(x, centroids) {
  # Find the distance between each row in a matrix and a list of centroids. 
  # Return a matrix of distances, with one row per datum and column per centroid.
  m_dist <- matrix(nrow = nrow(x), ncol = nrow(centroids))
  for(i in 1:nrow(centroids))
    m_dist[, i] <- euclidean_dist_row(x, centroids[i, ])
  return(m_dist)
}

cluster_points <- function(x, centroids) {
  # Assign each data point to a cluster according to its nearest centroid. 
  # Return a list of centroid ids: one per each data point.
  m_dist <- euclidean_dist_centroids(x, centroids)
  cluster_id <- vector(length = nrow(m_dist))
  for(i in seq_along(cluster_id))
    cluster_id[i] <- which.min(m_dist[i, ])
  return(cluster_id)
}

recalculate_centroids <- function(cluster_ids, x) {
  # Update centroids using the points which have been assigned to each cluster.
  df <- x %>% 
    as.data.frame() %>% 
    mutate(cluster=cluster_ids)
  output <- df %>% 
    group_by(cluster) %>% 
    summarise_all(.funs=mean) %>% 
    select(-cluster)
  return(output)
}

run_kmeans <- function(x, k, niter=50) {
  # Run k-means clustering for given iteration number
  for(i in 1:niter) {
    if(i == 1)
      centroids <- choose_random_centroids(x, k)
    else
      centroids <- recalculate_centroids(cluster_ids, x)
    cluster_ids <- cluster_points(x, centroids)
  }
  return(cluster_ids)
}