# Clustering

# ------------------------------------------------------------------------------
# Data Munging (Step 1)
# ------------------------------------------------------------------------------

source('step_001_data_munging.R')

suppressPackageStartupMessages(library(h2o))
suppressPackageStartupMessages(library(plotly))


# ------------------------------------------------------------------------------
# K-Means
# ------------------------------------------------------------------------------

# Load H2O

h2o.init(nthreads = -1)

h_bat_avg = as.h2o(d_bat_avg)

# Import Data
features = setdiff(colnames(h_bat_avg), c("playerID", "nameFirst", "nameLast"))


model_km_bat = h2o.kmeans(training_frame = h_bat_avg,
                          x = features,
                          model_id = "h2o_kmeans",
                          nfolds = 5,
                          k = 10,
                          seed = 1234)

h_clusters = h2o.predict(model_km_bat, h_bat_avg)


model_pca_bat = h2o.prcomp(training_frame = h_bat_avg,
                           x = features,
                           transform = "STANDARDIZE",
                           pca_method = "GLRM",
                           use_all_factor_levels = TRUE,
                           k = 10,
                           seed = 1234)
print(model_pca_bat)

h_pca = h2o.predict(model_pca_bat, h_bat_avg)



d_clusters = as.data.frame(h_clusters)
colnames(d_clusters) = "cluster"

d_pca = as.data.frame(h_pca)


d_bat_clusters = data.frame(d_bat_avg[, 1:11],
                            clusters = d_clusters,
                            d_pca)
d_bat_clusters$cluster = as.factor(d_bat_clusters$cluster)



# Create plotly plot
p <- plot_ly(data = d_bat_clusters, x = ~PC2, y = ~PC3, color = ~cluster,
             type = "scatter", mode = "markers", marker = list(size = 10),
             text = ~paste(nameFirst, nameLast)) %>%
  layout(yaxis = list(title = "PCy"), xaxis = list(title = "PCx")) %>%
  layout(title = "Clusters of Baseball Players")
print(p)
