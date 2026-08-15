library(testthat)

# ==============================================================================
# SECTION 1: FBI Dataset Tests (Chapter 1-3 Data Exploration)
# ==============================================================================
test_that("Q1a: FBI Dataset Import Verification [5 pts]", {
  # Confirms student read the file and stored it in the expected variable name
  expect_true(exists("fbi_data"), 
              label = "Could not find a dataframe named 'fbi_data'.")
  expect_s3_class(fbi_data, "data.frame")
  
  # Verifies integrity of the dataframe structure
  expect_gt(nrow(fbi_data), 0, 
            label = "The 'fbi_data' object appears to be completely empty.")
})

test_that("Q1b: FBI Sample Covariance Extraction [5 pts]", {
  expect_true(exists("fbi_cov_matrix"), 
              label = "Missing 'fbi_cov_matrix' covariance object.")
  
  # Asserts that the dimensions match a square matrix format
  expect_equal(nrow(fbi_cov_matrix), ncol(fbi_cov_matrix), 
               label = "Your calculated covariance layout is not a square matrix.")
})


# ==============================================================================
# SECTION 2: Men's Track Records Dataset Tests (Chapter 6 PCA)
# ==============================================================================
test_that("Q2a: Track Records PCA Structuring Check [5 pts]", {
  expect_true(exists("track_pca"), 
              label = "Could not find an evaluation object named 'track_pca'.")
  expect_s3_class(track_pca, "prcomp")
})

test_that("Q2b: PCA Standardization Scaling Check [5 pts]", {
  # Verifies the student enabled 'scale. = TRUE' inside prcomp()
  # Ensures the standard deviations of scaled variables equal 1
  expect_equal(track_pca$scale, TRUE, trues = TRUE,
               label = "Critical: You must scale the track features before extracting principal components.")
})


# ==============================================================================
# SECTION 3: Wine Quality Dataset Tests (Chapter 9 Clustering)
# ==============================================================================
test_that("Q3a: K-Means Cluster Assignment Map [5 pts]", {
  expect_true(exists("wine_clusters"), 
              label = "Missing a modeling object named 'wine_clusters'.")
  expect_s3_class(wine_clusters, "kmeans")
})

test_that("Q3b: Clustering Convergence Reproducibility [5 pts]", {
  # Checks if the student ran clustering targeting 3 wine categories
  expect_equal(wine_clusters$totss > 0, TRUE)
  expect_equal(length(unique(wine_clusters$cluster)), 3, 
               label = "Your k-means implementation did not generate exactly 3 cluster centers.")
})
