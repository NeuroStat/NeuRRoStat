library(dplyr)
context("Weighted Average - Univariate MA")

# Start Data: random t-values from 5 studies
test_data <- data.frame(tVal = c(1,3,3,5,7),
                        nsub = c(10,12,14,16,18))

test_that("g, var(g), tau and weighted average are correct", {
  # Calculate g and variance using functions hedgeG and varHedge
  heG <- mutate(test_data,
                g = hedgeG(t = tVal, N = nsub),
                varG = varHedge(g = g, N = nsub))

  # Between study
  tauEs <- summarise(heG, tau = tau(Y = g, W = (1/varG), k = 5))

  # Weighted average
  wAvg <- cbind(heG, tauEs) %>% mutate(weights = 1/(varG + tau)) %>%
    summarise(wAvg = wMean(Y = g, W = weights))

  # Now test the mean of g, var(g), tau and weighted average
  expect_equal(round(mean(heG$g),3), 0.922)
  expect_equal(round(mean(heG$varG),3), 0.115)
  expect_equal(unlist(round(tauEs,3)), c(tau = 0.110))
  expect_equal(unlist(round(wAvg,3)), c(wAvg = 0.903))
})




