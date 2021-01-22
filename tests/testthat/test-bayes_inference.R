test_that("multiplication works", {  # issue 15
  # 4.1.5 Example: TTHM in Tapwater"
  data(tapwater)
  # prior hyperparameters
  m_0 = 35; n_0 = 25;  s2_0 = 156.25; v_0 = n_0 - 1
  # sample summaries
  Y = tapwater$tthm
  ybar = mean(Y)
  s2 = var(Y)
  n = length(Y)
  # posterior hyperparamters
  n_n = n_0 + n
  m_n = (n*ybar + n_0*m_0)/n_n
  v_n = v_0 + n
  s2_n = ((n-1)*s2 + v_0*s2_0 + n_0*n*(m_0 - ybar)^2/n_n)/v_n
  ci = m_n + qt(c(0.025, 0.975), v_n)*sqrt(s2_n/n_n)
  out = bayes_inference(tthm, data=tapwater, prior="NG",
                  mu_0 = m_0, n_0=n_0, s_0 = sqrt(s2_0), v_0 = v_0,
                  stat="mean", type="ci", method="theoretical", 
                  show_res=TRUE, show_summ=TRUE, show_plot=FALSE)
  expect_equal(m_n, out$post_mean)
  expect_equal(ci, out$ci)
})
