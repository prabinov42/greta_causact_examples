library(tidyverse)
library(causact)

set.seed(2023)
n_subjects <- 200

df <- tibble(id=1:(n_subjects)) %>%
  mutate(
    cond = sample(c('ctrl','trtmt'),nrow(.), replace=TRUE) ,
    disc_cov = sample(c('a','b','c'),nrow(.), replace=TRUE),
    cont_cov = rnorm(nrow(.), 0,1),
    trials = sample(5:25, nrow(.), replace=TRUE),
    lin_pred = 0.2*cont_cov+0.3*case_when(disc_cov=='a'~1,  disc_cov=='b'~0,   disc_cov=='c'~ -1)+if_else(cond=='trtmt',1,0),
    prob = 1 / (1+exp(-lin_pred)),
    successes = rbinom(nrow(.),trials, prob)
    )


graph <- dag_create() %>%
  dag_node("Successes", "s", rhs = binomial(trials, prob), data = dfo$successes) %>%
  dag_node("Trials","trials", child = "s", data = dfo$trials) %>%
  dag_node("prob", "prob", rhs = 1 / (1+exp(-lin_pred)), child = "s") %>%
  dag_node("lin_pred", "lin_pred", rhs = mu, child = "prob") %>%  
  dag_node("mu", "mu", rhs = normal(0,1), child = "lin_pred") %>%
  dag_plate("Condition Effect", "i", nodeLabels = c("mu"), data = dfo$cond, addDataNode = TRUE) %>%
  dag_plate("Covariate Effect", "j", nodeLabels = c("mu"), data = dfo$disc_cov, addDataNode = TRUE) %>%
  dag_plate("Observation","o", nodeLabels = c("s","i","j","trials","prob","lin_pred"))
graph %>% dag_render()
drawsDF <- graph %>% dag_numpyro()
# 
# sample: 100%|██████████| 5000/5000 [00:14<00:00, 335.24it/s, 1023 steps of size 6.75e-04. acc. prob=0.81]
# Error in py_run_string_impl(code, local, convert) : 
#   ValueError: conflicting sizes for dimension 'i_dim': length 3 on the data but length 2 on coordinate 'i_dim'
# Run `reticulate::py_last_error()` for details.

