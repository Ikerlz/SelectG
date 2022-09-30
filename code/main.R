rm(list = ls())
setwd("~/Select_G/")
source("utils.R")
# Settings
repeat_num <- 500
# N_vec <- c(400)
# TT_vec <- c(200)
# N_vec <- c(100, 100, 200, 200, 400, 400)
# T_vec <- c(40, 60, 60, 120, 120, 200)
# N_vec <- c(200, 200, 400)
# T_vec <- c(125, 150, 125)
N_vec <- c(200, 200, 200, 400, 400, 400)
T_vec <- c(100, 125, 150, 100, 125, 150)
NT_mat <- matrix(data = c(N_vec, T_vec), nrow = length(N_vec))
G <- 4
max_group <- G+3
alpha2x <- 0.2
# 传入命令行参数
args <- commandArgs(trailingOnly = TRUE)
model <- args[1]
date <- args[2]
select_method <- args[3]
approximate <- ifelse(as.numeric(args[4])==1, TRUE, FALSE)
if_save <- ifelse(as.numeric(args[5])==1, TRUE, FALSE)
# model <- "static_lr"
# date <- "20220514"
# select_method <- "HT"
# approximate <- FALSE # 只针对CV这个方法
save_file <- paste0("./", as.character(date), "_", model, ".txt")
# if_save <- FALSE
# cat("G", "max_group", "alpha2x", "model", "select_method", "\n", file=save_file, append = TRUE)
# cat(G, max_group, alpha2x, select_method, "\n", file=save_file, append = TRUE)
# for (N in N_vec) {
#   for (TT in TT_vec) {
# 
#   }
# }
for (i in 1:length(N_vec)) {
  N <- NT_mat[i, 1]
  TT <- NT_mat[i, 2]
  res <- rep(0, max_group-1)
  for (step in 1:repeat_num) {
    print(paste0("----- 第", as.character(step), "次 -----"))
    seed = step # 2022
    init_beta = NULL
    # beta_mat = matrix(c(1.4, 0.6, 1, 1, 0.6, 1.4), nrow = G, byrow = TRUE) # Test HT
    # beta_mat = matrix(c(1.4, 0.6, 1, 1, 0.6, 1.4, 1.8, 0.2), nrow = G, byrow = TRUE) # 20220514_static_lr, 20220515_static_lr
    beta_mat = matrix(c(0.1, 1.4, 0.6, 0.1, 1, 1, 0.1, 0.6, 1.4, 0.1, 1.8, 0.2), nrow = G, byrow = TRUE) # 20220513_dynamic_lr, 20220515_dynamic_lr
    # beta_mat = matrix(c(0.1, 1.3, 0.6, 0.2, 0.9, 0.9, 0.3, 0.5, 1.2, 0.4, 1.5, 0.1), nrow = G, byrow = TRUE) # 20220512_dynamic_lr
    # beta_mat = matrix(c(0.2, 1.4, 0.6, 0.2, 1, 1, 0.2, 0.6, 1.4, 0.2, 1.8, 0.2), nrow = G, byrow = TRUE) # 20220508_1_static_lr.txt, 20220512_dynamic_lr
    # beta_mat = matrix(c(1.5, 0.5, 1, 1, 0.5, 1.5, 0.1, 1.9), nrow = G, byrow = TRUE) # 20220507
    # beta_mat = matrix(c(1.6, 0.4, 1, 1, 0.4, 1.6, 2.2, -0.2), nrow = G, byrow = TRUE) # 20220507_1
    # beta_mat = matrix(c(-1, 3, -0.2, 2.2, 0.6, 1.4, 1.4, 0.6), nrow = G, byrow = TRUE) # 20220507_2
    # beta_mat = matrix(c(1.6, 0.4, 1, 1, 0.4, 1.6, -1, -1), nrow = G, byrow = TRUE) # 20220506_1
    # beta_mat = matrix(c(1.6, 0.4, 1.2, 0.8, 0.8, 1.2, 0.4, 1.6), nrow = G, byrow = TRUE) # 20220506
    # beta_mat = matrix(c(1.6, 0.4, 1, 1, 0.4, 1.6), nrow = G, byrow = TRUE) # 20220505_test1
    # beta_mat = matrix(c(1, 1, 1, 1.5, 1, 1, 1, 2, 1, 1, 1, 3), nrow = G, byrow = TRUE) # 20220504_static_lr.txt # 20220505
    # beta_mat = matrix(c(1.3,-0.7,1,1,0.7,-1.3,-1,-1), nrow = G, byrow = TRUE) # 20220503_test1
    # beta_mat = matrix(c(1, 1, 1, 1.3, 1, 1, 1, 1.3, 1, 1, 1, 1.3), nrow = G, byrow = TRUE) # 20220503_static_lr.txt
    # beta_mat = matrix(c(1.6, 0.4, 1.8, 0.2, 1.2, 0.8, 0.4, 1.6), nrow = G, byrow = TRUE) # 20220502_G4
    # beta_mat = matrix(c(1.6, 0.4, 1, -1, 1, 1, 1, -1, 1, 1, 1, -1), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(1.6, 1.3, 0.7, 0.4, 1.3, 1.3, 0.7, 0.7, 1.6, 1.6, 0.4, 0.4, 1.3, 1.6, 0.4, 0.7), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(1.6, 0.4, 1, 1.3, 0.7, 1, 1.4, 0.6, 1, 1.2, 0.8, 1), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(1.6, 0.4, 1, 1, 1, 1, 1, 1, 0.4, 1, 1, 1.6, 1, 1.6, 0.4, 1), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(1.6, 0.4, 0.4, 1.6, 1.3, 0.7, 0.7, 1.3, 1, 1), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(1.6, 0.4, 0.4, 1.6, 1.3, 0.7, 0.7, 1.3), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(1.6, 0.4, 0.4, 1.6, 1.2, 0.8, 0.8, 1.2), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(1.6, 0.4, 0.4, 1.6, 1, 1), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(0.4, 1.6, 1, 0.6, 1, -1, 0.8, 0.4, 1.6), nrow = G, byrow = TRUE)
    # beta_mat = matrix(c(0.2, 0.4, 1.6, -0.3, 0.7, 1.3, -0.4, 1.6, 0.4, 0.5, 1.3, 0.7), nrow = G, byrow = TRUE)
    a <- generate_fixed_effect_panel_data(
      data_num = N,
      times = TT,
      alpha2x = alpha2x,
      beta_mat = beta_mat,
      model = model,
      seed0 = seed
    )
    # b <- generate_init_beta(
    #   group_num = G,
    #   data_X_tensor = a$X_tensor,
    #   data_y_mat = a$y_mat,
    #   init_method="kmeans",
    #   model=strsplit(model, "_")[[1]][2]
    # )
    temp <- select_best_group_number(
      init_beta = init_beta,
      data_X_tensor = a$X_tensor,
      data_y_mat = a$y_mat,
      model=strsplit(model, "_")[[1]][2],
      select_method = select_method,
      max_group=max_group,
      CV_split="random",
      init_method="kmeans",
      approximate=approximate
    )
    res[temp-1] <- res[temp-1] + 1
  }
  if (if_save) {
    cat(select_method, approximate, N, TT, res, "\n", file=save_file, append=TRUE) 
  }
  
}