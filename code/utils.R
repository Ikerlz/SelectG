library(plm)
library(bife)
library(MASS)
source("loss_calculation.R")
source("estimation.R")
generate_init_beta <- function(group_num, data_X_tensor, data_y_mat, init_method="kmeans", model="lr") {
  # """
  # :param data_X_tensor: 自变量张量, N * T * p
  # :param data_y_mat: 因变量矩阵, N * T
  # :param group_num: 组个数
  # :param init_method: 两种初始化方法，分别是kmeans和total_fit
  # :param model: 两种模型：线性回归（包括动态线性回归） & Probit模型
  # :return:初始化的参数矩阵 group * p
  # """
  data_num <- dim(data_X_tensor)[1]
  times <- dim(data_X_tensor)[2]
  dimension <- dim(data_X_tensor)[3]
  if (init_method == "kmeans") {
    beta_mat_all <- matrix(data = 0, nrow = data_num, ncol = dimension)
    if (model == "lr") {
      f <- "y~"
      for (i in 1:dimension) {
        if (i==1){
          f <- paste0(f, "V1")
        } else {
          f <- paste0(f, "+V", i)
        }
      }
      f <- as.formula(f)
      for (i in 1:data_num) {
        data <- as.data.frame(data_X_tensor[i,,])
        data$y <- data_y_mat[i,]
        data$N <- i
        data$Time <- 1:times
        beta_mat_all[i, ] <- as.numeric(plm(f, data = data, model = "within", effect = "individual", index=c("N", "Time"))$coefficients)
      }
    } else if (model == "probit") {
      f <- "y~"
      for (i in 1:dimension) {
        if (i==1){
          f <- paste0(f, "V1")
        } else {
          f <- paste0(f, "+V", i)
        }
      }
      f <- as.formula(paste0(f, "|N"))
      for (i in 1:data_num) {
        data <- as.data.frame(data_X_tensor[i,,])
        data$y <- data_y_mat[i,]
        data$N <- i
        data$Time <- 1:times
        beta_mat_all[i, ] <- as.numeric(bife(f, data = data, model = "probit")$coefficients)
        # print(beta_mat_all[i, ])
      }
    } else {
      stop("the model should be lr or probit")
    }
    beta_mat <- kmeans(beta_mat_all, group_num)$centers
  } else if (init_method == "total_fit") {
    X_mat <- apply(data_X_tensor, 3, as.vector)
    y_vec <- as.vector(data_y_mat)
    if (model == "lr") {
      f <- "y~"
      for (i in 1:dimension) {
        if (i==1){
          f <- paste0(f, "V1")
        } else {
          f <- paste0(f, "+V", i)
        }
      }
      f <- as.formula(f)
      data <- as.data.frame(X_mat)
      data$y <- y_vec
      data$N <- as.vector(sapply(1:data_num, rep, times))
      data$Time <- rep(1:times, data_num)
      beta0 <- as.numeric(plm(f, data = data, model = "within", effect = "individual", index=c("N", "Time"))$coefficients)
    } else if (model == "probit") {
      f <- "y~"
      for (i in 1:dimension) {
        if (i==1){
          f <- paste0(f, "V1")
        } else {
          f <- paste0(f, "+V", i)
        }
      }
      f <- as.formula(paste0(f, "|N"))
      data <- as.data.frame(X_mat)
      data$y <- y_vec
      data$N <- as.vector(sapply(1:data_num, rep, times))
      data$Time <- rep(1:times, data_num)
      beta0 <- as.numeric(bife(f, data = data, model = "probit")$coefficients)
    } else {
      stop("the model should be lr or probit")
    }
    beta_mat <- matrix(rep(beta0, group_num), nrow = group_num, byrow = TRUE) + mvrnorm(n = group_num, rep(0, dimension), diag(beta0 ^ 2))
  } else {
    stop("The initial method should be kmeans or total_fit")
  }
  return(beta_mat)
}


generate_fixed_effect_panel_data <- function(data_num, times, alpha2x, beta_mat, model="static_lr", seed0=123) {
  # """
  # :param data_num: N
  # :param times: T
  # :param alpha2x: 用于生成自变量： X_i = alpha2x * alpha_i + e_i
  # :param beta_mat: 真实参数矩阵，G * p，如果是动态模型，需要把自回归参数gamma放到第一列
  # :param model: 三种模型：static_lr -> 静态线性回归; dynamic_lr -> 动态线性回归; dynamic_probit -> 动态probit模型
  # :param seed0: 种子
  # :return: 返回一个list，其中包含三组数据：
  # $X_tensor: 自变量array
  # $Y_mat: 因变量matrix
  # $group_vec: 真实组vector
  # """
  res <- list()
  group_num <- dim(beta_mat)[1]
  if (model == "static_lr") {
    dimension <- dim(beta_mat)[2]
  } else {
    dimension <- dim(beta_mat)[2] - 1
  }
  N_vec <- rep(1:data_num, times)
  t_vec <- as.vector(sapply(1:times, rep, data_num))
  # 生成group
  set.seed(seed0)
  group_vec <- sample(1:group_num, data_num, replace = TRUE)
  group_vec1 <- rep(group_num, times)
  res$group_vec <- group_vec
  # 生成alpha
  set.seed(seed0+1)
  alpha <- runif(data_num, -1, 1)
  res$alpha <- alpha
  alpha_vec <- rep(alpha, times)
  alpha_mat <- matrix(rep(alpha_vec, dimension), ncol=dimension)
  # 生成e
  set.seed(seed0+2)
  e_mat <- matrix(rnorm(data_num*times*dimension), nrow = data_num*times, ncol = dimension) # 这里的e不是noise，下面的u才是
  res$e_mat <- e_mat
  # 生成u
  set.seed(seed0+3)
  u_vec <- rnorm(data_num*times, mean = 0, sd = 1)
  res$u_vec <- u_vec
  
  beta_mat_all <- matrix(NA, nrow = data_num, ncol = dim(beta_mat)[2])
  for (i in 1:data_num) {
    beta_mat_all[i,] <- beta_mat[group_vec[i],]
  }
  if (model == "static_lr") {
    X_mat <- e_mat + alpha2x * alpha_mat
    y_vec <- rowSums(X_mat * do.call(rbind, replicate(times, beta_mat_all, simplify = FALSE))) + u_vec + alpha_vec
    res$X_tensor <- array(data = X_mat, dim = c(data_num, times, dimension))
    res$y_mat <- matrix(y_vec, nrow = data_num)
  } else if (model == "dynamic_lr") {
    u_mat <- matrix(u_vec, nrow = data_num)
    X_mat <- e_mat + alpha2x * alpha_mat
    y_mat <- matrix(NA, nrow = data_num, ncol = times+1)
    X_tensor <- array(data = X_mat, dim = c(data_num, times, dimension))
    set.seed(seed0+4)
    y_mat[,1] <- runif(data_num, min = -1, max = 1)
    for (i in 2:(times+1)) {
      y_mat[,i] <- alpha * (1 - beta_mat_all[,1]) + y_mat[,i-1] * beta_mat_all[,1] + rowSums(X_tensor[,i-1,] * beta_mat_all[,2:(dimension+1)]) + u_mat[,i-1]
    }
    res$y_mat <- y_mat[,2:(times+1)]
    y_mat_old <- y_mat[,1:times]
    res$X_tensor <- array(data = cbind(as.vector(y_mat_old), X_mat), dim = c(data_num, times, dimension+1))
  } else if (model == "dynamic_probit") {
    u_mat <- matrix(u_vec, nrow = data_num)
    X_mat <- e_mat + alpha2x * alpha_mat
    y_mat <- matrix(0, nrow = data_num, ncol = times+1)
    X_tensor <- array(data = X_mat, dim = c(data_num, times, dimension))
    set.seed(seed0+4)
    y_mat[,1] <- rbinom(data_num, 1, 0.5)
    for (i in 2:(times+1)) {
      temp <- alpha + y_mat[,i-1] * beta_mat_all[,1] + rowSums(X_tensor[,i-1,] * beta_mat_all[,2:(dimension+1)]) - u_mat[i-1,]
      y_mat[, i] <- sapply(temp, function(x){ifelse(x>=0,1,0)})
    }
    res$y_mat <- y_mat[,2:(times+1)]
    y_mat_old <- y_mat[,1:times]
    res$X_tensor <- array(data = cbind(as.vector(y_mat_old), X_mat), dim = c(data_num, times, dimension+1))
  } else {
    stop("The model should be static_lr or dynamic_probit or dynamic_lr")
  }
  return(res)
}



select_best_group_number <- function(
  init_beta, data_X_tensor, data_y_mat, model="lr", select_method = "CV", max_group=10, CV_split="random", train_index=NA, IC_tuning_param=NA, init_method="kmeans", approximate=FALSE, significant_level=0.05) {
  # """
  # :param data_X_tensor: 自变量张量, N * T * p
  # :param data_y_mat: 因变量矩阵, N * T
  # :param model: lr 或 probit
  # :param init_beta: 初始化参数
  # :param select_method: 选择group的方法，一共有5种
  #   BIC: Bonhomme and Manresa (2015)
  #   LIC: Su et al. (2016)
  #   PC:  Liu et al. (2020)
  #   HT: To be updated
  #   CV: our method
  # :param CV_split: 如果使用的方法为CV, 则需要指定划分数据集的方式
  # :param init_method: beta初始化方法, 若init_beta不为NULL, 则此参数可忽略
  # :return:初始化的参数矩阵 group * p
  # """
  N <- dim(data_X_tensor)[1]
  Time <- dim(data_X_tensor)[2]
  p <- dim(data_X_tensor)[3]
  if (select_method == "CV") {
    if (CV_split == "random") {
      # set.seed(Time)
      train_index <- sample(1:Time, Time %/% 2)
    } else if (CV_split == "odd_even") {
      train_index <- seq(1, Time, 2)
    } else if (CV_split == "head_tail") {
      train_index <- 1: (Time %/% 2)
    } else if (CV_split == "self_define") {
      if (is.na(train_index)) {
        stop("The train_index should not be NA if the CV_split is self_define")
      }
    } else {
      stop("The CV_split should be random, odd_even, head_tail or self_define")
    }
    print(train_index)
    min_loss <- 1e10
    optimal_g <- NA
    for (g in 2:max_group) {
      # Fit
      res1 <- estimate_parameters(
        group_num = g,
        data_X_tensor = data_X_tensor[,train_index,],
        data_y_mat = data_y_mat[,train_index],
        init_beta = init_beta,
        tolerance = 1e-6,
        max_iter = 30,
        init_method = init_method,
        model = model,
        approximate = approximate
        )
      
      res2 <- estimate_parameters(
        group_num = g,
        data_X_tensor = data_X_tensor[,-train_index,],
        data_y_mat = data_y_mat[,-train_index],
        init_beta = init_beta,
        tolerance = 1e-6,
        max_iter = 30,
        init_method = init_method,
        model = model)
      # calculate approximate loss
      loss1 <- calculate_approximate_loss_all_groups_test(
        data_X_tensor = data_X_tensor[,train_index,],
        data_y_mat = data_y_mat[,train_index],
        alpha_vec = res2$alpha_vec,
        beta_mat = res2$beta_mat,
        group_label_vec = res2$group_vec,
        model = model)
      loss2 <- calculate_approximate_loss_all_groups_test(
        data_X_tensor = data_X_tensor[,-train_index,],
        data_y_mat = data_y_mat[,-train_index],
        alpha_vec = res1$alpha_vec,
        beta_mat = res1$beta_mat,
        group_label_vec = res1$group_vec,
        model = model)
      
      # total loss
      # loss <- log(loss1) + log(loss2)
      loss <- (loss1 + loss2)/2
      print(loss)
      # print(c(loss1, loss2, loss))
      print("+++++++++++++++++++++++++")
      # print(res1$beta_mat)
      
      # update
      if (loss < min_loss) {
        optimal_g <- g
        min_loss <- loss
      }
      
    }
  } else if (select_method == "LIC") {
    min_loss <- 1e10
    optimal_g <- NA
    # Fit
    for (g in 2:max_group) {
      res <- estimate_parameters(
        group_num = g,
        data_X_tensor = data_X_tensor,
        data_y_mat = data_y_mat,
        init_beta = init_beta,
        tolerance = 1e-6,
        max_iter = 50,
        init_method = init_method,
        model = model)
      if (model == "lr") {
        if (is.na(IC_tuning_param)) {
          loss <- log(1/(N*Time) * res$loss) + 2/3 * (N*Time) ^ (-0.5) * p * g
        } else {
          loss <- log(1/(N*Time) * res$loss) + IC_tuning_param * g
        }
      } else if (model == "probit") {
        if (is.na(IC_tuning_param)) {
          loss <- 1/(N*Time) * res$loss + 0.125 * log(log(Time)) / Time * p * g
        } else {
          loss <- 1/(N*Time) * res$loss + IC_tuning_param * g
        }
      } else {
        stop("The model should be lr or probit")
      }
      # total loss
      # loss <- loss1 + loss2
      print(loss)
      print("+++++++++++++++++++++++++")
      # update
      if (loss < min_loss) {
        optimal_g <- g
        min_loss <- loss
      }
    }
  } else if (select_method == "PC") {
    min_loss <- 1e10
    optimal_g <- NA
    # TODO: Test loss_diff 0502
    old_diff = 0
    # Fit
    for (g in 2:max_group) {
      res <- estimate_parameters(
        group_num = g,
        data_X_tensor = data_X_tensor,
        data_y_mat = data_y_mat,
        init_beta = init_beta,
        tolerance = 1e-6,
        max_iter = 50,
        init_method = init_method,
        model = model)
      if (model == "lr") {
        temp <- 1/(N*Time) * res$loss
        # print(temp - old_diff)
        old_diff <- temp
        if (is.na(IC_tuning_param)) {
          loss <- log(1/(N*Time) * res$loss) + 1 / (5 * log(Time) * Time^(1/8)) * g
        } else {
          loss <- log(1/(N*Time) * res$loss) + IC_tuning_param * g
        }
        # print(loss)
      } else if (model == "probit") {
        if (is.na(IC_tuning_param)) {
          loss <- 1/(N*Time) * res$loss + ((log(N)) ^ (1/8)) / (5 * log(Time) * Time^(1/8)) * g
        } else {
          loss <- 1/(N*Time) * res$loss + IC_tuning_param * g
        }
      } else {
        stop("The model should be lr or probit")
      }
      
      # total loss
      # loss <- loss1 + loss2
      print(loss)
      print("+++++++++++++++++++++++++")
      
      # update
      if (loss < min_loss) {
        optimal_g <- g
        min_loss <- loss
      }
    }
  } else if (select_method == "BIC") {
    min_loss <- 1e10
    optimal_g <- NA
    # Fit
    for (g in max_group:2) {
      res <- estimate_parameters(
        group_num = g,
        data_X_tensor = data_X_tensor,
        data_y_mat = data_y_mat,
        init_beta = init_beta,
        tolerance = 1e-6,
        max_iter = 50,
        init_method = init_method,
        model = model)
      # update
      if (g == max_group) {
        sigma_hat <- 1/((N-max_group)*Time - N - p) * res$loss
        # print(sigma_hat)
        print("++++++++++++++++++++++++++++++")
      }
      # print(sigma_hat * ((g * Time+ N + p) / (N * Time)) * log(N*Time))
      loss <- 1/(N*Time) * res$loss + sigma_hat * ((g * Time+ N + p) / (N * Time)) * log(N*Time)
      
      # total loss
      # loss <- loss1 + loss2
      print(loss)
      print("+++++++++++++++++++++++++")
      
      if (loss < min_loss) {
        optimal_g <- g
        min_loss <- loss
      }
    }
  } else if (select_method == "HT") {
    for (g in 2:max_group) {
      res <- estimate_parameters(
        group_num = g,
        data_X_tensor = data_X_tensor,
        data_y_mat = data_y_mat,
        init_beta = init_beta,
        tolerance = 1e-6,
        max_iter = 50,
        init_method = init_method,
        model = model)
      # construct LM type static
      N <- dim(data_y_mat)[1]
      TT <- dim(data_y_mat)[2]
      p <- dim(data_X_tensor)[3]
      u_mat <- matrix(data = 0, nrow = N, ncol = TT)
      group_label <- res$group_vec
      beta_mat <- res$beta_mat
      alpha_vec <- res$alpha_vec
      H_tensor = array(data = NA, dim = c(N, TT, TT))
      LM <- 0
      B_NT <- 0
      V_NT <- 0
      M0 <- diag(TT) - 1/TT * matrix(data = 1, nrow = TT, ncol = TT)
      for (i in 1:N) {
        H_tensor[i,,] <- M0 %*% data_X_tensor[i,,] %*% solve(t(data_X_tensor[i,,]) %*% M0 %*% data_X_tensor[i,,]) %*% t(data_X_tensor[i,,]) %*% M0
        temp <- data_y_mat[i,] - data_X_tensor[i,,] %*% beta_mat[group_label[i],] - alpha_vec[i]
        # u_mat[i,] <- temp - mean(temp)
        u_mat[i, ] <- temp
        LM <- LM + as.vector(u_mat[i,] %*% H_tensor[i,,] %*% u_mat[i,])
        B_NT <- B_NT + sum(u_mat[i, ] ^ 2 * diag(H_tensor[i,,]))
        # V_NT
        Omega_i = 1/TT * t(data_X_tensor[i,,]) %*% M0 %*% data_X_tensor[i,,]
        a.e=eigen(Omega_i)
        Q <- a.e$vectors
        Omega_i_temp = Q %*% diag(a.e$values ^ (-0.5)) %*% solve(Q)
        b_i_mat = matrix(data = NA, nrow = TT, ncol = p)
        Xi_ave <- colMeans(data_X_tensor[i,,])
        b_i_mat[1, ] = Omega_i_temp %*% (data_X_tensor[i, 1, ] - Xi_ave)
        for (t in 2:TT) {
          b_it <- Omega_i_temp  %*% (data_X_tensor[i, t,]  - Xi_ave)
          b_i_mat[t, ] <- b_it
          sum_b_is_u_is = rep(0, p)
          for (s in 1:(t-1)){
            sum_b_is_u_is <- sum_b_is_u_is + b_i_mat[s,] * u_mat[i, s]
          }
          V_NT <- V_NT + (u_mat[i, t] * (sum_b_is_u_is %*% b_it)[1,1]) ^ 2
        }
      }
      V_NT <- 4 / (TT ^ 2 * N) * V_NT
      B_NT <- B_NT / (sqrt(N))
      J_NT <- LM / sqrt(N * V_NT) - B_NT / sqrt(V_NT)
      print(abs(J_NT))
      if (abs(J_NT) < qnorm(1-significant_level/2)) {
        optimal_g <- g
        break
      }
      optimal_g <- g
    }
    # stop("To be updated !")
  } else {
    stop("The method should be CV, LIC, PC, BIC, HT")
  }
  return(optimal_g)
}





cal_mse_for_diff_group <- function(
  init_beta, data_X_tensor, data_y_mat, model="lr", max_group=10, T_tr, T_te, rolling_window, init_method="kmeans", approximate=FALSE
) {
  res <- c()
  for (g in 2:max_group) {
    TT = dim(data_X_tensor)[2]
    pred_times = (TT - T_tr - T_te) %/% rolling_window + 1
    mse_g <- 0
    for (i in 1:pred_times) {
      train_index=(1+rolling_window*(i-1)):(T_tr+rolling_window*(i-1))
      test_index=(T_tr+1+rolling_window*(i-1)):(T_tr+T_te+rolling_window*(i-1))
      res_train <- estimate_parameters(
          group_num = g,
          data_X_tensor = data_X_tensor[,train_index,],
          data_y_mat = data_y_mat[,train_index],
          init_beta = init_beta,
          tolerance = 1e-6,
          max_iter = 30,
          init_method = init_method,
          model = model,
          approximate = approximate
      )
      pred_mse <- calculate_mse_all_group(
          data_X_tensor=data_X_tensor[,test_index,], 
          data_y_mat=data_y_mat[,test_index], 
          alpha_vec=res_train$alpha_vec, 
          beta_mat=res_train$beta_mat, 
          group_label_vec=res_train$group_vec, 
          model="lr"
      )
      mse_g <- mse_g + pred_mse / pred_times
    }
    print("++++++++++++++++++++++")
    print(g)
    print(mse_g)
    res <- c(res, mse_g)
  }
  return(res)
}



cal_residual_for_diff_group <- function(
  init_beta, data_X_tensor, data_y_mat, model="lr", max_group=10, init_method="kmeans", approximate=FALSE
) {
  res <- list()
  for (g in 2:max_group) {
    res_est <- estimate_parameters(
      group_num = g,
      data_X_tensor = data_X_tensor,
      data_y_mat = data_y_mat,
      init_beta = init_beta,
      tolerance = 1e-6,
      max_iter = 30,
      init_method = init_method,
      model = model,
      approximate = approximate
    )
    res_residual <- calculate_residual(
      data_X_tensor=data_X_tensor, 
      data_y_mat=data_y_mat, 
      alpha_vec=res_est$alpha_vec, 
      beta_mat=res_est$beta_mat, 
      group_label_vec=res_est$group_vec, 
      model="lr")
    res[[g]] <- res_residual
  }
  return(res)
}