library(plm)
library(bife)

estimate_group <- function(data_y_mat, data_X_tensor, beta_mat, model="lr") {
  # """
  # :param data_X_tensor: 自变量张量, N * T * p
  # :param data_y_mat: 因变量矩阵, N * T
  # :param beta_mat: 参数矩阵, G * p
  # :param model:
  #               lr -> 线性回归
  #               probit -> probit模型
  # :return: 一个list
  #         $group_vec: 长度为N的向量，每个元素表示该个体属于哪个group
  #         $residual: 使用这组参数得到的损失值
  # """
  N <- dim(data_X_tensor)[1]
  Time <- dim(data_X_tensor)[2]
  # p <- dim(data_X_tensor)[3]
  G <- dim(beta_mat)[1]
  if (model == "lr") {
    # 先得到hat alpha
    alpha_mat <- 1/Time * (rowSums(data_y_mat) - t(apply(data_X_tensor, 1, colSums)) %*% t(beta_mat)) # N * G
    residual_mat <- matrix(data = NA, nrow = N, ncol = G)
    for (n in 1:N) {
      temp <- t(matrix(data = rep(data_y_mat[n,], G), nrow = Time)- data_X_tensor[n,,] %*% t(beta_mat)) - alpha_mat[n, ] # G * T
      residual_mat[n,] <- rowSums((temp) ^ 2)
    }
  } else if (model == "probit") {
    eps <- 1e-15
    residual_mat <- matrix(data = NA, nrow = N, ncol = G)
    for (n in 1:N) {
      temp <- as.vector(data_X_tensor[n,,] %*% t(beta_mat))
      temp[temp>1-eps] <- 1-eps
      temp[temp<eps] <- eps
      temp_mat <- matrix(data = temp, nrow = Time)
      temp_y <- matrix(data = rep(data_y_mat[n,], G), nrow = Time)
      residual_mat[n,] <- - colSums(temp_y * log(temp_mat) + (1 - temp_y) * log(1 - temp_mat))
    }
  } else {
    stop("the model should be lr or probit")
  }
  res <- list()
  res$group_vec <- apply(residual_mat, 1, which.min)
  res$residual <- sum(apply(residual_mat, 1, min))
  return(res)
}
# Forward_aic  <-step (lm(mpg~1, data=mtcars), direction="forward",scope=formula(fit.full), k=2, trace=0)

estimate_parameter_groupby_group <- function(data_y_mat, data_X_tensor, group_label, beta0_mat, model="lr", approximate=FALSE) {
  # :param approximate: 是否使用近似损失函数得到估计值，通常来说对于CV为TRUE，对于其他方法为FALSE
  G <- dim(beta0_mat)[1]
  N <- dim(data_y_mat)[1]
  times <- dim(data_X_tensor)[2]
  dimension <- dim(beta0_mat)[2]
  beta_mat <- matrix(data = NA, nrow = G, ncol = dimension)
  # TODO: 2022.4.24测试
  alpha_vec <- rep(NA, N)
  for (g in 1:G) {
    index_g <- which(group_label == g)
    if (length(index_g) == 0) {
      beta_mat[g, ] <- beta0_mat[g, ]
    } else {
      num_g <- length(index_g)
      if (num_g == 1) {
        X_mat <- data_X_tensor[index_g,,]
      } else {
        X_mat <- apply(data_X_tensor[index_g,,], 3, as.vector) 
      }
      y_vec <- as.vector(data_y_mat[index_g,])
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
        data$N <- as.vector(sapply(1:num_g, rep, times))
        data$Time <- rep(1:times, num_g)
        # beta_mat[g, ] <- as.numeric(plm(f, data = data, model = "within", effect = "individual", index=c("N", "Time"))$coefficients)
        # TODO: 2022.4.25 修改为两步估计
        beta1 <- as.numeric(plm(f, data = data, model = "within", effect = "individual", index=c("N", "Time"))$coefficients)
        if (approximate) {
          beta2 <- rep(0, dimension)
          for (i in 1:length(index_g)) {
            index_i <- index_g[i]
            data_y_vec <- data_y_mat[index_i, ] - mean(data_y_mat[index_i, ])
            data_X_mat1 <- t(t(data_X_tensor[index_i,,]) - colMeans(data_X_tensor[index_i,,]))
            Vi_hat_inv <- solve(t(data_X_mat1) %*% data_X_mat1)
            beta2 <- beta2 + as.vector(data_y_vec %*% data_X_mat1) %*% Vi_hat_inv
          } 
          beta_mat[g, ] <- 1 / length(index_g) * beta2
        } else {
          beta_mat[g, ] <- beta1
        }
        # TODO: 2022.4.24测试
        if (length(index_g) == 1) {
          alpha_vec[index_g] <- 1/times * (sum(data_y_mat[index_g,]) - sum(data_X_tensor[index_g,,] %*% beta_mat[g, ]))
        } else {
          alpha_vec[index_g] <- 1/times * (rowSums(data_y_mat[index_g,]) - t(apply(data_X_tensor[index_g,,], 1, colSums)) %*% beta_mat[g, ])  
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
        data <- as.data.frame(X_mat)
        data$y <- y_vec
        data$N <- as.vector(sapply(1:num_g, rep, times))
        data$Time <- rep(1:times, num_g)
        beta0_mat[g, ] <- as.numeric(bife(f, data = data, model = "probit")$coefficients)
      } else {
        stop("the model should be lr or probit")
      }
    }
  }
  # TODO: 2022.4.24测试
  res <- list()
  res$beta_mat <- beta_mat
  res$alpha_vec <- alpha_vec
  # return(beta_mat)
  return(res)
}


estimate_parameters <- function(group_num, data_X_tensor, data_y_mat, init_beta=NULL, tolerance=1e-6, max_iter=30, init_method="kmeans", model="lr", approximate=FALSE) {
  if (is.null(init_beta)) {
    init_beta <- generate_init_beta(
      group_num = group_num,
      data_X_tensor = data_X_tensor,
      data_y_mat = data_y_mat,
      init_method =  init_method,
      model = model
    )
  } else {
    stopifnot(dim(init_beta)[1] == group_num)
    stopifnot(dim(init_beta)[2] == dim(data_X_tensor)[3])
  }
  # estimate
  diff <- 100
  step <- 0
  beta_mat <- init_beta
  while ((diff > tolerance) & (step < max_iter)) {
    # Step 1: Update the group
    group_and_loss <- estimate_group(
      data_y_mat = data_y_mat,
      data_X_tensor = data_X_tensor,
      beta_mat = beta_mat,
      model = model
    )
    group_label <- group_and_loss$group_vec
    loss <- group_and_loss$residual
    # Step 2: estimate the beta
    # beta_mat_i <- estimate_parameter_groupby_group(
    #   data_y_mat = data_y_mat,
    #   data_X_tensor = data_X_tensor,
    #   group_label = group_label,
    #   beta0_mat = beta_mat,
    #   model = model
    # )
    # TODO: 2022.4.24测试
    res_i <- estimate_parameter_groupby_group(
      data_y_mat = data_y_mat,
      data_X_tensor = data_X_tensor,
      group_label = group_label,
      beta0_mat = beta_mat,
      model = model,
      approximate = approximate
    )
    beta_mat_i <- res_i$beta_mat
    # Update
    diff <- sum((beta_mat-beta_mat_i)^2)
    beta_mat <- beta_mat_i
    step <- step + 1
  }
  res <- list()
  res$beta_mat <- beta_mat
  # TODO: 2022.4.24测试
  res$alpha_vec <- res_i$alpha_vec
  res$loss <- loss
  res$group_vec <- group_label
  return(res)
}
