
calculate_approximate_loss <- function(data_X_tensor, data_y_mat, beta_vec, model="lr") {
  N <- dim(data_X_tensor)[1]
  times <- dim(data_y_mat)[2]
  p <- length(beta_vec)
  loss_vec <- rep(NA, N)
  if (model == "lr") {
    # 先计算alpha_vec
    alpha_vec <- 1/times * (rowSums(data_y_mat) - t(apply(data_X_tensor, 1, colSums)) %*% beta_vec) 
    for (i in 1:N) {
      w_i <- matrix(data = NA, nrow = p+1, ncol = p+1)
      temp_s <- data_y_mat[i, ] - as.vector(data_X_tensor[i,,] %*% beta_vec) - alpha_vec[i]
      s_i <- c(as.vector(temp_s %*% data_X_tensor[i,,]), sum(temp_s))
      w_i[1:p, 1:p] <- t(data_X_tensor[i,,]) %*% data_X_tensor[i,,]
      w_i[1:p, p+1] <- colSums(data_X_tensor[i,,])
      w_i[p+1, 1:p] <- colSums(data_X_tensor[i,,])
      w_i[p+1, p+1] <- 1
      loss_vec[i] <- 1/times * (s_i %*% solve(w_i) %*% s_i)
    }
  } else if (model == "probit") {
    eps <- 1e-15
    for (i in 1:N) {
      theta_vec <- as.vector(data_X_tensor[i,,] %*% beta_vec)
      Psi_vec <- pnorm(theta_vec)
      Psi_vec[which(Psi_vec<eps)] <- eps
      Psi_vec[which(Psi_vec>1-eps)] <- 1-eps
      psi_vec <- dnorm(theta_vec)
      psi_vec[which(Psi_vec<eps)] <- eps
      psi_vec[which(Psi_vec>1-eps)] <- 1-eps
      s_i <- (data_y_mat[i, ] * psi_vec / Psi_vec + (data_y_mat[i, ] - 1) * psi_vec / (1 - Psi_vec)) * data_X_tensor[i,,]
      temp <- data_y_mat[i,] * (theta_vec * psi_vec * Psi_vec + psi_vec ^ 2) / (Psi_vec ^ 2) +
        (1 - data_y_mat[i,]) * (psi_vec ^ 2 - theta_vec * psi_vec * (1 - Psi_vec)) / ((1 - Psi_vec) ^ 2)
      w_i <- t(data_X_tensor[i,,]) %*% diag(temp) %*% data_X_tensor[i,,]
      loss_vec[i] <- 1/times * (s_i %*% solve(w_i) %*% s_i)
    }
  } else {
    stop("the model should be lr or probit")
  }
  return(mean(loss_vec))
}



calculate_approximate_loss_all_groups <- function(data_X_tensor, data_y_mat, beta_mat, group_label_vec, model="lr") {
  res <- 0
  G <- dim(beta_mat)[1]
  for (g in 1:G) {
    index_g <- which(group_label_vec == g)
    if (length(index_g)!=0) {
      res <- res + calculate_approximate_loss(
        data_X_tensor = data_X_tensor[index_g,,],
        data_y_mat = data_y_mat[index_g,],
        beta_vec = beta_mat[g,],
        model = model
      ) 
    }
  }
  return(res)
}


# 
calculate_mse <- function(data_X_tensor, data_y_mat, alpha_vec, beta_vec, model="lr") {
  N <- dim(data_y_mat)[1]
  times <- dim(data_y_mat)[2]
  p <- length(beta_vec)
  if (is.null(N)) {
    return(sum((data_y_mat - as.vector(data_X_tensor %*% beta_vec) - alpha_vec) ^ 2))
  } else {
    loss_vec <- rep(NA, N)
    # print(N)
    for (n in 1:N) {
      # print(data_y_mat)
      loss_vec[n] <- sum((data_y_mat[n, ] - as.vector(data_X_tensor[n,,] %*% beta_vec) - alpha_vec[n]) ^ 2)
    }
  }
  return(sum(loss_vec))
}



calculate_mse_all_group <- function(data_X_tensor, data_y_mat, alpha_vec, beta_mat, group_label_vec, model="lr") {
  if (is.vector(data_y_mat)) {
    res <- 0
    G <- dim(beta_mat)[1]
    for (g in 1:G) {
      index_g <- which(group_label_vec == g)
      if (length(index_g)!=0) {
        # print("++++++++++")
        # print(index_g)
        # print("++++++++++")
        temp_mse <- calculate_mse(
          data_X_tensor = data_X_tensor[index_g,],
          data_y_mat = data_y_mat[index_g],
          alpha_vec = alpha_vec[index_g],
          beta_vec = beta_mat[g,],
          model = model
        )
        res <- res + temp_mse
      }
    }
  } else {
    res <- 0
    G <- dim(beta_mat)[1]
    for (g in 1:G) {
      index_g <- which(group_label_vec == g)
      if (length(index_g)!=0) {
        # print("++++++++++")
        # print(index_g)
        # print("++++++++++")
        temp_mse <- calculate_mse(
          data_X_tensor = data_X_tensor[index_g,,],
          data_y_mat = data_y_mat[index_g,],
          alpha_vec = alpha_vec[index_g],
          beta_vec = beta_mat[g,],
          model = model
        ) 
        res <- res + temp_mse
      }
    }
  }
  print(res)
  return(1/(dim(data_X_tensor)[1] * dim(data_X_tensor)[2]) * res)
}


calculate_residual <- function(data_X_tensor, data_y_mat, alpha_vec, beta_mat, group_label_vec, model="lr") {
  N <- dim(data_X_tensor)[1]
  TT <- dim(data_X_tensor)[2]
  e_mat <- matrix(nrow = N, ncol = TT)
  for (i in 1:N) {
    e_mat[i, ] <- data_y_mat[i, ] - as.vector(data_X_tensor[i,,] %*% beta_mat[group_label_vec[i],]) - alpha_vec[i]
  }
  return(e_mat)
}

# 
# calculate_rmse_all_group <- function(data_X_tensor, data_y_mat, alpha_vec, beta_mat, group_label_vec, model="lr") {
#   res <- 0
#   G <- dim(beta_mat)[1]
#   for (g in 1:G) {
#     index_g <- which(group_label_vec == g)
#     if (length(index_g)!=0) {
#       if (length(index_g) == 1) {
#         res <- res + sum((data_y_mat[index_g, ] - as.vector(data_X_tensor[index_g,,] %*% beta_mat[g, ]) - alpha_vec[index_g]) ^ 2)
#       } else {
#         res <- res + calculate_approximate_loss_test(
#           data_X_tensor = data_X_tensor[index_g,,],
#           data_y_mat = data_y_mat[index_g,],
#           alpha_vec = alpha_vec[index_g],
#           beta_vec = beta_mat[g,],
#           model = model
#         )
#       }
#     }
#   }
#   return(res)
# }



calculate_approximate_loss_test <- function(data_X_tensor, data_y_mat, alpha_vec, beta_vec, model="lr") {
  N <- dim(data_X_tensor)[1]
  times <- dim(data_y_mat)[2]
  p <- length(beta_vec)
  if (is.matrix(data_X_tensor)) {
    X_mat <- t(t(data_X_tensor) - colSums(data_X_tensor)) # T * p
    Vi_inv <- solve(t(X_mat) %*% X_mat) # p * p
    Ui <- colSums((data_y_mat - as.vector(data_X_tensor %*% beta_vec)) * X_mat) # p * 1 
    return(1/times * norm((Ui %*% Vi_inv), "2") ^ 2)
  } else {
    loss_vec <- rep(NA, N)
    for (i in 1:N) {
      X_mat <- t(t(data_X_tensor[i,,]) - colSums(data_X_tensor[i,,])) # T * p
      Vi_inv <- solve(t(X_mat) %*% X_mat) # p * p
      Ui <- colSums((data_y_mat[i, ] - as.vector(data_X_tensor[i,,] %*% beta_vec)) * X_mat) # p * 1 
      loss_vec[i] <- 1/times * norm((Ui %*% Vi_inv), "2") ^ 2
    }
    return(sum(loss_vec))
  }
}

calculate_approximate_loss_all_groups_test <- function(data_X_tensor, data_y_mat, alpha_vec, beta_mat, group_label_vec, model="lr") {
  res <- 0
  G <- dim(beta_mat)[1]
  for (g in 1:G) {
    index_g <- which(group_label_vec == g)
    if (length(index_g)!=0) {
      if (length(index_g) == 1) {
        res <- res + sum((data_y_mat[index_g, ] - as.vector(data_X_tensor[index_g,,] %*% beta_mat[g, ]) - alpha_vec[index_g]) ^ 2)
      } else {
        res <- res + calculate_approximate_loss_test(
          data_X_tensor = data_X_tensor[index_g,,],
          data_y_mat = data_y_mat[index_g,],
          alpha_vec = alpha_vec[index_g],
          beta_vec = beta_mat[g,],
          model = model
        )  
      }
    }
  }
  return(log(res/dim(data_y_mat)[1]))
}

# calculate_rmse <- function(data_X_tensor, data_y_mat, alpha_vec, beta_mat, group_label_vec, model="lr") {
#   res <- 0
#   G <- dim(beta_mat)[1]
#   for (g in 1:G) {
#     index_g <- which(group_label_vec == g)
#     if (length(index_g)!=0) {
#       if (length(index_g) == 1) {
#         res <- res + sum((data_y_mat[index_g, ] - as.vector(data_X_tensor[index_g,,] %*% beta_mat[g, ]) - alpha_vec[index_g]) ^ 2)
#       } else {
#         res <- res + calculate_approximate_loss_test(
#           data_X_tensor = data_X_tensor[index_g,,],
#           data_y_mat = data_y_mat[index_g,],
#           alpha_vec = alpha_vec[index_g],
#           beta_vec = beta_mat[g,],
#           model = model
#         )  
#       }
#     }
#   }
# }

