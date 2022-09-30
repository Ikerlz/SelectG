rm(list = ls())
source("utils.R")
library(haven)
library(R.matlab)
library(readxl)
library(ggplot2)
library(latex2exp)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


mytheme <-theme_bw() + 
  #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(legend.title=element_text(hjust = 0.5, colour="black", size=15, face="bold"))+
  theme(axis.text.x = element_text(size = 10, face = "bold", vjust = 0.5, hjust = 0.5))+
  theme(axis.text.y = element_text(size = 10, face = 'bold', vjust = 0.5, hjust = 0.5))+
  theme(axis.ticks = element_blank())+
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text=element_text(size=10,face="bold")) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) + 
  theme(panel.background = element_rect(colour = "black", size = 1)) # +
# theme(panel.background = element_rect(fill = NA), legend.position = c(0.955, 0.82))

example1 <- FALSE
example2 <- TRUE
example1_cal_mse <- FALSE
example2_cal_mse <- FALSE
example2_cal_residual <- FALSE
example1_plot <- TRUE
example2_plot <- TRUE


if (example1) {
  # data_democracy <- readMat("./data84.mat")
  data_democracy <- readMat("./app_saving_PGMM/balancedPanelX1995.mat")
  N <- 56
  TT <- 15
  p <- 4
  tuning_parameter <- 1/10 * (N*TT) ^ (-0.5) * p
  # tuning_parameter <- 1/10 * (N*TT) ^ (-0.5) * p
  # model <- "dynamic_lr"
  data_X_tensor0 <- array(data = NA, dim = c(N, TT, p))
  data_X_tensor0[, , 1] <- matrix(data_democracy$lagsaving, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 2] <- matrix(data_democracy$cpi, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 3] <- matrix(data_democracy$interest, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 4] <- matrix(data_democracy$gdp, nrow = N, byrow = TRUE)
  data_Y_mat0 <- matrix(data = data_democracy$saving, nrow = N, byrow = TRUE)
  
  # data_X_tensor <- data_X_tensor0[remain_index, ,]
  # data_Y_mat <- data_Y_mat0[remain_index, ]
  
  
  # temp <- select_best_group_number(
  #   init_beta = NULL,
  #   data_X_tensor = data_X_tensor0,
  #   data_y_mat = data_Y_mat0,
  #   model = "lr",
  #   select_method = "LIC",
  #   IC_tuning_param = tuning_parameter,
  #   max_group=6,
  #   CV_split="self_define",
  #   train_index = c(4, 1, 10, 3, 2, 15, 13),
  #   init_method="kmeans",
  #   approximate=TRUE
  # )
  
  temp_cv <- select_best_group_number(
    init_beta = NULL,
    data_X_tensor = data_X_tensor0,
    data_y_mat = data_Y_mat0,
    model = "lr",
    select_method = "CV",
    # IC_tuning_param = tuning_parameter,
    max_group=6,
    CV_split="self_define",
    train_index = c(4, 1, 10, 3, 2, 15, 13),
    init_method="kmeans",
    approximate=TRUE
  )
}


if (example1_cal_mse) {
  # data_democracy <- readMat("./data84.mat")
  data_democracy <- readMat("./app_saving_PGMM/balancedPanelX1995.mat")
  N <- 56
  TT <- 15
  p <- 4
  # tuning_parameter <- 1/10 * (N*TT) ^ (-0.5) * p
  # tuning_parameter <- 1/10 * (N*TT) ^ (-0.5) * p
  # model <- "dynamic_lr"
  data_X_tensor0 <- array(data = NA, dim = c(N, TT, p))
  data_X_tensor0[, , 1] <- matrix(data_democracy$lagsaving, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 2] <- matrix(data_democracy$cpi, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 3] <- matrix(data_democracy$interest, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 4] <- matrix(data_democracy$gdp, nrow = N, byrow = TRUE)
  data_Y_mat0 <- matrix(data = data_democracy$saving, nrow = N, byrow = TRUE)
  
  # data_X_tensor <- data_X_tensor0[remain_index, ,]
  # data_Y_mat <- data_Y_mat0[remain_index, ]
  
  
  # temp <- select_best_group_number(
  #   init_beta = NULL,
  #   data_X_tensor = data_X_tensor0,
  #   data_y_mat = data_Y_mat0,
  #   model = "lr",
  #   select_method = "LIC",
  #   IC_tuning_param = tuning_parameter,
  #   max_group=6,
  #   CV_split="self_define",
  #   train_index = c(4, 1, 10, 3, 2, 15, 13),
  #   init_method="kmeans",
  #   approximate=TRUE
  # )
  
  # temp_cv <- select_best_group_number(
  #   init_beta = NULL,
  #   data_X_tensor = data_X_tensor0,
  #   data_y_mat = data_Y_mat0,
  #   model = "lr",
  #   select_method = "CV",
  #   # IC_tuning_param = tuning_parameter,
  #   max_group=6,
  #   CV_split="self_define",
  #   train_index = c(4, 1, 10, 3, 2, 15, 13),
  #   init_method="kmeans",
  #   approximate=TRUE
  # )
  res <- cal_mse_for_diff_group(
    init_beta = NULL,
    data_X_tensor = data_X_tensor0,
    data_y_mat = data_Y_mat0,
    model="lr", 
    max_group=6,
    T_tr=8,
    T_te=3,
    rolling_window=2,
    init_method="kmeans"
  )
}


if (example2) {
  # data_democracy <- readMat("./data84.mat")
  data_democracy <- read_excel("./cleaneddata.xlsx")
  N <- 74
  TT <- 7
  p <- 2
  # model <- "dynamic_lr"
  data_X_tensor0 <- array(data = NA, dim = c(N, TT, p))
  data_X_tensor0[, , 1] <- matrix(data_democracy$X1, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 2] <- matrix(data_democracy$X2, nrow = N, byrow = TRUE)
  data_Y_mat0 <- matrix(data = data_democracy$Y, nrow = N, byrow = TRUE)
  
  remain_index <- c()
  for (index_i in 1:N) {
    temp <- sum(data_X_tensor0[index_i, ,1] - mean(data_X_tensor0[index_i, ,1]))
    if (temp != 0) {
      remain_index <- c(remain_index, index_i)
    }
  }
  
  data_X_tensor <- data_X_tensor0[remain_index, ,]
  data_Y_mat <- data_Y_mat0[remain_index, ]
  
 
  # temp <- select_best_group_number(
  #   init_beta = NULL,
  #   data_X_tensor = data_X_tensor,
  #   data_y_mat = data_Y_mat,
  #   model = "lr",
  #   select_method = "CV",
  #   max_group=6,
  #   # CV_split = "random",
  #   CV_split="self_define",
  #   # train_index = c(1,3,7),
  #   # train_index = c(2,5,7),
  #   train_index=c(1,3,5,7), # 变化最显著
  #   # train_index=c(1,2,3,4), # 可行
  #   init_method="kmeans",
  #   approximate=TRUE
  # )
  tuning_parameter <- 1/  (N*TT) ^ (-0.5) * p
  temp_lic <- select_best_group_number(
    init_beta = NULL,
    data_X_tensor = data_X_tensor0,
    data_y_mat = data_Y_mat0,
    model = "lr",
    select_method = "LIC",
    max_group=6,
    IC_tuning_param = tuning_parameter,
    # CV_split = "random",
    # CV_split="self_define",
    # train_index = c(1,3,7),
    # train_index = c(2,5,7),
    # train_index=c(1,3,5,7), # 变化最显著
    # train_index=c(1,2,3,4), # 可行
    init_method="kmeans",
    approximate=TRUE
  )
}

if (example2_cal_mse) {
  # data_democracy <- readMat("./data84.mat")
  data_democracy <- read_excel("./cleaneddata.xlsx")
  N <- 74
  TT <- 7
  p <- 2
  # model <- "dynamic_lr"
  data_X_tensor0 <- array(data = NA, dim = c(N, TT, p))
  data_X_tensor0[, , 1] <- matrix(data_democracy$X1, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 2] <- matrix(data_democracy$X2, nrow = N, byrow = TRUE)
  data_Y_mat0 <- matrix(data = data_democracy$Y, nrow = N, byrow = TRUE)
  
  remain_index <- c()
  for (index_i in 1:N) {
    temp <- sum(data_X_tensor0[index_i, ,1] - mean(data_X_tensor0[index_i, ,1]))
    if (temp != 0) {
      remain_index <- c(remain_index, index_i)
    }
  }
  
  data_X_tensor <- data_X_tensor0[remain_index, ,]
  data_Y_mat <- data_Y_mat0[remain_index, ]
  
  
  # temp <- select_best_group_number(
  #   init_beta = NULL,
  #   data_X_tensor = data_X_tensor,
  #   data_y_mat = data_Y_mat,
  #   model = "lr",
  #   select_method = "CV",
  #   max_group=6,
  #   # CV_split = "random",
  #   CV_split="self_define",
  #   # train_index = c(1,3,7),
  #   # train_index = c(2,5,7),
  #   train_index=c(1,3,5,7), # 变化最显著
  #   # train_index=c(1,2,3,4), # 可行
  #   init_method="kmeans",
  #   approximate=TRUE
  # )
  tuning_parameter <- 2/3 * (N*TT) ^ (-0.5) * p
  temp_lic <- select_best_group_number(
    init_beta = NULL,
    data_X_tensor = data_X_tensor0,
    data_y_mat = data_Y_mat0,
    model = "lr",
    select_method = "LIC",
    max_group=6,
    IC_tuning_param = tuning_parameter,
    # CV_split = "random",
    # CV_split="self_define",
    # train_index = c(1,3,7),
    # train_index = c(2,5,7),
    # train_index=c(1,3,5,7), # 变化最显著
    # train_index=c(1,2,3,4), # 可行
    init_method="kmeans",
    approximate=TRUE
  )
  # res <- cal_mse_for_diff_group(
  #   init_beta = NULL,
  #   data_X_tensor = data_X_tensor0,
  #   data_y_mat = data_Y_mat0,
  #   model="lr", 
  #   max_group=6,
  #   T_tr=4,
  #   T_te=2,
  #   rolling_window=1,
  #   init_method="kmeans"
  # )
}


if (example2_cal_residual) {
  # data_democracy <- readMat("./data84.mat")
  data_democracy <- read_excel("./cleaneddata.xlsx")
  N <- 74
  TT <- 7
  p <- 2
  # model <- "dynamic_lr"
  data_X_tensor0 <- array(data = NA, dim = c(N, TT, p))
  data_X_tensor0[, , 1] <- matrix(data_democracy$X1, nrow = N, byrow = TRUE)
  data_X_tensor0[, , 2] <- matrix(data_democracy$X2, nrow = N, byrow = TRUE)
  data_Y_mat0 <- matrix(data = data_democracy$Y, nrow = N, byrow = TRUE)
  
  remain_index <- c()
  for (index_i in 1:N) {
    temp <- sum(data_X_tensor0[index_i, ,1] - mean(data_X_tensor0[index_i, ,1]))
    if (temp != 0) {
      remain_index <- c(remain_index, index_i)
    }
  }
  
  data_X_tensor <- data_X_tensor0[remain_index, ,]
  data_Y_mat <- data_Y_mat0[remain_index, ]
  
  
  # temp <- select_best_group_number(
  #   init_beta = NULL,
  #   data_X_tensor = data_X_tensor,
  #   data_y_mat = data_Y_mat,
  #   model = "lr",
  #   select_method = "CV",
  #   max_group=6,
  #   # CV_split = "random",
  #   CV_split="self_define",
  #   # train_index = c(1,3,7),
  #   # train_index = c(2,5,7),
  #   train_index=c(1,3,5,7), # 变化最显著
  #   # train_index=c(1,2,3,4), # 可行
  #   init_method="kmeans",
  #   approximate=TRUE
  # )
  res <- cal_residual_for_diff_group(
    init_beta = NULL,
    data_X_tensor = data_X_tensor0,
    data_y_mat = data_Y_mat0,
    model="lr",
    max_group=6,
    init_method="kmeans"
  )
}


### -> Plot <- ###

# Example 1

if (example1_plot) {
  loss <- c(2.419803, 2.41677, 2.4125, 2.390347, 2.381726, 
            2.574041, 2.651392, 2.72533, 2.795978, 2.860233,
            2.436028, 2.432912, 2.444012, 2.436351, 2.447494
            )
  tuning <- c(rep("1/10", 5), rep("2/3", 5), rep("1/6", 5))
  
  group_vec <- rep(2:6, 3)
  example1_df_lic <- data.frame(group = group_vec, tuning = tuning, loss = loss)
  
  
  
  plot_example1_lic <- ggplot(data = example1_df_lic, mapping = aes(x=group, y=loss, 
                                                                    linetype=tuning, colour=tuning, shape=tuning, fill=tuning)) + 
    geom_line(size=2.5) + 
    geom_point(size=5) + #绘制线图和点图
    labs(x=TeX("$\\hat{G}$"), y="IC")+
    scale_linetype_manual(values=c(1,2,3)) + #自定义线条类型
    scale_color_manual(values=c('steelblue','darkred', 'black')) + #自定义颜色
    scale_shape_manual(values=c(21, 22, 23)) + #自定义点形状
    scale_fill_manual(values=c('steelblue','darkred', 'black'))  + #自定义点的填充色
    mytheme +
    theme(legend.position=c(0.07,0.87)) +
    guides(color=guide_legend(title = "C"),
           linetype=guide_legend(title = "C"),
           shape=guide_legend(title = "C"),
           fill=guide_legend(title = "C")) +
    # scale_fill_discrete(name=TeX("$\\lambda$")) + 
    # theme(legend.title = "c")+
    theme(axis.text.x = element_text(size = 20, face = "bold"))+
    theme(axis.text.y = element_text(size = 20, face = "bold"))+
    theme(strip.text = element_text(face="bold",size=15)
          # strip.background = element_rect(fill="lightblue",colour="white", size=1)
    )+
    theme(axis.title.x = element_text(size = 25, face = "bold"))+
    theme(axis.title.y = element_text(size = 25, face = "bold"))+
    theme(legend.title = element_text(size = 20, face = "bold")) +   ##设置标题，face="bold"加粗
    theme(legend.text = element_text(size = 15, face = "bold"))   ##设置标签文字
  
  loss <- c(2.712587, 5.620672, 5.626947, 5.626864, 6.098768)
  group_vec <- c(2, 3, 4, 5, 6)
  example1_df_cv <- data.frame(group = group_vec, loss = loss)
  
  plot_example1_cv <- ggplot(data = example1_df_cv, mapping = aes( x = group_vec, y = loss)) +
    geom_line(size=2.5) +
    geom_point(size=5)+
    labs(x=TeX("$\\hat{G}$"), y="MLTE") +
    mytheme + 
    theme(axis.text.x = element_text(size = 20, face = "bold"))+
    theme(axis.text.y = element_text(size = 20, face = "bold"))+
    theme(strip.text = element_text(face="bold",size=15)
          # strip.background = element_rect(fill="lightblue",colour="white", size=1)
    )+
    theme(axis.title.x = element_text(size = 25, face = "bold"))+
    theme(axis.title.y = element_text(size = 25, face = "bold"))+
    theme(legend.title = element_text(size = 20, face = "bold")) +   ##设置标题，face="bold"加粗
    theme(legend.text = element_text(size = 15, face = "bold"))   ##设置标签文字
  
  loss <- c(13.92573, 15.08129, 18.37679, 16.02371, 17.74063)
  group_vec <- c(2, 3, 4, 5, 6)
  example1_df_mpse <- data.frame(group = group_vec, loss = loss)
  
  plot_example1_mpse <- ggplot(data = example1_df_mpse, mapping = aes( x = group_vec, y = loss)) +
    geom_line(size=2.5) +
    geom_point(size=5)+
    labs(x=TeX("$\\hat{G}$"), y="MPSE") +
    mytheme + 
    theme(axis.text.x = element_text(size = 20, face = "bold"))+
    theme(axis.text.y = element_text(size = 20, face = "bold"))+
    theme(strip.text = element_text(face="bold",size=15)
          # strip.background = element_rect(fill="lightblue",colour="white", size=1)
    )+
    theme(axis.title.x = element_text(size = 25, face = "bold"))+
    theme(axis.title.y = element_text(size = 25, face = "bold"))+
    theme(legend.title = element_text(size = 20, face = "bold")) +   ##设置标题，face="bold"加粗
    theme(legend.text = element_text(size = 15, face = "bold"))   ##设置标签文字
}


if (example2_plot) {
  # 2. cleaneddata.xlsx
  
  loss <- c(-3.152807, -3.162849, -3.161851, -3.166872, -3.148176, 
            -3.053215, -3.013461, -2.954878, -2.910967, -2.906996,
            -3.14109, -3.145274, -3.134506, -3.130654, -3.17062)
  tuning <- c(rep("1/10", 5), rep("2/3", 5), rep("1/6", 5))
  
  group_vec <- rep(2:6, 3)
  example2_df_lic <- data.frame(group = group_vec, tuning = tuning, loss = loss)
  
  
  
  plot_example2_lic <- ggplot(data = example2_df_lic, mapping = aes(x=group, y=loss, 
                                                                    linetype=tuning, colour=tuning, shape=tuning, fill=tuning)) + 
    geom_line(size=2.5) + 
    geom_point(size=5) + #绘制线图和点图
    labs(x=TeX("$\\hat{G}$"), y="IC")+
    scale_linetype_manual(values=c(1,2,3)) + #自定义线条类型
    scale_color_manual(values=c('steelblue','darkred', 'black')) + #自定义颜色
    scale_shape_manual(values=c(21, 22, 23)) + #自定义点形状
    scale_fill_manual(values=c('steelblue','darkred', 'black'))  + #自定义点的填充色
    mytheme +
    theme(legend.position=c(0.07,0.87)) +
    guides(color=guide_legend(title = "C"),
           linetype=guide_legend(title = "C"),
           shape=guide_legend(title = "C"),
           fill=guide_legend(title = "C")) +
    # scale_fill_discrete(name=TeX("$\\lambda$")) + 
    # theme(legend.title = "c")+
    theme(axis.text.x = element_text(size = 20, face = "bold"))+
    theme(axis.text.y = element_text(size = 20, face = "bold"))+
    theme(strip.text = element_text(face="bold",size=15)
          # strip.background = element_rect(fill="lightblue",colour="white", size=1)
    )+
    theme(axis.title.x = element_text(size = 25, face = "bold"))+
    theme(axis.title.y = element_text(size = 25, face = "bold"))+
    theme(legend.title = element_text(size = 20, face = "bold")) +   ##设置标题，face="bold"加粗
    theme(legend.text = element_text(size = 15, face = "bold"))   ##设置标签文字
  
  
  loss <- c(0.6401961, 0.1673804, 1.525998, 1.666893, 1.569455)
  group_vec <- c(2, 3, 4, 5, 6)
  example2_df_cv <- data.frame(group = group_vec, loss = loss)
  
  plot_example2_cv <- ggplot(data = example2_df_cv, mapping = aes( x = group_vec, y = loss)) +
    geom_line(size=2.5) +
    geom_point(size=5)+
    labs(x=TeX("$\\hat{G}$"), y="MLTE") +
    mytheme + 
    theme(axis.text.x = element_text(size = 20, face = "bold"))+
    theme(axis.text.y = element_text(size = 20, face = "bold"))+
    theme(strip.text = element_text(face="bold",size=15)
          # strip.background = element_rect(fill="lightblue",colour="white", size=1)
    )+
    theme(axis.title.x = element_text(size = 25, face = "bold"))+
    theme(axis.title.y = element_text(size = 25, face = "bold"))+
    theme(legend.title = element_text(size = 20, face = "bold")) +   ##设置标题，face="bold"加粗
    theme(legend.text = element_text(size = 15, face = "bold"))   ##设置标签文字
  
  loss <- c(0.06735393, 0.06116089, 0.08064676, 0.08643607, 0.08617863)
  group_vec <- c(2, 3, 4, 5, 6)
  example2_df_mpse <- data.frame(group = group_vec, loss = loss)
  
  plot_example2_mpse <- ggplot(data = example2_df_mpse, mapping = aes( x = group_vec, y = loss)) +
    geom_line(size=2.5) +
    geom_point(size=5)+
    labs(x=TeX("$\\hat{G}$"), y="MPSE") +
    mytheme + 
    theme(axis.text.x = element_text(size = 20, face = "bold"))+
    theme(axis.text.y = element_text(size = 20, face = "bold"))+
    theme(strip.text = element_text(face="bold",size=15)
          # strip.background = element_rect(fill="lightblue",colour="white", size=1)
    )+
    theme(axis.title.x = element_text(size = 25, face = "bold"))+
    theme(axis.title.y = element_text(size = 25, face = "bold"))+
    theme(legend.title = element_text(size = 20, face = "bold")) +   ##设置标题，face="bold"加粗
    theme(legend.text = element_text(size = 15, face = "bold"))   ##设置标签文字
}




# multiplot(plot_example1_lic, plot_example2_lic, plot_example1_cv, plot_example2_cv, cols = 2)


ggsave(plot_example1_lic, file='plot_example1_lic.pdf', width = 8, height = 6, units = "in",   
       dpi = 1000)
# ggsave(plot_example1_cv, file='plot_example1_cv.pdf', width = 8, height = 6, units = "in",   
#        dpi = 1000)
ggsave(plot_example2_lic, file='plot_example2_lic.pdf', width = 8, height = 6, units = "in",   
       dpi = 1000)
# ggsave(plot_example2_cv, file='plot_example2_cv.pdf', width = 8, height = 6, units = "in",   
#        dpi = 1000)
# ggsave(plot_example1_mpse, file='plot_example1_mpse.pdf', width = 8, height = 6, units = "in",
#        dpi = 1000)
# ggsave(plot_example2_mpse, file='plot_example2_mpse.pdf', width = 8, height = 6, units = "in",
#        dpi = 1000)

# for (g in 2:6) {
#   temp <- as.data.frame(res[[g]])
#   write.csv(temp,paste0("emat_g", g, ".csv"), row.names=FALSE)
# }
