# Run this script right after the navStress_unityData_processing_script.R

############# Frechet's distance analyses
# (path 1 time, path 1 values of trajectory, path 2 time, path 2 values of trajectory)

path1 <- tibble(column1 = outer_passive_df_list[[1]]$pos_X, column2 = outer_passive_df_list[[1]]$pos_Z) # define the x and y values
plot(path1)
path1_rdp <- RamerDouglasPeucker(x = path1$column1, y = path1$column2, epsilon = .1, keep_index = TRUE) # decrease data points to important ones
plot(path1_rdp$x, path1_rdp$y)
p1vot <- subset(path1_rdp, select = -ncol(path1_rdp))
plot(p1vot)
p1t <- outer_passive_df_list[[1]]$time_sec # define the time points
p1t <- p1t[path1_rdp$index]


path2 <- tibble(column1 = outer_passive_df_list[[2]]$pos_X, column2 = outer_passive_df_list[[2]]$pos_Z) # define x and y values
plot(path2)
path2_rdp <- RamerDouglasPeucker(x = path2$column1, y = path2$column2, epsilon = .1, keep_index = TRUE)
plot(path2_rdp$x, path2_rdp$y)
p2vot <- subset(path2_rdp, select = -ncol(path2_rdp))
plot(p2vot)
p2t <- outer_passive_df_list[[2]]$time_sec # define time points
p2t <- p2t[path2_rdp$index]


distFrechet(p1t, p1vot, p2t, p2vot)
