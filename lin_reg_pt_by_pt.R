# generate some random data...
# I just added stuff to get a spread I liked
set.seed(1234)
dat <- data.frame(x = x <- runif(50, 0,1),
                  y = y <- c(3 * x[1:25] ^ (1/2) + rnorm(25, 0, 0.45), x[26:50] ^ (3) + rnorm(n = 25, 1, 0.45) ^ (2)))
dat <- dat[sample(nrow(dat)),]

# preliminary plot to see what the data looks like
# plot(dat$x, dat$y)

# find the complete model and influential points
model <- lm(y ~ x, data = dat)
# summary(influence.measures(model))
# summary(model)
influence_points <- sort(as.numeric(rownames(as.data.frame(summary(influence.measures(model))))))

# find graph bounds to keep the images consistent
y_min <- min(dat$y)
y_max <- max(dat$y)
x_min <- min(dat$x)
x_max <- max(dat$x)

# load ggplot
library(ggplot2)

# function to generate ggplot based on the current index of interest in a dataframe
create_lin_reg_plot <- function(index) {
  
  # used to pause the gif once every point has been added
  if (index > 50) {
    index <- 50
  }
  
  # color of the points: 0 - gray, 1 - black, 2 - red
  col <- c(rep(1, times = index), rep(0, times = 50 - index))
  
  # include influential measures from full model
  for (meas in influence_points) {
    if (meas <= index) {
      col[meas] <- 2
    }
  }
  
  # convert color into factor for ggplot 
  # should really put into the data frame
  col <- factor(col)
  
  # adjust coloring based on what 
  if (index == 50) {
    point_colors <- c('black', 'red')
  } else {
    point_colors <- c("grey", "black", "red")
  }
  
  # temporary model for data seen so far
  # get its slope coefficient and sum sq. resid.
  tmp_model <- lm(dat[1:index, ]$y ~ dat[1:index, ]$x)
  coef <- round(tmp_model$coefficients[[2]], 2) 
  
  if (index == 1) {
    sse <- NA # no sse for single point
  } else {
    sse <- round(anova(tmp_model)$`Sum Sq`[[2]], 2)
  }
  
  # generate the plot
  # do whatever you want here
  plt <- ggplot(data = dat, aes(x = x, y = y, color = col)) +
    geom_point(size = 4) +
    geom_smooth(data = dat[1:index, ], aes(x = x, y = y), color = 'red', method = 'lm', se = TRUE, formula = y ~ x, inherit.aes = FALSE) +
    geom_segment(aes(x = dat[index, ]$x, y = dat[index, ]$y,xend = dat[index, ]$x, yend = predict(tmp_model, newdata = dat[1:index, ])[[index]]),
                 color = 'black', linetype = 'dashed') +
    scale_x_continuous(limits = c(x_min - .05, x_max + 0.05)) + # adjusted manually cause lazy
    scale_y_continuous(limits = c(y_min - .50, y_max + 0.50)) +
    scale_color_manual(values = point_colors) +
    guides(color = FALSE) +
    xlab("X") +
    ylab("Y") +
    ggtitle("Linear regression of Y onto X") +
    theme_grey() +
    labs(caption = "*Influential points shown in red") +
    theme(plot.caption = element_text(size=10, hjust = 0.95, face="italic", color="black")) +
    annotate("text", x = x_max - 0.05, y = y_max, label = paste0("Slope estimate: ", coef)) +
    annotate("text", x = x_max - 0.05, y = y_max - 0.25, label = paste0("Sum of squared error: ", sse))
  
  # add all residual segments if at the last point
  if (index == 50) {
    
    plt <- plt +
      geom_segment(aes(x = dat$x, y = dat$y, xend = dat$x, yend = predict(model, newdata = dat)), color = "black", linetype = 'dashed')
    
  }

  print(plt)
  
}

# generate multiple plots
# I add the 5 so that it freezes at the end
# indexes greater than 50 are handled by the create_lin_reg_plot function internally
animate_lin_reg <- function() {
  
  lapply(1:(nrow(dat) + 5), create_lin_reg_plot)
  
}

# load animation
library(animation)

# save the gif
saveGIF(animate_lin_reg(), interval = .2, file = "temp3.gif", ani.width = 1200, ani.height = 600)