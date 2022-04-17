library(animint2)
library(ggplot2)

# The least squared regression function
x <- runif(15, min = 0, max = 15)
y <- 1.5*x + 0.5 + rnorm(15,0,2)
points <- data.frame(x=x,y=y)
  
# Function
least.squares = function( 
  slope, ani.type = NULL){
  temp_y <- slope*x+0.5
  RSS <- sum((y-temp_y)^2)
  line <- data.frame(slope = slope, RSS = RSS)
  invisible(
    list(temp_line = line, temp_y = temp_y)
  )
}


slopes<-seq(0,2,length=50)
lines = data.frame()
act_y = data.frame()
for(i in 1:50){
  data <- least.squares(slopes[i])
  temp_line <- data.frame(slopes = data$temp_line[1], RRS = data$temp_line[2], iteration = i)
  lines <- rbind(lines, temp_line)
  act_y <- rbind(act_y, data$temp_y)
}
act_y <- cbind(act_y, iteration = seq(1:50))


line.plot <- ggplot()+
    geom_point(data = points, aes(x=x,y=y), shape = 1) +
    geom_abline(slope = 1.5, intercept = 0.5, color = "grey", alpha = 0.5) +
    geom_abline(data = lines, aes(slope = slopes, intercept = 0.5, color= iteration)) 
#    geom_point(aes(x=points$x, y=act_y, showSelected = iteration))

line.plot

viz <- list(line = line.plot,
            time = list(variable = "iteration", ms = 2000), 
            title = "")
animint2dir(viz, out.dir = "least.squares")
animint2gist(viz, out.dir = "least.squares")
