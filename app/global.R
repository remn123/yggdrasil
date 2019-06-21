library(shiny)
library(shinydashboard)
library(visNetwork)
library(data.table)
library(Matrix)
library(dplyr)
library(MLmetrics)
library(lightgbm)
library(sparkline)
library(shinyBS)
library(purrr)

source("tree.R")
source("select_bars.R")


set.seed(257)

train = fread("./data/train.csv") %>% as.data.frame()
test  = fread("./data/test.csv")  %>% as.data.frame()


nodes <<- data.frame(id = 1:10,
                    
                    # add labels on nodes
                    label = paste("Node", 1:10),
                    
                    # add groups on nodes 
                    group = c("GrA", "GrB"),
                    
                    # size adding value
                    value = 10,          
                    
                    # control shape of nodes
                   # shape = c("square", "triangle", "box", "circle", "dot", "star",
                   #          "ellipse", "database", "text", "diamond"),
                   shape = "square", 
                   
                    # tooltip (html or character), when the mouse is above
                    title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),
                    
                    # color
                    #color = c("darkred", "grey", "orange", "darkblue", "purple"),
                    color = "darkblue",
                   
                    # shadow
                    shadow = TRUE) 
                   #shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE))             

# head(nodes)
# id  label group value    shape                     title    color shadow
#  1 Node 1   GrA     1   square <p><b>1</b><br>Node !</p>  darkred  FALSE
#  2 Node 2   GrB     2 triangle <p><b>2</b><br>Node !</p>     grey   TRUE

edges <<- data.frame(from = c(1,2,5,7,8,10), to = c(9,3,1,6,4,7))

library(rpart)

# Basic classification tree
res <- rpart(Species~., data=iris)


# Run the application 
# shinyApp(ui = ui, server = server)