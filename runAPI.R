#Run my API

library(plumber)
r <- plumb("myAPI.R")

#run it on the port in the Dockerfile
r$run(port=8000)

