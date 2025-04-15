library(plumber)
# plumber.R


#* @apiTitle Basic Plumber API
#* @apiDescription This is a simple API to demonstrate the use of plumber.
#* @apiVersion 1.0.0
#* @apiContact pierrick.kinif@datagrowth.io
#* @apiLicense MIT

#* Echo the parameter that was sent in
#* @param msg:string  The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot out data from the iris dataset - without error handling
#* @param spec:string If provided, filter the data to only this species (e.g. 'setosa')
#* @get /plot
#* @serializer png
function(spec){
  myData <- iris
  title <- "All Species"
  
  # Filter if the species was specified
  if (!missing(spec)) {
    title <- paste0("Only the '", spec, "' Species")
    myData <- subset(iris, Species == spec)
  }
  
  plot(myData$Sepal.Length, myData$Petal.Length,
       main = title, xlab = "Sepal Length", yla = "Petal Length")
}