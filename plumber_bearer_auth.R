library(plumber)
library(htmlwidgets)
library(plotly)


auth_helper <- function(res, req, FUN, ..., render_as_widget = FALSE) {
  # Get token from header: "Authorizat
  # ion: Bearer mytoken123"
  auth_header <- if (!is.null(req$HTTP_AUTHORIZATION)) req$HTTP_AUTHORIZATION else ""
  token <- sub("^Bearer ", "", auth_header)
  valid_token <- verify_token(token = token)

  if (valid_token){
    reduce_quota(token = token)
  }

  if (!valid_token) {
    res$status <- 401
    
    if (render_as_widget) {
      return(
        plotly::plotly_empty(type = "scatter") %>%
          layout(title = "Unauthorized: Invalid or missing token")
      )
    }
    
    return(list(error = "Unauthorized: Invalid or missing token"))
  }
  
  FUN(...)
}


# plumber.R


#* @apiTitle Basic Plumber API
#* @apiDescription This is a simple API to demonstrate the use of plumber.
#* @apiVersion 1.0.0
#* @apiContact g.steil@outlook.com
#* @apiLicense MIT

#* Echo the parameter that was sent in
#* @param msg:string  The message to echo back.
#* @get /echo
function(res, req, msg = "") {
  auth_helper(res, req, function(msg) {
    list(msg = paste0("The message is: '", msg, "'"))
  }, msg = msg)
}

#* @get /plot
#* @param spec:string If provided, filter the data to only this species (e.g. 'setosa')
#* @serializer htmlwidget
function(res, req, spec) {
  auth_helper(res, req, function(spec){
    
    myData <- iris
    title <- "All Species"
    
    if (!is.null(spec)) {
      myData <- subset(iris, Species == spec)
      if (nrow(myData) == 0) {
        res$status <- 404
        return(plotly::plotly_empty(type = "scatter") %>%
                 layout(title = paste0("Error 404: Species '", spec, "' not found")))
      }
      title <- paste0("Only the '", spec, "' Species")
    }
    
    return(plotly::plot_ly(
      data = myData,
      x = ~Sepal.Length,
      y = ~Petal.Length,
      type = "scatter",
      mode = "markers",
      color = ~Species
    ) %>%
      layout(
        title = title,
        xaxis = list(title = "Sepal Length"),
        yaxis = list(title = "Petal Length")
      ))
  }, spec = spec, render_as_widget = T )
  
  
}

