# plumber.R
library(plumber)

#---------------------------
# AUTHENTICATION FUNCTION
#---------------------------
auth_helper <- function(res, req, FUN, ...) {
  req_has_key <- "HTTP_X_API_KEY" %in% names(req)
  key_is_valid <- req$HTTP_X_API_KEY == Sys.getenv("API_KEY")
  env_not_set <- nchar(Sys.getenv("API_KEY")) <= 1
  
  if (!req_has_key || !key_is_valid || env_not_set) {
    res$body <- "Unauthorized"
    res$status <- 401
    return("Missing or invalid API key, or environment not set.")
  } else {
    FUN(...)
  }
}

#---------------------------
# ADD AUTH TO OPENAPI DOC
#---------------------------
add_auth <- function(api, paths = NULL) {
  api[["components"]] <- list(
    securitySchemes = list(
      ApiKeyAuth = list(
        type = "apiKey",
        `in` = "header",
        name = "X-API-KEY",
        description = "Enter your API key here"
      )
    )
  )
  
  if (is.null(paths)) paths <- names(api$paths)
  for (path in paths) {
    methods <- names(api$paths[[path]])
    for (method in intersect(methods, c("get", "post", "put", "delete", "head"))) {
      api$paths[[path]][[method]] <- c(
        api$paths[[path]][[method]],
        list(security = list(list(ApiKeyAuth = vector())))
      )
    }
  }
  
  api
}

#---------------------------
# PLUMBER ROUTER WITH ENDPOINTS
#---------------------------
pr() |>
  pr_set_api_spec(function(api) add_auth(api)) |>
  
  # CORS HEADERS + OPTIONS SUPPORT
  pr_hook("preroute", function(req, res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "X-API-KEY, Accept")
    res$setHeader("Access-Control-Allow-Credentials", "true")
    
    if (req$REQUEST_METHOD == "OPTIONS") {
      res$status <- 200
      return(list())
    }
    
    plumber::forward()
  }) |>
  
  # Public endpoint (no auth)
  pr_get("/ping", function() {
    list(status = "ok")
  }) |>
  
  # Protected endpoint (requires API key)
  pr_get("/secret", function(req, res) {
    auth_helper(res, req, function() {
      list(message = "You accessed a protected endpoint successfully!")
    })
  }) |>
  
  # Protected echo endpoint
  pr_get("/echo", function(req, res, msg = "") {
    auth_helper(res, req, function() {
      list(message = paste0("You said: '", msg, "'"))
    })
  }) |>
  
  # Run the API
  pr_run(port = 8008, host = "0.0.0.0")
