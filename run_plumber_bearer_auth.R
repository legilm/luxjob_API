# ------------------------------------------------------------------------------
#' Add Bearer Token Authentication to the OpenAPI Documentation
#'
#' This function modifies the Plumber API specification to declare a Bearer token
#' authentication scheme. It does not enforce token validation — it's used to
#' inform clients (like Swagger UI) that a Bearer token is expected.
#'
#' Each secured route will display a lock icon in Swagger UI and allow the user
#' to click "Authorize" and input their token.
#'
#' NOTE: Actual token enforcement is done done in the endpoint logic (e.g. with
#' a helper like `auth_helper()`).
# ------------------------------------------------------------------------------


source("verify_token.R")
source("reduce_quota.R")


add_bearer_auth <- function(api, paths = NULL) {
  
  # Define the Bearer token security scheme to show in Swagger UI
  api$components <- list(
    securitySchemes = list(
      BearerAuth = list(
        type = "http",      # HTTP-based authentication
        scheme = "bearer",  # Expect tokens in the Authorization header: "Bearer xxx"
        description = "Enter a Bearer token like: Bearer abc123"
      )
    )
  )
  
  # If no specific paths were given, apply the security spec to all paths
  if (is.null(paths)) paths <- names(api$paths)
  
  # Loop over all endpoints and HTTP methods to add the BearerAuth requirement
  for (path in paths) {
    methods <- names(api$paths[[path]])
    
    # Only secure standard HTTP methods (not OPTIONS or others)
    for (method in intersect(methods, c("get", "post", "put", "delete", "head"))) {
      api$paths[[path]][[method]] <- c(
        api$paths[[path]][[method]],
        list(security = list(list(BearerAuth = list())))
      )
    }
  }
  
  # Return the modified API spec
  api
}


# ------------------------------------------------------------------------------
# Plumber API Setup
# ------------------------------------------------------------------------------
plumber::pr("plumber_bearer_auth.R") |> 
  
  # Attach the BearerAuth declaration to the OpenAPI documentation
  # ⚠️ This is for Swagger UI display only — real token checks must happen in endpoint logic
  plumber::pr_set_api_spec(add_bearer_auth) |>

  # Add a preroute hook to support CORS (Cross-Origin Resource Sharing)
  # ✅ Required if a frontend app (e.g., Rshiny) is calling the API from another port
  # ❌ Not needed if testing with Postman, curl, or R
  plumber::pr_hook("preroute", function(req, res) {

    # Allow frontend from this origin (adjust as needed)
    res$setHeader("Access-Control-Allow-Origin", "http://localhost:1234")

    # Allow these HTTP methods for cross-origin requests
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")

    # Allow custom headers, including API_KEY or Authorization
    res$setHeader("Access-Control-Allow-Headers", "API_KEY, Accept")

    # Allow sending of credentials (like cookies or Authorization headers)
    res$setHeader("Access-Control-Allow-Credentials", "true")

    # Handle preflight OPTIONS requests (used by browsers before real request)
    if (req$REQUEST_METHOD == "OPTIONS") {
      res$status <- 200
      return(list())
    }

    # Continue with normal routing
    plumber::forward()
  }) |>
  
  # Run the API server on port 8008, accessible from other machines via 0.0.0.0
  plumber::pr_run(
    port = 8008,
    host = "0.0.0.0"
  )