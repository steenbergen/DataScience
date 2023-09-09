###############################################################################
# CODE FOR CONVERTING ORTHOGONAL TO RAW POLYNOMIALS                           #
###############################################################################

# Create some artificial data to work with
n <- 1000
set.seed(1234)
x <- runif(n, min = 0, max = 10)
e <- rnorm(n, mean = 0, sd = 2)
y <- 2 + 2*(x - 5) - 1*(x - 5)^2 + e
df <- cbind.data.frame(x,y)

# Run an orthogonal polynomial regression
fit <- lm(y ~ poly(x, degree = 2), data = df)
coef(fit)

# Step 1: Load the following function

get_poly_orth_map <- function(object){
  stopifnot(inherits(object, "poly"))
  sigs  <- attr(object, "coefs")$norm2
  alpha <- attr(object, "coefs")$alpha
  
  nc <- length(alpha) + 1L
  Gamma <- matrix(0., nc, nc)
  Gamma[1, 1] <- 1
  if(nc > 1){
    Gamma[ , 2] <- -alpha[1] * Gamma[, 1]
    Gamma[2, 2] <- 1
  }
  if(nc > 2)
    for(i in 3:nc){
      i_m1 <- i - 1L
      Gamma[, i] <- c(0, Gamma[-nc, i_m1]) - alpha[i_m1] * Gamma[, i_m1] - 
        sigs[i] / sigs[i_m1] * Gamma[, i - 2L]
    }
  
  tmp <- sigs[-1]
  tmp[1] <- 1
  Gamma / rep(sqrt(tmp), each = nc)
}

# Step 2: Get coefficients
gamma <- get_poly_orth_map(poly(df$x, 2))
drop(gamma %*% coef(fit))

# Step 3: Get standard errors
sqrt(diag(tcrossprod(gamma %*% vcov(fit), gamma)))
