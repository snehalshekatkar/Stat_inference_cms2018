roll.number <- 0305 # use your own
theta       <- 0.1 * ( roll.number %% 100 )

n <- 30   # data size
m <- 1000 # number of samples

# theta.hat below will be a m X 2 matrix;
# two columns for the two estimators):
theta.hat <- t( sapply( 1:m,
                        function( i )
                         {
                          x <- runif( n, 0, theta )
                          c( max( x ), 2 * mean( x ), ( 2 + 1 / n ) * mean( x ) )
                         }
                      )
              )
