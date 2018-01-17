# estimated MSE for the three estimators
mse.hat <- apply( theta.hat, 2, function( x ) { mean( x - theta )^2 } )

# true MSE
mse <- theta^2 * c(
                    1 / ( n + 1 )^2 + n / ( n + 2 ) - ( n / ( n + 1 ) )^2,
                    1 / ( 3 * n ),
                    0.25 / n^2 + ( 2 + 1 / n )^2 / ( 12 * n )
                  )

# difference between true MSE and estimated MSE
mse.table <- cbind( mse, mse.hat, abs( mse - mse.hat ), abs( ( mse - mse.hat ) / mse ) )
colnames( mse.table ) <- c( 'mse', 'mse.hat', 'absdiff', 'reldiff' )
rownames( mse.table ) <- paste( 'theta', 1:ncol( theta.hat ), sep = '' )
print( mse.table )
