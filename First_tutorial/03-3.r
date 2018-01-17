op <- par( mfrow = c( 1, ncol( theta.hat ) ) )
 for ( i in 1:ncol( theta.hat ) )
  hist( ( theta.hat[,i] - theta ) / sd( theta.hat[,i] ),
        breaks = 'FD', col = 'gray', border = 'white', main = paste( 'Estimator', i ),
        xlab = expression( ( hat( theta ) - theta ) / hat( se )( hat( theta ) ) ) )
par( op )
