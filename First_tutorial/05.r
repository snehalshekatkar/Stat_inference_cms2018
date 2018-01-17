# estimate through simulation the coverage of the
# two confidence interval prescriptions for Bernoulli(p)

p       <- 0.3                      # Bernoulli p parameter
N       <- c( 5, 10, 50, 100, 500 ) # data sizes
alpha   <- 0.05                     # expected coverage of CI
nrepeat <- 200                      # #replications

zab2 <- qnorm( 1 - 0.5 * alpha )     # Normal
ea2  <- sqrt( log( 2 / alpha ) / 2 ) # Hoeffding

op <- par( mfcol = c( 2, length( N ) ) )

for ( n in N ) # loop over data sizes
 {
  # replicate n coin tosses nrepeat times, and get estimates of p:

  # direct simulation of coin tosses using runif( )
  # p.hat <- replicate( nrepeat, sum( runif( n ) <= p ) / n )

  # short-cut: using the fact that #successes ~ Binomial( n, p )
  p.hat <- rbinom( nrepeat, n, p ) / n

  # for each replicate, compute the two CIs on p:
  ci.n <- sapply( p.hat,
                  function( p.hat )
                   {
                    ci <- p.hat + c( -1, +1 ) * zab2 * sqrt( p.hat * ( 1 - p.hat ) / n )
                    return( pmin( 1, pmax( 0, ci ) ) )
                   }
                )

  ci.h <- sapply( p.hat,
                  function( p.hat )
                   {
                    ci <- p.hat + c( -1, +1 ) * ea2 / sqrt( n )
                    return( pmin( 1, pmax( 0, ci ) ) )
                   }
                )

  # check if each CI contains p:
  contains.n <- apply( ci.n, 2, function( ci ) { ( ci[1] <= p ) && ( ci[2] >= p ) } )
  contains.h <- apply( ci.h, 2, function( ci ) { ( ci[1] <= p ) && ( ci[2] >= p ) } )

  # compute proportion of CIs that contain p:
  coverage.n <- mean( contains.n )
  coverage.h <- mean( contains.h )

  # average CI lengths
  mean.length.n <- mean( apply( ci.n, 2, function( ci ) { diff( ci ) } ) )
  mean.length.h <- mean( apply( ci.h, 2, function( ci ) { diff( ci ) } ) )

  # visualize CIs with reference to p:
  plot.new( ); plot.window( xlim = c( 0, 1 ), ylim = c( 1, nrepeat ) )
  title( main = paste( 'Normal | N =', n, '| Repeat =', nrepeat ),
         xlab  = paste( 'Coverage =', format( coverage.n, digits = 2 ),
                        '| Mean Length =', format( mean.length.n, digits = 2 ) ) )
  axis( 1, at = c( 0, p, 1 ), labels = c( '0', 'p', '1' ) )
  t <- sapply( 1:nrepeat,
               function( i )
                {
                 col = if ( contains.n[i] ) 'green' else 'red'
                 points( ci.n[,i], rep( i, 2 ), pch = 19, cex = 0.2, col = col )
                 lines( ci.n[,i], rep( i, 2 ), col = col )
                }
             )
  abline( v = p, col = 'blue' )

  plot.new( ); plot.window( xlim = c( 0, 1 ), ylim = c( 1, nrepeat ) )
  title( main = paste( 'Hoeffding | N =', n, '| Repeat =', nrepeat ),
         xlab  = paste( 'Coverage =', format( coverage.h, digits = 2 ),
                        '| Mean Length =', format( mean.length.h, digits = 2 ) ) )
  axis( 1, at = c( 0, p, 1 ), labels = c( '0', 'p', '1' ) )
  t <- sapply( 1:nrepeat,
               function( i )
                {
                 col = if ( contains.h[i] ) 'green' else 'red'
                 points( ci.h[,i], rep( i, 2 ), pch = 19, cex = 0.2, col = col )
                 lines( ci.h[,i], rep( i, 2 ), col = col )
                }
             )
  abline( v = p, col = 'blue' )
 }

par( op )
