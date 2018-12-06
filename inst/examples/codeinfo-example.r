
library( "codeinfo" )

#   Create an arbitrary R object to be "hashed"
m <- matrix( 1:4, 2, 2 )

#   Create an arbitrary file for which the MD5 checksum will
#   be calculated
tf1 <- tempfile() 
tf2 <- tempfile() 
writeLines( text = c( letters, LETTERS, 0:9 ), con = tf1 )
writeLines( text = c( letters, LETTERS, 0:8 ), con = tf2 )



#   use codeinfo() to obtain information
codeinfo(
    packages = c( "utils", "tools" ), 
    files      = c( tf1, tf2 ), 
    objects = list( "m" = m, "quantile" = quantile ) )



#   Clean-up
file.remove( tf1, tf2 )
rm( m, tf1, tf2 )
