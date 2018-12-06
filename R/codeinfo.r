
# +--------------------------------------------------------+ 
# | Package:    See 'Package' in file ../DESCRIPTION       | 
# | Author:     Julien MOEYS                               | 
# | Language:   R                                          | 
# | Contact:    See 'Maintainer' in file ../DESCRIPTION    | 
# | License:    See 'License' in file ../DESCRIPTION       | 
# |             and file ../LICENSE                        | 
# +--------------------------------------------------------+ 



#' Compute the MD5 checksums of each files in an installed package and the checksum of the checksums
#'
#' Compute the \code{\link[tools:md5sum]{MD5 checksums}} of 
#'  each files in an installed package and the checksum of 
#'  the checksums (i.e. the MD5 checksum of a file containing 
#'  the checksums of each files in the package)
#'
#'
#'@param package 
#'  Single character string. Name of an installed package.
#'
#'@param exclude_MD5 
#'  Single logical value. If \code{TRUE} (the default), the 
#'  file MD5 is excluded from the output.
#'
#'
#'@return 
#'  Returns a \code{\link[base]{list}} with two named items: 
#'  \code{checksums} and \code{checksums_checksum}. 
#'  \code{checksums} is a \code{\link[base]{data.frame}} with 
#'  the \code{\link[tools:md5sum]{MD5 checksums}} of each files 
#'  in the package (the first column contains the checksums 
#'  and the second column the name and relative path to the 
#'  file). It is formatted to look alike the \code{MD5} file 
#'  of installed packages (when present). 
#'  \code{checksums_checksum} is a single character string, 
#'  the MD5 checksum calculated on a file containing 
#'  the table \code{checksums}, without row and column 
#'  names, with a space as column separator and without quotes.
#'
#'
#'@seealso \code{\link[base]{find.package}} and 
#'  \code{\link[tools]{md5sum}}.
#'
#'
#'@export
#'
#'@importFrom tools md5sum
#'@importFrom utils write.table 
#'
package_md5sum <- function( package, exclude_MD5 = TRUE ){
    package_path <- find.package( package = package, 
        quiet = TRUE ) 
    
    if( length( package_path ) == 0L ){
        stop( sprintf( 
            "The package '%s' was not found (not installed?)" ) )
    }   
    
    md5_f <- tempfile(
        pattern = sprintf( "md5_%s_", package ), 
        fileext = ".txt" ) 
    
    files_in_package <- dir( package_path, 
        recursive = TRUE ) 
    
    if( exclude_MD5 ){
        files_in_package <- files_in_package[ 
            files_in_package != "MD5" ]
    }   
    
    checksums <- as.character( tools::md5sum( 
        files = file.path( package_path, 
        files_in_package ) ) )
    
    checksums <- data.frame(
        "md5_checksums" = checksums, 
        "files" = paste0( "*", files_in_package ), 
        stringsAsFactors = TRUE )
    
    write.table(
        x = checksums, 
        file = md5_f, 
        quote = FALSE, 
        row.names = FALSE,
        col.names = FALSE, 
        fileEncoding = "UTF-8" ) 
    
    return( list( 
        "checksums"          = checksums, 
        "checksums_checksum" = as.character( 
            tools::md5sum( files = md5_f ) ) ) )
}   

#   package_md5sum( package = "tools" )



#'Information on the system, R-version, packages, files and R-objects, for traceability
#'
#'Information on the system, R-version, packages, files and 
#'  R-objects, for traceability. Includes checksums of 
#'  files, packages and R-objects.
#'
#'
#'@param r 
#'  Single logical value. If \code{TRUE} (the default), 
#'  the function returns information on the system and R.
#'
#'@param packages 
#'  Vector of character strings, naming installed packages 
#'  for which information should be returned. If 
#'  \code{length(packages)} is \code{0} (the default), no 
#'  information is returned on packages.
#'
#'@param files 
#'  Vector of character strings. Name, optionally including 
#'  the full or relative path (see \code{files_path} below), 
#'  for which \code{\link[tools:md5sum]{MD5 Checksums}} should 
#'  be returned. If \code{length(files)} is \code{0} 
#'  (the default), no information is returned on files.
#'
#'@param files_path 
#'  Single character string. General path to a single folder 
#'  where all the files in \code{files} are located. If the 
#'  path differ between files, then it should be passed via 
#'  \code{files} instead. Default value for \code{files} is 
#'  \code{character(0)}, meaning that no general path is 
#'  provided, and that the files are either in the working 
#'  directory or included in \code{files} instead.
#'
#'@param objects 
#'  A labelled list of arbitrary R-objects, whose 
#'  \code{\link[tools:md5sum]{MD5 Checksums}} should be 
#'  returned. For example \code{objects =}\code{list( a = object1, } 
#'  \code{b = object2}, where \code{object1} and \code{object2} 
#'  are R-objects and \code{a} and \code{b} are labels.
#'
#'
#'@seealso \code{\link{Sys.info}}, 
#'  \code{\link[utils]{sessionInfo}}, 
#'  \code{\link[utils]{installed.packages}} and 
#'  \code{\link[tools]{md5sum}}.
#'
#'
#'@example inst/examples/codeinfo-examples.r
#'
#'@export
#'
#'@importFrom utils sessionInfo
#'@importFrom utils installed.packages 
#'@importFrom utils packageDate
#'@importFrom tools md5sum
#'
codeinfo <- function(
    r = TRUE, 
    packages = character(0), 
    files = character(0), 
    files_path = character(0), 
    objects = list() 
){  
    lapply(
        X   = c( "packages", "files", "files_path" ), 
        FUN = function(a){
            a0 <- get( x = a ) 
            
            if( !("character" %in% class( a0 )) ){
                stop( sprintf( 
                    "Argument '%s' must be a character string. Now class %s", 
                    a, paste( class( a0 ), collapse = " " ) ) ) 
            }   
        }   
    )   
    
    
    if( length( files_path ) > 1L ){
        stop( sprintf( 
            "length(files_path) must be 0 or 1. Now length %s", 
            length( files_path ) ) ) 
    }   
    
    objects_names <- names( objects )
    
    if( (length( objects ) != 0) ){
        if( is.null( objects_names ) ){
            stop( "names( objects ) is NULL. All items in 'objects' have a label" )
        }else{ 
            test_objects_names <- objects_names == "" 
            
            if( any( test_objects_names ) ){
                stop( "Some values in names( objects ) are '\"\"'. All items in 'objects' have a label" )
            }   
            
            rm(test_objects_names )
        }   
    }   
    
    
    
    all_output <- c( "r", "packages", "files", "objects" )
    
    out <- vector( length = length( all_output ), 
        mode = "list" ) 
    names( out ) <- all_output 
    rm( all_output )
    
    
    
    if( r ){
        session_info <- utils::sessionInfo() 
        
        out[[ "r" ]] <- data.frame(
            "variable"  = c( "OS", "system", "R_version", "GUI" ), 
            "value"     = c(
                session_info[[ "running" ]], 
                session_info[[ "R.version" ]][[ "system" ]], 
                session_info[[ "R.version" ]][[ "version.string" ]], 
                .Platform[[ "GUI" ]] ), 
            stringsAsFactors = FALSE )   
    }   
    
    
    
    if( length( packages ) > 0L ){ 
        installed_packages <- utils::installed.packages() 
        
        packages_template <- data.frame(
            "package"               = NA_character_, 
            "version"               = NA_character_, 
            "revision"              = NA_character_, 
            "date_packaged"         = NA_character_, 
            "checksums_checksum"    = NA_character_, 
            stringsAsFactors        = FALSE )   
        
        revision_names <- c( "GIT_REVISION", "GIT_VERSION", 
            "SVN_REVISION", "SVN_VERSION", "REVISION" ) 
        
        out[[ "packages" ]] <- do.call( 
            what = "rbind", args = lapply(
            X   = packages, 
            FUN = function(p){
                out_p <- packages_template 
                
                sel <- p == installed_packages[, "Package" ]
                
                if( any( sel ) ){
                    sel <- which( sel )[ 1L ] 
                    
                    out_p[ 1L, "package" ] <- p 
                    
                    
                    
                    out_p[ 1L, "version" ] <- 
                        installed_packages[ sel, "Version" ]
                    
                    p_path <- system.file( package = p )
                    
                    revision_names <- file.path( p_path, revision_names )
                    
                    revision_exists <- file.exists( 
                        revision_names ) 
                    
                    if( any( revision_exists ) ){
                        revision_index <- which( revision_exists )[ 1L ] 
                        
                        revision <- readLines( 
                            revision_names[ revision_index ] )[ 1L ]
                        
                        revision <- strsplit( x = revision, 
                            split = " " )[[ 1L ]][ 1L ]
                        
                        out_p[ 1L, "revision" ] <- revision
                        
                        rm( revision, revision_index )
                    }   
                    rm( revision_names, revision_exists ) 
                    
                    
                    
                    out_p[ 1L, "date_packaged" ] <- 
                        format( utils::packageDate( pkg = p ), 
                        "%Y-%m-%d" ) 
                    
                    
                    
                    out_p[ 1L, "checksums_checksum" ] <- 
                        package_md5sum( package = p)[[ 
                        "checksums_checksum" ]]
                    
                }else{
                    out_p[ 1L, "package" ] <- p 
                }   
                
                return( out_p )
            }   
        ) ) 
    }   
    
    
    
    if( length( files ) > 0L ){ 
        if( length( files_path ) > 0L ){
            files_with_path <- file.path( files_path, files ) 
        }else{
            files_with_path <- files 
        }   
        
        out[[ "files" ]] <- data.frame(
            "files"         = files, 
            "md5_checksums" = as.character( tools::md5sum( files_with_path ) ), 
            stringsAsFactors = FALSE 
        )   
    }   
    
    
    
    if( length( objects ) > 0L ){ 
        out_o <- unlist( lapply(
            X   = 1:length( objects ), 
            FUN = function(i){
                #   Extract the body of the function into a temporary 
                #   file
                f_file <- tempfile( 
                    pattern = sprintf( "object_%s_", 
                        objects_names[ i ] ), 
                    fileext = ".txt" 
                )   
                
                dput( x = objects[[ i ]], file = f_file ) 
                
                #   Compute MD5 sums
                o_md5 <- as.character( 
                    tools::md5sum( files = f_file ) )
                
                #   Delete the temporary file
                file.remove( f_file )
                
                return( o_md5 )
            }   
        ) )
        
        
        out[[ "objects" ]] <- data.frame(
            "name"          = objects_names, 
            "md5_checksums" = out_o, 
            "class"         = unlist( lapply( 
                X   = 1:length( objects ), 
                FUN = function(i){
                    return( paste( class( objects[[ i ]] ), 
                        collapse = "|" ) ) 
                }   
            ) ), 
            "environment" = unlist( lapply( 
                X   = 1:length( objects ), 
                FUN = function(i){
                    return( environmentName( environment( 
                        objects[[ i ]] ) ) ) 
                }   
            ) ), 
            stringsAsFactors = TRUE ) 
    }   
    
    
    
    out_is_null <- unlist( lapply( X = out, FUN = is.null ) )
    
    return( out[ !out_is_null ] )
}   


