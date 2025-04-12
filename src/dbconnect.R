# a function to connect to a PostgreSQL database                                                                                                   
                                                                                                                                                   
library(RPostgreSQL)                                                                                                                               
                                                                                                                                                   
# run similar to this:                                                                                                                             
# foo <- connectdb(dbname = "foo", username = "csde")                                                                                              
# disconnectdb(foo)                                                                                                                                
                                                                                                                                                   
# make a connection                                                                                                                                
connectdb <- connectDB <- function(dbname, host="localhost", port=5432, username){                                                                 
    # if the db connection does not exist                                                                                                          
    if(!exists(dbname)){                                                                                                                           
        # make the connection                                                                                                                      
        conn <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, host=host, port=port, user = username)
        # print a message                                                                                                                          
        message("connecting to ", dbname)                                                                                                          
        return(conn)                                                                                                                               
    }                                                                                                                                              
    # if the db connection exists                                                                                                                  
    if(exists(dbname)){                                                                                                                            
        # check to see if the connection is stale                                                                                                  
        cmd <- sprintf("isPostgresqlIdCurrent(%s)", dbname)                                                                                        
        connectionIsCurrent <- eval(parse(text=cmd))                                                                                               
        # if the connection is stale, reconnect                                                                                                    
        if(!connectionIsCurrent){                                                                                                                  
            message("establishing connection to ", dbname)                                                                                         
            conn <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, host=host, port=port, user=username)                                      
        } else {                                                                                                                                   
            # otherwise assign the string name to the db connection                                                                                
            cmd <- sprintf("conn <- %s", dbname)                                                                                                   
            eval(parse(text=cmd))                                                                                                                  
        }                                                                                                                                          
        return(conn)                                                                                                                               
    }                                                                                                                                              
}                                                                                                                                                  
                                                                                                                                                   
# disconnect one                                                                                                                                   
disconnectdb <- function(conn){                                                                                                                    
    dbname <- dbGetInfo(dbObj = conn)$dbname                                                                                                       
    # check to see if the object exists                                                                                                            
    if(exists(dbname)){                                                                                                                            
        # check to see if the connection is current                                                                                                
        cmd <- sprintf("isPostgresqlIdCurrent(%s)", dbname)                                                                                        
        connectionIsCurrent <- eval(parse(text=cmd))                                                                                               
        if(connectionIsCurrent){                                                                                                                   
            message(sprintf("disconnecting %s", dbname))                                                                                           
            cmd <- sprintf("dbDisconnect(conn = %s)", dbname)                                                                                      
            eval(parse(text = cmd))                                                                                                                
            cmd <- sprintf("rm(%s, envir = .GlobalEnv)", dbname)                                                                                   
            eval(parse(text = cmd))                                                                                                                
                                                                                                                                                   
        }                                                                                                                                          
    }                                                                                                                                              
}                                                                                                                                                  
                                                                                                                                                   
# disconnect all connections                                                                                                                       
disconnectall <- function(verbose = FALSE){                                                                                                        
    # parse over all current connections, disconnecting each one                                                                                   
    O <- lapply(dbListConnections(drv = PostgreSQL()), dbDisconnect)                                                                               
    if(verbose){                                                                                                                                   
        message(O)                                                                                                                                 
    }                                                                                                                                              
}                                                                                                                                                  
                                                                                                                                                   
# options for using sqldf (connect to PostgreSQL rather than SQLite)                                                                               
# settings                                                                                                                                         
sqldf.pg.options <- function(dbname="test", host="localhost", port=5432, user=Sys.getenv("USER") ){                                                
    # get the password                                                                                                                             
    if(Sys.info()['sysname'] == "Windows"){                                                                                                        
        pgpass = scan(file.path(Sys.getenv("APPDATA"), "postgresql/pgpass.conf"), what = "character")                                              
    } else {                                                                                                                                       
        pgpass = scan("~/.pgpass", what = "character")                                                                                             
    }                                                                                                                                              
    passwd <- strsplit(pgpass, ":")[[1]][5]                                                                                                        
                                                                                                                                                   
    # set the options                                                                                                                              
    options(                                                                                                                                       
        sqldf.RPostgreSQL.user = user,                                                                                                             
        sqldf.RPostgreSQL.password = passwd,                                                                                                       
        sqldf.RPostgreSQL.dbname = dbname,                                                                                                         
        sqldf.RPostgreSQL.host = host,                                                                                                             
        sqldf.RPostgreSQL.port = port                                                                                                              
    )                                                                                                                                              
}                                                                                                                                                  
                                                                                                                                                   
# convenience function for testing whether a db table exists?                                                                                      
tExists <- dbTableExists <- function(conn, table_schema, table_name){                                                                              
    sql <- sprintf("select count(*) = 1 from information_schema.tables where table_schema = '%s' and table_name = '%s';", table_schema, table_name)
    dbGetQuery(conn = conn, sql)[1,1]                                                                                                              
}    
