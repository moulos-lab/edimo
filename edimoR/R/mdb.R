openMongo <- function(conf,db,collection="test") {
    if (!requireNamespace("mongolite"))
        stop("R package mongolite is required!")
    if (missing(db))
        db <- 1
    
    .validateConf(conf)
    if (is.character(conf))
        creds <- fromJSON(conf)
    else
        creds <- conf
    
    if ("databases" %in% names(conf))
        creds <- .validateCreds(creds$databases[[db]])
    else
        creds <- .validateCreds(creds)
    
    if (!.checkSanityCollection(creds)) {
        message("Creating collections...")
        .initCollections(creds)
    }
    
    con <- mongo(url=.mongoURI(creds),collection=collection)
    return(con)
}

# A lighter version - assuming ready database and credentials
mongoConnect <- function(conf,collection) {
    if (!requireNamespace("mongolite"))
        stop("R package mongolite is required!")
    
    .validateConf(conf)
    if (is.character(conf))
        creds <- .validateCreds(fromJSON(conf))
    else
        creds <- .validateCreds(conf)
    return(mongo(url=.mongoURI(creds),collection=collection))
}

mongoDisconnect <- function(con) {
    invisible(con$disconnect())
}

testMongoConnection <- function(conf,db) {
    if (missing(db))
     db <- 1
    con <- tryCatch(openMongo(conf,db),error=function(e) {
        message("Caught error ",e)
        stop("Cannot open database!")
    },finally="")
    mongoDisconnect(con)
}

.checkSanityCollection <- function(creds) {
    con <- mongo(url=.mongoURI(creds))
    collections <- con$run(toJSON(list(listCollections=1),auto_unbox=TRUE))
    con$disconnect()
    return(any(sapply(collections$cursor$firstBatch,function(x) {
        x$name==creds$sanity
    })))
}


.initCollections <- function(creds) {
    uri <- .mongoURI(creds)
    schemas <- .localCollectionDef()
    con <- mongo(url=uri)
    for (n in names(schemas))
        con$run(schemas[[n]])
    
    ## We also need to populate certain static collections - options etc.
    #message("Populating statics...")
    #.populateStatics(uri)
    
    # And initiate an admin user
    if (grepl("edimoclin",uri)) {
        message("Initiating admin user...")
        .initAdminUser(uri)
    }
}

.initAdminUser <- function(uri) {
    udata <- CONFIG$tmp_admin
    
    con <- mongo(url=uri,collection="users")
    on.exit(con$disconnect())
    
    currentTime <- Sys.time()
    # Below - serve as example on how to insert dates
    # If not use unbox, toJSON converts to array even with auto_unbox
    # Also mongo_options(date_as_char=TRUE) on bootstrap
    tmpAdminUser <- list(
        username=udata$user,
        password=list(
            bcrypt=bcrypt::hashpw(udata$password)
        ),
        metadata=list(
            date_created=unbox(currentTime),
            date_updated=NULL,
            last_login=NULL,
            last_login_attempt=NULL,
            login_attempts=0,
            role="administrator",
            account_locked=FALSE
        ),
        emails = list(
            list(
                address=udata$email,
                verified=TRUE,
                main=TRUE,
                verification_token="a13Cvn6aPFGgyTtv"
            )
        ),
        profile=list(
            name=udata$name,
            surname=udata$surname,
            dob=unbox(as.POSIXct("1990-01-15T00:00:00Z",tz="EET")),
            phone=list(
                fix="+1234567890",
                mobile="+1234567891"
            ),
            address=list(
                institution="Admin Institution",
                street="Admin str 1",
                city="Admina",
                state=NULL,
                zip="99999",
                country="Greece"
            )
        )
    )
    
    json <- .toMongoJSON(tmpAdminUser)
    
    con$insert(json)
}

.toMongoJSON <- function(dat,...) {
    return(toJSON(dat,auto_unbox=TRUE,null="null",na="null",POSIXt="mongo",...))
}

.localCollectionDef <- function() {
    return(list(
        users=.defineUsersCollection()
        logs=.defineLogsCollection()
    ))
}

.defineUsersCollection <- function() {
    return(
    '{
      "create": "users",
      "validator": {
        "$jsonSchema": {
          "bsonType": "object",
          "required": ["username","password","metadata"],
          "properties": {
            "username": { "bsonType": "string" },
            "password": {
              "bsonType": "object",
              "required": ["bcrypt"],
              "properties": {
                "bcrypt": { "bsonType": "string" }
              }
            },
            "metadata": {
              "bsonType": "object",
              "required": ["date_created","date_updated","last_login"],
              "properties": {
                "date_created": { "bsonType": "date" },
                "date_updated": { "bsonType": ["date","null"] },
                "last_login": { "bsonType": ["date","null"] },
                "last_login_attempt": { "bsonType": ["date","null"] },
                "login_attempts": { "bsonType": "int" },
                "role": { "bsonType": "string" },
                "account_locked": { "bsonType": "bool" }
              }
            },
            "emails": {
              "bsonType": "array",
              "items": {
                "bsonType": "object",
                "properties": {
                  "address": { "bsonType": "string" },
                  "verified": { "bsonType": "bool" },
                  "main": { "bsonType": "bool" },
                  "verification_token": { "bsonType": "string" }
                }
              }
            },
            "profile": {
              "bsonType": "object",
              "properties": {
                "name": { "bsonType": "string" },
                "surname": { "bsonType": "string" },
                "dob": { "bsonType": ["date","null"] },
                "phone": {
                  "bsonType": "object",
                  "properties": {
                    "fix": { "bsonType": ["string","null"] },
                    "mobile": { "bsonType": "string" }
                  }
                },
                "address": {
                  "bsonType": "object",
                  "properties": {
                    "institution": { "bsonType": ["string","null"] },
                    "street": { "bsonType": ["string","null"] },
                    "city": { "bsonType": ["string","null"] },
                    "state": { "bsonType": ["string","null"] },
                    "zip": { "bsonType": ["string","null"] },
                    "country": { "bsonType": "string"}
                  }
                }
              }
            }
          },
          "additionalProperties": true
        }
      },
      "collation": {
        "locale": "el",
        "strength": 2
      }
    }'
    )
}

.defineLogsCollection <- function() {
    return(
    '{
        "create": "logs",
        "validator": {
            "$jsonSchema": {
                "bsonType": "object",
                "required": ["timestamp","level"],
                "properties": {
                    "timestamp": { "bsonType": "date" },
                    "level": {
                        "bsonType": "string",
                        "maxLength": 16
                    },
                    "caller": {
                        "bsonType": ["string", "null"],
                        "maxLength": 128
                    },
                    "message": {
                        "bsonType": ["string", "null"],
                    },
                    "sys_uname": {
                        "bsonType": ["string", "null"],
                        "maxLength": 64
                    },
                    "user_name": {
                        "bsonType": ["string", "null"],
                        "maxLength": 128
                    }
                },
                "additionalProperties": true
            }
        },
        "collation": {
            "locale": "el",
            "strength": 2
        }
    }'
    )
}
.mongoURI <- function(creds) {
    url <- "mongodb://"
    if (creds$username!="" && creds$password=="")
        url <- paste0(url,creds$username,"@")
    if (creds$username!="" && creds$password!="")
        url <- paste0(url,creds$username,":",creds$password,"@")
    url <- paste0(url,creds$host,":",creds$port,"/",creds$db)
    return(url)
}

.validateCreds <- function(creds) {
    if (is.null(creds$host) || is.na(creds$host) || creds$host == "") {
        warning("MongoDB host not provided! Assuming default (127.0.0.1)...",
            immediate.=TRUE)
        creds$host <- "127.0.0.1"
    }
    if (is.null(creds$port) || is.na(creds$port) || creds$port == "") {
        warning("MongoDB port not provided! Assuming default (27017)...",
            immediate.=TRUE)
        creds$port <- "27017"
    }
    if (is.null(creds$username) || is.na(creds$username) 
        || creds$username == "") {
        warning("MongoDB username not provided! Assuming no username...",
            immediate.=TRUE)
        creds$username <- ""
    }
    if (is.null(creds$password) || is.na(creds$password) 
        || creds$password == "") {
        warning("MongoDB password not provided! Assuming no password...",
            immediate.=TRUE)
        creds$password <- ""
    }
    if (is.null(creds$db) || is.na(creds$db) || creds$db == "") {
        warning("MongoDB database not provided! Assuming default (test)...",
            immediate.=TRUE)
        creds$db <- "test"
    }
    return(creds)
}

.validateConf <- function(conf) {
    if (missing(conf))
        stop("A configuration JSON file must be provided!")
    if (!is.list(conf)) {
        if (!is.character(conf))
            stop("The configuration file must be a string!")
        if (!file.exists(conf))
            stop("The configuration file must be an existing file!")
    }
}

#.defineUsersCollection <- function() {
#    return(list(
#        validator=list(
#            `$jsonSchema`=list(
#                bsonType="object",
#                required=c("username","password","metadata"),
#                properties=list(
#                    username=list(bsonType="string"),
#                    password=list(
#                        bsonType="object",
#                        required=c("bcrypt"),
#                        properties=list(
#                            bcrypt=list(bsonType="string")
#                        )
#                    ),
#                    metadata=list(
#                        bsonType="object",
#                        required=c("date_created","date_updated","last_login"),
#                        properties=list(
#                            date_created=list(bsonType="date"),
#                            date_updated=list(bsonType="date"),
#                            last_login=list(bsonType="date"),
#                            last_login_attempt=list(bsonType="date"),
#                            login_attempts=list(bsonType="int"),
#                            role=list(bsonType="string"),
#                            account_locked=list(bsonType="bool")
#                        )
#                    ),
#                    emails=list(
#                        bsonType="array",
#                        items=list(
#                            bsonType="object",
#                            properties=list(
#                                address=list(bsonType="string"),
#                                verified=list(bsonType="bool"),
#                                main=list(bsonType="bool"),
#                                verification_token=list(bsonType="string")
#                            )
#                        )
#                    ),
#                    profile=list(
#                        bsonType="object",
#                        properties=list(
#                            name=list(bsonType="string"),
#                            surname=list(bsonType="string"),
#                            dob=list(bsonType="date"),
#                            phone=list(
#                                bsonType="object",
#                                properties=list(
#                                    fix=list(bsonType="string"),
#                                    mobile=list(bsonType="string")
#                                )
#                            ),
#                            address=list(
#                                bsonType="object",
#                                properties=list(
#                                    institution=list(bsonType="string"),
#                                    street=list(bsonType="string"),
#                                    city=list(bsonType="string"),
#                                    state=list(bsonType="string"),
#                                    zip=list(bsonType="string"),
#                                    country=list(bsonType="string")
#                                )
#                            )
#                        )
#                    )
#                )
#            )
#        ),
#        # Add the collation for Greek locale with case-insensitivity
#        collation=list(
#            locale="el",
#            strength=2  # Strength 2 for case-insensitive comparisons
#        )
#    ))
#}
