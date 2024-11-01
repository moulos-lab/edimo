mongoConnect <- function(conf,db) {
    if (!requireNamespace("mongolite"))
        stop("R package mongolite is required!")
    if (missing(db))
        db <- 1
    
    .validateConf(conf)
    if (is.character(conf))
        creds <- fromJSON(conf)
    else
        creds <- conf
    
    creds <- .validateCreds(creds[[db]])
    uri <- .mongoURI(creds)
    
    if (!.checkSanityCollection(uri)) {
        message("Creating collections...")
        .initCollections(uri)
    }
    
    #return(con)
}

.checkSanityCollection <- function(uri) {
    con <- mongo(url=uri)
    collections <- con$run(toJSON(list(listCollections=1),auto_unbox=TRUE))
    con$disconnect()
    return(any(sapply(collections$cursor$firstBatch,function(x) {
        x$name==creds$sanity
    })))
}


mongoDisconnect <- function(con) {
    invisible(con$disconnect())
}

#~ testDbConnection <- function(conf,db) {
#~     if (missing(db))
#~      db <- 1
#~     con <- tryCatch(openDatabase(conf,db),error=function(e) {
#~         message("Caught error ",e)
#~         stop("Cannot open database!")
#~     },finally="")
#~     mongoDisconnect(con)
#~ }

.initCollections <- function(uri) {
    schemas <- .localCollectionDef()
    con <- mongo(url=uri)
    for (n in names(schemas))
        con$run(schemas[[n]])
}

.localCollectionDef <- function() {
    return(list(
        users=.defineUsersCollection()
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
                "date_updated": { "bsonType": "date" },
                "last_login": { "bsonType": "date" },
                "last_login_attempt": { "bsonType": "date" },
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
                "dob": { "bsonType": "date" },
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
