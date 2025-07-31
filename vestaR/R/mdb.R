openMongo <- function(collection="test",db=NULL,conf=NULL) {
    if (!requireNamespace("mongolite"))
        stop("R package mongolite is required!")
    if (missing(db) || is.null(db))
        db <- 1
    if (missing(conf) || is.null(conf))
        conf <- .getDbCreds()
    
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
mongoConnect <- function(collection,db=c("user","back"),conf=NULL) {
    if (!requireNamespace("mongolite"))
        stop("R package mongolite is required!")
    if (missing(collection))
        stop("A collection must be specified for this function!")
    db <- db[1]
    if (missing(conf) || is.null(conf)) {
        if (db == "user")
            conf <- .getDbCreds()
        else if (db == "back")
            conf <- .getDbBackCreds()
    }
    
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

testMongoConnection <- function(db=NULL,conf=NULL) {
    if (missing(db) || is.null(db))
        db <- 1
    if (missing(conf) || is.null(conf))
        conf <- .getDbCreds()
    con <- tryCatch({
        openMongo("test",db,conf)
    },error=function(e) {
        message("Caught error ",e)
        return(FALSE)
        stop("Cannot open database!")
    },finally="")
    if (is(con,"mongo"))
        mongoDisconnect(con)
}

.checkSanityCollection <- function(creds) {
    con <- mongo(url=.mongoURI(creds))
    collections <- con$run(toJSON(list(listCollections=1),auto_unbox=TRUE))
    con$disconnect()
    #return(any(sapply(collections$cursor$firstBatch,function(x) {
    #    x$name==creds$sanity
    #})))
    return(any(collections$cursor$firstBatch$name==creds$sanity))
}

.initCollections <- function(creds) {
    uri <- .mongoURI(creds)
    con <- mongo(url=uri)
    on.exit(con$disconnect())
    
    schemas <- .localCollectionDef()
    for (n in names(schemas))
        con$run(schemas[[n]])
    
    # We also need to populate certain static collections - options etc.
    message("Populating statics...")
    .populateStatics(uri)
    
    # And initiate an admin user
    if (grepl("edimoclin",uri)) {
        message("Initiating admin user...")
        .initAdminUser(uri)
    }
}

.initAdminUser <- function(uri) {
    udata <- .CONFIG$tmp_admin
    
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
                verification_token=udata$token
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
    
    invisible(con$insert(json))
}

.populateStatics <- function(uri) {
    # Connect to collection
    cons <- mongo(url=uri,collection="statics")
    coni <- mongo(url=uri,collection="institutions")
    on.exit({
        cons$disconnect()
        coni$disconnect()
    })
    
    # Get the static data
    countries <- ..getCountries()
    sex <- ..getSex()
    analysis_status <- ..getAnalysisStatus()
    organism <- ..getOrganisms()
    genome_version <- ..getGenomeVersions()
    sequencing_platform <- ..getSequencingPlatforms()
    sequencing_protocol <- ..getSequencingProtocols()
    sequencing_kit <- ..getSequencingKits()
    variant_type <- ..getVariantTypes()
    variant_zygosity <- ..getVariantZygosities()
    variant_location <- ..getVariantLocation()
    variant_effect_snpeff <- ..getVariantEffectSnpEff()
    pathocuts <- ..getPathocuts()
    so_terms <- ..getVariantSoTerms()
    
    # Construct the data to be batch inserted
    statics <- list(
        static_type="static_form_options",
        countries=countries,
        sex=sex,
        analysis_status=analysis_status,
        organism=organism,
        genome_version=genome_version,
        sequencing_platform=sequencing_platform,
        sequencing_protocol=sequencing_protocol,
        sequencing_kit=sequencing_kit,
        variant_type=variant_type,
        variant_zygosity=variant_zygosity,
        variant_location=variant_location,
        variant_effect_snpeff=variant_effect_snpeff,
        pathocust=pathocuts,
        so_terms=so_terms
    )
    statics <- .toMongoJSON(statics,pretty=T)
    
    cons$insert(statics)
    
    # Instututions
    institutions = ..getInstitutions()
    invisible(lapply(institutions,function(i) {
        coni$insert(.toMongoJSON(i))
    }))
    
}

.localCollectionDef <- function() {
    return(list(
        users=.defineUsersCollection(),
        logs=.defineLogsCollection(),
        statics=.defineStaticsCollection(),
        institutions=.defineInstitutionsCollection(),
        samples=.defineSamplesCollection(),
        analyses=.defineAnalysesCollection(),
        variants=.defineVariantsCollection(),
        filters=.defineFiltersCollection(),
        table_views=.defineTableviewsCollection(),
        sessions=.defineSessionsCollection(),
        varstores=.defineVarstoresCollection()#,
        #states=.defineStatesCollection()#,
        #varstore_variants=.defineVarstoreVariantsCollection()
    ))
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
