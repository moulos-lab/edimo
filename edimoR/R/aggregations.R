.getVariantStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    pipeline <- list(
        list(
            "$match"=list(
                "analysis_id"=list(`$oid`=aid)
            )
        ),
        list(
            "$unwind"="$identity.type"
        ),
        list(
            "$group"=list(
                "_id"="$identity.type",
                "count"=list(
                    "$sum"=1
                )
            )
        ),
        list(
            "$project"=list(
                "_id"=0L,
                "type"="$_id",
                "count"="$count"
            )
        )
    )
    
    return(con$aggregate(.toMongoJSON(pipeline)))
}

.getVariantStatusStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    # All query
    allQuery <- list(
        "analysis_id"=list(`$oid`=aid)
    )
    # Novel query
    novelQuery <- list(
        "analysis_id"=list(`$oid`=aid),
        "identity.rsid"=list(
            "$eq"=NULL
        )
    )
    
    allvars <- con$count(.toMongoJSON(allQuery))
    novelvars <- con$count(.toMongoJSON(novelQuery))
    
    return(list(
        known=allvars-novelvars,
        novel=novelvars
    ))
}

.getVariantPopulationStats <- function() {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    # All variants query
    allQuery <- list(
        "analysis_id"=list(`$oid`=aid)
    )
    # TGP variants query
    tgpQuery <- list(
        "analysis_id"=list(`$oid`=aid),
        "population.tgp.af"=list(
            "$ne"=NULL
        )
    )
    # ExAC variants query
    exacQuery <- list(
        "analysis_id"=list(`$oid`=aid),
        "population.exac.af"=list(
            "$ne"=NULL
        )
    )
    # ESP variants query
    espQuery <- list(
        "analysis_id"=list(`$oid`=aid),
        "population.esp.af"=list(
            "$ne"=NULL
        )
    )
    # UK10K variants query
    uk10kQuery <- list(
        "analysis_id"=list(`$oid`=aid),
        "population.uk10k.af"=list(
            "$ne"=NULL
        )
    )
    # ALFA variants query
    alfaQuery <- list(
        "analysis_id"=list(`$oid`=aid),
        "population.alfa.af"=list(
            "$ne"=NULL
        )
    )
    # gnomAD exomes variants query
    gnomadExomesQuery <- list(
        "analysis_id"=list(`$oid`=aid),
        "population.gnomad_exomes.af"=list(
            "$ne"=NULL
        )
    )
    # gnomAD exomes variants query
    gnomadGenomesQuery <- list(
        "analysis_id"=list(`$oid`=aid),
        "population.gnomad_genomes.af"=list(
            "$ne"=NULL
        )
    )
    
    # Count
    allVars <- con$count(.toMongoJSON(allQuery))
    tgpVars <- con$count(.toMongoJSON(tgpQuery))
    exacVars <- con$count(.toMongoJSON(exacQuery))
    espVars <- con$count(.toMongoJSON(espQuery))
    uk10kVars <- con$count(.toMongoJSON(uk10kQuery))
    alfaVars <- con$count(.toMongoJSON(alfaQuery))
    gnomadExomesVars <- con$count(.toMongoJSON(gnomadExomesQuery))
    gnomadGenomesVars <- con$count(.toMongoJSON(gnomadGenomesQuery))
    
    return(list(
        in_tgp=tgpVars,
        nin_tgp=allVars-tgpVars,
        in_exac=exacVars,
        nin_exac=allVars-exacVars,
        in_esp=espVars,
        nin_esp=allVars-espVars,
        in_uk10k=uk10kVars,
        nin_uk10k=allVars-uk10kVars,
        in_alfa=alfaVars,
        nin_alfa=allVars-alfaVars,
        in_gnomad_exomes=gnomadExomesVars,
        nin_gnomad_exomes=allVars-gnomadExomesVars,
        in_gnomad_genomes=gnomadGenomesVars,
        nin_gnomad_genomes=allVars-gnomadGenomesVars
    ))   
}

.getVariantQualityRanges <- function() {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    retVal <- list(
        minQual=0,
        maxQual=0,
        minDp=0L,
        maxDp=0L
    )
    
    # Min qual
    minQualQuery <- list(
        "analysis_id"=list(`$oid`=aid)
    )
    minQualFields <- list(
        "_id"=0L,
        "identity.qual"=1L
    )
    minQualSort <- list(
        "identity.qual"=1L
    )
    
    result <- con$find(
        query=.toMongoJSON(minQualQuery),
        fields=.toMongoJSON(minQualFields),
        sort=.toMongoJSON(minQualSort),
        limit=1
    )
    if (nrow(result) > 0)
        retVal$minQual <- result[[1]]$qual
        
    # Max qual
    maxQualQuery <- list(
        "analysis_id"=list(`$oid`=aid)
    )
    maxQualFields <- list(
        "_id"=0L,
        "identity.qual"=1L
    )
    maxQualSort <- list(
        "identity.qual"=-1L
    )
    
    result <- con$find(
        query=.toMongoJSON(maxQualQuery),
        fields=.toMongoJSON(maxQualFields),
        sort=.toMongoJSON(maxQualSort),
        limit=1
    )
    if (nrow(result) > 0)
        retVal$maxQual <- result[[1]]$qual
    
    # Min depth
    minDpQuery <- list(
        "analysis_id"=list(`$oid`=aid)
    )
    minDpFields <- list(
        "_id"=0L,
        "metrics.dp"=1L
    )
    minDpSort <- list(
        "metrics.dp"=1L
    )
    
    result <- con$find(
        query=.toMongoJSON(minDpQuery),
        fields=.toMongoJSON(minDpFields),
        sort=.toMongoJSON(minDpSort),
        limit=1
    )
    if (nrow(result) > 0)
        retVal$minDp <- result[[1]]$dp
        
    # Max depth
    maxDpQuery <- list(
        "analysis_id"=list(`$oid`=aid)
    )
    maxDpFields <- list(
        "_id"=0L,
        "metrics.dp"=1L
    )
    maxDpSort <- list(
        "metrics.dp"=-1L
    )
    
    result <- con$find(
        query=.toMongoJSON(maxDpQuery),
        fields=.toMongoJSON(maxDpFields),
        sort=.toMongoJSON(maxDpSort),
        limit=1
    )
    if (nrow(result) > 0)
        retVal$maxDp <- result[[1]]$dp
    
    return(retVal)
}
