updateAnalysisStats <- function(aid) {
    con <- mongoConnect("analyses")
    on.exit(mongoDisconnect(con))
    
    # At this point default filters are same as quality ranges
    qualRanges <- .getVariantQualityRanges(aid)
    defaultFilter <- list(
        qual=list(
            low=qualRanges$minQual,
            high=qualRanges$maxQual
        ),
        dp=list(
            low=qualRanges$minDp,
            high=qualRanges$maxDp
        )
    )
    
    filterQuery <- .toMongoJSON(list(
        `_id`=list(`$oid`=aid)
    ))
    updateQuery <- .toMongoJSON(list(
        `$set`=list(
            stats.variantType=.getVariantStats(aid),
            stats.variantStatus=.getVariantStatusStats(aid),
            stats.variantPopulation=.getVariantPopulationStats(aid),
            stats.variantQualityRanges=qualRanges,
            stats.defaultFilter=defaultFilter,
            stats.variantCgdInherit=.getVariantCgdInheritStats(aid),
            stats.variantLocation=.getVariantLocationStats(aid),
            stats.summaryImpact=.getSummaryImpactStats(aid),
            stats.effectImpact=.getEffectImpactStats(aid),
            stats.variantZygosity=.getVariantZygosityStats(aid),
            stats.variantPredictionSummary=.getVariantPredictionSummaryStats(aid),
            stats.variantPathogenRanges=.getVariantPathogenRanges(aid),
            stats.variantPopulationRanges=.getVariantPopulationRanges(aid),
            stats.variantClinDb=.getVariantClinDbStats(aid),
            stats.geneClinDb=.getGeneClinDbStats(aid)
        )
    ))
    
    return(con$update(filterQuery,updateQuery))
}

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

.getVariantZygosityStats <- function(aid) {
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
                "_id"="$genotypes.gn",
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
    
    result <- con$aggregate(.toMongoJSON(pipeline))
    if ("1/0" %in% result$type) {
        heti <- which(result$type == "0/1")
        remi <- which(result$type == "1/0")
        result[heti,"count"] <- result[heti,"count"] + result[remi,"count"]
        result <- result[-remi,,drop=FALSE]
    }
    
    return(result)
}

.getVariantPopulationStats <- function(aid) {
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

.getVariantQualityRanges <- function(aid) {
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
        retVal$minQual <- as.numeric(result[[1]]$qual)
        
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
        retVal$maxQual <- as.numeric(result[[1]]$qual)
    
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

.getVariantCgdInheritStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    pipeline <- list(
        list(
            "$match"=list(
                "analysis_id"=list(`$oid`=aid)
            )
        ),
        list(
            "$unwind"="$annotation.genes"
        ),
        list(
            "$unwind"="$annotation.genes.cgd"
        ),
        list(
            "$group"=list(
                "_id"="$annotation.genes.cgd.inheritance",
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

.getVariantLocationStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    pipeline <- list(
        list(
            "$match"=list(
                "analysis_id"=list(`$oid`=aid)
            )
        ),
        list(
            "$unwind"="$annotation.genes"
        ),
        list(
            "$unwind"="$annotation.genes.transcripts"
        ),
        list(
            "$group"=list(
                "_id"="$annotation.genes.transcripts.location",
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
    
    ## Alternative pipeline for canonical transcripts
    #pipeline <- list(
    #    list(
    #        "$match"=list(
    #            "analysis_id"=list(`$oid`=aid)
    #        )
    #    ),
    #    list(
    #        "$unwind"="$annotation.genes"
    #    ),
    #    list(
    #        "$unwind"="$annotation.genes.transcripts"
    #    ),
    #    list(
    #        "$group"=list(
    #            "_id"="$_id",
    #            "canonical"=list(
    #                "$first"="$annotation.genes.transcripts.location"
    #            )
    #        )
    #    ),
    #    list(
    #        "$group"=list(
    #            "_id"="$canonical",
    #            "count"=list(
    #                "$sum"=1
    #            )
    #        )
    #    ),
    #    list(
    #        "$project"=list(
    #            "_id"=0L,
    #            "type"="$_id",
    #            "count"="$count"
    #        )
    #    )
    #)
    
    return(con$aggregate(.toMongoJSON(pipeline)))
}

.getSummaryImpactStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    pipeline <- list(
        list(
            "$match"=list(
                "analysis_id"=list(`$oid`=aid)
            )
        ),
        list(
            "$unwind"="$annotation.genes"
        ),
        list(
            "$unwind"="$annotation.genes.transcripts"
        ),
        list(
            "$group"=list(
                "_id"="$annotation.genes.transcripts.impact_snpeff",
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
    
    ## Alternative pipeline for canonical transcripts
    #pipeline <- list(
    #    list(
    #        "$match"=list(
    #            "analysis_id"=list(`$oid`=aid)
    #        )
    #    ),
    #    list(
    #        "$unwind"="$annotation.genes"
    #    ),
    #    list(
    #        "$unwind"="$annotation.genes.transcripts"
    #    ),
    #    list(
    #        "$group"=list(
    #            "_id"="$_id",
    #            "canonical"=list(
    #                "$first"="$annotation.genes.transcripts.impact_snpeff"
    #            )
    #        )
    #    ),
    #    list(
    #        "$group"=list(
    #            "_id"="$canonical",
    #            "count"=list(
    #                "$sum"=1
    #            )
    #        )
    #    ),
    #    list(
    #        "$project"=list(
    #            "_id"=0L,
    #            "type"="$_id",
    #            "count"="$count"
    #        )
    #    )
    #)
    
    return(con$aggregate(.toMongoJSON(pipeline)))
}

.getEffectImpactStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    pipeline <- list(
        list(
            "$match"=list(
                "analysis_id"=list(`$oid`=aid)
            )
        ),
        list(
            "$unwind"="$annotation.genes"
        ),
        list(
            "$unwind"="$annotation.genes.transcripts"
        ),
        list(
            "$group"=list(
                "_id"="$annotation.genes.transcripts.impact_so",
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
    
    ## Alternative pipeline for canonical transcripts
    #pipeline <- list(
    #    list(
    #        "$match"=list(
    #            "analysis_id"=list(`$oid`=aid)
    #        )
    #    ),
    #    list(
    #        "$unwind"="$annotation.genes"
    #    ),
    #    list(
    #        "$unwind"="$annotation.genes.transcripts"
    #    ),
    #    list(
    #        "$group"=list(
    #            "_id"="$_id",
    #            "canonical"=list(
    #                "$first"="$annotation.genes.transcripts.impact_so"
    #            )
    #        )
    #    ),
    #    list(
    #        "$group"=list(
    #            "_id"="$canonical",
    #            "count"=list(
    #                "$sum"=1
    #            )
    #        )
    #    ),
    #    list(
    #        "$project"=list(
    #            "_id"=0L,
    #            "type"="$_id",
    #            "count"="$count"
    #        )
    #    )
    #)
    
    return(con$aggregate(.toMongoJSON(pipeline)))
}

.getVariantPredictionSummaryStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    ..makeVarPredSumPipeline <- function(aid,scr) {
        return(list(
            list(
                "$match"=list(
                    "analysis_id"=list(`$oid`=aid)
                )
            ),
            list(
                "$unwind"=list(
                    "path"=paste0("$pathogenicity.",scr,"_pred"),
                    "preserveNullAndEmptyArrays"=TRUE
                )
            ),
            list(
                "$group"=list(
                    "_id"=paste0("$pathogenicity.",scr,"_pred"),
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
        ))
    }
    
    pipelines <- list(
        sift=..makeVarPredSumPipeline(aid,"sift"),
        sift_4g=..makeVarPredSumPipeline(aid,"sift4g"),
        polyphen2_hdiv=..makeVarPredSumPipeline(aid,"polyphen2_hdiv"),
        polyphen2_hvar=..makeVarPredSumPipeline(aid,"polyphen2_hvar"),
        lrt=..makeVarPredSumPipeline(aid,"lrt"),
        mutationtaster=..makeVarPredSumPipeline(aid,"mutationtaster"),
        mutationassessor=..makeVarPredSumPipeline(aid,"mutationassessor"),
        fathmm=..makeVarPredSumPipeline(aid,"fathmm"),
        provean=..makeVarPredSumPipeline(aid,"provean"),
        metasvm=..makeVarPredSumPipeline(aid,"metasvm"),
        metalr=..makeVarPredSumPipeline(aid,"metalr"),
        metarnn=..makeVarPredSumPipeline(aid,"metarnn"),
        m_cap=..makeVarPredSumPipeline(aid,"m_cap"),
        primateai=..makeVarPredSumPipeline(aid,"primateai"),
        deogen2=..makeVarPredSumPipeline(aid,"deogen2"),
        clinpred=..makeVarPredSumPipeline(aid,"clinpred"),
        alphamissense=..makeVarPredSumPipeline(aid,"alphamissense")#,
        #aloft=..makeVarPredSumPipeline(aid,"aloft")
    )
    
    results <- lapply(pipelines,function(x) {
        y <- con$aggregate(.toMongoJSON(x))
        y$type[is.na(y$type)] <- "U"
        return(y)
    })
    names(results) <- names(pipelines)
    
    return(results)
}

.getVariantPathogenRanges <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    ..makeVarPathRangeQueries <- function(aid,scr) {
        main <- list(
            "analysis_id"=list(`$oid`=aid),
            tobenamed=list(
                "$ne"=NULL
            )
        )
        mins <- list(tobenamed=1L)
        maxs <- list(tobenamed=-1L)
        f <- list("_id"=0L,tobenamed=1L)
        names(main)[2] <- names(mins)[1] <- names(maxs)[1] <- names(f)[2] <-
            paste0("pathogenicity.",scr,"_score")
        return(list(main=main,sortmin=mins,sortmax=maxs,fields=f))
    }
    
    queries <- list(
        sift=..makeVarPathRangeQueries(aid,"sift"),
        sift4g=..makeVarPathRangeQueries(aid,"sift4g"),
        polyphen2_hdiv=..makeVarPathRangeQueries(aid,"polyphen2_hdiv"),
        polyphen2_hvar=..makeVarPathRangeQueries(aid,"polyphen2_hvar"),
        lrt=..makeVarPathRangeQueries(aid,"lrt"),
        mutationtaster=..makeVarPathRangeQueries(aid,"mutationtaster"),
        mutationassessor=..makeVarPathRangeQueries(aid,"mutationassessor"),
        fathmm=..makeVarPathRangeQueries(aid,"fathmm"),
        provean=..makeVarPathRangeQueries(aid,"provean"),
        metasvm=..makeVarPathRangeQueries(aid,"metasvm"),
        metalr=..makeVarPathRangeQueries(aid,"metalr"),
        metarnn=..makeVarPathRangeQueries(aid,"metarnn"),
        m_cap=..makeVarPathRangeQueries(aid,"m_cap"),
        primateai=..makeVarPathRangeQueries(aid,"primateai"),
        deogen2=..makeVarPathRangeQueries(aid,"deogen2"),
        clinpred=..makeVarPathRangeQueries(aid,"clinpred"),
        alphamissense=..makeVarPathRangeQueries(aid,"alphamissense"),
        dann=..makeVarPathRangeQueries(aid,"dann"),
        vest4=..makeVarPathRangeQueries(aid,"vest4"),
        revel=..makeVarPathRangeQueries(aid,"revel")
    )
    
    results <- lapply(queries,function(x) {
        # Find mininum score
        minScore <- con$find(
            query=.toMongoJSON(x$main),
            fields=.toMongoJSON(x$fields),
            sort=.toMongoJSON(x$sortmin),
            limit=1
        )
        if (nrow(minScore) > 0) {
            maxScore <- con$find(
                query=.toMongoJSON(x$main),
                fields=.toMongoJSON(x$fields),
                sort=.toMongoJSON(x$sortmax),
                limit=1
            )
            return(list(
                min=minScore[[1]][1,1],
                max=maxScore[[1]][1,1],
                exists=TRUE
            ))
        }
        else {
            return(list(
                min=NA,
                max=NA,
                exists=FALSE
            ))
        }
    })
    names(results) <- names(queries)
    
    return(results)
}

.getVariantPopulationRanges <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    ..makeVarPopRangeQueries <- function(aid,pop) {
        main <- list(
            "analysis_id"=list(`$oid`=aid),
            tobenamed=list(
                "$ne"=NULL
            )
        )
        mins <- list(tobenamed=1L)
        maxs <- list(tobenamed=-1L)
        f <- list("_id"=0L,tobenamed=1L)
        names(main)[2] <- names(mins)[1] <- names(maxs)[1] <- names(f)[2] <-
            paste0("population.",pop,".af")
        return(list(main=main,sortmin=mins,sortmax=maxs,fields=f))
    }
    
    queries <- list(
        tgp=..makeVarPopRangeQueries(aid,"tgp"),
        exac=..makeVarPopRangeQueries(aid,"exac"),
        esp=..makeVarPopRangeQueries(aid,"esp"),
        uk10k=..makeVarPopRangeQueries(aid,"uk10k"),
        alfa=..makeVarPopRangeQueries(aid,"alfa"),
        gnomad_exomes=..makeVarPopRangeQueries(aid,"gnomad_exomes"),
        gnomad_genomes=..makeVarPopRangeQueries(aid,"gnomad_genomes")
    )
    
    results <- lapply(queries,function(x) {
        # Find mininum score
        minAf <- con$find(
            query=.toMongoJSON(x$main),
            fields=.toMongoJSON(x$fields),
            sort=.toMongoJSON(x$sortmin),
            limit=1
        )
        if (nrow(minAf) > 0) {
            maxAf <- con$find(
                query=.toMongoJSON(x$main),
                fields=.toMongoJSON(x$fields),
                sort=.toMongoJSON(x$sortmax),
                limit=1
            )
            return(list(
                min=minAf[[1]][1,1],
                max=maxAf[[1]][1,1],
                exists=TRUE
            ))
        }
        else {
            return(list(
                min=NA,
                max=NA,
                exists=FALSE
            ))
        }
    })
    names(results) <- names(queries)
    
    return(results)
}

.getVariantClinDbStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    retVal <- list(
        disgenet=0,
        pharmgkb=0,
        oncokb=0,
        civic=0,
        clinvar_sig=0,
        clinvar_onc=0
    )
    
    retVal$disgenet <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.variant.disgenet"=list("$ne"=NULL)
    )))
    retVal$pharmgkb <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.variant.pharmgkb"=list("$ne"=NULL)
    )))
    retVal$oncokb <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.variant.oncokb"=list("$ne"=NULL)
    )))
    retVal$civic <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.variant.civic"=list("$ne"=NULL)
    )))
    retVal$clinvar_sig <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "pathogenicity.clinvar_clnsig"=list("$ne"=NULL)
    )))
    retVal$clinvar_onc <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "pathogenicity.clinvar_onc"=list("$ne"=NULL)
    )))
    
    return(retVal)
}

.getGeneClinDbStats <- function(aid) {
    con <- mongoConnect("variants")
    on.exit(mongoDisconnect(con))
    
    retVal <- list(
        disgenet=0,
        hpo=0,
        ctd=0,
        cgd=0,
        omim=0,
        omimMorbid=0
    )
    
    retVal$disgenet <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.genes.disgenet"=list("$ne"=NULL)
    )))
    retVal$hpo <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.genes.hpo"=list("$ne"=NULL)
    )))
    retVal$ctd <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.genes.ctd"=list("$ne"=NULL)
    )))
    retVal$cgd <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.genes.cgd"=list("$ne"=NULL)
    )))
    retVal$omim <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.genes.omim"=list("$ne"=NULL)
    )))
    retVal$omimMorbid <- con$count(.toMongoJSON(list(
        analysis_id=list(`$oid`=aid),
        "annotation.genes.omim_morbid"=list("$ne"=NULL)
    )))
    
    return(retVal)
}
