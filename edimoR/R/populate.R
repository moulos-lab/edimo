..getCountries <- function() {
    if (!require("countrycode"))
        stop("R library countrycode is required!")
    cols = c("country.name.en","iso2c","iso3c","iso3n","cldr.name.el")
    countries <- codelist[,cols]
    countries <- countries[!is.na(countries$iso3n),]
    countries$iso3n <- formatC(countries$iso3n,width=3,format="d",flag="0")
    names(countries) <- c("name_en","iso2c","iso3c","iso3n","name_el")
    countries <- as.data.frame(countries)
}

..getAnalysisStatus <- function() {
    return(data.frame(
        value=c(-1L:3L),
        description=c("Failed","Scheduled","Running","Finished","Locked")
    ))
}

..getSex <- function() {
    return(data.frame(
        value=c(1L,2L),
        name=c("Male","Female")
    ))
}

..getOrganisms <- function() {
    return(c("Homo sapiens"," "))
}

..getGenomeVersions <- function() {
    return(data.frame(
        ucsc=c("hg19","hg38"),
        ensembl=c("GRCh37","GRCh38"),
        description=c("Human genome version hg19 (GRCh37)",
            "Human genome version hg38 (GRCh38)")
    ))
}

..getSequencingPlatforms <- function() {
    return(c("Illumina","Ion Torrent"))
}

..getSequencingProtocols <- function() {
    return(c("Panel","WES"))
}

# Most likely temporary - an admin in the future should be able to dynamically
# add/remove institutions
..getInstitutions <- function() {
    return(list(
        list(
            iid=1L,
            institution=paste0("ΕΚΠΑ - Μονάδες Πλασματοκυτταρικών Δυσκρασιών ",
                "της Θεραπευτικής Κλινικής, Ιατρική Σχολή & Κυτταρομετρίας ",
                "Ροής, Τμήμα Βιολογίας"),
            street="Πανεπιστημιούπολη Ζωγράφου",
            city="Αθήνα",
            state="Αττική",
            zip=15772,
            country="Ελλάδα"
        ),
        list(
            iid=2L,
            institution=paste0("ΕΚΠΑ - Εργαστήριο Ιστολογίας-Εμβρυολογίας, ",
                "Ιατρική Σχολή"),
            street="Μικράς Ασίας 75",
            city="Αθήνα",
            state="Αττική",
            zip=11527,
            country="Ελλάδα"
        ),
        list(
            iid=3L,
            institution=paste0("ΕΚΠΑ - Εργαστήριο Ανάλυσης Κυκλοφορούντων ",
                "Καρκινικών Κυττάρων, Εργαστήριο Αναλυτικής Χημείας, Τμήμα ",
                "Χημείας"),
            street="Πανεπιστημιούπολη Ζωγράφου",
            city="Ζωγράφου",
            state="Αττική",
            zip=15772,
            country="Ελλάδα"
        ),
        list(
            iid=4L,
            institution=paste0("Ε.ΚΕ.Β.Ε. Αλέξανδρος Φλέμινγκ"),
            street="Φλέμινγκ 34",
            city="Βάρη",
            state="Αττική",
            zip=16672,
            country="Ελλάδα"
        )
    ))
}
