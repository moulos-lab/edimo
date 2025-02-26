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
        ucsc=c("hg19","hg38","From sample"),
        ensembl=c("GRCh37","GRCh38","From sample"),
        description=c("Human genome version hg19 (GRCh37)",
            "Human genome version hg38 (GRCh38)","From sample")
    ))
}

..getSequencingPlatforms <- function() {
    return(c("Illumina","Ion Torrent"))
}

..getSequencingProtocols <- function() {
    return(c("Panel","WES"))
}

..getSequencingKits <- function() {
    return(c("Kit 1","Kit 2"))
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

..getPathocuts <- function() {
    return(list(
        sift=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=0.05,
                D=0.05
            )
        ),
        sift4g=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=0.05,
                D=0.05
            )
        ),
        polyphen2_hdiv=list(
            pre=list(
                B="Benign",
                P="Possibly",
                D="Probably"
            ),
            cut=list(
                B=0.452,
                P=0.454,
                D=0.957
            )
        ),
        polyphen2_hvar=list(
            pre=list(
                B="Benign",
                P="Possibly",
                D="Probably"
            ),
            cut=list(
                B=0.446,
                P=0.447,
                D=0.909
            )
        ),
        lrt=list(
            pre=list(
                N="Neutral",
                D="Deleterious",
                U="Unknown"
            ),
            cut=list(
                N=0.01,
                D=0.01,
                U=0.01
            )
        ),
        mutationtaster=list(
            pre=list(
                A="Disease causing automatic",
                N="Polymorphism",
                D="Disease causing",
                P="Polymorphism automatic",
                Z="Unavailable"
            ),
            cut=list(
                N=0.5,
                D=0.5
            )
        ),
        mutationassessor=list(
            pre=list(
                N="Neutral",
                L="Low",
                M="Medium",
                H="High"
            ),
            cut=list(
                N=0.8,
                L=0.8,
                M=1.935,
                H=3.5
            )
        ),
        fathmm=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=-1.5,
                D=-1.5
            )
        ),
        provean=list(
            pre=list(
                N="Neutral", 
                D="Damaging"
            ),
            cut=list(
                T=-2.5,
                D=-2.5
            )
        ),
        metasvm=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=0.5,
                D=0.5
            )
        ),
        metarnn=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=0.5,
                D=0.5
            )
        ),
        m_cap=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=0.025,
                D=0.025
            )
        ),
        primateai=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=0.803,
                D=0.803
            )
        ),
        deogen2=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=0.5,
                D=0.5
            )
        ),
        clinpred=list(
            pre=list(
                T="Tolerated",
                D="Damaging"
            ),
            cut=list(
                T=0.5,
                D=0.5
            )
        ),
        alphamissense=list(
            pre=list(
                B="Benign",
                A="Ambiguous",
                P="Likely Pathogenic"
            ),
            cut=list(
                B=0.4,
                A=0.6,
                P=0.7
            )
        )
    ))
}
