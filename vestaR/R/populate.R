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
    return(c("Ion Torrent S3","Ιon Torrent S5","Ιon Torrent S5 XL",
        "Ιon Torrent PGM","Illumina NextSeq 1000","Illumina NextSeq 2000",
        "Illumina MiSeq","Other"))
}

..getSequencingProtocols <- function() {
    return(c("Targeted Gene Sequencing","Whole Exome Sequencing"))
}

..getSequencingKits <- function() {
    return(list(
        list(
            name="Oncomine Focus Assay (DNA+RNA)", 
            manufacturer="ThermoFisher Scientific", 
            product_id="A29230"
        ),
        list(
            name="Oncomine comprehensive assay plus, manual preparation", 
            manufacturer="ThermoFisher Scientific", 
            product_id="A48577"
        ),
        list(
            name="Custom ThermoFisher panel (IAD248137_182)", 
            manufacturer="ThermoFisher Scientific", 
            product_id="10336014"
        ),
        list(
            name="Oncomine tumor specific panel HRR Pathway", 
            manufacturer="ThermoFisher Scientific", 
            product_id="10336014"
        ),
        list(
            name="Trusight Hereditary Cancer Panel", 
            manufacturer="Illumina", 
            product_id=NULL
        ),
        list(
            name="Illumina DNA Prep with Exome 2.5 Enrichment", 
            manufacturer="Illumina", 
            product_id="20077595"
        ),
        list(
            name="QIAseq Targeted DNA Pro Human Breast Cancer Research Panel", 
            manufacturer="QIAGEN", 
            product_id="PHS-001Z"
        ),
        list(
            name="Other", 
            manufacturer="Other", 
            product_id="Other"
        )
    ))
}

..getVariantTypes <- function() {
    return(c("SNP","INS","DEL","MNP"))
}

..getVariantZygosities <- function() {
    return(c("0/0","0/1","1/1"))
}

..getVariantLocation <- function() {
    return(c("exonic","intronic","upstream","downstream","5'utr",
        "3'utr","unspecified"))
}

..getVariantEffectSnpEff <- function() {
    return(c("HIGH","LOW","MODERATE","MODIFIER"))
}

..getVariantSoTerms <- function() {
    return(c(
        "splice_acceptor_variant_NMD_escaping",
        "splice_donor_variant_NMD_escaping",
        "splice_acceptor_variant_NMD_triggering",
        "frameshift_variant_NMD_escaping",
        "splice_donor_variant_NMD_triggering",
        "polymorphic_sequence_variant",
        "sequence_variant_affecting_copy_number",
        "5_prime_UTR_uORF_stop_codon_gain_variant",
        "5_prime_UTR_uORF_stop_codon_loss_variant",
        "5_prime_UTR_uORF_stop_codon_variant",
        "5_prime_UTR_uORF_frameshift_variant",
        "5_prime_UTR_uORF_variant",
        "oncogenic_variant",
        "sequence_variant",
        "decreased_polyadenylation_variant",
        "increased_polyadenylation_variant",
        "synonymous_variant",
        "protein_altering_variant",
        "coding_variant_quality",
        "feature_variant",
        "sequence_variant_affecting_polyadenylation",
        "sequence_variant_obs",
        "frameshift_variant_NMD_triggering",
        "NMD_escaping_variant",
        "NMD_triggering_variant",
        "function_uncertain_variant",
        "sequence_variant_affecting_regulatory_region",
        "sequence_variant_causing_complex_change_of_translational_product",
        "sequence_variant_causing_amino_acid_insertion",
        "sequence_variant_causing_nonconservative_amino_acid_substitution",
        "sequence_variant_causing_conservative_amino_acid_substitution",
        "sequence_variant_causing_amino_acid_substitution",
        "sequence_variant_causing_polypeptide_elongation",
        "sequence_variant_causing_polypeptide_truncation",
        "sequence_variant_causing_amino_acid_deletion",
        "sequence_variant_causing_missense_codon_change_in_transcript",
        "sequence_variant_causing_non_synonymous_codon_change_in_transcript",
        "sequence_variant_causing_synonymous_codon_change_in_transcript",
        "sequence_variant_causing_terminator_codon_change_in_transcript",
        "sequence_variant_causing_nonsense_codon_change_in_transcript",
        "sequence_variant_causing_nonconservative_missense_codon_change_in_transcript",
        "sequence_variant_causing_conservative_missense_codon_change_in_transcript",
        "sequence_variant_causing_minus_1_frameshift",
        "sequence_variant_causing_plus_1_frameshift_mutation",
        "sequence_variant_causing_no_change_in_transcript",
        "sequence_variant_causing_amino_acid_coding_codon_change_in_transcript",
        "sequence_variant_causing_initiator_codon_change_in_transcript",
        "sequence_variant_affecting_rate_of_transcription",
        "sequence_variant_increasing_rate_of_transcription",
        "sequence_variant_decreasing_transcript_stability",
        "sequence_variant_increasing_transcript_stability",
        "sequence_variant_affecting_transcript_stability",
        "sequence_variant_causing_no_change_of_translational_product",
        "sequence_variant_affecting_translational_product",
        "sequence_variant_causing_minus_2_frameshift",
        "sequence_variant_causing_plus_2_frameshift",
        "sequence_variant_affecting_transcript_processing",
        "sequence_variant_causing_cryptic_splice_activation",
        "sequence_variant_affecting_splice_acceptor",
        "sequence_variant_affecting_splice_donor",
        "sequence_variant_affecting_splicing",
        "sequence_variant_decreasing_rate_of_transcription",
        "sequence_variant_affecting_transcription",
        "sequence_variant_affecting_editing",
        "disease_associated_variant",
        "benign_variant",
        "lethal_variant",
        "disease_causing_variant",
        "fixed_variant",
        "common_variant",
        "variant_phenotype",
        "unique_variant",
        "variant_frequency",
        "polymorphic_variant",
        "rare_variant",
        "variant_origin",
        "variant_quality",
        "non_coding_transcript_exon_variant",
        "exon_variant",
        "splice_donor_5th_base_variant",
        "TF_binding_site_variant",
        "de_novo_variant",
        "population_specific_variant",
        "pedigree_specific_variant",
        "germline_variant",
        "maternal_variant",
        "quantitative_variant",
        "somatic_variant",
        "paternal_variant",
        "splice_polypyrimidine_tract_variant",
        "sequence_length_variant",
        "genic_upstream_transcript_variant",
        "genic_downstream_transcript_variant",
        "splice_donor_region_variant",
        "sequence_variant_causing_polypeptide_fusion",
        "sequence_variant_affecting_transcript_secondary_structure",
        "sequence_variant_causing_gain_of_function_of_polypeptide",
        "sequence_variant_causing_partial_loss_of_function_of_polypeptide",
        "sequence_variant_causing_compensatory_transcript_secondary_structure_mutation",
        "sequence_variant_effect",
        "sequence_variant_affecting_gene_structure",
        "polypeptide_sequence_variant",
        "sequence_variant_causes_exon_loss",
        "complex_change_of_translational_product_variant",
        "sequence_variant_causing_gene_fusion",
        "conformational_change_variant",
        "sequence_variant_causing_cryptic_splice_donor_activation",
        "sequence_variant_causes_intron_gain",
        "complex_3D_structural_variant",
        "splice_region_variant",
        "splice_site_variant",
        "intergenic_variant",
        "3_prime_UTR_variant",
        "intron_variant",
        "incomplete_terminal_codon_variant",
        "NMD_transcript_variant",
        "mature_miRNA_variant",
        "5_prime_UTR_variant",
        "UTR_variant",
        "non_coding_transcript_variant",
        "inframe_variant",
        "2KB_upstream_variant",
        "5KB_upstream_variant",
        "downstream_gene_variant",
        "upstream_gene_variant",
        "500B_downstream_variant",
        "5KB_downstream_variant",
        "sequence_variant_causing_complex_3D_structural_change",
        "sequence_variant_causing_no_3D_structural_change",
        "sequence_variant_causing_inactive_ligand_binding_site",
        "sequence_variant_causing_loss_of_function_of_polypeptide",
        "sequence_variant_affecting_polypeptide_function",
        "sequence_variant_causing_conformational_change",
        "sequence_variant_causing_polypeptide_post_translational_processing_change",
        "sequence_variant_causing_polypeptide_localization_change",
        "sequence_variant_causing_inactive_catalytic_site",
        "sequence_variant_increasing_level_of_translation_product",
        "sequence_variant_decreasing_level_of_translation_product",
        "sequence_variant_affecting_level_of_translational_product",
        "sequence_variant_affecting_polypeptide_amino_acid_sequence",
        "sequence_variant_affecting_3D_structure_of_polypeptide",
        "frame_restoring_sequence_variant",
        "variant_of",
        "3D_polypeptide_structure_variant",
        "translational_product_structure_variant",
        "mutated_variant_site",
        "natural_variant_site",
        "conserved_intron_variant",
        "start_retained_variant",
        "conserved_intergenic_variant",
        "intragenic_variant",
        "rare_amino_acid_variant",
        "loss_of_function_variant",
        "dominant_negative_variant",
        "gain_of_function_variant",
        "sequence_variant_causing_cryptic_splice_acceptor_activation",
        "3_prime_UTR_exon_variant",
        "non_coding_transcript_splice_region_variant",
        "2KB_downstream_variant",
        "exonic_splice_region_variant",
        "incomplete_transcript_coding_splice_variant",
        "incomplete_transcript_exonic_variant",
        "incomplete_transcript_intronic_variant",
        "incomplete_transcript_splice_region_variant",
        "incomplete_transcript_3UTR_variant",
        "incomplete_transcript_5UTR_variant",
        "intergenic_1kb_variant",
        "incomplete_transcript_variant",
        "5_prime_UTR_exon_variant",
        "structural_interaction_variant",
        "3_prime_UTR_intron_variant",
        "5_prime_UTR_intron_variant",
        "non_coding_transcript_intron_variant",
        "coding_transcript_intron_variant",
        "coding_transcript_variant",
        "five_prime_UTR_premature_start_codon_location_variant",
        "nonsynonymous_variant",
        "variant_collection",
        "5_prime_UTR_premature_start_codon_gain_variant",
        "downstream_transcript_variant",
        "variant_genome",
        "5_prime_UTR_premature_start_codon_loss_variant",
        "5_prime_UTR_premature_start_codon_variant",
        "upstream_transcript_variant",
        "extended_intronic_splice_region_variant",
        "increased_transcription_rate_variant",
        "rate_of_transcription_variant",
        "translational_product_level_variant",
        "decreased_transcription_rate_variant",
        "increased_transcript_stability_variant",
        "decreased_transcript_stability_variant",
        "transcription_variant",
        "editing_variant",
        "transcript_processing_variant",
        "transcript_stability_variant",
        "polyadenylation_variant",
        "level_of_transcript_variant",
        "increased_transcript_level_variant",
        "decreased_transcript_level_variant",
        "structural_variant",
        "functional_effect_variant",
        "translational_product_function_variant",
        "transcript_function_variant",
        "intron_gain_variant",
        "exon_loss_variant",
        "splice_donor_variant",
        "splice_acceptor_variant",
        "cryptic_splice_site_variant",
        "regulatory_region_variant",
        "splicing_variant",
        "stop_retained_variant",
        "polypeptide_post_translational_processing_variant",
        "gene_variant",
        "polypeptide_loss_of_function_variant",
        "polypeptide_localization_variant",
        "polypeptide_function_variant",
        "polypeptide_gain_of_function_variant",
        "plus_2_frameshift_variant",
        "plus_1_frameshift_variant",
        "compensatory_transcript_secondary_structure_variant",
        "transcript_secondary_structure_variant",
        "frame_restoring_variant",
        "terminator_codon_variant",
        "minus_2_frameshift_variant",
        "minus_1_frameshift_variant",
        "frameshift_variant",
        "missense_variant",
        "non_conservative_missense_variant",
        "conservative_missense_variant",
        "coding_sequence_variant",
        "initiator_codon_variant",
        "complex_transcript_variant",
        "transcript_variant",
        "transcript_sequence_variant",
        "disruptive_inframe_deletion",
        "disruptive_inframe_insertion",
        "stop_gained"
    ))
}

# Most likely temporary - an admin in the future should be able to dynamically
# add/remove institutions
..getInstitutions <- function() {
    return(list(
        list(
            profile=list(
                name=paste0("ΕΚΠΑ - Μονάδες Πλασματοκυτταρικών Δυσκρασιών ",
                    "της Θεραπευτικής Κλινικής, Ιατρική Σχολή & ",
                    "Κυτταρομετρίας Ροής, Τμήμα Βιολογίας"),
                title="ΕΚΠΑ - Μονάδες Πλασματοκυτταρικών Δυσκρασιών",
                street="Πανεπιστημιούπολη Ζωγράφου",
                city="Αθήνα",
                state="Αττική",
                zip=15772,
                country="Ελλάδα",
                email="eterpos@med.uoa.gr",
                tel="+302132162846",
                contact_person="Ευστάθιος Καστρίτης",
                web_page=NULL
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("ΕΚΠΑ - Εργαστήριο Ιστολογίας-Εμβρυολογίας, ",
                    "Ιατρική Σχολή"),
                title="ΕΚΠΑ - Εργαστήριο Ιστολογίας-Εμβρυολογίας",
                street="Μικράς Ασίας 75",
                city="Αθήνα",
                state="Αττική",
                zip=11527,
                country="Ελλάδα",
                email="eterpos@med.uoa.gr",
                tel="+302132162846",
                contact_person="Βασίλης Γοργούλης",
                web_page=NULL
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("ΕΚΠΑ - Εργαστήριο Ανάλυσης Κυκλοφορούντων ",
                    "Καρκινικών Κυττάρων, Εργαστήριο Αναλυτικής Χημείας, ",
                    "Τμήμα Χημείας"),
                title="ΕΚΠΑ - Εργαστήριο Ανάλυσης Κυκλοφορούντων Καρκινικών",
                street="Μικράς Ασίας 75",
                city="Αθήνα",
                state="Αττική",
                zip=11527,
                country="Ελλάδα",
                email="eterpos@med.uoa.gr",
                tel="+302132162846",
                contact_person="Ευρύκλεια Λιανίδου",
                web_page=NULL
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Ε.ΚΕ.Β.Ε. Αλέξανδρος Φλέμινγκ"),
                title="Φλέμινγκ",
                street="Φλέμινγκ 34",
                city="Βάρη",
                state="Αττική",
                zip=16672,
                country="Ελλάδα",
                email="genomics@fleming.gr",
                tel="+302109656310",
                contact_person="Παντελής Χατζής",
                web_page="www.fleming.gr"
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Δημόκριτος - Εργαστήριο Μοριακής Γενετικής ",
                    "του Ανθρώπου"),
                title="Δημόκριτος - ΕΜΓΑ",
                street="Πατριάρχου Γρηγορίου Ε’ & Νεαπόλεως 27",
                city="Αγία Παρασκευή",
                state="Αττική",
                zip=15341,
                country="Ελλάδα",
                email="yannouka@rrp.demokritos.gr",
                tel="+302106503936",
                contact_person="Δρακούλης Γιαννουκάκος",
                web_page=paste0("https://inrastes.demokritos.gr/laboratories/",
                    "molecular-diagnostics-laboratory/")
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Δημόκριτος - Εργαστήριο Υγειοφυσικής, ",
                    "Ραδιοβιολογίας & Κυτταρογενετικής"),
                title="Δημόκριτος - ΕΥΡΚ",
                street="Πατριάρχου Γρηγορίου Ε’ & Νεαπόλεως 27",
                city="Αγία Παρασκευή",
                state="Αττική",
                zip=15341,
                country="Ελλάδα",
                email="yannouka@rrp.demokritos.gr",
                tel="+302106503936",
                contact_person="Κωνσταντίνα Σαμπάνη",
                web_page=paste0("https://inrastes.demokritos.gr/laboratories/",
                    "molecular-diagnostics-laboratory/")
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Ελληνικό Ινστιτούτο Pasteur - Μονάδα ",
                    "Βιοπληροφορικής και Εφαρμοσμένης Γενωμικής"),
                title="Pasteur",
                street="Βασιλίσσης Σοφίας 127",
                city="Αθήνα",
                state="Αττική",
                zip=11521,
                country="Ελλάδα",
                email="tkaram@pasteur.gr",
                tel="+302106503936",
                contact_person="Τιμοκράτης Καραμήτρος",
                web_page=paste0("https://www.pasteur.gr/en/research-",
                    "technological-infrastructure/bioinformatics-and-",
                    "applied-genomics-unit")
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Πανεπιστήμιο Κρήτης - Εργαστήριο ",
                    "Μεταφραστικής Oγκολογίας/Παθολογική Ογκολογική Κλινική"),
                title="Πανεπιστήμιο Κρήτης - ΕΜΟΠΟΚ",
                street="Πτέρυγα 9Β, Ιατρική Σχολή, Πανεπιστημιούπολη Βουτών",
                city="Ηράκλειο",
                state="Ηράκλειο",
                zip=71500,
                country="Ελλάδα",
                email="mavroudis@uoc.gr",
                tel="+302810392750",
                contact_person="Δημήτριος Μαυρουδής",
                web_page="https://medonc.gr/"
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Πανεπιστήμιο Κρήτης - Εργαστήριο Μελέτης ",
                    "Αμοποίησης/Αιματολογική Κλινική"),
                title="Πανεπιστήμιο Κρήτης - ΕΜΑΚ",
                street="Πτέρυγα 9Β, Ιατρική Σχολή, Πανεπιστημιούπολη Βουτών",
                city="Ηράκλειο",
                state="Ηράκλειο",
                zip=71500,
                country="Ελλάδα",
                email="mavroudis@uoc.gr",
                tel="+302810392750",
                contact_person="Ελένη Παπαδάκη",
                web_page="https://www.haemopoiesis.med.uoc.gr/"
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name="ΙΤΕ - ΙΜΒΒ - Μονάδα ΕΔΙΜΟ",
                title="ΙΤΕ - ΙΜΒΒ - Μονάδα ΕΔΙΜΟ",
                street="Νικολάου Πλαστήρα 100, Βασιλικά Βουτών",
                city="Ηράκλειο",
                state="Ηράκλειο",
                zip=70013,
                country="Ελλάδα",
                email="castratakis@imbb.forth.gr",
                tel="+302810391725",
                contact_person="Κωνσταντίνος Στρατάκης",
                web_page="https://www.imbb.forth.gr/"
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name="ΙΤΕ - ΙΜΒΒ - Μονάδα ΔΙΓΕΝΙΑ",
                title="ΙΤΕ - ΙΜΒΒ - Μονάδα ΔΙΓΕΝΙΑ",
                street="Νικολάου Πλαστήρα 100, Βασιλικά Βουτών",
                city="Ηράκλειο",
                state="Ηράκλειο",
                zip=70013,
                country="Ελλάδα",
                email="castratakis@imbb.forth.gr",
                tel="+302810391725",
                contact_person="Εμμανουέλα Λιναρδάκη",
                web_page="https://www.digenia.gr/index.php/el/"
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Πανεπιστήμιο Θεσσαλίας - Εργαστήριο Ογκολογίας"),
                title="Πανεπιστήμιο Θεσσαλίας - ΕΟ",
                street="Πανεπιστημιακό Νοσοκομειο Λάρισας, Μεζούρλο",
                city="Λάρισσα",
                state="Λάρισσα",
                zip=41110,
                country="Ελλάδα",
                email="thankotsakis@uth.gr",
                tel="+302413502009",
                contact_person="Αθανάσιος Κωτσάκης",
                web_page="https://oncology.med.uth.gr/"
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Πανεπιστήμιο Θεσσαλίας - Εργαστήριο Εργαστήριο ",
                    "Παθολογικής Ανατομικής και Κυτταρολογίας"),
                title="Πανεπιστήμιο Θεσσαλίας - ΕΠΑΚ",
                street="Πανεπιστημιακό Νοσοκομειο Λάρισας, Μεζούρλο",
                city="Λάρισσα",
                state="Λάρισσα",
                zip=41110,
                country="Ελλάδα",
                email="thankotsakis@uth.gr",
                tel="+302413502009",
                contact_person="Μαρία Ιωάννου",
                web_page=paste0("https://ee.uth.gr/el/content/ergastirio-",
                    "pathologikis-anatomikis-kai-kyttarologias")
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Πανεπιστήμιο Πατρών - Μονάδα Μοριακής ",
                    "Γενετικής, Εργαστήριο Γενικής Βιολογίας, Τμήμα Ιατρικής"),
                title="Πανεπιστήμιο Πατρών - Γενετική",
                street="Ασκληπιού 1, Πανεπιστημιούπολη",
                city="Ρίο",
                state="Πάτρα",
                zip=26504,
                country="Ελλάδα",
                email="lygerou@upatras.gr",
                tel="+302610997610",
                contact_person="Ζωή Λυγερού",
                web_page=paste0("https://pek.upatras.gr/?iwj_candidate=",
                    "ινστιτουτο-ιατρικησ-ακριβειασ")
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        ),
        list(
            profile=list(
                name=paste0("Πανεπιστήμιο Πατρών - Εργαστήριο Βιοχημείας/",
                "Μεταστατικής σηματοδότησης, Τομέας Γενετικής Βιολογίας ",
                "Κυττάρου & Ανάπτυξης, Τμήμα Βιολογίας"),
                title="Πανεπιστήμιο Πατρών - Βιοχημεία",
                street="Ασκληπιού 1, Πανεπιστημιούπολη",
                city="Ρίο",
                state="Πάτρα",
                zip=26504,
                country="Ελλάδα",
                email="lygerou@upatras.gr",
                tel="+302610997610",
                contact_person="Γαλάτεια Καλλέργη",
                web_page="https://mopy.upatras.gr/"
            ),
            tracking=list(
                date_created=unbox(Sys.time()),
                date_updated=NULL,
                inserted_by="Panagiotis Moulos",
                edited_by=NULL
            )
        )
    ))
}

# Most likely temporary - an admin in the future should be able to dynamically
# add/remove institutions
..getInstitutions0 <- function() {
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
        ),
        list(
            iid=5L,
            institution=paste0("Δημόκριτος - Εργαστήριο Μοριακής Γενετικής ",
                "του Ανθρώπου"),
            street="Πατριάρχου Γρηγορίου Ε’ & Νεαπόλεως 27",
            city="Αγία Παρασκευή",
            state="Αττική",
            zip=15341,
            country="Ελλάδα"
        ),
        list(
            iid=6L,
            institution=paste0("Δημόκριτος - Εργαστήριο Υγειοφυσικής, ",
                "Ραδιοβιολογίας & Κυτταρογενετικής"),
            street="Πατριάρχου Γρηγορίου Ε’ & Νεαπόλεως 27",
            city="Αγία Παρασκευή",
            state="Αττική",
            zip=15341,
            country="Ελλάδα"
        ),
        list(
            iid=7L,
            institution=paste0("Ελληνικό Ινστιτούτο Pasteur - Μονάδα ",
                "Βιοπληροφορικής και Εφαρμοσμένης Γενωμικής"),
            street="Βασιλίσσης Σοφίας 127",
            city="Αθήνα",
            state="Αττική",
            zip=11521,
            country="Ελλάδα"
        ),
        list(
            iid=8L,
            institution=paste0("Πανεπιστήμιο Κρήτης - Εργαστήριο ",
                "Μεταφραστικής Oγκολογίας/Παθολογική Ογκολογική Κλινική"),
            street="Πτέρυγα 9Β, Ιατρική Σχολή, Πανεπιστημιούπολη Βουτών",
            city="Ηράκλειο",
            state="Ηράκλειο",
            zip=71500,
            country="Ελλάδα"
        ),
        list(
            iid=9L,
            institution=paste0("Πανεπιστήμιο Κρήτης - Εργαστήριο ",
                "Μελέτης Αμοποίησης/Αιματολογική Κλινική"),
            street="Πτέρυγα 9Β, Ιατρική Σχολή, Πανεπιστημιούπολη Βουτών",
            city="Ηράκλειο",
            state="Ηράκλειο",
            zip=71500,
            country="Ελλάδα"
        ),
        list(
            iid=10L,
            institution=paste0("ΙΤΕ - ΙΜΒΒ - Μονάδα ΕΔΙΜΟ"),
            street="Νικολάου Πλαστήρα 100, Βασιλικά Βουτών",
            city="Ηράκλειο",
            state="Ηράκλειο",
            zip=70013,
            country="Ελλάδα"
        ),
        list(
            iid=11L,
            institution=paste0("ΙΤΕ - ΙΜΒΒ - Μονάδα ΔΙΓΕΝΙΑ"),
            street="Νικολάου Πλαστήρα 100, Βασιλικά Βουτών",
            city="Ηράκλειο",
            state="Ηράκλειο",
            zip=70013,
            country="Ελλάδα"
        ),
        list(
            iid=12L,
            institution=paste0("Πανεπιστήμιο Θεσσαλίας - Εργαστήριο ",
                "Ογκολογίας"),
            street="Πανεπιστημιακό Νοσοκομειο Λάρισας, Μεζούρλο",
            city="Λάρισσα",
            state="Λάρισσα",
            zip=41110,
            country="Ελλάδα"
        ),
        list(
            iid=13L,
            institution=paste0("Πανεπιστήμιο Θεσσαλίας - Εργαστήριο ",
                "Εργαστήριο Παθολογικής Ανατομικής και Κυτταρολογίας"),
            street="Πανεπιστημιακό Νοσοκομειο Λάρισας, Μεζούρλο",
            city="Λάρισσα",
            state="Λάρισσα",
            zip=41110,
            country="Ελλάδα"
        ),
        list(
            iid=14L,
            institution=paste0("Πανεπιστήμιο Πατρών - Μονάδα Μοριακής ",
                "Γενετικής, Εργαστήριο Γενικής Βιολογίας, Τμήμα Ιατρικής"),
            street="Ασκληπιού 1, Πανεπιστημιούπολη",
            city="Ρίο",
            state="Πάτρα",
            zip=26504,
            country="Ελλάδα"
        ),
        list(
            iid=15L,
            institution=paste0("Πανεπιστήμιο Πατρών - Εργαστήριο Βιοχημείας/",
                "Μεταστατικής σηματοδότησης, Τομέας Γενετικής Βιολογίας ",
                "Κυττάρου & Ανάπτυξης, Τμήμα Βιολογίας"),
            street="Ασκληπιού 1, Πανεπιστημιούπολη",
            city="Ρίο",
            state="Πάτρα",
            zip=26504,
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
