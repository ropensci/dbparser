# parse DrugBank DB - default params

    Code
      dvobj$result
    Output
      $drugs
      $drugs$general_information
      # A tibble: 1 × 15
        primary_key other_keys      type  created updated name  description cas_number
        <chr>       <chr>           <chr> <chr>   <chr>   <chr> <chr>       <chr>     
      1 DB00001     BTD00024;BIOD0… biot… 2005-0… 2018-0… Lepi… Lepirudin … 138068-37…
      # … with 7 more variables: unii <chr>, average_mass <chr>,
      #   monoisotopic_mass <chr>, state <chr>, synthesis_reference <chr>,
      #   fda_label <chr>, msds <chr>
      
      $drugs$drug_classification
      # A tibble: 1 × 9
        description direct_parent kingdom   superclass class subclass alternative_par…
        <chr>       <chr>         <chr>     <chr>      <chr> <chr>    <chr>           
      1 ""          Peptides      Organic … Organic A… Carb… Amino A… ""              
      # … with 2 more variables: substituents <chr>, drugbank_id <chr>
      
      $drugs$synonyms
      # A tibble: 2 × 4
        synonym               language coder `drugbank-id`
        <chr>                 <chr>    <chr> <chr>        
      1 Hirudin variant-1     ""       ""    DB00001      
      2 Lepirudin recombinant ""       ""    DB00001      
      
      $drugs$pharmacology
      # A tibble: 1 × 12
        drugbank_id indication   pharmacodynamics mechanism_of_ac… toxicity metabolism
        <chr>       <chr>        <chr>            <chr>            <chr>    <chr>     
      1 DB00001     For the tre… "\n  Lepirudin … "\n  Lepirudin … "\n  In… "\n  Lepi…
      # … with 6 more variables: absorption <chr>, half_life <chr>,
      #   protein_binding <chr>, route_of_elimination <chr>,
      #   volume_of_distribution <chr>, clearance <chr>
      
      $drugs$international_brands
      # A tibble: 0 × 0
      
      $drugs$mixtures
      # A tibble: 1 × 3
        name     ingredients parent_key
        <chr>    <chr>       <chr>     
      1 Refludan Lepirudin   DB00001   
      
      $drugs$packagers
      # A tibble: 2 × 3
        name             url                            parent_key
        <chr>            <chr>                          <chr>     
      1 Bayer Healthcare http://www.bayerhealthcare.com DB00001   
      2 Berlex Labs      http://www.berlex.com          DB00001   
      
      $drugs$manufacturers
      # A tibble: 1 × 4
        manufacturer                         generic url   `drugbank-id`
        <chr>                                <chr>   <chr> <chr>        
      1 Bayer healthcare pharmaceuticals inc false   ""    DB00001      
      
      $drugs$prices
      # A tibble: 1 × 5
        description         currency cost   unit  parent_key
        <chr>               <chr>    <chr>  <chr> <chr>     
      1 Refludan 50 mg vial USD      273.19 vial  DB00001   
      
      $drugs$categories
      # A tibble: 14 × 3
         category                            `mesh-id` parent_key
         <chr>                               <chr>     <chr>     
       1 Amino Acids, Peptides, and Proteins "D000602" DB00001   
       2 Anticoagulants                      "D000925" DB00001   
       3 Antithrombin Proteins               "D058833" DB00001   
       4 Antithrombins                       "D000991" DB00001   
       5 Blood and Blood Forming Organs      ""        DB00001   
       6 Cardiovascular Agents               "D002317" DB00001   
       7 Enzyme Inhibitors                   "D004791" DB00001   
       8 Fibrin Modulating Agents            "D050299" DB00001   
       9 Hematologic Agents                  "D006401" DB00001   
      10 Peptides                            "D010455" DB00001   
      11 Protease Inhibitors                 "D011480" DB00001   
      12 Proteins                            "D011506" DB00001   
      13 Serine Proteinase Inhibitors        "D015842" DB00001   
      14 Serpins                             "D015843" DB00001   
      
      $drugs$dosages
      # A tibble: 3 × 4
        form                             route       strength parent_key
        <chr>                            <chr>       <chr>    <chr>     
      1 Injection, solution, concentrate Intravenous 20 mg    DB00001   
      2 Injection, solution, concentrate Intravenous 50 mg    DB00001   
      3 Powder, for solution             Intravenous 50 mg    DB00001   
      
      $drugs$atc_codes
      # A tibble: 1 × 10
        atc_code level_1           code_1 level_2 code_2 level_3 code_3 level_4 code_4
        <chr>    <chr>             <chr>  <chr>   <chr>  <chr>   <chr>  <chr>   <chr> 
      1 B01AE02  Direct thrombin … B01AE  ANTITH… B01A   ANTITH… B01    BLOOD … B     
      # … with 1 more variable: `drugbank-id` <chr>
      
      $drugs$patents
      # A tibble: 1 × 6
        number  country       approved   expires    `pediatric-extension` parent_key
        <chr>   <chr>         <chr>      <chr>      <chr>                 <chr>     
      1 5180668 United States 1993-01-19 2010-01-19 false                 DB00001   
      
      $drugs$drug_interactions
      # A tibble: 398 × 4
         `drugbank-id` name                 description                     parent_key
         <chr>         <chr>                <chr>                           <chr>     
       1 DB01323       St. John's Wort      The metabolism of Lepirudin ca… DB00001   
       2 DB00346       Alfuzosin            The serum concentration of Alf… DB00001   
       3 DB13783       Acemetacin           The risk or severity of bleedi… DB00001   
       4 DB06605       Apixaban             Apixaban may increase the anti… DB00001   
       5 DB06695       Dabigatran etexilate Dabigatran etexilate may incre… DB00001   
       6 DB09075       Edoxaban             Edoxaban may increase the anti… DB00001   
       7 DB06228       Rivaroxaban          Lepirudin may increase the ant… DB00001   
       8 DB00227       Lovastatin           The serum concentration of Lov… DB00001   
       9 DB00013       Urokinase            Urokinase may increase the ant… DB00001   
      10 DB09030       Vorapaxar            The risk or severity of advers… DB00001   
      # … with 388 more rows
      
      $drugs$sequences
      # A tibble: 1 × 3
        sequence                                                     format parent_key
        <chr>                                                        <chr>  <chr>     
      1 ">DB00001 sequence\nLVYTDCTESGQNLCLCEGSNVCGQGNKCILGSDGEKNQC… FASTA  DB00001   
      
      $drugs$calculated_properties
      # A tibble: 0 × 0
      
      $drugs$experimental_properties
      # A tibble: 8 × 3
        resource                     identifier    parent_key
        <chr>                        <chr>         <chr>     
      1 Drugs Product Database (DPD) 11916         DB00001   
      2 PubChem Substance            46507011      DB00001   
      3 KEGG Drug                    D06880        DB00001   
      4 PharmGKB                     PA450195      DB00001   
      5 UniProtKB                    P01050        DB00001   
      6 Therapeutic Targets Database DAP000541     DB00001   
      7 Wikipedia                    Lepirudin     DB00001   
      8 ChEMBL                       CHEMBL1201666 DB00001   
      
      $drugs$external_identifiers
      # A tibble: 5 × 4
        kind              value             source                          parent_key
        <chr>             <chr>             <chr>                           <chr>     
      1 Melting Point     65 °C             "Otto, A. & Seckler, R. Eur. J… DB00001   
      2 Hydrophobicity    -0.777            ""                              DB00001   
      3 Isoelectric Point 4.04              ""                              DB00001   
      4 Molecular Weight  6963.425          ""                              DB00001   
      5 Molecular Formula C287H440N80O110S6 ""                              DB00001   
      
      $drugs$pathway
      $drugs$pathway$general_information
      # A tibble: 1 × 4
        smpdb_id name                     category    parent_key
        <chr>    <chr>                    <chr>       <chr>     
      1 SMP00278 Lepirudin Action Pathway drug_action DB00001   
      
      $drugs$pathway$pathway_drugs
      # A tibble: 3 × 3
        `drugbank-id` name          parent_key
        <chr>         <chr>         <chr>     
      1 DB00001       Lepirudin     SMP00278  
      2 DB01022       Phylloquinone SMP00278  
      3 DB01373       Calcium       SMP00278  
      
      $drugs$pathway$pathway_enzyme
      # A tibble: 20 × 2
         enzyme pathway_id
         <chr>  <chr>     
       1 P00734 SMP00278  
       2 P00748 SMP00278  
       3 P02452 SMP00278  
       4 P03952 SMP00278  
       5 P03951 SMP00278  
       6 P00740 SMP00278  
       7 P00451 SMP00278  
       8 P12259 SMP00278  
       9 P00742 SMP00278  
      10 P02671 SMP00278  
      11 P02675 SMP00278  
      12 P02679 SMP00278  
      13 P00488 SMP00278  
      14 P05160 SMP00278  
      15 P00747 SMP00278  
      16 P00750 SMP00278  
      17 P08709 SMP00278  
      18 P13726 SMP00278  
      19 Q9BQB6 SMP00278  
      20 P38435 SMP00278  
      
      
      $drugs$snp_effects
      # A tibble: 0 × 0
      
      $drugs$snp_adverse_reactions
      # A tibble: 0 × 0
      
      $drugs$food_interactions
      # A tibble: 0 × 0
      
      $drugs$pdb_entries
      # A tibble: 0 × 0
      
      $drugs$ahfs_codes
      # A tibble: 0 × 0
      
      $drugs$affected_organisms
      # A tibble: 1 × 2
        affected_organism        drugbank_id
        <chr>                    <chr>      
      1 Humans and other mammals DB00001    
      
      $drugs$groups
      # A tibble: 1 × 2
        group    `drugbank-id`
        <chr>    <chr>        
      1 approved DB00001      
      
      $drugs$external_links
      # A tibble: 2 × 3
        resource  url                                             parent_key
        <chr>     <chr>                                           <chr>     
      1 RxList    http://www.rxlist.com/cgi/generic/lepirudin.htm DB00001   
      2 Drugs.com http://www.drugs.com/cdi/lepirudin.html         DB00001   
      
      
      $salts
      # A tibble: 0 × 0
      
      $products
      # A tibble: 5 × 19
        name     labeller          `ndc-id` `ndc-product-c…` `dpd-id` `ema-product-c…`
        <chr>    <chr>             <chr>    <chr>            <chr>    <chr>           
      1 Refludan Bayer             ""       ""               "022409… ""              
      2 Refludan Celgene Europe L… ""       ""               ""       "EMEA/H/C/00012…
      3 Refludan Celgene Europe L… ""       ""               ""       "EMEA/H/C/00012…
      4 Refludan Celgene Europe L… ""       ""               ""       "EMEA/H/C/00012…
      5 Refludan Celgene Europe L… ""       ""               ""       "EMEA/H/C/00012…
      # … with 13 more variables: `ema-ma-number` <chr>,
      #   `started-marketing-on` <chr>, `ended-marketing-on` <chr>,
      #   `dosage-form` <chr>, strength <chr>, route <chr>,
      #   `fda-application-number` <chr>, generic <chr>, `over-the-counter` <chr>,
      #   approved <chr>, country <chr>, source <chr>, parent_key <chr>
      
      $references
      $references$drugs
      $references$drugs$books
      # A tibble: 0 × 0
      
      $references$drugs$articles
      # A tibble: 3 × 3
        `pubmed-id` citation                                                parent_key
        <chr>       <chr>                                                   <chr>     
      1 16244762    Smythe MA, Stephens JL, Koerber JM, Mattson JC: A comp… DB00001   
      2 16690967    Tardy B, Lecompte T, Boelhen F, Tardy-Poncet B, Elalam… DB00001   
      3 16241940    Lubenow N, Eichler P, Lietz T, Greinacher A: Lepirudin… DB00001   
      
      $references$drugs$links
      # A tibble: 1 × 3
        title        url                                                    parent_key
        <chr>        <chr>                                                  <chr>     
      1 Google books http://books.google.com/books?id=iadLoXoQkWEC&pg=PA440 DB00001   
      
      $references$drugs$attachments
      # A tibble: 0 × 0
      
      
      $references$carriers
      $references$carriers$books
      # A tibble: 0 × 0
      
      $references$carriers$articles
      # A tibble: 0 × 0
      
      $references$carriers$links
      # A tibble: 0 × 0
      
      $references$carriers$attachments
      # A tibble: 0 × 0
      
      
      $references$enzymes
      $references$enzymes$books
      # A tibble: 0 × 0
      
      $references$enzymes$articles
      # A tibble: 0 × 0
      
      $references$enzymes$links
      # A tibble: 0 × 0
      
      $references$enzymes$attachments
      # A tibble: 0 × 0
      
      
      $references$targets
      $references$targets$books
      # A tibble: 0 × 0
      
      $references$targets$articles
      # A tibble: 6 × 3
        `pubmed-id` citation                                                parent_key
        <chr>       <chr>                                                   <chr>     
      1 10505536    Turpie AG: Anticoagulants in acute coronary syndromes.… BE0000048 
      2 10912644    Warkentin TE: Venous thromboembolism in heparin-induce… BE0000048 
      3 11055889    Eriksson BI: New therapeutic options in deep vein thro… BE0000048 
      4 11467439    Fabrizio MC: Use of ecarin clotting time (ECT) with le… BE0000048 
      5 11807012    Szaba FM, Smiley ST: Roles for thrombin and fibrin(oge… BE0000048 
      6 11752352    Chen X, Ji ZL, Chen YZ: TTD: Therapeutic Target Databa… BE0000048 
      
      $references$targets$links
      # A tibble: 0 × 0
      
      $references$targets$attachments
      # A tibble: 0 × 0
      
      
      $references$transporters
      $references$transporters$books
      # A tibble: 0 × 0
      
      $references$transporters$articles
      # A tibble: 0 × 0
      
      $references$transporters$links
      # A tibble: 0 × 0
      
      $references$transporters$attachments
      # A tibble: 0 × 0
      
      
      
      $cett
      $cett$carriers
      $cett$carriers$general_information
      # A tibble: 0 × 0
      
      $cett$carriers$actions
      # A tibble: 0 × 0
      
      $cett$carriers$polypeptides
      $cett$carriers$polypeptides$general_information
      # A tibble: 0 × 0
      
      $cett$carriers$polypeptides$external_identy
      # A tibble: 0 × 0
      
      $cett$carriers$polypeptides$synonyms
      # A tibble: 0 × 0
      
      $cett$carriers$polypeptides$pfams
      # A tibble: 0 × 0
      
      $cett$carriers$polypeptides$go
      # A tibble: 0 × 0
      
      
      
      $cett$enzymes
      $cett$enzymes$general_information
      # A tibble: 0 × 0
      
      $cett$enzymes$actions
      # A tibble: 0 × 0
      
      $cett$enzymes$polypeptides
      $cett$enzymes$polypeptides$general_information
      # A tibble: 0 × 0
      
      $cett$enzymes$polypeptides$external_identy
      # A tibble: 0 × 0
      
      $cett$enzymes$polypeptides$synonyms
      # A tibble: 0 × 0
      
      $cett$enzymes$polypeptides$pfams
      # A tibble: 0 × 0
      
      $cett$enzymes$polypeptides$go
      # A tibble: 0 × 0
      
      
      
      $cett$targets
      $cett$targets$general_information
      # A tibble: 1 × 6
        id        name        organism known_action position parent_key
        <chr>     <chr>       <chr>    <chr>        <lgl>    <chr>     
      1 BE0000048 Prothrombin Human    yes          NA       DB00001   
      
      $cett$targets$actions
      # A tibble: 1 × 2
        action    parent_id
        <chr>     <chr>    
      1 inhibitor BE0000048
      
      $cett$targets$polypeptides
      $cett$targets$polypeptides$general_information
      # A tibble: 1 × 20
        id     source     name       general_function specific_functi… gene_name locus
        <chr>  <chr>      <chr>      <chr>            <chr>            <chr>     <chr>
      1 P00734 Swiss-Prot Prothromb… Thrombospondin … Thrombin, which… F2        11p1…
      # … with 13 more variables: cellular_location <chr>,
      #   transmembrane_regions <chr>, signal_regions <chr>, theoretical_pi <chr>,
      #   molecular_weight <chr>, chromosome_location <chr>, organism <chr>,
      #   organism_ncbi_taxonomy_id <chr>, amino_acid_sequence <chr>,
      #   amino_acid_format <chr>, gene_sequence <chr>, gene_format <chr>,
      #   parent_id <chr>
      
      $cett$targets$polypeptides$external_identy
      # A tibble: 7 × 3
        resource                                identifier parent_key
        <chr>                                   <chr>      <chr>     
      1 HUGO Gene Nomenclature Committee (HGNC) HGNC:3535  BE0000048 
      2 GenAtlas                                F2         BE0000048 
      3 GenBank Gene Database                   M17262     BE0000048 
      4 GenBank Protein Database                339641     BE0000048 
      5 Guide to Pharmacology                   2362       BE0000048 
      6 UniProtKB                               P00734     BE0000048 
      7 UniProt Accession                       THRB_HUMAN BE0000048 
      
      $cett$targets$polypeptides$synonyms
      # A tibble: 2 × 2
        synonym               parent_key
        <chr>                 <chr>     
      1 3.4.21.5              BE0000048 
      2 Coagulation factor II BE0000048 
      
      $cett$targets$polypeptides$pfams
      # A tibble: 4 × 3
        identifier name           parent_key
        <chr>      <chr>          <chr>     
      1 PF00594    Gla            BE0000048 
      2 PF00051    Kringle        BE0000048 
      3 PF00089    Trypsin        BE0000048 
      4 PF09396    Thrombin_light BE0000048 
      
      $cett$targets$polypeptides$go
      # A tibble: 46 × 3
         category  description                      parent_key
         <chr>     <chr>                            <chr>     
       1 component blood microparticle              BE0000048 
       2 component extracellular matrix             BE0000048 
       3 component Golgi lumen                      BE0000048 
       4 component cytosol                          BE0000048 
       5 component extracellular exosome            BE0000048 
       6 component extracellular region             BE0000048 
       7 component endoplasmic reticulum lumen      BE0000048 
       8 component plasma membrane                  BE0000048 
       9 component extracellular space              BE0000048 
      10 function  thrombospondin receptor activity BE0000048 
      # … with 36 more rows
      
      
      
      $cett$transporters
      $cett$transporters$general_information
      # A tibble: 0 × 0
      
      $cett$transporters$actions
      # A tibble: 0 × 0
      
      $cett$transporters$polypeptides
      $cett$transporters$polypeptides$general_information
      # A tibble: 0 × 0
      
      $cett$transporters$polypeptides$external_identy
      # A tibble: 0 × 0
      
      $cett$transporters$polypeptides$synonyms
      # A tibble: 0 × 0
      
      $cett$transporters$polypeptides$pfams
      # A tibble: 0 × 0
      
      $cett$transporters$polypeptides$go
      # A tibble: 0 × 0
      
      
      
      
      attr(,"class")
      [1] "dvobject"
      attr(,"original_db_info")
      attr(,"original_db_info")$db_type
      [1] "DrugBank"
      
      attr(,"original_db_info")$db_version
      [1] "5.1"
      
      attr(,"original_db_info")$db_exported_date
      [1] "2018-07-03"
      

