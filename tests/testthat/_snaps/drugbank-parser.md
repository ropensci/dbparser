# parse DrugBank DB - default params

    Code
      dvobj$result
    Output
      $drugs
      $drugs$general_information
      # A tibble: 1 × 15
        primary_key other_…¹ type  created updated name  descr…² cas_n…³ unii  avera…⁴
        <chr>       <chr>    <chr> <chr>   <chr>   <chr> <chr>   <chr>   <chr> <chr>  
      1 DB00001     BTD0002… biot… 2005-0… 2018-0… Lepi… Lepiru… 138068… Y43G… <NA>   
      # … with 5 more variables: monoisotopic_mass <chr>, state <chr>,
      #   synthesis_reference <chr>, fda_label <chr>, msds <chr>, and abbreviated
      #   variable names ¹​other_keys, ²​description, ³​cas_number, ⁴​average_mass
      
      $drugs$drug_classification
      # A tibble: 1 × 9
        description direct_par…¹ kingdom super…² class subcl…³ alter…⁴ subst…⁵ drugb…⁶
        <chr>       <chr>        <chr>   <chr>   <chr> <chr>   <chr>   <chr>   <chr>  
      1 ""          Peptides     Organi… Organi… Carb… Amino … ""      ""      DB00001
      # … with abbreviated variable names ¹​direct_parent, ²​superclass, ³​subclass,
      #   ⁴​alternative_parents, ⁵​substituents, ⁶​drugbank_id
      
      $drugs$synonyms
      # A tibble: 2 × 4
        synonym               language coder `drugbank-id`
        <chr>                 <chr>    <chr> <chr>        
      1 Hirudin variant-1     ""       ""    DB00001      
      2 Lepirudin recombinant ""       ""    DB00001      
      
      $drugs$pharmacology
      # A tibble: 1 × 12
        drugbank_id indication pharm…¹ mecha…² toxic…³ metab…⁴ absor…⁵ half_…⁶ prote…⁷
        <chr>       <chr>      <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
      1 DB00001     For the t… "\n  L… "\n  L… "\n  I… "\n  L… Bioava… Approx… ""     
      # … with 3 more variables: route_of_elimination <chr>,
      #   volume_of_distribution <chr>, clearance <chr>, and abbreviated variable
      #   names ¹​pharmacodynamics, ²​mechanism_of_action, ³​toxicity, ⁴​metabolism,
      #   ⁵​absorption, ⁶​half_life, ⁷​protein_binding
      
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
        atc_code level_1   code_1 level_2 code_2 level_3 code_3 level_4 code_4 drugb…¹
        <chr>    <chr>     <chr>  <chr>   <chr>  <chr>   <chr>  <chr>   <chr>  <chr>  
      1 B01AE02  Direct t… B01AE  ANTITH… B01A   ANTITH… B01    BLOOD … B      DB00001
      # … with abbreviated variable name ¹​`drugbank-id`
      
      $drugs$patents
      # A tibble: 1 × 6
        number  country       approved   expires    `pediatric-extension` parent_key
        <chr>   <chr>         <chr>      <chr>      <chr>                 <chr>     
      1 5180668 United States 1993-01-19 2010-01-19 false                 DB00001   
      
      $drugs$drug_interactions
      # A tibble: 398 × 4
         `drugbank-id` name                 description                        paren…¹
         <chr>         <chr>                <chr>                              <chr>  
       1 DB01323       St. John's Wort      The metabolism of Lepirudin can b… DB00001
       2 DB00346       Alfuzosin            The serum concentration of Alfuzo… DB00001
       3 DB13783       Acemetacin           The risk or severity of bleeding … DB00001
       4 DB06605       Apixaban             Apixaban may increase the anticoa… DB00001
       5 DB06695       Dabigatran etexilate Dabigatran etexilate may increase… DB00001
       6 DB09075       Edoxaban             Edoxaban may increase the anticoa… DB00001
       7 DB06228       Rivaroxaban          Lepirudin may increase the antico… DB00001
       8 DB00227       Lovastatin           The serum concentration of Lovast… DB00001
       9 DB00013       Urokinase            Urokinase may increase the antico… DB00001
      10 DB09030       Vorapaxar            The risk or severity of adverse e… DB00001
      # … with 388 more rows, and abbreviated variable name ¹​parent_key
      
      $drugs$sequences
      # A tibble: 1 × 3
        sequence                                                        format paren…¹
        <chr>                                                           <chr>  <chr>  
      1 ">DB00001 sequence\nLVYTDCTESGQNLCLCEGSNVCGQGNKCILGSDGEKNQCVTG… FASTA  DB00001
      # … with abbreviated variable name ¹​parent_key
      
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
        kind              value             source                             paren…¹
        <chr>             <chr>             <chr>                              <chr>  
      1 Melting Point     65 °C             "Otto, A. & Seckler, R. Eur. J. B… DB00001
      2 Hydrophobicity    -0.777            ""                                 DB00001
      3 Isoelectric Point 4.04              ""                                 DB00001
      4 Molecular Weight  6963.425          ""                                 DB00001
      5 Molecular Formula C287H440N80O110S6 ""                                 DB00001
      # … with abbreviated variable name ¹​parent_key
      
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
      
      
      $drugs$reactions
      $drugs$reactions$general_information
      # A tibble: 0 × 0
      
      $drugs$reactions$reactions_enzymes
      # A tibble: 0 × 0
      
      
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
        name   label…¹ ndc-i…² ndc-p…³ dpd-i…⁴ ema-p…⁵ ema-m…⁶ start…⁷ ended…⁸ dosag…⁹
        <chr>  <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
      1 Reflu… Bayer   ""      ""      "02240… ""      ""      2000-0… 2013-0… Powder…
      2 Reflu… Celgen… ""      ""      ""      "EMEA/… "EU/1/… 1997-0… 2012-0… Inject…
      3 Reflu… Celgen… ""      ""      ""      "EMEA/… "EU/1/… 1997-0… 2012-0… Inject…
      4 Reflu… Celgen… ""      ""      ""      "EMEA/… "EU/1/… 1997-0… 2012-0… Inject…
      5 Reflu… Celgen… ""      ""      ""      "EMEA/… "EU/1/… 1997-0… 2012-0… Inject…
      # … with 9 more variables: strength <chr>, route <chr>,
      #   `fda-application-number` <chr>, generic <chr>, `over-the-counter` <chr>,
      #   approved <chr>, country <chr>, source <chr>, parent_key <chr>, and
      #   abbreviated variable names ¹​labeller, ²​`ndc-id`, ³​`ndc-product-code`,
      #   ⁴​`dpd-id`, ⁵​`ema-product-code`, ⁶​`ema-ma-number`, ⁷​`started-marketing-on`,
      #   ⁸​`ended-marketing-on`, ⁹​`dosage-form`
      
      $references
      $references$drugs
      $references$drugs$books
      # A tibble: 0 × 0
      
      $references$drugs$articles
      # A tibble: 3 × 3
        `pubmed-id` citation                                                   paren…¹
        <chr>       <chr>                                                      <chr>  
      1 16244762    Smythe MA, Stephens JL, Koerber JM, Mattson JC: A compari… DB00001
      2 16690967    Tardy B, Lecompte T, Boelhen F, Tardy-Poncet B, Elalamy I… DB00001
      3 16241940    Lubenow N, Eichler P, Lietz T, Greinacher A: Lepirudin in… DB00001
      # … with abbreviated variable name ¹​parent_key
      
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
        `pubmed-id` citation                                                   paren…¹
        <chr>       <chr>                                                      <chr>  
      1 10505536    Turpie AG: Anticoagulants in acute coronary syndromes. Am… BE0000…
      2 10912644    Warkentin TE: Venous thromboembolism in heparin-induced t… BE0000…
      3 11055889    Eriksson BI: New therapeutic options in deep vein thrombo… BE0000…
      4 11467439    Fabrizio MC: Use of ecarin clotting time (ECT) with lepir… BE0000…
      5 11807012    Szaba FM, Smiley ST: Roles for thrombin and fibrin(ogen) … BE0000…
      6 11752352    Chen X, Ji ZL, Chen YZ: TTD: Therapeutic Target Database.… BE0000…
      # … with abbreviated variable name ¹​parent_key
      
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
        id     source     name   gener…¹ speci…² gene_…³ locus cellu…⁴ trans…⁵ signa…⁶
        <chr>  <chr>      <chr>  <chr>   <chr>   <chr>   <chr> <chr>   <chr>   <chr>  
      1 P00734 Swiss-Prot Proth… Thromb… Thromb… F2      11p1… Secret… ""      1-24   
      # … with 10 more variables: theoretical_pi <chr>, molecular_weight <chr>,
      #   chromosome_location <chr>, organism <chr>, organism_ncbi_taxonomy_id <chr>,
      #   amino_acid_sequence <chr>, amino_acid_format <chr>, gene_sequence <chr>,
      #   gene_format <chr>, parent_id <chr>, and abbreviated variable names
      #   ¹​general_function, ²​specific_function, ³​gene_name, ⁴​cellular_location,
      #   ⁵​transmembrane_regions, ⁶​signal_regions
      
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
      

# parse DrugBank DB - small molecule

    Code
      dvobj$result
    Output
      $drugs
      $drugs$general_information
      # A tibble: 1 × 15
        primary_key other_…¹ type  created updated name  descr…² cas_n…³ unii  avera…⁴
        <chr>       <chr>    <chr> <chr>   <chr>   <chr> <chr>   <chr>   <chr> <chr>  
      1 DB00006     BTD0007… smal… 2005-0… 2018-0… Biva… Bivali… 128270… TN9B… 2180.2…
      # … with 5 more variables: monoisotopic_mass <chr>, state <chr>,
      #   synthesis_reference <chr>, fda_label <chr>, msds <chr>, and abbreviated
      #   variable names ¹​other_keys, ²​description, ³​cas_number, ⁴​average_mass
      
      $drugs$drug_classification
      # A tibble: 1 × 9
        description      direc…¹ kingdom super…² class subcl…³ alter…⁴ subst…⁵ drugb…⁶
        <chr>            <chr>   <chr>   <chr>   <chr> <chr>   <chr>   <chr>   <chr>  
      1 This compound b… Polype… Organi… Organi… Poly… ""      1-hydr… 1-hydr… DB00006
      # … with abbreviated variable names ¹​direct_parent, ²​superclass, ³​subclass,
      #   ⁴​alternative_parents, ⁵​substituents, ⁶​drugbank_id
      
      $drugs$synonyms
      # A tibble: 3 × 4
        synonym       language  coder `drugbank-id`
        <chr>         <chr>     <chr> <chr>        
      1 Bivalirudina  "Spanish" ""    DB00006      
      2 Bivalirudinum "Latin"   ""    DB00006      
      3 Hirulog       ""        ""    DB00006      
      
      $drugs$pharmacology
      # A tibble: 1 × 12
        drugbank_id indication pharm…¹ mecha…² toxic…³ metab…⁴ absor…⁵ half_…⁶ prote…⁷
        <chr>       <chr>      <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
      1 DB00006     For treat… "Bival… Inhibi… Based … 80% pr… Follow… "* Nor… Other …
      # … with 3 more variables: route_of_elimination <chr>,
      #   volume_of_distribution <chr>, clearance <chr>, and abbreviated variable
      #   names ¹​pharmacodynamics, ²​mechanism_of_action, ³​toxicity, ⁴​metabolism,
      #   ⁵​absorption, ⁶​half_life, ⁷​protein_binding
      
      $drugs$international_brands
      # A tibble: 1 × 3
        brand  company `drugbank-id`
        <chr>  <chr>   <chr>        
      1 Angiox ""      DB00006      
      
      $drugs$mixtures
      # A tibble: 4 × 3
        name                                ingredients parent_key
        <chr>                               <chr>       <chr>     
      1 Angiomax                            Bivalirudin DB00006   
      2 Bivalirudin for Injection           Bivalirudin DB00006   
      3 Bivalirudin                         Bivalirudin DB00006   
      4 Bivalirudin in 0.9% Sodium Chloride Bivalirudin DB00006   
      
      $drugs$packagers
      # A tibble: 4 × 3
        name                          url                                  parent_key
        <chr>                         <chr>                                <chr>     
      1 Ben Venue Laboratories Inc.   "http://www.benvenue.com"            DB00006   
      2 Oryx Pharmaceuticals Inc.     ""                                   DB00006   
      3 Sepracor Pharmaceuticals Inc. "http://www.sepracor.com"            DB00006   
      4 The Medicines Co.             "http://www.themedicinescompany.com" DB00006   
      
      $drugs$manufacturers
      # A tibble: 1 × 4
        manufacturer     generic url   `drugbank-id`
        <chr>            <chr>   <chr> <chr>        
      1 The medicines co false   ""    DB00006      
      
      $drugs$prices
      # A tibble: 1 × 5
        description          currency cost  unit  parent_key
        <chr>                <chr>    <chr> <chr> <chr>     
      1 Angiomax 250 mg vial USD      780.0 vial  DB00006   
      
      $drugs$categories
      # A tibble: 11 × 3
         category                            `mesh-id` parent_key
         <chr>                               <chr>     <chr>     
       1 Amino Acids, Peptides, and Proteins "D000602" DB00006   
       2 Anticoagulants                      "D000925" DB00006   
       3 Antithrombin Proteins               "D058833" DB00006   
       4 Antithrombins                       "D000991" DB00006   
       5 Blood and Blood Forming Organs      ""        DB00006   
       6 Enzyme Inhibitors                   "D004791" DB00006   
       7 Hematologic Agents                  "D006401" DB00006   
       8 Peptides                            "D010455" DB00006   
       9 Proteins                            "D011506" DB00006   
      10 Serine Proteinase Inhibitors        "D015842" DB00006   
      11 Serpins                             "D015843" DB00006   
      
      $drugs$dosages
      # A tibble: 6 × 4
        form                                         route          strength   paren…¹
        <chr>                                        <chr>          <chr>      <chr>  
      1 Injection, powder, lyophilized, for solution Intravenous    250 mg/1   DB00006
      2 Injection                                    Intracavernous 250 mg/1   DB00006
      3 Injection, powder, lyophilized, for solution Intravenous    250 mg/5mL DB00006
      4 Powder, for solution                         Intravenous    250 mg     DB00006
      5 Injection                                    Intravenous    250 mg/50… DB00006
      6 Injection                                    Intravenous    500 mg/10… DB00006
      # … with abbreviated variable name ¹​parent_key
      
      $drugs$atc_codes
      # A tibble: 1 × 10
        atc_code level_1   code_1 level_2 code_2 level_3 code_3 level_4 code_4 drugb…¹
        <chr>    <chr>     <chr>  <chr>   <chr>  <chr>   <chr>  <chr>   <chr>  <chr>  
      1 B01AE06  Direct t… B01AE  ANTITH… B01A   ANTITH… B01    BLOOD … B      DB00006
      # … with abbreviated variable name ¹​`drugbank-id`
      
      $drugs$patents
      # A tibble: 4 × 6
        number  country       approved   expires    `pediatric-extension` parent_key
        <chr>   <chr>         <chr>      <chr>      <chr>                 <chr>     
      1 5196404 United States 1993-05-23 2010-05-23 false                 DB00006   
      2 2065150 Canada        1999-12-14 2010-08-17 false                 DB00006   
      3 7598343 United States 2009-01-27 2029-01-27 true                  DB00006   
      4 7582727 United States 2009-01-27 2029-01-27 true                  DB00006   
      
      $drugs$drug_interactions
      # A tibble: 398 × 4
         `drugbank-id` name                 description                        paren…¹
         <chr>         <chr>                <chr>                              <chr>  
       1 DB01323       St. John's Wort      The metabolism of Bivalirudin can… DB00006
       2 DB00346       Alfuzosin            The serum concentration of Alfuzo… DB00006
       3 DB13783       Acemetacin           The risk or severity of bleeding … DB00006
       4 DB06605       Apixaban             Apixaban may increase the anticoa… DB00006
       5 DB06695       Dabigatran etexilate Dabigatran etexilate may increase… DB00006
       6 DB09075       Edoxaban             Edoxaban may increase the anticoa… DB00006
       7 DB06228       Rivaroxaban          Bivalirudin may increase the anti… DB00006
       8 DB00227       Lovastatin           The serum concentration of Lovast… DB00006
       9 DB00013       Urokinase            Urokinase may increase the antico… DB00006
      10 DB09030       Vorapaxar            The risk or severity of adverse e… DB00006
      # … with 388 more rows, and abbreviated variable name ¹​parent_key
      
      $drugs$sequences
      # A tibble: 0 × 0
      
      $drugs$calculated_properties
      # A tibble: 26 × 4
         kind                   value                                   source paren…¹
         <chr>                  <chr>                                   <chr>  <chr>  
       1 logP                   -0.76                                   ALOGPS DB00006
       2 logS                   -4.7                                    ALOGPS DB00006
       3 Water Solubility       4.64e-02 g/l                            ALOGPS DB00006
       4 logP                   -14                                     ChemA… DB00006
       5 IUPAC Name             (4S)-4-[(2S)-2-[(2S)-2-[(2S)-2-{2-[(2S… ChemA… DB00006
       6 Traditional IUPAC Name bivalirudin                             ChemA… DB00006
       7 Molecular Weight       2180.2853                               ChemA… DB00006
       8 Monoisotopic Weight    2178.985813062                          ChemA… DB00006
       9 SMILES                 CC[C@H](C)[C@H](NC(=O)[C@H](CCC(O)=O)N… ChemA… DB00006
      10 Molecular Formula      C98H138N24O33                           ChemA… DB00006
      # … with 16 more rows, and abbreviated variable name ¹​parent_key
      
      $drugs$experimental_properties
      # A tibble: 10 × 3
         resource                     identifier    parent_key
         <chr>                        <chr>         <chr>     
       1 Drugs Product Database (DPD) 12945         DB00006   
       2 ChEBI                        59173         DB00006   
       3 PubChem Compound             16129704      DB00006   
       4 PubChem Substance            46507415      DB00006   
       5 KEGG Drug                    D03136        DB00006   
       6 ChemSpider                   10482069      DB00006   
       7 PharmGKB                     PA10032       DB00006   
       8 Therapeutic Targets Database DAP000542     DB00006   
       9 Wikipedia                    Bivalirudin   DB00006   
      10 ChEMBL                       CHEMBL2103749 DB00006   
      
      $drugs$external_identifiers
      # A tibble: 0 × 0
      
      $drugs$pathway
      $drugs$pathway$general_information
      # A tibble: 1 × 4
        smpdb_id name                       category    parent_key
        <chr>    <chr>                      <chr>       <chr>     
      1 SMP00277 Bivalirudin Action Pathway drug_action DB00006   
      
      $drugs$pathway$pathway_drugs
      # A tibble: 3 × 3
        `drugbank-id` name          parent_key
        <chr>         <chr>         <chr>     
      1 DB00006       Bivalirudin   SMP00277  
      2 DB01022       Phylloquinone SMP00277  
      3 DB01373       Calcium       SMP00277  
      
      $drugs$pathway$pathway_enzyme
      # A tibble: 20 × 2
         enzyme pathway_id
         <chr>  <chr>     
       1 P00734 SMP00277  
       2 P00748 SMP00277  
       3 P02452 SMP00277  
       4 P03952 SMP00277  
       5 P03951 SMP00277  
       6 P00740 SMP00277  
       7 P00451 SMP00277  
       8 P12259 SMP00277  
       9 P00742 SMP00277  
      10 P02671 SMP00277  
      11 P02675 SMP00277  
      12 P02679 SMP00277  
      13 P00488 SMP00277  
      14 P05160 SMP00277  
      15 P00747 SMP00277  
      16 P00750 SMP00277  
      17 P08709 SMP00277  
      18 P13726 SMP00277  
      19 Q9BQB6 SMP00277  
      20 P38435 SMP00277  
      
      
      $drugs$reactions
      $drugs$reactions$general_information
      # A tibble: 0 × 0
      
      $drugs$reactions$reactions_enzymes
      # A tibble: 0 × 0
      
      
      $drugs$snp_effects
      # A tibble: 0 × 0
      
      $drugs$snp_adverse_reactions
      # A tibble: 0 × 0
      
      $drugs$food_interactions
      # A tibble: 2 × 2
        food_interaction                                                       drugb…¹
        <chr>                                                                  <chr>  
      1 Dan shen, dong quai, evening primrose oil, gingko, policosanol, willo… DB00006
      2 Echinacea                                                              DB00006
      # … with abbreviated variable name ¹​drugbank_id
      
      $drugs$pdb_entries
      # A tibble: 0 × 0
      
      $drugs$ahfs_codes
      # A tibble: 1 × 2
        ahfs_code   drugbank_id
        <chr>       <chr>      
      1 20:12.04.12 DB00006    
      
      $drugs$affected_organisms
      # A tibble: 1 × 2
        affected_organism        drugbank_id
        <chr>                    <chr>      
      1 Humans and other mammals DB00006    
      
      $drugs$groups
      # A tibble: 2 × 2
        group           `drugbank-id`
        <chr>           <chr>        
      1 approved        DB00006      
      2 investigational DB00006      
      
      $drugs$external_links
      # A tibble: 2 × 3
        resource  url                                            parent_key
        <chr>     <chr>                                          <chr>     
      1 RxList    http://www.rxlist.com/cgi/generic/angiomax.htm DB00006   
      2 Drugs.com http://www.drugs.com/cdi/bivalirudin.html      DB00006   
      
      
      $salts
      # A tibble: 0 × 0
      
      $products
      # A tibble: 15 × 19
         name  label…¹ ndc-i…² ndc-p…³ dpd-i…⁴ ema-p…⁵ ema-m…⁶ start…⁷ ended…⁸ dosag…⁹
         <chr> <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
       1 Angi… Sandoz… ""      ""      "02246… ""      ""      "2003-… ""      Powder…
       2 Angi… The Me… ""      "65293… ""      ""      ""      "2000-… ""      Inject…
       3 Angi… Cardin… ""      "55154… ""      ""      ""      "2000-… "2017-… Inject…
       4 Biva… Fresen… ""      "63323… ""      ""      ""      "2016-… ""      Inject…
       5 Biva… Sandoz… ""      ""      "02450… ""      ""      ""      ""      Powder…
       6 Biva… Sandoz  ""      "0781-… ""      ""      ""      "2015-… ""      Inject…
       7 Biva… Dr Red… ""      "55111… ""      ""      ""      "2017-… ""      Inject…
       8 Biva… Sandoz  ""      "0781-… ""      ""      ""      "2015-… ""      Inject…
       9 Biva… Mylan … ""      "67457… ""      ""      ""      "2018-… ""      Inject…
      10 Biva… Sagent… ""      "25021… ""      ""      ""      "2016-… ""      Inject…
      11 Biva… Apotex… ""      "60505… ""      ""      ""      "2017-… ""      Inject…
      12 Biva… Hospir… ""      "0409-… ""      ""      ""      "2015-… ""      Inject…
      13 Biva… Fresen… ""      ""      "02435… ""      ""      ""      ""      Powder…
      14 Biva… Baxter… ""      "0338-… ""      ""      ""      "2017-… ""      Inject…
      15 Biva… Baxter… ""      "0338-… ""      ""      ""      "2017-… ""      Inject…
      # … with 9 more variables: strength <chr>, route <chr>,
      #   `fda-application-number` <chr>, generic <chr>, `over-the-counter` <chr>,
      #   approved <chr>, country <chr>, source <chr>, parent_key <chr>, and
      #   abbreviated variable names ¹​labeller, ²​`ndc-id`, ³​`ndc-product-code`,
      #   ⁴​`dpd-id`, ⁵​`ema-product-code`, ⁶​`ema-ma-number`, ⁷​`started-marketing-on`,
      #   ⁸​`ended-marketing-on`, ⁹​`dosage-form`
      
      $references
      $references$drugs
      $references$drugs$books
      # A tibble: 0 × 0
      
      $references$drugs$articles
      # A tibble: 7 × 3
        `pubmed-id` citation                                                   paren…¹
        <chr>       <chr>                                                      <chr>  
      1 16466327    Seybert AL, Coons JC, Zerumsky K: Treatment of heparin-in… DB00006
      2 17381384    Dager WE, Dougherty JA, Nguyen PH, Militello MA, Smythe M… DB00006
      3 16553503    Dang CH, Durkalski VL, Nappi JM: Evaluation of treatment … DB00006
      4 11156732    Robson R: The use of bivalirudin in patients with renal i… DB00006
      5 21108549    Van De Car DA, Rao SV, Ohman EM: Bivalirudin: a review of… DB00006
      6 16614733    Shammas NW: Bivalirudin: pharmacology and clinical applic… DB00006
      7 12851152    Gleason TG, Chengelis CP, Jackson CB, Lindstrom P: A 24-h… DB00006
      # … with abbreviated variable name ¹​parent_key
      
      $references$drugs$links
      # A tibble: 0 × 0
      
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
      # A tibble: 1 × 3
        `pubmed-id` citation                                                   paren…¹
        <chr>       <chr>                                                      <chr>  
      1 18701766    Rudolph V, Rudolph TK, Schopfer FJ, Bonacci G, Lau D, Szo… BE0001…
      # … with abbreviated variable name ¹​parent_key
      
      $references$enzymes$links
      # A tibble: 0 × 0
      
      $references$enzymes$attachments
      # A tibble: 0 × 0
      
      
      $references$targets
      $references$targets$books
      # A tibble: 0 × 0
      
      $references$targets$articles
      # A tibble: 6 × 3
        `pubmed-id` citation                                                   paren…¹
        <chr>       <chr>                                                      <chr>  
      1 11060732    Scatena R: Bivalirudin: a new generation antithrombotic d… BE0000…
      2 11504570    Bates ER: Bivalirudin for percutaneous coronary intervent… BE0000…
      3 11833835    Gladwell TD: Bivalirudin: a direct thrombin inhibitor. Cl… BE0000…
      4 11923794    Kleiman NS, Klem J, Fernandes LS, Rubin H, Challa S, Solo… BE0000…
      5 11929334    Carswell CI, Plosker GL: Bivalirudin: a review of its pot… BE0000…
      6 11752352    Chen X, Ji ZL, Chen YZ: TTD: Therapeutic Target Database.… BE0000…
      # … with abbreviated variable name ¹​parent_key
      
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
      # A tibble: 1 × 8
        id        name            organism known_act…¹ posit…² paren…³ inhib…⁴ induc…⁵
        <chr>     <chr>           <chr>    <chr>       <chr>   <chr>   <chr>   <chr>  
      1 BE0001075 Myeloperoxidase Human    unknown     1       DB00006 unknown unknown
      # … with abbreviated variable names ¹​known_action, ²​position, ³​parent_key,
      #   ⁴​`inhibition-strength`, ⁵​`induction-strength`
      
      $cett$enzymes$actions
      # A tibble: 1 × 2
        action    parent_id
        <chr>     <chr>    
      1 inhibitor BE0001075
      
      $cett$enzymes$polypeptides
      $cett$enzymes$polypeptides$general_information
      # A tibble: 1 × 20
        id     source     name   gener…¹ speci…² gene_…³ locus cellu…⁴ trans…⁵ signa…⁶
        <chr>  <chr>      <chr>  <chr>   <chr>   <chr>   <chr> <chr>   <chr>   <chr>  
      1 P05164 Swiss-Prot Myelo… Peroxi… Part o… MPO     17q2… Lysoso… ""      1-48   
      # … with 10 more variables: theoretical_pi <chr>, molecular_weight <chr>,
      #   chromosome_location <chr>, organism <chr>, organism_ncbi_taxonomy_id <chr>,
      #   amino_acid_sequence <chr>, amino_acid_format <chr>, gene_sequence <chr>,
      #   gene_format <chr>, parent_id <chr>, and abbreviated variable names
      #   ¹​general_function, ²​specific_function, ³​gene_name, ⁴​cellular_location,
      #   ⁵​transmembrane_regions, ⁶​signal_regions
      
      $cett$enzymes$polypeptides$external_identy
      # A tibble: 7 × 3
        resource                                identifier parent_key
        <chr>                                   <chr>      <chr>     
      1 HUGO Gene Nomenclature Committee (HGNC) HGNC:7218  BE0001075 
      2 GenAtlas                                MPO        BE0001075 
      3 GenBank Gene Database                   J02694     BE0001075 
      4 GenBank Protein Database                189040     BE0001075 
      5 Guide to Pharmacology                   2789       BE0001075 
      6 UniProtKB                               P05164     BE0001075 
      7 UniProt Accession                       PERM_HUMAN BE0001075 
      
      $cett$enzymes$polypeptides$synonyms
      # A tibble: 2 × 2
        synonym  parent_key
        <chr>    <chr>     
      1 1.11.2.2 BE0001075 
      2 MPO      BE0001075 
      
      $cett$enzymes$polypeptides$pfams
      # A tibble: 1 × 3
        identifier name          parent_key
        <chr>      <chr>         <chr>     
      1 PF03098    An_peroxidase BE0001075 
      
      $cett$enzymes$polypeptides$go
      # A tibble: 24 × 3
         category  description           parent_key
         <chr>     <chr>                 <chr>     
       1 component extracellular exosome BE0001075 
       2 component azurophil granule     BE0001075 
       3 component mitochondrion         BE0001075 
       4 component secretory granule     BE0001075 
       5 component nucleus               BE0001075 
       6 component extracellular space   BE0001075 
       7 component lysosome              BE0001075 
       8 function  heme binding          BE0001075 
       9 function  peroxidase activity   BE0001075 
      10 function  heparin binding       BE0001075 
      # … with 14 more rows
      
      
      
      $cett$targets
      $cett$targets$general_information
      # A tibble: 1 × 6
        id        name        organism known_action position parent_key
        <chr>     <chr>       <chr>    <chr>        <lgl>    <chr>     
      1 BE0000048 Prothrombin Human    yes          NA       DB00006   
      
      $cett$targets$actions
      # A tibble: 1 × 2
        action    parent_id
        <chr>     <chr>    
      1 inhibitor BE0000048
      
      $cett$targets$polypeptides
      $cett$targets$polypeptides$general_information
      # A tibble: 1 × 20
        id     source     name   gener…¹ speci…² gene_…³ locus cellu…⁴ trans…⁵ signa…⁶
        <chr>  <chr>      <chr>  <chr>   <chr>   <chr>   <chr> <chr>   <chr>   <chr>  
      1 P00734 Swiss-Prot Proth… Thromb… Thromb… F2      11p1… Secret… ""      1-24   
      # … with 10 more variables: theoretical_pi <chr>, molecular_weight <chr>,
      #   chromosome_location <chr>, organism <chr>, organism_ncbi_taxonomy_id <chr>,
      #   amino_acid_sequence <chr>, amino_acid_format <chr>, gene_sequence <chr>,
      #   gene_format <chr>, parent_id <chr>, and abbreviated variable names
      #   ¹​general_function, ²​specific_function, ³​gene_name, ⁴​cellular_location,
      #   ⁵​transmembrane_regions, ⁶​signal_regions
      
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
      

# parse DrugBank DB - zim file

    Code
      dvobj$result
    Output
      $drugs
      $drugs$general_information
      # A tibble: 1 × 15
        primary_key other_…¹ type  created updated name  descr…² cas_n…³ unii  avera…⁴
        <chr>       <chr>    <chr> <chr>   <chr>   <chr> <chr>   <chr>   <chr> <chr>  
      1 DB00001     BTD0002… biot… 2005-0… 2018-0… Lepi… Lepiru… 138068… Y43G… <NA>   
      # … with 5 more variables: monoisotopic_mass <chr>, state <chr>,
      #   synthesis_reference <chr>, fda_label <chr>, msds <chr>, and abbreviated
      #   variable names ¹​other_keys, ²​description, ³​cas_number, ⁴​average_mass
      
      $drugs$drug_classification
      # A tibble: 1 × 9
        description direct_par…¹ kingdom super…² class subcl…³ alter…⁴ subst…⁵ drugb…⁶
        <chr>       <chr>        <chr>   <chr>   <chr> <chr>   <chr>   <chr>   <chr>  
      1 ""          Peptides     Organi… Organi… Carb… Amino … ""      ""      DB00001
      # … with abbreviated variable names ¹​direct_parent, ²​superclass, ³​subclass,
      #   ⁴​alternative_parents, ⁵​substituents, ⁶​drugbank_id
      
      $drugs$synonyms
      # A tibble: 2 × 4
        synonym               language coder `drugbank-id`
        <chr>                 <chr>    <chr> <chr>        
      1 Hirudin variant-1     ""       ""    DB00001      
      2 Lepirudin recombinant ""       ""    DB00001      
      
      $drugs$pharmacology
      # A tibble: 1 × 12
        drugbank_id indication pharm…¹ mecha…² toxic…³ metab…⁴ absor…⁵ half_…⁶ prote…⁷
        <chr>       <chr>      <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
      1 DB00001     For the t… "\n  L… "\n  L… "\n  I… "\n  L… Bioava… Approx… ""     
      # … with 3 more variables: route_of_elimination <chr>,
      #   volume_of_distribution <chr>, clearance <chr>, and abbreviated variable
      #   names ¹​pharmacodynamics, ²​mechanism_of_action, ³​toxicity, ⁴​metabolism,
      #   ⁵​absorption, ⁶​half_life, ⁷​protein_binding
      
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
        atc_code level_1   code_1 level_2 code_2 level_3 code_3 level_4 code_4 drugb…¹
        <chr>    <chr>     <chr>  <chr>   <chr>  <chr>   <chr>  <chr>   <chr>  <chr>  
      1 B01AE02  Direct t… B01AE  ANTITH… B01A   ANTITH… B01    BLOOD … B      DB00001
      # … with abbreviated variable name ¹​`drugbank-id`
      
      $drugs$patents
      # A tibble: 1 × 6
        number  country       approved   expires    `pediatric-extension` parent_key
        <chr>   <chr>         <chr>      <chr>      <chr>                 <chr>     
      1 5180668 United States 1993-01-19 2010-01-19 false                 DB00001   
      
      $drugs$drug_interactions
      # A tibble: 398 × 4
         `drugbank-id` name                 description                        paren…¹
         <chr>         <chr>                <chr>                              <chr>  
       1 DB01323       St. John's Wort      The metabolism of Lepirudin can b… DB00001
       2 DB00346       Alfuzosin            The serum concentration of Alfuzo… DB00001
       3 DB13783       Acemetacin           The risk or severity of bleeding … DB00001
       4 DB06605       Apixaban             Apixaban may increase the anticoa… DB00001
       5 DB06695       Dabigatran etexilate Dabigatran etexilate may increase… DB00001
       6 DB09075       Edoxaban             Edoxaban may increase the anticoa… DB00001
       7 DB06228       Rivaroxaban          Lepirudin may increase the antico… DB00001
       8 DB00227       Lovastatin           The serum concentration of Lovast… DB00001
       9 DB00013       Urokinase            Urokinase may increase the antico… DB00001
      10 DB09030       Vorapaxar            The risk or severity of adverse e… DB00001
      # … with 388 more rows, and abbreviated variable name ¹​parent_key
      
      $drugs$sequences
      # A tibble: 1 × 3
        sequence                                                        format paren…¹
        <chr>                                                           <chr>  <chr>  
      1 ">DB00001 sequence\nLVYTDCTESGQNLCLCEGSNVCGQGNKCILGSDGEKNQCVTG… FASTA  DB00001
      # … with abbreviated variable name ¹​parent_key
      
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
        kind              value             source                             paren…¹
        <chr>             <chr>             <chr>                              <chr>  
      1 Melting Point     65 °C             "Otto, A. & Seckler, R. Eur. J. B… DB00001
      2 Hydrophobicity    -0.777            ""                                 DB00001
      3 Isoelectric Point 4.04              ""                                 DB00001
      4 Molecular Weight  6963.425          ""                                 DB00001
      5 Molecular Formula C287H440N80O110S6 ""                                 DB00001
      # … with abbreviated variable name ¹​parent_key
      
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
      
      
      $drugs$reactions
      $drugs$reactions$general_information
      # A tibble: 0 × 0
      
      $drugs$reactions$reactions_enzymes
      # A tibble: 0 × 0
      
      
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
        name   label…¹ ndc-i…² ndc-p…³ dpd-i…⁴ ema-p…⁵ ema-m…⁶ start…⁷ ended…⁸ dosag…⁹
        <chr>  <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
      1 Reflu… Bayer   ""      ""      "02240… ""      ""      2000-0… 2013-0… Powder…
      2 Reflu… Celgen… ""      ""      ""      "EMEA/… "EU/1/… 1997-0… 2012-0… Inject…
      3 Reflu… Celgen… ""      ""      ""      "EMEA/… "EU/1/… 1997-0… 2012-0… Inject…
      4 Reflu… Celgen… ""      ""      ""      "EMEA/… "EU/1/… 1997-0… 2012-0… Inject…
      5 Reflu… Celgen… ""      ""      ""      "EMEA/… "EU/1/… 1997-0… 2012-0… Inject…
      # … with 9 more variables: strength <chr>, route <chr>,
      #   `fda-application-number` <chr>, generic <chr>, `over-the-counter` <chr>,
      #   approved <chr>, country <chr>, source <chr>, parent_key <chr>, and
      #   abbreviated variable names ¹​labeller, ²​`ndc-id`, ³​`ndc-product-code`,
      #   ⁴​`dpd-id`, ⁵​`ema-product-code`, ⁶​`ema-ma-number`, ⁷​`started-marketing-on`,
      #   ⁸​`ended-marketing-on`, ⁹​`dosage-form`
      
      $references
      $references$drugs
      $references$drugs$books
      # A tibble: 0 × 0
      
      $references$drugs$articles
      # A tibble: 3 × 3
        `pubmed-id` citation                                                   paren…¹
        <chr>       <chr>                                                      <chr>  
      1 16244762    Smythe MA, Stephens JL, Koerber JM, Mattson JC: A compari… DB00001
      2 16690967    Tardy B, Lecompte T, Boelhen F, Tardy-Poncet B, Elalamy I… DB00001
      3 16241940    Lubenow N, Eichler P, Lietz T, Greinacher A: Lepirudin in… DB00001
      # … with abbreviated variable name ¹​parent_key
      
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
        `pubmed-id` citation                                                   paren…¹
        <chr>       <chr>                                                      <chr>  
      1 10505536    Turpie AG: Anticoagulants in acute coronary syndromes. Am… BE0000…
      2 10912644    Warkentin TE: Venous thromboembolism in heparin-induced t… BE0000…
      3 11055889    Eriksson BI: New therapeutic options in deep vein thrombo… BE0000…
      4 11467439    Fabrizio MC: Use of ecarin clotting time (ECT) with lepir… BE0000…
      5 11807012    Szaba FM, Smiley ST: Roles for thrombin and fibrin(ogen) … BE0000…
      6 11752352    Chen X, Ji ZL, Chen YZ: TTD: Therapeutic Target Database.… BE0000…
      # … with abbreviated variable name ¹​parent_key
      
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
        id     source     name   gener…¹ speci…² gene_…³ locus cellu…⁴ trans…⁵ signa…⁶
        <chr>  <chr>      <chr>  <chr>   <chr>   <chr>   <chr> <chr>   <chr>   <chr>  
      1 P00734 Swiss-Prot Proth… Thromb… Thromb… F2      11p1… Secret… ""      1-24   
      # … with 10 more variables: theoretical_pi <chr>, molecular_weight <chr>,
      #   chromosome_location <chr>, organism <chr>, organism_ncbi_taxonomy_id <chr>,
      #   amino_acid_sequence <chr>, amino_acid_format <chr>, gene_sequence <chr>,
      #   gene_format <chr>, parent_id <chr>, and abbreviated variable names
      #   ¹​general_function, ²​specific_function, ³​gene_name, ⁴​cellular_location,
      #   ⁵​transmembrane_regions, ⁶​signal_regions
      
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
      

