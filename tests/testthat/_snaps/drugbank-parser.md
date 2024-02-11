# parse DrugBank DB - default params

    Code
      dvobj$result
    Output
      $drugs
      $drugs$general_information
      # A tibble: 1 × 15
        drugbank_id other_keys      type  created updated name  description cas_number
        <chr>       <chr>           <chr> <chr>   <chr>   <chr> <chr>       <chr>     
      1 DB00001     BTD00024;BIOD0… biot… 2005-0… 2018-0… Lepi… Lepirudin … 138068-37…
      # ℹ 7 more variables: unii <chr>, average_mass <chr>, monoisotopic_mass <chr>,
      #   state <chr>, synthesis_reference <chr>, fda_label <chr>, msds <chr>
      
      
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
        drugbank_id other_keys      type  created updated name  description cas_number
        <chr>       <chr>           <chr> <chr>   <chr>   <chr> <chr>       <chr>     
      1 DB00006     BTD00076;EXPT0… smal… 2005-0… 2018-0… Biva… Bivalirudi… 128270-60…
      # ℹ 7 more variables: unii <chr>, average_mass <chr>, monoisotopic_mass <chr>,
      #   state <chr>, synthesis_reference <chr>, fda_label <chr>, msds <chr>
      
      $drugs$drug_classification
      # A tibble: 1 × 9
        description                    direct_parent kingdom superclass class subclass
        <chr>                          <chr>         <chr>   <chr>      <chr> <chr>   
      1 This compound belongs to the … Polypeptides  Organi… Organic P… Poly… ""      
      # ℹ 3 more variables: alternative_parents <chr>, substituents <chr>,
      #   drugbank_id <chr>
      
      $drugs$synonyms
      # A tibble: 3 × 4
        synonym       language  coder drugbank_id
        <chr>         <chr>     <chr> <chr>      
      1 Bivalirudina  "Spanish" ""    DB00006    
      2 Bivalirudinum "Latin"   ""    DB00006    
      3 Hirulog       ""        ""    DB00006    
      
      $drugs$pharmacology
      # A tibble: 1 × 12
        drugbank_id indication           pharmacodynamics mechanism_of_action toxicity
        <chr>       <chr>                <chr>            <chr>               <chr>   
      1 DB00006     For treatment of he… "Bivalirudin di… Inhibits the actio… Based o…
      # ℹ 7 more variables: metabolism <chr>, absorption <chr>, half_life <chr>,
      #   protein_binding <chr>, route_of_elimination <chr>,
      #   volume_of_distribution <chr>, clearance <chr>
      
      $drugs$international_brands
      # A tibble: 1 × 3
        brand  company drugbank_id
        <chr>  <chr>   <chr>      
      1 Angiox ""      DB00006    
      
      $drugs$mixtures
      # A tibble: 4 × 3
        name                                ingredients drugbank_id
        <chr>                               <chr>       <chr>      
      1 Angiomax                            Bivalirudin DB00006    
      2 Bivalirudin for Injection           Bivalirudin DB00006    
      3 Bivalirudin                         Bivalirudin DB00006    
      4 Bivalirudin in 0.9% Sodium Chloride Bivalirudin DB00006    
      
      $drugs$packagers
      # A tibble: 4 × 3
        name                          url                                  drugbank_id
        <chr>                         <chr>                                <chr>      
      1 Ben Venue Laboratories Inc.   "http://www.benvenue.com"            DB00006    
      2 Oryx Pharmaceuticals Inc.     ""                                   DB00006    
      3 Sepracor Pharmaceuticals Inc. "http://www.sepracor.com"            DB00006    
      4 The Medicines Co.             "http://www.themedicinescompany.com" DB00006    
      
      $drugs$manufacturers
      # A tibble: 1 × 4
        manufacturer     generic url   drugbank_id
        <chr>            <chr>   <chr> <chr>      
      1 The medicines co false   ""    DB00006    
      
      $drugs$prices
      # A tibble: 1 × 5
        description          currency cost  unit  drugbank_id
        <chr>                <chr>    <chr> <chr> <chr>      
      1 Angiomax 250 mg vial USD      780.0 vial  DB00006    
      
      $drugs$categories
      # A tibble: 11 × 3
         category                            mesh_id   drugbank_id
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
        form                                         route        strength drugbank_id
        <chr>                                        <chr>        <chr>    <chr>      
      1 Injection, powder, lyophilized, for solution Intravenous  250 mg/1 DB00006    
      2 Injection                                    Intracavern… 250 mg/1 DB00006    
      3 Injection, powder, lyophilized, for solution Intravenous  250 mg/… DB00006    
      4 Powder, for solution                         Intravenous  250 mg   DB00006    
      5 Injection                                    Intravenous  250 mg/… DB00006    
      6 Injection                                    Intravenous  500 mg/… DB00006    
      
      $drugs$atc_codes
      # A tibble: 1 × 10
        atc_code level_1           code_1 level_2 code_2 level_3 code_3 level_4 code_4
        <chr>    <chr>             <chr>  <chr>   <chr>  <chr>   <chr>  <chr>   <chr> 
      1 B01AE06  Direct thrombin … B01AE  ANTITH… B01A   ANTITH… B01    BLOOD … B     
      # ℹ 1 more variable: drugbank_id <chr>
      
      $drugs$patents
      # A tibble: 4 × 6
        number  country       approved   expires    pediatric_extension drugbank_id
        <chr>   <chr>         <chr>      <chr>      <chr>               <chr>      
      1 5196404 United States 1993-05-23 2010-05-23 false               DB00006    
      2 2065150 Canada        1999-12-14 2010-08-17 false               DB00006    
      3 7598343 United States 2009-01-27 2029-01-27 true                DB00006    
      4 7582727 United States 2009-01-27 2029-01-27 true                DB00006    
      
      $drugs$drug_interactions
      # A tibble: 398 × 4
         drugbank_id name                 description                      drugbank_id
         <chr>       <chr>                <chr>                            <chr>      
       1 DB01323     St. John's Wort      The metabolism of Bivalirudin c… DB00006    
       2 DB00346     Alfuzosin            The serum concentration of Alfu… DB00006    
       3 DB13783     Acemetacin           The risk or severity of bleedin… DB00006    
       4 DB06605     Apixaban             Apixaban may increase the antic… DB00006    
       5 DB06695     Dabigatran etexilate Dabigatran etexilate may increa… DB00006    
       6 DB09075     Edoxaban             Edoxaban may increase the antic… DB00006    
       7 DB06228     Rivaroxaban          Bivalirudin may increase the an… DB00006    
       8 DB00227     Lovastatin           The serum concentration of Lova… DB00006    
       9 DB00013     Urokinase            Urokinase may increase the anti… DB00006    
      10 DB09030     Vorapaxar            The risk or severity of adverse… DB00006    
      # ℹ 388 more rows
      
      $drugs$sequences
      # A tibble: 0 × 0
      
      $drugs$calculated_properties
      # A tibble: 26 × 4
         kind                   value                               source drugbank_id
         <chr>                  <chr>                               <chr>  <chr>      
       1 logP                   -0.76                               ALOGPS DB00006    
       2 logS                   -4.7                                ALOGPS DB00006    
       3 Water Solubility       4.64e-02 g/l                        ALOGPS DB00006    
       4 logP                   -14                                 ChemA… DB00006    
       5 IUPAC Name             (4S)-4-[(2S)-2-[(2S)-2-[(2S)-2-{2-… ChemA… DB00006    
       6 Traditional IUPAC Name bivalirudin                         ChemA… DB00006    
       7 Molecular Weight       2180.2853                           ChemA… DB00006    
       8 Monoisotopic Weight    2178.985813062                      ChemA… DB00006    
       9 SMILES                 CC[C@H](C)[C@H](NC(=O)[C@H](CCC(O)… ChemA… DB00006    
      10 Molecular Formula      C98H138N24O33                       ChemA… DB00006    
      # ℹ 16 more rows
      
      $drugs$experimental_properties
      # A tibble: 0 × 0
      
      $drugs$external_identifiers
      # A tibble: 10 × 3
         resource                     identifier    drugbank_id
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
      
      $drugs$pathway
      $drugs$pathway$general_information
      # A tibble: 1 × 4
        smpdb_id name                       category    drugbank_id
        <chr>    <chr>                      <chr>       <chr>      
      1 SMP00277 Bivalirudin Action Pathway drug_action DB00006    
      
      $drugs$pathway$pathway_drugs
      # A tibble: 3 × 3
        drugbank_id name          smpdb_id
        <chr>       <chr>         <chr>   
      1 DB00006     Bivalirudin   SMP00277
      2 DB01022     Phylloquinone SMP00277
      3 DB01373     Calcium       SMP00277
      
      $drugs$pathway$pathway_enzyme
      # A tibble: 20 × 2
         enzyme smpdb_id
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
        food_interaction                                                   drugbank_id
        <chr>                                                              <chr>      
      1 Dan shen, dong quai, evening primrose oil, gingko, policosanol, w… DB00006    
      2 Echinacea                                                          DB00006    
      
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
        group           drugbank_id
        <chr>           <chr>      
      1 approved        DB00006    
      2 investigational DB00006    
      
      $drugs$external_links
      # A tibble: 2 × 3
        resource  url                                            drugbank_id
        <chr>     <chr>                                          <chr>      
      1 RxList    http://www.rxlist.com/cgi/generic/angiomax.htm DB00006    
      2 Drugs.com http://www.drugs.com/cdi/bivalirudin.html      DB00006    
      
      
      $salts
      # A tibble: 0 × 0
      
      $products
      # A tibble: 15 × 19
         name   labeller ndc_id ndc_product_code dpd_id ema_product_code ema_ma_number
         <chr>  <chr>    <chr>  <chr>            <chr>  <chr>            <chr>        
       1 Angio… Sandoz … ""     ""               "0224… ""               ""           
       2 Angio… The Med… ""     "65293-001"      ""     ""               ""           
       3 Angio… Cardina… ""     "55154-2275"     ""     ""               ""           
       4 Bival… Freseni… ""     "63323-562"      ""     ""               ""           
       5 Bival… Sandoz … ""     ""               "0245… ""               ""           
       6 Bival… Sandoz   ""     "0781-3158"      ""     ""               ""           
       7 Bival… Dr Redd… ""     "55111-652"      ""     ""               ""           
       8 Bival… Sandoz   ""     "0781-9158"      ""     ""               ""           
       9 Bival… Mylan I… ""     "67457-256"      ""     ""               ""           
      10 Bival… Sagent … ""     "25021-405"      ""     ""               ""           
      11 Bival… Apotex … ""     "60505-6101"     ""     ""               ""           
      12 Bival… Hospira… ""     "0409-8300"      ""     ""               ""           
      13 Bival… Freseni… ""     ""               "0243… ""               ""           
      14 Bival… Baxter … ""     "0338-9572"      ""     ""               ""           
      15 Bival… Baxter … ""     "0338-9576"      ""     ""               ""           
      # ℹ 12 more variables: started_marketing_on <chr>, ended_marketing_on <chr>,
      #   dosage_form <chr>, strength <chr>, route <chr>,
      #   fda_application_number <chr>, generic <chr>, over_the_counter <chr>,
      #   approved <chr>, country <chr>, source <chr>, drugbank_id <chr>
      
      $references
      $references$drugs
      $references$drugs$books
      # A tibble: 0 × 0
      
      $references$drugs$articles
      # A tibble: 7 × 3
        pubmed_id citation                                                 drugbank_id
        <chr>     <chr>                                                    <chr>      
      1 16466327  Seybert AL, Coons JC, Zerumsky K: Treatment of heparin-… DB00006    
      2 17381384  Dager WE, Dougherty JA, Nguyen PH, Militello MA, Smythe… DB00006    
      3 16553503  Dang CH, Durkalski VL, Nappi JM: Evaluation of treatmen… DB00006    
      4 11156732  Robson R: The use of bivalirudin in patients with renal… DB00006    
      5 21108549  Van De Car DA, Rao SV, Ohman EM: Bivalirudin: a review … DB00006    
      6 16614733  Shammas NW: Bivalirudin: pharmacology and clinical appl… DB00006    
      7 12851152  Gleason TG, Chengelis CP, Jackson CB, Lindstrom P: A 24… DB00006    
      
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
        pubmed_id citation                                                   enzyme_id
        <chr>     <chr>                                                      <chr>    
      1 18701766  Rudolph V, Rudolph TK, Schopfer FJ, Bonacci G, Lau D, Szo… BE0001075
      
      $references$enzymes$links
      # A tibble: 0 × 0
      
      $references$enzymes$attachments
      # A tibble: 0 × 0
      
      
      $references$targets
      $references$targets$books
      # A tibble: 0 × 0
      
      $references$targets$articles
      # A tibble: 6 × 3
        pubmed_id citation                                                   target_id
        <chr>     <chr>                                                      <chr>    
      1 11060732  Scatena R: Bivalirudin: a new generation antithrombotic d… BE0000048
      2 11504570  Bates ER: Bivalirudin for percutaneous coronary intervent… BE0000048
      3 11833835  Gladwell TD: Bivalirudin: a direct thrombin inhibitor. Cl… BE0000048
      4 11923794  Kleiman NS, Klem J, Fernandes LS, Rubin H, Challa S, Solo… BE0000048
      5 11929334  Carswell CI, Plosker GL: Bivalirudin: a review of its pot… BE0000048
      6 11752352  Chen X, Ji ZL, Chen YZ: TTD: Therapeutic Target Database.… BE0000048
      
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
        id        name  organism known_action position drugbank_id inhibition_strength
        <chr>     <chr> <chr>    <chr>        <chr>    <chr>       <chr>              
      1 BE0001075 Myel… Human    unknown      1        DB00006     unknown            
      # ℹ 1 more variable: induction_strength <chr>
      
      $cett$enzymes$actions
      # A tibble: 1 × 2
        action    enzyme_id
        <chr>     <chr>    
      1 inhibitor BE0001075
      
      $cett$enzymes$polypeptides
      $cett$enzymes$polypeptides$general_information
      # A tibble: 1 × 20
        id     source     name      general_function specific_function gene_name locus
        <chr>  <chr>      <chr>     <chr>            <chr>             <chr>     <chr>
      1 P05164 Swiss-Prot Myeloper… Peroxidase acti… Part of the host… MPO       17q2…
      # ℹ 13 more variables: cellular_location <chr>, transmembrane_regions <chr>,
      #   signal_regions <chr>, theoretical_pi <chr>, molecular_weight <chr>,
      #   chromosome_location <chr>, organism <chr>, organism_ncbi_taxonomy_id <chr>,
      #   amino_acid_sequence <chr>, amino_acid_format <chr>, gene_sequence <chr>,
      #   gene_format <chr>, enzyme_id <chr>
      
      $cett$enzymes$polypeptides$external_identy
      # A tibble: 7 × 3
        resource                                identifier enzyme_id
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
        synonym  enzyme_id
        <chr>    <chr>    
      1 1.11.2.2 BE0001075
      2 MPO      BE0001075
      
      $cett$enzymes$polypeptides$pfams
      # A tibble: 1 × 3
        identifier name          enzyme_id
        <chr>      <chr>         <chr>    
      1 PF03098    An_peroxidase BE0001075
      
      $cett$enzymes$polypeptides$go
      # A tibble: 24 × 3
         category  description           enzyme_id
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
      # ℹ 14 more rows
      
      
      
      $cett$targets
      $cett$targets$general_information
      # A tibble: 1 × 6
        id        name        organism known_action position drugbank_id
        <chr>     <chr>       <chr>    <chr>        <lgl>    <chr>      
      1 BE0000048 Prothrombin Human    yes          NA       DB00006    
      
      $cett$targets$actions
      # A tibble: 1 × 2
        action    target_id
        <chr>     <chr>    
      1 inhibitor BE0000048
      
      $cett$targets$polypeptides
      $cett$targets$polypeptides$general_information
      # A tibble: 1 × 20
        id     source     name      general_function specific_function gene_name locus
        <chr>  <chr>      <chr>     <chr>            <chr>             <chr>     <chr>
      1 P00734 Swiss-Prot Prothrom… Thrombospondin … Thrombin, which … F2        11p1…
      # ℹ 13 more variables: cellular_location <chr>, transmembrane_regions <chr>,
      #   signal_regions <chr>, theoretical_pi <chr>, molecular_weight <chr>,
      #   chromosome_location <chr>, organism <chr>, organism_ncbi_taxonomy_id <chr>,
      #   amino_acid_sequence <chr>, amino_acid_format <chr>, gene_sequence <chr>,
      #   gene_format <chr>, target_id <chr>
      
      $cett$targets$polypeptides$external_identy
      # A tibble: 7 × 3
        resource                                identifier target_id
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
        synonym               target_id
        <chr>                 <chr>    
      1 3.4.21.5              BE0000048
      2 Coagulation factor II BE0000048
      
      $cett$targets$polypeptides$pfams
      # A tibble: 4 × 3
        identifier name           target_id
        <chr>      <chr>          <chr>    
      1 PF00594    Gla            BE0000048
      2 PF00051    Kringle        BE0000048
      3 PF00089    Trypsin        BE0000048
      4 PF09396    Thrombin_light BE0000048
      
      $cett$targets$polypeptides$go
      # A tibble: 46 × 3
         category  description                      target_id
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
      # ℹ 36 more rows
      
      
      
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
      

# parse DrugBank DB - zip file

    Code
      dvobj$result
    Output
      $drugs
      $drugs$general_information
      # A tibble: 1 × 15
        drugbank_id other_keys      type  created updated name  description cas_number
        <chr>       <chr>           <chr> <chr>   <chr>   <chr> <chr>       <chr>     
      1 DB00001     BTD00024;BIOD0… biot… 2005-0… 2018-0… Lepi… Lepirudin … 138068-37…
      # ℹ 7 more variables: unii <chr>, average_mass <chr>, monoisotopic_mass <chr>,
      #   state <chr>, synthesis_reference <chr>, fda_label <chr>, msds <chr>
      
      
      attr(,"class")
      [1] "dvobject"
      attr(,"original_db_info")
      attr(,"original_db_info")$db_type
      [1] "DrugBank"
      
      attr(,"original_db_info")$db_version
      [1] "5.1"
      
      attr(,"original_db_info")$db_exported_date
      [1] "2018-07-03"
      

