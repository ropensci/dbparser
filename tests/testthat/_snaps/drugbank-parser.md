# parse DrugBank DB - default params

    Code
      dvobj$result
    Output
      $drugs
      $drugs$general_information
      # A tibble: 1 × 15
        drugbank_id other_keys      type  created updated name  description cas_number
        <chr>       <chr>           <chr> <chr>   <chr>   <chr> <chr>       <chr>     
      1 DB00001     BTD00024;BIOD0… biot… 2005-0… 2024-0… Lepi… "Lepirudin… 138068-37…
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
      1 DB00006     BTD00076;EXPT0… smal… 2005-0… 2024-0… Biva… Bivalirudi… 128270-60…
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
        synonym       language coder      drugbank_id
        <chr>         <chr>    <chr>      <chr>      
      1 Bivalirudin   english  "inn/usan" DB00006    
      2 Bivalirudina  spanish  ""         DB00006    
      3 Bivalirudinum latin    ""         DB00006    
      
      $drugs$pharmacology
      # A tibble: 1 × 12
        drugbank_id indication           pharmacodynamics mechanism_of_action toxicity
        <chr>       <chr>                <chr>            <chr>               <chr>   
      1 DB00006     For treatment of he… "Bivalirudin me… Inhibits the actio… Based o…
      # ℹ 7 more variables: metabolism <chr>, absorption <chr>, half_life <chr>,
      #   protein_binding <chr>, route_of_elimination <chr>,
      #   volume_of_distribution <chr>, clearance <chr>
      
      $drugs$international_brands
      # A tibble: 2 × 3
        brand   company drugbank_id
        <chr>   <chr>   <chr>      
      1 Angiox  ""      DB00006    
      2 Hirulog ""      DB00006    
      
      $drugs$mixtures
      # A tibble: 0 × 0
      
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
      # A tibble: 10 × 3
         category                            mesh_id   drugbank_id
         <chr>                               <chr>     <chr>      
       1 Amino Acids, Peptides, and Proteins "D000602" DB00006    
       2 Anticoagulants                      "D000925" DB00006    
       3 Antithrombins                       "D000991" DB00006    
       4 Blood and Blood Forming Organs      ""        DB00006    
       5 Enzyme Inhibitors                   "D004791" DB00006    
       6 Hematologic Agents                  "D006401" DB00006    
       7 Peptides                            "D010455" DB00006    
       8 Protease Inhibitors                 "D011480" DB00006    
       9 Serine Protease Inhibitors          "D015842" DB00006    
      10 Thrombin Inhibitors                 ""        DB00006    
      
      $drugs$dosages
      # A tibble: 17 × 4
         form                                             route   strength drugbank_id
         <chr>                                            <chr>   <chr>    <chr>      
       1 "Injection, powder, lyophilized, for solution"   Intrav… "250 mg… DB00006    
       2 "Injection, solution"                            Intrav… "250 mg… DB00006    
       3 "Injection, powder, lyophilized, for solution"   Intrav… "250 mg" DB00006    
       4 ""                                               Intrav… "250 mg" DB00006    
       5 "Injection, powder, for solution"                Intrav… "250 MG" DB00006    
       6 "Injection"                                      Intrac… "250 mg… DB00006    
       7 "Injection"                                      Intrav… "250 mg… DB00006    
       8 "Injection, powder, lyophilized, for solution"   Intrav… "250 mg… DB00006    
       9 "Injection, powder, lyophilized, for suspension" Intrav… "250 mg… DB00006    
      10 "Injection, powder, lyophilized, for solution"   Intrav… ""       DB00006    
      11 "Powder, for solution"                           Intrav… "250 mg… DB00006    
      12 "Injection"                                      Intrav… "250 mg… DB00006    
      13 "Injection"                                      Intrav… "500 mg… DB00006    
      14 "Solution"                                       Intrav… "5 mg /… DB00006    
      15 "Injection, solution"                            Intrav… "5 mg/1… DB00006    
      16 "Powder"                                         Intrav… "250 MG" DB00006    
      17 "Powder"                                         Intrav… ""       DB00006    
      
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
      1 5196404 United States 1993-03-23 2010-05-23 false               DB00006    
      2 2065150 Canada        1999-12-14 2010-08-17 false               DB00006    
      3 7598343 United States 2009-10-06 2029-01-27 true                DB00006    
      4 7582727 United States 2009-09-01 2029-01-27 true                DB00006    
      
      $drugs$drug_interactions
      # A tibble: 639 × 4
         target_drugbank_id name                       description         drugbank_id
         <chr>              <chr>                      <chr>               <chr>      
       1 DB06605            Apixaban                   Apixaban may incre… DB00006    
       2 DB06695            Dabigatran etexilate       Dabigatran etexila… DB00006    
       3 DB01254            Dasatinib                  The risk or severi… DB00006    
       4 DB01609            Deferasirox                The risk or severi… DB00006    
       5 DB01586            Ursodeoxycholic acid       The risk or severi… DB00006    
       6 DB02123            Glycochenodeoxycholic Acid The risk or severi… DB00006    
       7 DB02659            Cholic Acid                The risk or severi… DB00006    
       8 DB02691            Glycocholic acid           The risk or severi… DB00006    
       9 DB03619            Deoxycholic acid           The risk or severi… DB00006    
      10 DB04348            Taurocholic acid           The risk or severi… DB00006    
      # ℹ 629 more rows
      
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
      # A tibble: 12 × 3
         resource                     identifier    drugbank_id
         <chr>                        <chr>         <chr>      
       1 Drugs Product Database (DPD) 12945         DB00006    
       2 ChEBI                        59173         DB00006    
       3 PubChem Compound             16129704      DB00006    
       4 PubChem Substance            46507415      DB00006    
       5 KEGG Drug                    D03136        DB00006    
       6 ChemSpider                   10482069      DB00006    
       7 BindingDB                    50248103      DB00006    
       8 PharmGKB                     PA10032       DB00006    
       9 Therapeutic Targets Database DAP000542     DB00006    
      10 Wikipedia                    Bivalirudin   DB00006    
      11 ChEMBL                       CHEMBL2103749 DB00006    
      12 RxCUI                        60819         DB00006    
      
      $drugs$pathway
      $drugs$pathway$general_information
      # A tibble: 1 × 4
        smpdb_id   name                       category    drugbank_id
        <chr>      <chr>                      <chr>       <chr>      
      1 SMP0000277 Bivalirudin Action Pathway drug_action DB00006    
      
      $drugs$pathway$pathway_drugs
      # A tibble: 3 × 3
        drugbank_id name          smpdb_id  
        <chr>       <chr>         <chr>     
      1 DB00006     Bivalirudin   SMP0000277
      2 DB01022     Phylloquinone SMP0000277
      3 DB01373     Calcium       SMP0000277
      
      $drugs$pathway$pathway_enzyme
      # A tibble: 20 × 2
         enzyme smpdb_id  
         <chr>  <chr>     
       1 P00734 SMP0000277
       2 P00748 SMP0000277
       3 P02452 SMP0000277
       4 P03952 SMP0000277
       5 P03951 SMP0000277
       6 P00740 SMP0000277
       7 P00451 SMP0000277
       8 P12259 SMP0000277
       9 P00742 SMP0000277
      10 P02671 SMP0000277
      11 P02675 SMP0000277
      12 P02679 SMP0000277
      13 P00488 SMP0000277
      14 P05160 SMP0000277
      15 P00747 SMP0000277
      16 P00750 SMP0000277
      17 P08709 SMP0000277
      18 P13726 SMP0000277
      19 Q9BQB6 SMP0000277
      20 P38435 SMP0000277
      
      
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
      1 Avoid echinacea.                                                   DB00006    
      2 Avoid herbs and supplements with anticoagulant/antiplatelet activ… DB00006    
      
      $drugs$pdb_entries
      # A tibble: 0 × 0
      
      $drugs$ahfs_codes
      # A tibble: 0 × 0
      
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
      # A tibble: 34 × 19
         name   labeller ndc_id ndc_product_code dpd_id ema_product_code ema_ma_number
         <chr>  <chr>    <chr>  <chr>            <chr>  <chr>            <chr>        
       1 Angio… Cardina… ""     "55154-2275"     ""     ""               ""           
       2 Angio… The Med… ""     "65293-001"      ""     ""               ""           
       3 Angio… Sandoz … ""     "0781-3447"      ""     ""               ""           
       4 Angio… Sandoz … ""     ""               "0224… ""               ""           
       5 Angio… MAIA Ph… ""     "70511-141"      ""     ""               ""           
       6 Angiox The Med… ""     ""               ""     "EMEA/H/C/00056… "EU/1/04/289…
       7 Bival… Accord … ""     "16729-275"      ""     ""               ""           
       8 Bival… Cipla U… ""     "69097-601"      ""     ""               ""           
       9 Bival… Mylan I… ""     "67457-256"      ""     ""               ""           
      10 Bival… Eugia U… ""     "55150-210"      ""     ""               ""           
      # ℹ 24 more rows
      # ℹ 12 more variables: started_marketing_on <chr>, ended_marketing_on <chr>,
      #   dosage_form <chr>, strength <chr>, route <chr>,
      #   fda_application_number <chr>, generic <chr>, over_the_counter <chr>,
      #   approved <chr>, country <chr>, source <chr>, drugbank_id <chr>
      
      $references
      $references$drugs
      $references$drugs$books
      # A tibble: 0 × 0
      
      $references$drugs$articles
      # A tibble: 7 × 4
        ref_id pubmed_id citation                                          drugbank_id
        <chr>  <chr>     <chr>                                             <chr>      
      1 A17    16466327  Seybert AL, Coons JC, Zerumsky K: Treatment of h… DB00006    
      2 A18    17381384  Dager WE, Dougherty JA, Nguyen PH, Militello MA,… DB00006    
      3 A19    16553503  Dang CH, Durkalski VL, Nappi JM: Evaluation of t… DB00006    
      4 A20    11156732  Robson R: The use of bivalirudin in patients wit… DB00006    
      5 A21    21108549  Van De Car DA, Rao SV, Ohman EM: Bivalirudin: a … DB00006    
      6 A22    16614733  Shammas NW: Bivalirudin: pharmacology and clinic… DB00006    
      7 A23    12851152  Gleason TG, Chengelis CP, Jackson CB, Lindstrom … DB00006    
      
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
      # A tibble: 1 × 4
        ref_id pubmed_id citation                                            enzyme_id
        <chr>  <chr>     <chr>                                               <chr>    
      1 A17596 18701766  Rudolph V, Rudolph TK, Schopfer FJ, Bonacci G, Lau… BE0001075
      
      $references$enzymes$links
      # A tibble: 0 × 0
      
      $references$enzymes$attachments
      # A tibble: 0 × 0
      
      
      $references$targets
      $references$targets$books
      # A tibble: 0 × 0
      
      $references$targets$articles
      # A tibble: 6 × 4
        ref_id pubmed_id citation                                            target_id
        <chr>  <chr>     <chr>                                               <chr>    
      1 A1733  11060732  Scatena R: Bivalirudin: a new generation antithrom… BE0000048
      2 A1736  11504570  Bates ER: Bivalirudin for percutaneous coronary in… BE0000048
      3 A1738  11833835  Gladwell TD: Bivalirudin: a direct thrombin inhibi… BE0000048
      4 A1739  11923794  Kleiman NS, Klem J, Fernandes LS, Rubin H, Challa … BE0000048
      5 A1741  11929334  Carswell CI, Plosker GL: Bivalirudin: a review of … BE0000048
      6 A9     11752352  Chen X, Ji ZL, Chen YZ: TTD: Therapeutic Target Da… BE0000048
      
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
        enzyme_id name  organism known_action position drugbank_id inhibition_strength
        <chr>     <chr> <chr>    <chr>        <chr>    <chr>       <chr>              
      1 BE0001075 Myel… Humans   unknown      1        DB00006     unknown            
      # ℹ 1 more variable: induction_strength <chr>
      
      $cett$enzymes$actions
      # A tibble: 1 × 2
        action    enzyme_id
        <chr>     <chr>    
      1 inhibitor BE0001075
      
      $cett$enzymes$polypeptides
      $cett$enzymes$polypeptides$general_information
      # A tibble: 1 × 20
        polypeptide_id source name  general_function specific_function gene_name locus
        <chr>          <chr>  <chr> <chr>            <chr>             <chr>     <chr>
      1 P05164         Swiss… Myel… Peroxidase acti… Part of the host… MPO       17q2…
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
       1 component azurophil granule     BE0001075
       2 component extracellular exosome BE0001075
       3 component extracellular space   BE0001075
       4 component lysosome              BE0001075
       5 component mitochondrion         BE0001075
       6 component nucleus               BE0001075
       7 component secretory granule     BE0001075
       8 function  chromatin binding     BE0001075
       9 function  heme binding          BE0001075
      10 function  heparin binding       BE0001075
      # ℹ 14 more rows
      
      
      
      $cett$targets
      $cett$targets$general_information
      # A tibble: 1 × 6
        target_id name        organism known_action position drugbank_id
        <chr>     <chr>       <chr>    <chr>        <chr>    <chr>      
      1 BE0000048 Prothrombin Humans   yes          1        DB00006    
      
      $cett$targets$actions
      # A tibble: 1 × 2
        action    target_id
        <chr>     <chr>    
      1 inhibitor BE0000048
      
      $cett$targets$polypeptides
      $cett$targets$polypeptides$general_information
      # A tibble: 1 × 20
        polypeptide_id source name  general_function specific_function gene_name locus
        <chr>          <chr>  <chr> <chr>            <chr>             <chr>     <chr>
      1 P00734         Swiss… Prot… Thrombospondin … Thrombin, which … F2        11p1…
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
         category  description                 target_id
         <chr>     <chr>                       <chr>    
       1 component blood microparticle         BE0000048
       2 component cytosol                     BE0000048
       3 component endoplasmic reticulum lumen BE0000048
       4 component extracellular exosome       BE0000048
       5 component extracellular matrix        BE0000048
       6 component extracellular region        BE0000048
       7 component extracellular space         BE0000048
       8 component Golgi lumen                 BE0000048
       9 component plasma membrane             BE0000048
      10 function  calcium ion binding         BE0000048
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
      

