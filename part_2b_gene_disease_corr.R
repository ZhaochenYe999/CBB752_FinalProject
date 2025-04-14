

# LOADING LIBARIES --------------------------------------------------------


library(tidyverse)
library(stringr)
library(table1)

df = read_csv("abstracts.csv")


# FUNCTIONS ---------------------------------------------------------------
map_disease_name <- function(value) {
    case_when(
        nchar(value) == 1 ~ NA_character_,
        
        value %in% c("diabetes", "diabetes mellitus", "dm") ~ "Diabetes",
        
        value %in% c("chronic kidney disease", "ckd", "chronic renal insufficiency", "ksd",
                     "renal disease", "renal impairment", "impairment of renal function", 
                     "kidney disease", "digestive and kidney disease", 
                     "end - stage kidney disease", "hpsronic kidney disease", 
                     "apol1 kidney disease", "proteinuric kidney diseases", 
                     "kidney stone disease", "stone", "end - stage organ disease", 
                     "kd", "eskd", "amkd", "dkd") ~ "Kidney Disease",
        
        value %in% c("hcc") ~ "Liver Cancer",
        
        value %in% c("sickle cell", "sickle cell anemia", "sickle cell disease", 
                     "sca", "scd", "sse") ~ "Sickle Cell Disease",
        
        value %in% c("idiopathic pulmonary fibrosis", "ipf", "pulmonary fibrosis", 
                     "restrictive lung disease", "interstitial lung disease", "##iratory disease") ~ "Lung Disease",
        
        value %in% c("##cytop") ~ "Podocytopathy",
        
        str_detect(value, "enterella") ~ "Salmonella",
        
        value %in% c("hearing loss", "genetic hearing loss", "deafness", 
                     "sensorineural deafness", "sensorineural hearing loss", 
                     "sensorineural disorders", "nonsyndromic deafness", 
                     "autosomal recessive non - syndromic deafness", 
                     "snhl", "hl") ~ "Hearing Loss",
        
        value %in% c("infertility", "azoospermia", "non - obstructive azoospermia", 
                     "male infertility", "noa", "no") ~ "Infertility",
        
        str_detect(value, "alzheimer") ~ "Alzheimer's Disease",
        
        value %in% c("depression", "depressive disorder") ~ "Depression",
        
        value %in% c("schizophrenia", "scz") ~ "Schizophrenia",
        
        value %in% c("breast cancer", "brea") ~ "Breast Cancer",
        
        value %in% c("kirc") ~ "Kidney Cancer",
        
        value %in% c("tumorige", "lung adenocarcinoma", "luad") ~ "Lung Cancer",
        
        value %in% c("gastric cancer", "gc", "gastrointestinal cancer", "gastrointestinal cancers") ~ "Gastric Cancer",
        
        value %in% c("ovarian tumors", "ovarian cancer", "oc") ~ "Ovarian Cancer",
        
        value %in% c("maligna") ~ "Malignancies",
        
        value %in% c("glaucoma") ~ "Glaucoma",
        
        value %in% c("hermansky - pudlak syndrome", "hps", "hp", "autosomal recessive disorder", "herman", "hsp") ~ "Hermansky-Pudlak Syndrome",
        
        value %in% c("##matory bowel disease", "ibd") ~ "Inflammatory Bowel Disease",
        
        value %in% c("inflam", "inf", "##lammation") ~ "Inflammation",
        
        value %in% c("bleeding diath") ~ "Bleeding Diathesis",
        
        value %in% c("pulmona") ~ "Pulmonary Fibrosis",
        
        value %in% c("immu") ~ "Immunodeficiency",
        
        value %in% c("nec") ~ "Necrosis",
        
        value %in% c("oculocuta", "oca", "oca1", "oca type 1", "periodic albinism") ~ "Albinism",
        
        value %in% c("parasuis", "glaesserella parasui", "para") ~ "Glaesserella Parasuis",
        
        value %in% c("thala") ~ "Thalassemia",
        
        value %in% c("hyper") ~ "Hypertension",
        
        value %in% c("hy") ~ "Hypotoxic",
        
        value %in% c("cyt") ~ "Cytotoxicity",
        
        value %in% c("pe") ~ "Preeclampsia",
        
        value %in% c("bt") ~ "Bioavailable Testosterone",
        
        value %in% c("dr") ~ "Nonsyndromic",
        
        value %in% c("md") ~ "Ménière's Disease",
        
        value %in% c("dis") ~ "Disease",
        
        value %in% c("obes") ~ "Obesity",
        
        value %in% c("cah") ~ "Congenital Adrenal Hyperplasia",
        
        value %in% c("kfs", "kfs4") ~ "Klippel-Feil Syndrome",
        
        value %in% c("kfa") ~ "Klippel-Feil Anomaly",
        
        value %in% c("mld") ~ "Metachromatic Leukodystrophy",
        
        value %in% c("mfs") ~ "Marfan Syndrome",
        
        value %in% c("msd") ~ "Multiple Sulfatase Deficiency",
        
        value %in% c("poi") ~ "Primary Ovarian Insufficiency",
        
        value %in% c("purp", "rp") ~ "Strongylocentrotus Purpuratus",
        
        value %in% c("ptc") ~ "Papillary Thyroid Carcinoma",
        
        value %in% c("pai") ~ "Primary Adrenal Insufficiency",
        
        value %in% c("dsd") ~ "Disorder of Sex Development",
        
        value %in% c("fa") ~ "Fanconi Anemia",
        
        value %in% c("st", "ry fibrosis", "al", "##tension", "idy", "magp", "ol1", "in", "can", "choc", "ch", "ic", "sa", "resp") ~ NA_character_,
        
        value %in% c("arsa", "arte", "ar") ~ "Aberrant Right Subclavian Artery",
        
        value %in% c("cf") ~ "Cystic Fibrosis",
        
        value %in% c("nb") ~ "Neuroblastoma",
        
        value %in% c("lvh") ~ "Left Ventricular Hypertrophy",
        
        value %in% c("mpn") ~ "Myeloproliferative Neoplasms",
        
        value %in% c("lu") ~ "Lungs",
        
        str_detect(value, "parkinson") ~ "Parkinson's Disease",
        
        str_detect(value, "huntington") ~ "Huntington's Disease",
        
        value %in% c("pd") ~ "Parkinson Disease",
        
        str_detect(value, "kommerell") ~ "Kommerell's Diverticulum",
        
        str_detect(value, "ménière") ~ "Ménière's Disease",
        
        value %in% c("dl") ~ "Dysphagia Lusoria",
        
        value %in% c("pca") ~ "Prostate Cancer",
        
        value %in% c("fat") ~ "Fat Droplet Formation",
        
        value %in% c("cmd") ~ "Congenital Muscular Dystrophy",
        
        value %in% c("gerd") ~ "Gastroesophageal Reflux Disease",
        
        value %in% c("malf") ~ "Malformations",
        
        value %in% c("csvd") ~ "Cerebral Small-Vessel Disease",
        
        value %in% c("pms", "pm") ~ "Phelan-McDermid Syndrome",
        
        value %in% c("chd", "cad") ~ "Heart Disease",
        
        value %in% c("all") ~ "Acute Lymphoblastic Leukemia",
        
        value %in% c("osa") ~ "Obstructive Sleep Apnea",
        
        value %in% c("fsgs") ~ "Focal Segmental Glomerular Sclerosis",
        
        value %in% c("vus") ~ "Variants of Unknown Significance",
        
        value %in% c("sr", "srd") ~ "Specific Reading Disability",
        
        value %in% c("hlhs") ~ "Hypoplastic Left Heart Syndrome",
        
        value %in% c("hnc") ~ "Head and Neck Cancer",
        
        value %in% c("cscc") ~ "Cutaneous Squamous-Cell Carcinoma",
        
        value %in% c("cud") ~ "Cannabis Use Disorder",
        
        value %in% c("od") ~ "Organ Dysfunction",
        
        value %in% c("ptoa") ~ "Post-Traumatic Osteoarthritis",
        
        value %in% c("ntd", "ntds") ~ "Neural Tube Defects",
        
        value %in% c("rs") ~ "Respiratory Sinus Arrhythmia",
        
        value %in% c("mdd") ~ "Major Depressive Disorder",
        
        value %in% c("scm") ~ "Subclinical Mastitis",
        
        TRUE ~ value
    )
}



# CLEANING DISEASE ENTITIES -----------------------------------------------

df_clean = df %>%
    #not relevant publication
    filter(pmid != "39406866") %>%
    mutate(disease_entities = str_replace_all(disease_entities, "\\[|\\]|'", "")) %>%
    mutate(disease_entities = str_split(disease_entities, ",\\s*"))


disease_long = df_clean %>%
    unnest(disease_entities) %>%
    mutate(disease_entities = str_trim(disease_entities)) %>%
    filter(disease_entities != "") %>%
    mutate(std_name = map_disease_name(disease_entities)) %>%
    drop_na()

# BARPLOT -----------------------------------------------------------------

top_terms = disease_long %>%
    count(std_name, sort = TRUE)

top_terms %>%
    slice_max(n, n = 20) %>%
    ggplot(aes(x = reorder(std_name, n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Most Frequent Disease Mentions",
        x = "Disease Entity", y = "Frequency"
    ) +
    theme_minimal()


View(disease_long)


# HEATMAP -----------------------------------------------------------------


gene_disease = disease_long %>%
    count(gene, std_name, sort = TRUE)


gene_disease_matrix = disease_long %>%
    count(gene, std_name) %>%
    pivot_wider(names_from = std_name, values_from = n, values_fill = 0)

gene_disease_long = gene_disease_matrix %>%
    pivot_longer(-gene, names_to = "disease", values_to = "count")

top_diseases = gene_disease_long %>%
    group_by(disease) %>%
    summarise(total = sum(count)) %>%
    arrange(desc(total)) %>%
    slice_max(total, n = 20) %>%
    pull(disease)

heatmap_data = gene_disease_long %>%
    filter(disease %in% top_diseases)


ggplot(heatmap_data, aes(x = disease, y = gene, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
        title = "Gene-Disease Co-occurrence Heatmap",
        x = "Disease Entity",
        y = "Gene",
        fill = "Co-occurrence"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# STAT TESTS -------------------------------------------------------

table1(~std_name, data=disease_long)

contingency_tbl <- table(disease_long$gene, disease_long$disease_entities)

chi_test <- chisq.test(contingency_tbl)

# View the p-values for associations
chi_test$p.value  # overall test

# For individual gene-disease combinations (optional)
residuals <- chi_test$stdres  # standardized residuals (higher = stronger deviation)

View
