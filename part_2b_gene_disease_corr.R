# Load libraries
library(tidyverse)
library(stringr)


View(df)

df = read_csv("abstracts.csv")

df_clean = df %>%
    mutate(disease_entities = str_replace_all(disease_entities, "\\[|\\]|'", "")) %>%
    mutate(disease_entities = str_split(disease_entities, ",\\s*"))


disease_long = df_clean %>%
    unnest(disease_entities) %>%
    mutate(disease_entities = str_trim(disease_entities)) %>%
    filter(disease_entities != "")

top_terms = disease_long %>%
    count(disease_entities, sort = TRUE)


top_terms %>%
    slice_max(n, n = 20) %>%
    ggplot(aes(x = reorder(disease_entities, n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
        title = "Top 20 Most Frequent Disease Mentions",
        x = "Disease Entity", y = "Frequency"
    ) +
    theme_minimal()

gene_disease = disease_long %>%
    count(gene, disease_entities, sort = TRUE)


gene_disease_matrix = disease_long %>%
    count(gene, disease_entities) %>%
    pivot_wider(names_from = disease_entities, values_from = n, values_fill = 0)

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

