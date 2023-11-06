library(DBI)
library(ROracle)
library(tidyverse)
library(dplyr)
library(cluster)
library(tidyr)
library(factoextra)  # For visualization
library(cluster)     # For hierarchical clustering
library(FactoMineR)  # For PCA
library(klaR)
library(hms)
library(caret)
library(ggplot2)
library(plotly)
library(RColorBrewer)


drv = dbDriver("Oracle")
con <- dbConnect(drv,
                 username = "graham_kerford_lvl2",
                 password = "Windfield101!1",
                 dbname = "HI_PRD.AQYP")

# Get CHSA data
DT <- dbSendQuery(con, "SELECT
    DCSD_LABEL,
    RGSTR_NUM,
    RGSTN_DATE,
    TM_OF_DTH,
    DCSD_AGE_LABEL,
    DCSD_GENDER_LABEL,
    cnt.DCSD_BRTH_COUNTRY,
    DCSD_BRTH_ADDR_PROV,
    DCSD_OCCPN,
    geog.DCSD_RSDC_HA_AREA,
    geog.DCSD_RSDC_HSDA,
    geog.DCSD_RSDC_LHA,
    geog.DCSD_RSDC_CHSA,
    DCSD_MRTL_STATUS_DSCR,
    ucod.UCOD,
    ucod.CS_124_LVL_2,
    hpt.HSP,
    hpt_tp.DTH_PLC_TP,
    RCNT_SURG_FLG,
    DTH_DURING_PRGCY_FLG,
    DCSD_WORK_YRS,
    ENVMNTL_OR_LFSTYL_FLG,
    HRT_VALVE_RPLCD_FLG,
    ORGAN_TRNSPLT_RECIP_FLG,
    ALCL_REL_FLG,
    DRUG_REL_FLG,
    ACDNTL_DTH_ACTVY_LABEL
FROM
    CB_DTL_FT_DTHEVT_VWP dth,
    CB_DTL_DM_DCSD_BRTH_CTRY_VW cnt,
    CB_DTL_DM_DCSD_RSDC_GEOG_VW geog,
    CB_DTL_DM_UCOD_VW ucod,
    CB_DTL_DM_HSP_VW hpt,
    CB_DTL_DM_DTH_PLC_TP_VW hpt_tp
WHERE
    dth.DCSD_BRTH_CTRY_PRIMARY_ANON_ID = cnt.PRIMARY_ANON_ID
    AND dth.DCSD_RSDC_GEOG_PRIMARY_ANON_ID = geog.PRIMARY_ANON_ID
    AND dth.UCOD_LABEL = ucod.LABEL
    AND dth.UCOD_TYDF_CODE = ucod.TYDF_CODE
    AND dth.HSP_LABEL = hpt.LABEL
    AND dth.HSP_TYDF_CODE = hpt.TYDF_CODE
    AND dth.DTH_PLC_TP_LABEL = hpt_tp.LABEL
    AND dth.DTH_PLC_TP_EFF_DATE = hpt_tp.EFF_DATE")
data <- dbFetch(DT)

# Get geographic data
# DT<- dbSendQuery(con, 'SELECT * FROM GEOG_CHSA')
# GEOG  <- dbFetch(DT)
dbDisconnect(con)
dbUnloadDriver(drv)


# Calculate counts and filter condition
ucod_counts <- data %>%
  group_by(UCOD) %>%
  summarise(count = n()) %>%
  filter(count >= 5000)

# Filter the main dataframe based on the calculated condition
data_wk <- data %>%
  filter(UCOD %in% ucod_counts$UCOD)

# Randomly remove part of the dataset due to size constraints
set.seed(123)

data_wk <- data_wk %>%
  mutate(row_num = row_number()) %>%  # Add a row number column
  sample_frac(0.05) %>%  # Randomly sample 50% of the rows
  select(-row_num)  # Remove the row number column

# Remove non-numeric values from DCSD_AGE_LABEL and normalize it
data_wk$DCSD_AGE_LABEL <- as.numeric(gsub("[^0-9]", "", data_wk$DCSD_AGE_LABEL))
data_wk$DCSD_AGE_LABEL <- (data_wk$DCSD_AGE_LABEL - min(data_wk$DCSD_AGE_LABEL, na.rm = T)) / (max(data_wk$DCSD_AGE_LABEL, na.rm = T) - min(data_wk$DCSD_AGE_LABEL, na.rm = T))

# normalize time
data_wk$TM_OF_DTH <- data_wk$TM_OF_DTH %>% as.numeric()
data_wk$TM_OF_DTH <- (data_wk$TM_OF_DTH - min(data_wk$TM_OF_DTH)) / (max(data_wk$TM_OF_DTH) - min(data_wk$TM_OF_DTH))

data_wk$DCSD_WORK_YRS <- (data_wk$DCSD_WORK_YRS - min(data_wk$DCSD_WORK_YRS)) / (max(data_wk$DCSD_WORK_YRS) - min(data_wk$DCSD_WORK_YRS)
)

# Normalize date relative to the current date
data_wk <- data_wk %>%
  mutate(RGSTN_DATE = as.Date(RGSTN_DATE),  # Convert RGSTR_DATE column to Date type
         NormalizedDate = (as.numeric(Sys.Date() - RGSTN_DATE) - min(as.numeric(Sys.Date() - RGSTN_DATE), na.rm = TRUE)) /
           (max(as.numeric(Sys.Date() - RGSTN_DATE), na.rm = TRUE) - min(as.numeric(Sys.Date() - RGSTN_DATE), na.rm = TRUE)))


# Convert categorical variables to factors
categorical_vars <- c("DCSD_GENDER_LABEL", "DCSD_BRTH_COUNTRY", "DCSD_BRTH_ADDR_PROV", "DCSD_OCCPN", "DCSD_RSDC_HA_AREA",
                      "DCSD_RSDC_HSDA", "DCSD_RSDC_LHA", "DCSD_RSDC_CHSA", "DCSD_MRTL_STATUS_DSCR", "CS_124_LVL_2",
                      "HSP", "DTH_PLC_TP", "RCNT_SURG_FLG", "DTH_DURING_PRGCY_FLG", "ENVMNTL_OR_LFSTYL_FLG", "UCOD",
                      "HRT_VALVE_RPLCD_FLG", "ORGAN_TRNSPLT_RECIP_FLG", "ALCL_REL_FLG", "DRUG_REL_FLG", "ACDNTL_DTH_ACTVY_LABEL")


# Preform PCA and clustering of numerical data and categorical data
#-----------------------

data_cat <- data_wk[categorical_vars] %>% select(UCOD)

# One-hot encode the categorical variable UCOD
data_cat_encoded <- as.data.frame(model.matrix(~ 0 + UCOD, data = data_cat))

data_wk_num <- data_wk %>% select(-c(categorical_vars, DCSD_LABEL, RGSTR_NUM, RGSTN_DATE))

# Combine one-hot encoded data with numerical data
combined_data <- cbind(data_cat_encoded, data_wk_num)


# Perform PCA
pca_result <- prcomp(combined_data, scale = TRUE)


# Calculate distances between observations based on PCA components
pca_dist <- dist(pca_result$x[, 1:2])

# Hierarchical clustering
pca_hclust <- hclust(pca_dist, method = "ward.D2")

# Plot the dendrogram
dend <- as.dendrogram(pca_hclust)
plot(dend)

# You can interactively determine the number of clusters by cutting the dendrogram
# or you can use an automatic method like the "cutree" function

# Example: Cut dendrogram into k clusters
k <- 8  # Number of clusters
clusters <- cutree(pca_hclust, k)

# Define color palette
clst_cl <- colorRampPalette(c("blue", "green", "red"))

# Add cluster labels to data
combined_data <- combined_data %>%
  mutate(cluster = clusters)

nmb_axes <- 8

for(i in 2:nmb_axes-1){
  print(i)
  for(j in 2:nmb_axes){
    print(j)
    if(j > i){
      pca_plot <- fviz_pca_ind(pca_result, geom.ind = "point", col.ind = combined_data$cluster, axes = c(i,j)) +
        theme_bw() +  # Set a white background theme
        theme(plot.background = element_rect(fill = "white"),
              text = element_text(size = 24),  # Adjust text size
              axis.text = element_text(size = 20),  # Adjust axis text size
              legend.title = element_text(size = 24),  # Adjust legend title size
              legend.text = element_text(size = 20),  # Adjust legend text size
              legend.key.size = unit(1.5, "lines")) +  # Adjust legend key size
        scale_color_gradientn(colors = clst_cl(100)) 
      
      ggsave(paste0("./cluster_images/Numeric_UCOD/PCA_components/pca_",i,"_",j,".png"), plot = pca_plot, width = 25, height = 25)
      
    }
    
  }
  
}


# Identify binary columns
# binary_columns <- colnames(combined_data)[sapply(combined_data, function(col) all(col %in% c(0, 1)))]
# 
# # Group by 'cluster' and summarize binary columns
# binary_summary <- combined_data %>%
#   group_by(cluster) %>%
#   summarize(across(all_of(binary_columns), list(count_1 = ~ sum(. == 1), count_0 = ~ sum(. == 0))))

# Group by 'cluster' and calculate summary statistics for numerical columns
numerical_summary <- combined_data %>%
  group_by(cluster) %>%
  summarize(across(where(is.numeric), list(mean = ~ mean(., na.rm = TRUE), median = ~ median(., na.rm = TRUE), sd = ~ sd(., na.rm = TRUE))))

# Merge the two summaries based on 'cluster'
# summary_data <- left_join(binary_summary, numerical_summary, by = "cluster")

# Print the summary data
# print(summary_data)


# Identify columns containing values, medians, and SDs
value_columns <- colnames(numerical_summary %>% select(-c(cluster)))[c(TRUE, FALSE, FALSE)]

# Select cluster and mean columns
value_data <- numerical_summary %>% select(cluster, all_of(value_columns))

# Remove "_mean" from column names
new_colnames <- str_remove(colnames(value_data)[-1], "_mean")
colnames(value_data)[-1] <- new_colnames

value_data_lg <- value_data %>% pivot_longer(cols = new_colnames, names_to = "variable", values_to = "mean")



sd_columns <- colnames(numerical_summary %>% select(-c(cluster)))[c(FALSE, FALSE, TRUE)]

# Select cluster and SD columns
sd_data <- numerical_summary %>% select(cluster, all_of(sd_columns))

# Remove "_sd" from column names
new_colnames <- str_remove(colnames(sd_data)[-1], "_sd")
colnames(sd_data)[-1] <- new_colnames

sd_data_lg <- sd_data %>% pivot_longer(cols = new_colnames, names_to = "variable", values_to = "sd")


comb_data_lg <- merge(value_data_lg, sd_data_lg, by = c("cluster", "variable"))


# List of unique variables
unique_variables <- unique(comb_data_lg$variable)


# Function to generate and save plots for all variables
generate_and_save_plots <- function(variable_name) {
  plot_data <- comb_data_lg %>%
    filter(variable == variable_name)
  
  plot <- ggplot(plot_data, aes(x = cluster, y = mean, fill = factor(cluster))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(width = 0.9)) +
    labs(title = paste("Bar Plot of", variable_name, "by Cluster"), x = "Cluster", y = "Mean") +
    theme_minimal() +
    theme_bw() +  # Set a white background theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(name = "Cluster")
  
  plot_filename <- paste0("bar_chart/", gsub(" ", "_", variable_name), ".png")
  ggsave(plot, filename = plot_filename, width = 6.5, height = 5.72)
}

# Generate and save plots for all variables
walk(unique_variables, generate_and_save_plots)









# Example: Hierarchical clustering of variables based on their summary statistics
hclust_result <- hclust(dist(summary_data[, -1]))  # Exclude the 'cluster' column
plot(hclust_result)











# -----------------------


numeric_vars <- colnames(combined_data %>% select(-c(cluster)))

# Loop through each pair of numeric variables
for (var1 in numeric_vars) {
  for (var2 in numeric_vars) {
    if (var1 != var2) {
      # Create a scatter plot of variables var1 and var2 with cluster colors
      plot_title <- paste("Comparison of", var1, "and", var2)
      plot_path <- paste0("./comparison_plots/", var1, "_vs_", var2, ".png")
      
      p <- ggplot(combined_data, aes(x = !!sym(var1), y = !!sym(var2), color = as.factor(cluster))) +
        geom_point() +
        labs(title = plot_title, x = var1, y = var2) +
        theme_bw() +  # Set a white background theme
        theme(plot.background = element_rect(fill = "white"),
              text = element_text(size = 24),  # Adjust text size
              axis.text = element_text(size = 20),  # Adjust axis text size
              legend.title = element_text(size = 24),  # Adjust legend title size
              legend.text = element_text(size = 20),  # Adjust legend text size
              legend.key.size = unit(1.5, "lines"))   # Adjust legend key size
      
      
      # Save the plot as an image with adjusted size and resolution using ggsave
      ggsave(paste0("./cluster_images/Numeric_UCOD/var_comp/var_",var1,"_",var2,".png"), plot = p, width = 25, height = 25)
    }
  }
}



# Calculate cluster averages
cluster_avg <- data_wk_num %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)
cluster_avg




# Preform PCA and clustering of numerical data
#-----------------------

data_wk_num <- data_wk %>% select(-c(categorical_vars, DCSD_LABEL, RGSTR_NUM, RGSTN_DATE))

# Perform PCA
pca_result <- prcomp(data_wk_num, scale = TRUE)

# Calculate distances between observations based on PCA components
pca_dist <- dist(pca_result$x[, 1:2])

# Hierarchical clustering
pca_hclust <- hclust(pca_dist, method = "ward.D2")

# Plot the dendrogram
dend <- as.dendrogram(pca_hclust)
plot(dend)

# You can interactively determine the number of clusters by cutting the dendrogram
# or you can use an automatic method like the "cutree" function

# Example: Cut dendrogram into k clusters
k <- 6  # Number of clusters
clusters <- cutree(pca_hclust, k)

# Define color palette
clst_cl <- colorRampPalette(c("blue", "green", "red"))

# Add cluster labels to data
data_wk_num <- data_wk_num %>%
  mutate(cluster = clusters)

nmb_axes <- 4

for(i in 2:nmb_axes-1){
 print(i)
  for(j in 2:nmb_axes){
    print(j)
    if(j > i){
      pca_plot <- fviz_pca_ind(pca_result, geom.ind = "point", col.ind = data_wk_num$cluster, axes = c(i,j)) +
        theme_bw() +  # Set a white background theme
        theme(plot.background = element_rect(fill = "white"),
              text = element_text(size = 24),  # Adjust text size
              axis.text = element_text(size = 20),  # Adjust axis text size
              legend.title = element_text(size = 24),  # Adjust legend title size
              legend.text = element_text(size = 20),  # Adjust legend text size
              legend.key.size = unit(1.5, "lines")) +  # Adjust legend key size
        scale_color_gradientn(colors = clst_cl(100)) 
      
      ggsave(paste0("./Numeric/cluster_images/PCA_components/pca_",i,"_",j,".png"), plot = pca_plot, width = 25, height = 25)
     
    }
    
  }
  
}

numeric_vars <- colnames(data_wk_num %>% select(-c(cluster)))

# Loop through each pair of numeric variables
for (var1 in numeric_vars) {
  for (var2 in numeric_vars) {
    if (var1 != var2) {
      # Create a scatter plot of variables var1 and var2 with cluster colors
      plot_title <- paste("Comparison of", var1, "and", var2)
      plot_path <- paste0("./comparison_plots/", var1, "_vs_", var2, ".png")
      
      p <- ggplot(data_wk_num, aes(x = !!sym(var1), y = !!sym(var2), color = as.factor(cluster))) +
        geom_point() +
        labs(title = plot_title, x = var1, y = var2) +
        theme_bw() +  # Set a white background theme
        theme(plot.background = element_rect(fill = "white"),
              text = element_text(size = 24),  # Adjust text size
              axis.text = element_text(size = 20),  # Adjust axis text size
              legend.title = element_text(size = 24),  # Adjust legend title size
              legend.text = element_text(size = 20),  # Adjust legend text size
              legend.key.size = unit(1.5, "lines"))   # Adjust legend key size
        
      
      # Save the plot as an image with adjusted size and resolution using ggsave
      ggsave(paste0("./Numeric/cluster_images/var_comp/var_",var1,"_",var2,".png"), plot = p, width = 25, height = 25)
    }
  }
}



# Calculate cluster averages
cluster_avg <- data_wk_num %>%
  group_by(cluster) %>%
  summarise_all(mean, na.rm = TRUE)
cluster_avg

