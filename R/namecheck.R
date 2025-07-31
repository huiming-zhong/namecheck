#學名比對
#setting path ====
setwd("F:/iucn")
getwd()
rm(list=ls())

##library prep ====
library(readxl) # import xlsx
library(magrittr) # %>%
library(dplyr)
library(stringr) #word

#:::學名修改:::====
#import data & 建立資料夾====
namecheck <- read.csv("combine.table_namecheck_20250429.csv")

TT_name <- read.csv("taxatree_20250429.csv")
TC_taxon <- read.csv("TaiCOL_taxon_20250429.csv")

if (!dir.exists("output_name change")) {
  dir.create("output_name change", recursive = TRUE)
}
##Part I :確認新學名是否已存在TT ====

##Part II :找出含亞屬分類群，更改屬 ====
namecheck <- namecheck %>%
  mutate(subgenus = grepl("\\(", simplifiedScientificName),
         newScientificName = gsub("\\s*\\([^\\)]+\\)", "", namecheck$simplifiedScientificName),
         match_name = newScientificName == new_simple_name)

genus_lookup <- TT_name %>%
  filter(taxonRank == "genus",
         kingdom != "Plantae") %>%
  select(taxonUUID, simplifiedScientificName) %>%
  rename(genus = simplifiedScientificName,
         parentUUID = taxonUUID)

namecheck_subgenus <- left_join(namecheck, TT_name[c(1,43)], by = "taxonUUID") %>% #找出屬名
  filter(subgenus == TRUE,
         match_name == TRUE,
         taxonRank == "species") %>% #找出有亞屬，TT及TC名相同，rank為種
  rename(genus = genus.y) %>%
  left_join(genus_lookup, by = "genus") %>% #屬名對應的UUID
  select(taxonUUID,parentUUID,genus)

namecheck_subgenus_infra <-  left_join(namecheck,TT_name[c(1,4,43)], by = "taxonUUID") %>% #找出種UUID,屬名
  filter(subgenus == TRUE,
         match_name == TRUE,
         taxonRank == "infraspecies") %>%
  rename(genus = genus.y,
         speciesUUID = parentUUID)  %>%
  left_join(genus_lookup, by = "genus") %>%
  select(speciesUUID,parentUUID,genus) %>%
  rename(taxonUUID = speciesUUID)

namecheck_subgenus_output <- namecheck_subgenus %>%
  rbind(namecheck_subgenus_infra)

write.csv(namecheck_subgenus_output,"output_name change/namecheck_subgenus_output.csv",,row.names = F)

##Part III :TT與TC的Rank相同，更改上階層(屬名or種的屬名) ====
namecheck_sameRank <- namecheck%>%
  filter(taxonRank == rank)

#種階層
namecheck_sameRank_sp <- namecheck_sameRank%>%
  filter(taxonRank == c("species"))
#比較雙方的 屬名、種小名

#1.屬名不同 => 更改parentUUID

#2.種小名不同 => 更改種小名

#種下階層
namecheck_sameRank_infrasp <- namecheck_sameRank%>%
  filter(taxonRank == c("infraspecies"))
#比較雙方的屬名、種小名、亞種名

#屬名不同 => 找出種階層，再更改parentUUID (同Part II :亞屬更改屬)

#種小名不同 => 找出種階層，更改種小名

#亞種名不同 => 更改亞種名



#:::儀表板 Validation Dashboard:::====
#import data & 建立資料夾====
attributeerror <- read.csv("TT_attributeerror_result.csv")

if (!dir.exists("output_Dashboard change")) {
  dir.create("output_Dashboard change", recursive = TRUE)
}

unique(attributeerror$reason)

##Part I :原生性修改 ====
native <- attributeerror %>%
  filter(reason == "種與種下原生性空白") %>%
  left_join(TT_name[c("taxonUUID","taiCOLNameCode","kingdom")],by = "taxonUUID") %>%
  rename(taxon_id = taiCOLNameCode) %>%
  left_join(TC_taxon[c("taxon_id","alien_type")], by = "taxon_id") %>%
  mutate(alien_type = recode(alien_type,
                             "native" = "原生 Native")) %>%
  select(taxonUUID,kingdom,taxonRank,taxon_id,simplifiedScientificName,nativeness,alien_type) %>%
  filter(!is.na(alien_type),
         alien_type != "")

write.csv(native,"output_Dashboard change/nativeness_change_output.csv",row.names = F)

##Part II :敏感狀態修改 ====
non_native_sensitive <- attributeerror %>%
  filter(reason == "敏感狀態不等於無的外來種") %>%
  left_join(TT_name[c("taxonUUID","kingdom")],by = "taxonUUID") %>%
  mutate(new_sensitive = " ") %>%
  select(taxonUUID,kingdom,taxonRank,simplifiedScientificName,new_sensitive)
#write.csv(non_native_sensitive,"output_Dashboard change/sensitive_change_output.csv",row.names = F)

non_sensitive_redlist <- attributeerror %>%
  filter(reason == "敏感狀態=無的國內紅皮書等級高於VU的物種" | reason == "敏感狀態=無的國際紅皮書等級高於VU的物種") %>%
  left_join(TT_name[c("taxonUUID","kingdom")],by = "taxonUUID") %>%
  mutate(new_sensitive = ifelse(nativeness == "原生 Native" , "敏感","")) %>%
  select(taxonUUID,kingdom,taxonRank,simplifiedScientificName,new_sensitive)
#write.csv(non_native_redlist,"output_Dashboard change/sensitive_change_output.csv",row.names = F)

sensitive_change_final <- bind_rows(non_native_sensitive, non_sensitive_redlist)

write.csv(sensitive_change_final,"output_Dashboard change/sensitive_change_final_output.csv",row.names = F)


#總結表單 ====
output_folders <- c("output_name change","output_Dashboard change")
# 取得所有CSV檔案
all_csv_files <- list.files(path = output_folders, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# 建立 summary 表單
library(readr)
library(fs)
summary_table <- lapply(output_folders, function(folder) {
  csv_files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
  
  lapply(csv_files, function(file) {
    data <- read_csv(file, show_col_types = FALSE)
    data.frame(
      folder_name = basename(folder),
      file_name = basename(file),
      row_count = nrow(data)
    )
  }) %>% bind_rows()
}) %>% bind_rows()

# 顯示 summary
print(summary_table)

# 可選：輸出為 summary 表單
write.csv(summary_table, "summary_table_namecheck.dashboard.csv", row.names = FALSE)
