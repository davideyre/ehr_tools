# 2022.04.14
# Calculate Comorbidity Index (1-year lookback)
# Calculating Charlson and Elixhauser scores using diagnostic codes from 1-year lookback (remove the primary diagnose from current bacteraemia episode)

library(data.table)
library(lubridate)
library(comorbidity)

# Read datasets: InpatientEpisodes (to extract Episode info), InpatientDiagnoses (to extract diagnostic codes)
InpatientEpisodes <- fread(file = "~/Data/Sepsis/IORD_SepsisResponseScore_29_20220106/InpatientEpisodes.csv",
                           select = c("ClusterID", "EpisodeID", "EpisodeStartDate", "EpisodeEndDate", "AdmissionDate"))
InpatientDiagnoses <- fread("~/Data/Sepsis/IORD_SepsisResponseScore_29_20220106/InpatientDiagnoses.csv")

# Extract Episode info and match related DiagCode
InpatientEpisodes <- unique(InpatientEpisodes, by = c("EpisodeID"))
ID_EpisodeDiag <- InpatientDiagnoses[InpatientEpisodes,  on = "EpisodeID"]
ID_EpisodeDiag <- ID_EpisodeDiag[order(ClusterID, EpisodeID, DiagNumber)]

# Extract first record in each bacteraemia episode
ID_FirstInEpisodeDiag <- ID_EpisodeDiag[DiagNumber == 1]
# Define a time windows to look back of the episode prior to the currenr bacteraemia episode
ID_FirstInEpisodeDiag[, `:=`(join_time_start = EpisodeStartDate - years(1), join_time_end = EpisodeStartDate)]
ID_FirstInEpisodeDiag <- ID_FirstInEpisodeDiag[, .(ClusterID, EpisodeID, join_time_start, join_time_end)]
# Out-episode DiagCode: Merge the previous Episode & DiagCode info into first record in each current bacteraemia episode
ID_EpisodeDiag[, join_time := EpisodeStartDate]
ID_OutEpisodeDiag <- ID_EpisodeDiag[ID_FirstInEpisodeDiag,
                                    on = .(ClusterID, 
                                           join_time >= join_time_start, 
                                           join_time < join_time_end)][, c("join_time", "join_time.1") := NULL]
ID_OutEpisodeDiag <- ID_OutEpisodeDiag[!is.na(DiagCode)]

ID_OutEpisodeDiag <- ID_OutEpisodeDiag[, .(ClusterID, i.EpisodeID, DiagNumber, DiagCode)]
setnames(ID_OutEpisodeDiag, old = "i.EpisodeID", new = "EpisodeID")

# In-episode DiagCode: remove the primary DiagCode from current bacteraemia episode
ID_InEpisodeDiag <- ID_EpisodeDiag[, .SD[DiagNumber != 1], by = EpisodeID]
ID_InEpisodeDiag <- ID_InEpisodeDiag[, .(ClusterID, EpisodeID, DiagNumber, DiagCode)]

# The final Episode & DiagCode dataset: Bind datasets containing in-episode DiagCodes and out-episode DiagCodes for each current bacteraemia episode
ID_AllEpisodeDiag <- rbindlist(list(ID_OutEpisodeDiag, ID_InEpisodeDiag))
ID_AllEpisodeDiag <- ID_AllEpisodeDiag[order(ClusterID, EpisodeID)]

# De-duplicate repeated DiagCode for each 1-year-look-back episode
ID_AllEpisodeDiag <- unique(ID_AllEpisodeDiag, by = c("EpisodeID", "DiagCode"))
# Remove any primary diagnosis number with Rxx ICD
ID_AllEpisodeDiag[, DiagCode := fifelse(DiagNumber == 1 & str_detect(DiagCode, 'R'), NA_character_, DiagCode)]
# New numeric id for calculating comorbidity scores
ID_AllEpisodeDiag[, new_id := .GRP, by = EpisodeID]

# Calculate Comorbidity Index
## Charlson
com <- comorbidity(ID_AllEpisodeDiag, id = "new_id", code = "DiagCode", map = "charlson_icd10_quan", assign0 = FALSE)
charlson <- cbind(com, Charlson = score(com, assign0 = FALSE, weights = NULL))
charlson <- as.data.table(charlson)
charlson <- charlson[, .(new_id, Charlson)]
## Elixhauser
com <- comorbidity(ID_AllEpisodeDiag, id = "new_id", code = "DiagCode", map = "elixhauser_icd10_quan", assign0 = FALSE)
elixhauser <- cbind(com, Elixhauser = score(com, assign0 = FALSE, weights = NULL))
elixhauser <- as.data.table(elixhauser)
elixhauser <- elixhauser[, .(new_id, Elixhauser)]

# The final dataset: EpisodeID and Comorbidity scores
EpisodeID_Comorbidity <- charlson[ID_AllEpisodeDiag, on = "new_id"]
EpisodeID_Comorbidity <- elixhauser[EpisodeID_Comorbidity, on = "new_id"]
EpisodeID_Comorbidity <- EpisodeID_Comorbidity[, .(EpisodeID, Charlson, Elixhauser)]

fwrite(EpisodeID_Comorbidity, file = "~/Data/Sepsis/EpisodeID_Comorbidity.csv")