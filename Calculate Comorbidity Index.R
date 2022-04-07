# 2022.04.05
# Calculate Comorbidity Index (1-year lookback)
# Calculating Charlson and Elixhauser scores using diagnostic codes from 1-year lookback (remove the primary diagnose from current bacteraemia episode)

library(data.table)
library(lubridate)
library(comorbidity) # {Comorbidity} Package version: 1.0.0

InpatientEpisodes <- fread("~/Data/Sepsis/IORD_SepsisResponseScore_29_20220106/InpatientEpisodes.csv")
InpatientDiagnoses <- fread("~/Data/Sepsis/IORD_SepsisResponseScore_29_20220106/InpatientDiagnoses.csv")
cultures <- fread(file = "~/Data/Sepsis/IORD_SepsisResponseScore_29_20220106/Micro.csv", 
                  select = c("ClusterID", "AccessionNumber", "BatTestCode", "CollectionDateTime", "ReceiveDateTime", "BugCode", "BugName"),
                  colClasses=c(ClusterID="character"))

# Extract Episode info and match related DiagCode
ID_EpisodeDate <- InpatientEpisodes[, .(ClusterID, EpisodeID, EpisodeStartDate, EpisodeEndDate, AdmissionDate)]
ID_EpisodeDate <- unique(ID_EpisodeDate, by = c("EpisodeID"))
ID_EpisodeDate <- InpatientDiagnoses[ID_EpisodeDate,  on = "EpisodeID"]
ID_EpisodeDate <- ID_EpisodeDate[order(ClusterID, EpisodeID, DiagNumber)]

# Extract blood culture info
ID_CultureDate <- cultures[, .(ClusterID, CollectionDateTime)]

# Merge blood culture info into Episode & DiagCode dataset
ID_CultureDate[, join_time := CollectionDateTime]
ID_EpisodeDate[, `:=`(join_time_start = EpisodeStartDate - hours(48), join_time_end = EpisodeEndDate)]
ID_CulturedEpisode <- ID_CultureDate[ID_EpisodeDate, 
                                     on = .(ClusterID, 
                                            join_time >= join_time_start, 
                                            join_time <= join_time_end)][, c("join_time", "join_time.1") := NULL]
# De-duplicate rows generated while merging
ID_CulturedEpisode <- unique(ID_CulturedEpisode, by = c("EpisodeID", "DiagNumber"))
ID_CulturedEpisode <- ID_CulturedEpisode[order(ClusterID, EpisodeID, DiagNumber)]

# Keep in-episode DiagCode: only include bacteraemia episodes
ID_InEpisodeDiag <- ID_CulturedEpisode[!is.na(CollectionDateTime)]

# Extract first record in each bacteraemia episode
ID_FirstInEpisodeDiag <- ID_InEpisodeDiag[DiagNumber == 1]
# Define a time windows to look back of the episode prior to the currenr bacteraemia episode
ID_FirstInEpisodeDiag[, `:=`(join_time_start = EpisodeStartDate - years(1), join_time_end = EpisodeStartDate)]
ID_FirstInEpisodeDiag <- ID_FirstInEpisodeDiag[, .(ClusterID, EpisodeID, join_time_start, join_time_end)]
# Out-episode DiagCode: Merge the previous Episode & DiagCode info into first record in each current bacteraemia episode
ID_EpisodeDate[, join_time := EpisodeStartDate][, c("join_time_start", "join_time_end") := NULL]
ID_OutEpisodeDiag <- ID_EpisodeDate[ID_FirstInEpisodeDiag,
                                    on = .(ClusterID, 
                                           join_time >= join_time_start, 
                                           join_time < join_time_end)][, c("join_time", "join_time.1") := NULL]
ID_OutEpisodeDiag <- ID_OutEpisodeDiag[!is.na(DiagCode)]

# The final InEpisodeDiag dataset: remove the primary DiagCode from current bacteraemia episode
ID_InEpisodeDiag <- ID_InEpisodeDiag[, .SD[DiagNumber != 1], by = EpisodeID]
ID_InEpisodeDiag <- ID_InEpisodeDiag[, .(ClusterID, EpisodeID, DiagNumber, DiagCode)]

# The final OutEpisodeDiag dataset: 1 year look back
ID_OutEpisodeDiag <- ID_OutEpisodeDiag[, .(ClusterID, i.EpisodeID, DiagNumber, DiagCode)]
setnames(ID_OutEpisodeDiag, old = "i.EpisodeID", new = "EpisodeID")

# The final Episode & DiagCode dataset: Bind datasets containing in-episode DiagCodes and out-episode DiagCodes for each current bacteraemia episode
ID_AllEpisodeDiag <- rbindlist(list(ID_OutEpisodeDiag, ID_InEpisodeDiag))
ID_AllEpisodeDiag <- ID_AllEpisodeDiag[order(ClusterID, EpisodeID)]

# De-duplicate repeated DiagCode for each 1-year-look-back episode
ID_AllEpisodeDiag <- unique(ID_AllEpisodeDiag, by = c("EpisodeID", "DiagCode"))
# Remove any primary diagnosis number with Rxx ICD
ID_AllEpisodeDiag <- ID_AllEpisodeDiag[!(DiagNumber == 1 & str_detect(DiagCode, 'R'))]

# Calculate Comorbidity Index
## Charlson
com <- comorbidity(ID_AllEpisodeDiag, id = "EpisodeID", code = "DiagCode", map = "charlson_icd10_quan", assign0 = FALSE)
charlson <- cbind(com, Charlson = score(com, assign0 = FALSE, weights = NULL))
charlson <- as.data.table(charlson)
charlson <- charlson[, .(EpisodeID, Charlson)]
## Elixhauser
com <- comorbidity::comorbidity(ID_AllEpisodeDiag, id = "EpisodeID", code = "DiagCode", map = "elixhauser_icd10_quan", assign0 = FALSE)
elixhauser <- cbind(com, Elixhauser = score(com, assign0 = FALSE, weights = NULL))
elixhauser <- as.data.table(elixhauser)
elixhauser <- elixhauser[, .(EpisodeID, Elixhauser)]
