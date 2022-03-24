# To group BugName from blood cultures into several main groups
# Adapted from David's codes

library(tidyverse)

ENTEROBACTERALES = c('Arsenophonus', 'Biostraticola', 'Brenneria', 'Buchnera ', 
                     'Budvicia', 'Buttiauxella', 'Cedecea', 'Chania', 'Citrobacter', 
                     'Cosenzaea', 'Cronobacter', 'Dickeya', 'Edwardsiella', 'Enterobacillus', 
                     'Enterobacter', 'Erwinia', 'Escherichia', 'Ewingella', 'Franconibacter', 
                     'Gibbsiella', 'Hafnia', 'Izhakiella', 'Klebsiella', 'Kluyvera', 
                     'Kosakonia', 'Leclercia', 'Lelliottia', 'Leminorella', 'Limnobaculum', 
                     'Lonsdalea', 'Mangrovibacter', 'Metakosakonia', 'Mixta', 'Moellerella', 
                     'Morganella', 'Obesumbacterium', 'Pantoea', 'Pectobacterium', 'Phaseolibacter', 
                     'Photorhabdus', 'Phytobacter', 'Plesiomonas', 'Pluralibacter', 'Pragia', 
                     'Proteus', 'Providencia', 'Pseudescherichia', 'Pseudocitrobacter', 
                     'Rahnella', 'Raoultella', 'Rosenbergiella', 'Rouxiella', 
                     'Saccharobacter', 'Salmonella', 'Samsonia', 'Scandinavium', 
                     'Serratia', 'Shigella', 'Shimwellia', 'Siccibacter', 'Sodalis', 
                     'Superficieibacter', 'Tatumella', 'Trabulsiella', 'Wigglesworthia', 
                     'Xenorhabdus', 'Yersinia', 'Yokenella', 'Coliforms')

df = read_csv('~/Documents/Sepsis/bugs_pathogen_count.csv') %>% 
        select(BugCode, BugName, count)

df = df %>% 
        separate(BugName, into="Genus", sep=" ", remove=F, extra="drop") %>% 
        mutate(
                BugGroup = case_when(
                        str_detect(BugName, 'STAPHYLOCOCCUS AUREUS') ~ 'S AUREUS',
                        str_detect(BugName, 'COAGULASE NEGATIVE STAPH|CoNS|STAPH') ~ 'CoNS (CONTAMINANT)',
                        str_detect(BugName, 'ESCHERICHIA COLI') ~ 'E COLI',
                        str_detect(BugName, 'PSEUDOMONAS AERUGINOSA') ~ 'P AERUGINOSA',
                        str_detect(BugName, 'KLEBSIELLA') ~ 'KLEBSIELLA',
                        str_detect(BugName, 'ENTEROBACTER') ~ 'ENTEROBACTER',
                        str_detect(BugName, 'STREPTOCOCCUS PNEUMONIAE') ~ 'S PNEUMONIAE',
                        str_detect(BugName, 'PROPIONIBACTERIUM|DIPHTHEROIDS|MICROCOCCUS|^BACILLUS|Bacillus|AEROBIC SPORE BEARER|CORYNEBACTERIUM') ~ 'OTHER CONTAMINANT',
                        str_detect(BugName, 'ENTEROCOCC') ~ 'ENTEROCOCCUS',
                        str_detect(BugName, 'CANDIDA') ~ 'CANDIDA',
                        str_to_title(Genus) %in% ENTEROBACTERALES ~ 'OTHER ENTEROBACTERALES',
                        str_detect(BugName, '^BACTEROIDES|ANAEROBIC STREPTOCOCCI|PEPTOSTREPTOCOCCUS|CLOSTRIDIUM|^ANAEROB|PARVIMONAS|PREVOTELLA|VEILLONELLA|PEPTONIPHILUS|FUSOBACTERIUM') ~ 'ANAEROBES',
                        str_detect(BugName, 
                                   'STREPTOCOCCUS AGALACTIAE|STREPTOCOCCUS DYSGALACTIAE|GROUP A STREP|STREPTOCOCCUS PYOGENES|BETA HAEMOLYTIC GROUP|STREPTOCOCCUS SERO GROUP C|STREPTOCOCCUS EQUI|BETA HAEMOLYTIC STREPTOCOCCI|GROUP B STREPTOCOCCUS|STREPTOCOCCUS SERO GROUP G') ~ 'BETA HAEMOLYTIC STREP',
                        str_detect(BugName,'STREP') ~ 'OTHER STREPS',
                        #str_detect(BugName, 'STENOTROPHOMONAS|ACINETOBACTER|HAEMOPHILUS|NEISSERIA|MORAXELLA|PSEUDOMONAS') ~ 'OTHER GRAM NEGATIVE',
                        str_detect(BugName, 'NULL') ~ 'NULL',
                        TRUE ~ 'OTHER'
                )
        ) 

df %>% 
        group_by(BugGroup) %>% 
        summarise(n=sum(count))

df %>% 
        filter(BugGroup=="OTHER") %>% 
        group_by(Genus) %>% 
        summarise(n=sum(count)) %>% 
        arrange(-n) %>% 
        print(n=500)

write_csv(df %>% select(-Genus), '~/Documents/Sepsis/bug_group_table.csv')