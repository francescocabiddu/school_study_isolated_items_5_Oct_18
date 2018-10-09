# load libraries
libs <- c("magrittr", "tidyverse", "lme4", "sjstats", "TMB")
lapply(libs, require, character.only = TRUE)

# load homemade functions
source("homemade_funs.R")

#### tidy data ####
# remember to set working directory where .csv file is located (session > set working directory > choose directory)
# at the bottom of the script items and pairs datasets will be saved in the chosen directory
df <- "Span Test schools_mixed_version2_13_06_18_5 October 2018_13.05.csv" %>%
  read.delim(sep=",", header=F, stringsAsFactors = F, check.names = F)

# select columns of interest
df %<>%
  rename_at(vars(colnames(df)), ~ df[1,]) %>%
  select(StartDate, id:gender, `list5 i1_1`:`list6 i4_13`)

# drop row 3 containing not useful info
df %<>%
  slice(-3)

# join header with first coloumn
for (i in seq_along(df)) {
  df[1,i] <- paste(df[1:2,i], collapse = "&")
} ; rm(i)  

# set new colnames and delete 1st and 2nd row with repetition of colnames
colnames(df) <- df[1,]
df <- df[-c(1,2),]

# drop test observations
df %<>%
  slice(-1:-12)

# delete empty row (SPECIFIC TO CURRENT DATASET)
df %<>%
  slice(-57)
  
# convert Start date variable in date format
df$`StartDate&Start Date` <- as.Date(df$`StartDate&Start Date`)

# create new variable joining age month with age year, and convert to date format
df %<>%
  unite("age", c("age month&age month", "age year&age year"), sep = " ")

df$age <- as.Date(paste("15", df$age, sep = " "), "%d %m %Y")

# convert age in age in months
df$age_month <-  abs(mondf(df$`StartDate&Start Date`, df$age))

# change wrong dates input (SPECIFIC TO CURRENT DATASET)
df$age_month[57] <- 62
df$age_month[58] <- 67

# change wrong id (SPECIFIC TO CURRENT DATASET)
df$`id&id`[23] <- "DS_C23"

#filter df for new coloums 
df %<>% 
  select(id = `id&id`, age_month, gender = `gender&gender`, 
         `list5 i1_1&seq: 7 2 house door 5 - finish`:`list6 i4_13&seq: 3 water bear 9 school boat - other`)

# id to uppercase
df %<>%
  mutate(id = toupper(id))

# combine all lists coloums in a sigle one (from wide to long format df)
df %<>%
  gather(item, resp, 
         `list5 i1_1&seq: 7 2 house door 5 - finish`:`list6 i4_13&seq: 3 water bear 9 school boat - other`)

# separate item coloum in multiple coloums
df$list <- gsub("^list", "", str_extract(df$item, "^list[56]{1}"))
df$seq_num <- gsub("^i","", str_extract(df$item, "i[0-9]{1,2}"))
df$num <- gsub("^- ", "", str_extract(df$item, "-.*$"))
df$seq <- gsub(" -$", "", gsub("^seq: ", "", str_extract(df$item, "seq:.*-")))
df$len_seq <- sapply(df$seq, function(x) length(str_split(x, " ")[[1]]))

# get rid of variable item and rearrange coloums order
df %<>%
  select(id:gender, list, seq_num, seq, len_seq, num, resp)

# table of number of mixed list recalled
df_mixed_recalled <- df %>%
  (function(x) {
    # table of correct responses as reference
    corr_table <- x %>%
      distinct(seq_num, seq, num) %>%
      filter(num != "other") %>%
      group_by(seq) %>%
      mutate(corr_resp = c(length(seq), 1:(length(seq)-1)))
    
    # check if the participant gave the correct response of each stimulus
    x %<>% 
      inner_join(corr_table, x, by = c("seq_num", "seq", "num")) %>%
      mutate(corr_resp = resp == corr_resp)
    
    # arrange x by id
    x %<>%
      arrange(id)
    
    # create a variable of correct list recall
    x %<>%
      group_by(id, seq) %>%
      mutate(corr_list = ifelse(sum(corr_resp) == length(corr_resp), TRUE, FALSE))
    
    x %>%
      group_by(id, age_month, gender, list, seq_num, seq) %>%
      summarise(recalled_list = ifelse(sum(corr_list) > 0, TRUE, FALSE)) %>%
      group_by(id, age_month, gender) %>%
      summarise(recalled_total = sum(recalled_list))
  })

# arrange df by id
df %<>%
  arrange(id)

# group df
df %<>%
  mutate(seq_num = as.numeric(seq_num)) %>%
  group_by(id, list)

# recall of isolated items and pairs (not considering position)
df_isolated <- df %>% #  
  select(id:len_seq) %>%
  distinct(seq, .keep_all = T)

# extract isolated digits and digit pairs from sequences
for (i in seq_along(df_isolated$seq)) {
  seq <- unlist(str_match_all(df_isolated$seq[i], "[0-9 ]*"))
  seq <- seq[!seq %in% c("", " ")]
  seq <- gsub(" $", "", gsub("^ ", "", seq))
  seq <- str_split(seq, " ")
  if (length(seq[lengths(seq) > 2]) > 0) {
    first_pair <- list(seq[lengths(seq) > 2][[1]][1:2])
    second_pair <- list(seq[lengths(seq) > 2][[1]][2:3])
    
    seq <- c(seq[lengths(seq) <= 2], first_pair, second_pair)
  }
  
  df_isolated$single_d[[i]] <- if (length(seq[lengths(seq) == 1]) == 0) NA else seq[lengths(seq) == 1]
  df_isolated$pair_d[[i]] <- if (length(seq[lengths(seq) == 2]) == 0) NA else seq[lengths(seq) == 2]
} ; rm(seq, first_pair, second_pair, i) 

# extract isolated digits and digit pairs from sequences
df_isolated$single_w <- list(rep(NA, nrow(df_isolated)))
df_isolated$pair_w <- list(rep(NA, nrow(df_isolated)))
for (i in seq_along(df_isolated$seq)) {
  seq <- unlist(str_match_all(df_isolated$seq[i], "[a-z ]*"))
  seq <- seq[!seq %in% c("", " ")]
  seq <- gsub(" $", "", gsub("^ ", "", seq))
  seq <- str_split(seq, " ")
  if (length(seq[lengths(seq) > 2]) > 0) {
    first_pair <- list(seq[lengths(seq) > 2][[1]][1:2])
    second_pair <- list(seq[lengths(seq) > 2][[1]][2:3])
    
    seq <- c(seq[lengths(seq) <= 2], first_pair, second_pair)
  }
  
  df_isolated$single_w[[i]] <- if (length(seq[lengths(seq) == 1]) == 0) NA else seq[lengths(seq) == 1]
  df_isolated$pair_w[[i]] <- if (length(seq[lengths(seq) == 2]) == 0) NA else seq[lengths(seq) == 2]
} ; rm(seq, first_pair, second_pair, i)

# filter out stimuli with a position higher than finish in each sequence
df %<>%
  mutate(resp = as.numeric(resp)) %>%
  group_by(id, list, seq_num) %>%
  filter(resp < resp[1]) 

# assign position (on empty position) to each isolated/pair element in df_isolated
df_isolated %<>%
  mutate(single_d_resp = single_d,
         pair_d_resp = pair_d,
         single_w_resp = single_w,
         pair_w_resp = pair_w)

for (vars in c("single_d_resp", "pair_d_resp", "single_w_resp", "pair_w_resp")) {
  for (i in seq_along(df_isolated[[vars]])) {
    for (j in seq_along(df_isolated[[vars]][[i]])) {
      for (z in seq_along(df_isolated[[vars]][[i]][[j]])) {
        resp <- df$resp[which(df$id == df_isolated$id[i] &
                                            df$list == df_isolated$list[i] &
                                            df$seq == df_isolated$seq[i] &
                                            df$num == df_isolated[[vars]][[i]][[j]][z])]
        if (length(resp) == 0) {
          df_isolated[[vars]][[i]][[j]][z] <- NA
        } else {
          df_isolated[[vars]][[i]][[j]][z] <- resp
        }
      }
    }
  }
} ; rm(vars, i, j, z, resp)

# replace pairs not recalled in the right order with NA
for (vars in c("pair_d_resp", "pair_w_resp")) {
  for (i in seq_along(df_isolated[[vars]])) {
    for (j in seq_along(df_isolated[[vars]][[i]])) {
      check_order <- as.numeric(df_isolated[[vars]][[i]][[j]][1]) - as.numeric(df_isolated[[vars]][[i]][[j]][2])
      
      if (is.na(check_order) | check_order != -1) {
        df_isolated[[vars]][[i]][[j]] <- NA
      }
      
    }
  }
} ; rm(vars, i, j, check_order)

# replace positional values with TRUE and FALSE
for (vars in c("single_d_resp", "pair_d_resp", "single_w_resp", "pair_w_resp")) {
  for (i in seq_along(df_isolated[[vars]])) {
    for (j in seq_along(df_isolated[[vars]][[i]])) {
      stimulus <- df_isolated[[vars]][[i]][[j]]
      
      if (is.na(sum(nchar(stimulus)))) {
        df_isolated[[vars]][[i]][[j]] <- F
      } else {
        df_isolated[[vars]][[i]][[j]] <- T
      }
    }
  }
} ; rm(stimulus, vars, i, j)

# convert to long format to list all items and pairs
df_isolated %<>%
  (function(x) {
    x1 <- x %>%
      select(id:pair_w) %>%
      gather(stimulus_type, isolated_x, c(single_d, pair_d, single_w, pair_w))
    
  
    x2 <- x %>%
      select(id:len_seq, single_d_resp:pair_w_resp) %>%
      gather(stimulus_resp, response, c(single_d_resp, pair_d_resp, single_w_resp, pair_w_resp))
    
    x1 %>%
      cbind(x2 %>% select(stimulus_resp, response))
  })  %>%
  select(-id1, -list1, -stimulus_resp) %>%
  arrange(id, list, seq) %>%
  mutate(response = ifelse(is.na(isolated_x), NA, response)) 

# unlist in different rows elements of df_isolated$isolated_x e df_isolated$response
unlist_rows <- function(df, i) {
  tibble(id = df$id[i],
         age_month = df$age_month[i],
         gender = df$gender[i],
         list = df$list[i],
         seq_num = df$seq_num[i],
         seq = df$seq[i],
         len_seq = df$len_seq[i],
         stimulus_type = df$stimulus_type[i],
         isolated_x = df$isolated_x[[i]] %>% sapply(paste0, collapse = " "),
         response = df$response[[i]] %>% unlist())
} 

df_isolated %<>%
  (function(x) {
    new_df <- unlist_rows(df_isolated, 1)
    for (i in 2:nrow(df_isolated)) {
      new_df %<>%
        rbind(unlist_rows(df_isolated, i))
    }
    
    new_df %>% mutate(isolated_x = ifelse(isolated_x == "NA", NA, isolated_x)) %>%
      na.omit()
  })

# make two df for isolated items and pairs (long format for R analysis)
df_isolated_item <- df_isolated %>% 
  filter(stimulus_type %in% c("single_d", "single_w")) %>%
  mutate(recalled = response %>% as.integer() %>% as.factor()) %>%
  select(-response)

df_isolated_pair <- df_isolated %>% 
  filter(stimulus_type %in% c("pair_d", "pair_w")) %>%
  mutate(recalled = response %>% as.integer() %>% as.factor()) %>%
  select(-response)

#### make dataset in wide format for items and pairs (ready for SPSS) and save them to working directory ####
df_wide <- function(df, stimuli, name_file) {
  # stimuli: a vector with stimulus_type names e.g. c("single_d", "single_w")
  df %>%
    filter(stimulus_type %in% stimuli) %>%
    mutate(i = row_number()) %>%
    spread(stimulus_type, isolated_x) %>%
    arrange(i) %>%
    mutate(recalled = response %>% as.integer() %>% as.factor()) %>%
    select(-i, - response, - len_seq, -seq_num) %>%
    write.table(file = paste(name_file, ".csv", sep = ""), 
                sep = ",", 
                quote = FALSE,
                row.names = FALSE)
}

df_wide(df_isolated, c("single_d", "single_w"), "df_isolated_item")
df_wide(df_isolated, c("pair_d", "pair_w"), "df_isolated_pair")