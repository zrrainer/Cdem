satisfaction_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="not at all satisfied" ~ 1,
      {{col}}=="not very satisfied" ~ 2,
      {{col}}=="fairly satisfied" ~ 3,
      {{col}}=="very satisfied" ~ 4,
      {{col}}=="don't know/ prefer not to answer" ~ NA))
  
  return(df)
}


agreement_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="strongly disagree" ~ 1,
      {{col}}=="somewhat disagree" ~ 2,
      {{col}}=="neither agree nor disagree" ~ 3,
      {{col}}=="somewhat agree" ~ 4,
      {{col}}=="strongly agree" ~ 5,
      {{col}}=="don't know/ prefer not to answer" ~ NA))
  
  return(df)
}

news_con_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="none" ~ 1,
      {{col}}=="1-10 minutes" ~ 2,
      {{col}}=="11-30 minutes" ~ 3,
      {{col}}=="31-60 minutes" ~ 4,
      {{col}}=="between 1 and 2 hours" ~ 5,
      {{col}}=="more than 2 hours" ~ 6,
      {{col}}=="don't know/ prefer not to answer" ~ NA))
  
  return(df)
}


volunteer_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="never" ~ 1,
      {{col}}=="just once" ~ 2,
      {{col}}=="a few times" ~ 3,
      {{col}}=="more than five times" ~ 4,
      {{col}}=="don't know/ prefer not to answer" ~ NA))
  
  return(df)
}

voting_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="certain not to vote" ~ 1,
      {{col}}=="unlikely to vote" ~ 2,
      {{col}}=="likely to vote" ~ 3,
      {{col}}=="certain to vote" ~ 4,
      {{col}}=="i already voted (by mail, advance poll, etc.)" ~ 5,
      {{col}}=="don't know/ prefer not to answer" ~ NA,
      {{col}}=="i am not eligible to vote" ~ NA))
  
  return(df)
}


education_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="no schooling" ~ 1,
      {{col}}=="some elementary school" ~ 2,
      {{col}}=="completed elementary school" ~ 3,
      {{col}}=="some secondary/ high school" ~ 4,
      {{col}}=="completed secondary/ high school" ~ 5,
      {{col}}=="some technical, community college, cegep, college classique" ~ 6,
      {{col}}=="completed technical, community college, cegep, college classique" ~ 7,
      {{col}}=="some university" ~ 8,
      {{col}}=="bachelor's degree" ~ 9,
      {{col}}=="master's degree" ~ 10,
      {{col}}=="professional degree or doctorate" ~ 11,
      {{col}}=="don't know/ prefer not to answer" ~ NA
      ))
  return(df)
}

interest_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := as.numeric({{col}})) |>
    mutate({{col}} := case_when(
      {{col}}==-99 ~ NA,
      TRUE ~ {{col}}
    ))
  
  return(df)
}


conf_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="none at all" ~ 1,
      {{col}}=="not very much" ~ 2,
      {{col}}=="quite a lot" ~ 3,
      {{col}}=="a great deal" ~ 4,
      {{col}}=="don't know/ prefer not to answer" ~ NA
    ))
  return(df)
}


follow_pol_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="not at all" ~ 1,
      {{col}}=="not very closely" ~ 2,
      {{col}}=="fairly closely" ~ 3,
      {{col}}=="very closely" ~ 4,
      {{col}}=="don't know/ prefer not to answer" ~ NA
    ))
  return(df)
}

lived_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="less than 1 year" ~ 1,
      {{col}}=="1-3 years" ~ 2,
      {{col}}=="3-10 years" ~ 3,
      {{col}}=="more than 10 years" ~ 4,
      {{col}}=="don't know/ prefer not to answer" ~ NA
    ))
  return(df)
}

gap_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="much less" ~ 1,
      {{col}}=="somewhat less" ~ 2,
      {{col}}=="about the same as now" ~ 3,
      {{col}}=="somewhat more" ~ 4,
      {{col}}=="much more" ~ 5,
      {{col}}=="don't know/ prefer not to answer" ~ NA
    ))
  return(df)
}

inequal_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="definitely not" ~ 1,
      {{col}}=="probably not" ~ 2,
      {{col}}=="not sure" ~ 3,
      {{col}}=="probably yes" ~ 4,
      {{col}}=="definitely yes" ~ 5,
      {{col}}=="don't know/ prefer not to answer" ~ NA
    ))
  return(df)
}

abort_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="no" ~ 1,
      {{col}}=="in some circumstances" ~ 2,
      {{col}}=="yes" ~ 3,
      {{col}}=="don't know/ prefer not to answer" ~ NA
    ))
  return(df)
}

#govgen name: Mary Simon
govgen_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="mary simon" ~ 1,
      TRUE ~ 0
    ))
  return(df)
}


#finmin name: Chrystia Freeland
finmin_to_numeric <- function(df, col){
  df <- df %>%
    mutate({{col}} := case_when(
      {{col}}=="chrystia freeland" ~ 1,
      TRUE ~ 0
    ))
  return(df)
}


premier_to_numeric <- function(df, province, premier){
  df <- df %>%
    mutate({{premier}} := case_when(
      ({{province}}=="british columbia" & {{premier}}=="john horgan") ~ 1,
      ({{province}}=="alberta" & {{premier}}=="jason kenney") ~ 1,
      ({{province}}=="saskatchewan" & {{premier}}=="scott moe") ~ 1,
      ({{province}}=="yukon" & {{premier}}=="sandy silver") ~ 1,
      ({{province}}=="northwest territories" & {{premier}}=="caroline cochrane") ~ 1,
      ({{province}}=="nunavut" & {{premier}}=="joe savikataaq") ~ 1,
      ({{province}}=="manitoba" & {{premier}}=="brian pallister") ~ 1,
      ({{province}}=="ontario" & {{premier}}=="doug ford") ~ 1,
      ({{province}}=="quebec" & {{premier}}=="françois legault") ~ 1,
      ({{province}}=="new brunswick" & {{premier}}=="blaine higgs") ~ 1,
      ({{province}}=="nova scotia" & {{premier}}=="tim houston") ~ 1,
      ({{province}}=="prince edward island" & {{premier}}=="dennis king") ~ 1,
      ({{province}}=="newfoundland and labrador" & {{premier}}=="andrew furey") ~ 1,
      TRUE ~ 0
    ))
  
  return(df)
}