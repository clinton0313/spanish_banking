################################################################################
################################################################################
# File: ###################  Event Study regressions  #######################
################################################################################
################################################################################

# Author: Clinton Leung and David Ampudia Vicente
# Last Update: 03-12-22

################################################################################
#### Package and directory Control Panel ####
################################################################################

# Clean out the workspace
rm(list = ls())
memory.limit(72000)
options(max.print=1000)

# Check installation & LOAD PACKAGES 
list.of.packages <- c("tidyverse", "ggplot2", "data.table", "fixest", "broom",
                      "fastDummies", "RColorBrewer", "did", "here", "patchwork", "multcomp",
                      "MatchIt", "optmatch", "estimatr", "fields")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")

figures <- "empirical/output/figures/"
tables <- "empirical/output/tables/"
datasets <- "data/"
figs <- list()

getwd()

select <- dplyr::select
summarise <- dplyr::summarise
summarize <- dplyr::summarize
################################################################################
#### Load and define data ####
################################################################################
main_df = read.csv(paste(datasets, "lagged_municipalities_w_cost.csv", sep = "")) %>%
  select(-c(ID_REL, COD_INE, COD_GEO, ipl, full_sample))

main_df <- main_df %>%
  group_by(full_mun) %>%
  ungroup %>%
  mutate(
    commercial_bank = ifelse(num_commercial_banks>0,1,0),
    savings_bank = ifelse(num_savings_banks>0,1,0),
    rel_year = year - 1962,
    comm_lic_1_p1000c = (comm_lic_1 / pops)*1000,
    comm_lic_2_p1000c = (comm_lic_2 / pops)*1000,
    comm_lic_3_p1000c = (comm_lic_3 / pops)*1000,
    comm_lic_4_p1000c = (comm_lic_4 / pops)*1000,
    comm_lic_5_p1000c = (comm_lic_5 / pops)*1000,
    comm_lic_6_p1000c = (comm_lic_6 / pops)*1000,
    comm_lic_7_p1000c = (comm_lic_7 / pops)*1000,
    comm_lic_8_p1000c = (comm_lic_8 / pops)*1000,
    comm_lic_9_p1000c = (comm_lic_9 / pops)*1000,
    comm_lic_total_p1000c = (comm_lic_total / pops)*1000,
    trucks_p1000c = (trucks / pops)*1000,
    telephones_p1000c = (telephones / pops)*1000,
    cultural_show_tax_p1000c = (cultural_show_tax / pops)*1000,
    mun_budget_p1000c = (mun_budget / pops) *1000,
    num_commercial_banks_p1000c = (num_commercial_banks / pops)*1000,
    polo_mun = ifelse(dist_polo < 10, 1, 0),
    bank = ifelse(bank ==  "", "Neither", bank),
    correspondents = ifelse(correspondents>0,1,0),
  )

unbanked <- main_df %>% filter(year==1964) %>% 
  group_by(full_mun) %>% 
  summarize(bnk_mean = mean(commercial_bank)) %>%
  filter(bnk_mean==0) %>% select(full_mun) %>% pull(full_mun)

corresponds <- main_df %>% filter((year==1964)&(category=="A")) %>% 
  group_by(full_mun) %>%
  filter(correspondents >0) %>% select(full_mun) %>% pull(full_mun)

no_corresponds <- main_df %>% filter((year==1964)&(category=="A")) %>% 
  group_by(full_mun) %>%
  filter(correspondents == 0) %>% select(full_mun) %>% pull(full_mun)

far_sea <- main_df %>% filter((year==1964)&(category=="A")) %>% 
  group_by(full_mun) %>%
  filter(dist_sea>25) %>% select(full_mun) %>% pull(full_mun)

close_sea <- main_df %>% filter((year==1964)&(category=="A")) %>% 
  group_by(full_mun) %>%
  filter(dist_sea<25) %>% select(full_mun) %>% pull(full_mun)

far_sea <- main_df %>% filter((year==1964)&(category=="A")) %>% 
  group_by(full_mun) %>%
  filter(dist_sea>25) %>% select(full_mun) %>% pull(full_mun)

main_df <- main_df %>% mutate(rel_year = as.factor(rel_year),
                              num_id = as.numeric(as.factor(full_mun)),
                              province = as.factor(province))

#### Event Study Panel ####

# Define treatment years for plans
main_df <- main_df %>% mutate(treat_year = case_when(
  plan_group == 1 ~ 1965,
  plan_group == 2 ~ 1966,
  plan_group == 3 ~ 1967,
  plan_group == 4 ~ 1968,
  plan_group == 5 ~ 1969,
  plan_group == 6 ~ 1970,
  plan_group == 7 ~ 1971,
  plan_group == 0 ~ NaN
))

# If bank requests it
main_df <- main_df %>% mutate(true_treat_year = case_when(
  (plan_group == 1) & (bank=="Requested")~ 1965,
  (plan_group == 2) & (bank=="Requested")~ 1966,
  (plan_group == 3) & (bank=="Requested")~ 1967,
  (plan_group == 4) & (bank=="Requested")~ 1968,
  (plan_group == 5) & (bank=="Requested")~ 1969,
  (plan_group == 6) & (bank=="Requested")~ 1970,
  (plan_group == 7) & (bank=="Requested")~ 1971,
  plan_group == 0 ~ NaN
))

# Create variable with relative year to treatment
main_df <- main_df %>% mutate(rel_treat = year - treat_year,
                              true_rel_treat = year - true_treat_year)

# Create dummy dataset
main_df_dumm <- main_df %>% 
  dummy_cols(select_columns = "rel_treat", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_treat_"), ~replace_na(., 0))) %>% 
  dummy_cols(select_columns = "true_rel_treat", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("true_rel_treat_"), ~replace_na(., 0)))


# List rel years to treatment 
yrs <- sort(unique(main_df_dumm$rel_treat))
yrs <- yrs[which(yrs != min(yrs) & yrs != -1)] # Ignore year -1 to use as baseline

################################################################################
#### Category Subsets ####
################################################################################
sub.samples <- list(
  all = main_df_dumm,
  A = main_df_dumm %>% filter(category!="B" & category!="C" & category!="D"),
  subA = main_df_dumm %>% filter(category!="B" & category!="C" & category!="D" & full_mun %in% unbanked),
  corresponds = main_df_dumm %>% filter(category=="None" | full_mun %in% corresponds),
  nocorresponds = main_df_dumm %>% filter(category=="None" | full_mun %in% no_corresponds),
  closesea = main_df_dumm %>% filter(category=="None" | full_mun %in% close_sea),
  farsea = main_df_dumm %>% filter(category=="None" | full_mun %in% far_sea),
  B = main_df_dumm %>% filter(category!="A" & category!="C" & category!="D"),
  D = main_df_dumm %>% filter(category!="A" & category!="B" & category!="C"),
  BD = main_df_dumm %>% filter(category!="A" & category!="C")
)


# m <- sub.samples$A %>% filter(category=="A" & treat_year==1968) %>% select(year, pops, full_mun, comm_lic_total_p1000c)
# 
# m <- m %>% group_by(full_mun) %>% mutate(diff = comm_lic_total_p1000c - lag(comm_lic_total_p1000c)) %>% filter(year==1972)

################################################################################
#### Event Study Regressions ####
################################################################################
lagsleads <- c(-5:-2, 0:9)
regs <- c(paste0("`", "rel_treat_", lagsleads, "`"))
true_regs <- c(paste0("`", "true_rel_treat_", lagsleads, "`"))

### Covariates ###
covs <- list()
covs[["base0"]] = c("1","1")

covs[["base1"]] = c("log(pops)",
                    "pop_growth")
covs[["base2"]] = c(covs$base1, 
                    "longitude:rel_year", 
                    "latitude:rel_year", 
                    "log(altitude):rel_year", 
                    "log(dist_sea):rel_year")

covs[["base3"]] = c(covs$base2,
                    "tourism_idx:rel_year",
                    "dist_polo:rel_year",
                    "savings_bank",
                    "correspondents:rel_year",
                    "log(1 + mun_budget_p1000c)",
                    "log(1 + cultural_show_tax_p1000c)",
                    "lag_triangular_20_num_commercial_banks_p1000c",
                    "commercial_cost",
                    "savings_cost"
                    )

covs[["base4"]] = c(covs$base2,
                    "tourism_idx:rel_year",
                    "dist_polo:rel_year",
                    "savings_bank",
                    "correspondents:rel_year",
                    "log(1 + mun_budget_p1000c)",
                    "log(1 + cultural_show_tax_p1000c)",
                    "lag_gaussian_20_num_commercial_banks_p1000c",
                    "commercial_cost",
                    "savings_cost"
)


covs[["interact"]] <- c(
  "savings_bank:rel_year", 
  "log(pops):rel_year",
  "pop_growth:rel_year",
  "tourism_idx:rel_year",
  "dist_polo:rel_year",
  "correspondents:rel_year",
  "init_dist_bank:rel_year",
  "log(pops):rel_year",
  "longitude:rel_year",
  "latitude:rel_year",
  "log(altitude):rel_year",
  "log(dist_sea):rel_year",
  "mun_budget_p1000c:rel_year",
  "cultural_show_tax_p1000c:rel_year",
  "commercial_cost:rel_year",
  "savings_cost:rel_year"
)

FE1 <- c("rel_year","full_mun")
FE2 <- c("rel_year^province", "full_mun")

form.list <- list()
results_EXT <- list()
results_INT <- list()

first_base0 <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(regs, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))

first_base1 <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(regs, collapse = " + "),
         " + ", paste0(covs$base1, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))


first_base2 <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(regs, collapse = " + "),
         " + ", paste0(covs$base2, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))


first_full_tria <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(regs, collapse = " + "),
         " + ", paste0(covs$base3, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))


first_full_gaus <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(regs, collapse = " + "),
         " + ", paste0(covs$base3, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))

first_interact <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(regs, collapse = " + "),
         " + ", paste0(covs$interact, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))

treat_full <- as.formula(
  paste0("commercial_bank ~ ",
         paste0(regs, collapse = " + "),
         " + ", paste0(covs$base3, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))


did_base <- feols(comm_lic_total_p1000c ~ treatment | rel_year^province +  full_mun, 
                  cluster = "full_mun", data = sub.samples[["A"]])

did_full <- feols(comm_lic_total_p1000c ~ treatment + log(pops) + pop_growth + 
                    longitude:rel_year + latitude:rel_year + log(altitude):rel_year + 
                    log(dist_sea):rel_year + tourism_idx:rel_year + dist_polo:rel_year + 
                    savings_bank + correspondents:rel_year + init_dist_bank:rel_year + 
                    log(1 + mun_budget_p1000c) + log(1 + cultural_show_tax_p1000c) + 
                    commercial_cost + savings_cost | rel_year^province + full_mun,
                  cluster = "full_mun", data = sub.samples[["A"]])

did_treat <- feols(commercial_bank ~ treatment + log(pops) + pop_growth + 
                     longitude:rel_year + latitude:rel_year + log(altitude):rel_year + 
                     log(dist_sea):rel_year + tourism_idx:rel_year + dist_polo:rel_year + 
                     savings_bank + correspondents:rel_year + init_dist_bank:rel_year + 
                     log(1 + mun_budget_p1000c) + log(1 + cultural_show_tax_p1000c) +
                     commercial_cost + savings_cost | rel_year^province + full_mun,
                   cluster = "full_mun", data = sub.samples[["A"]])


m_base <- feols(first_base0, cluster = "full_mun", data = sub.samples[["A"]])
m_base1 <- feols(first_base1, cluster = "full_mun", data = sub.samples[["A"]])
m_base2 <- feols(first_base2, cluster = "full_mun", data = sub.samples[["A"]])
m_full_tria <- feols(first_full_tria, cluster = "full_mun", data = sub.samples[["A"]])
m_full_gaus <- feols(first_full_gaus, cluster = "full_mun", data = sub.samples[["A"]])
m_interact <- feols(first_interact, cluster = "full_mun", data = sub.samples[["A"]])
m_treat <- feols(treat_full, cluster = "full_mun", data = sub.samples[["A"]])

m_closesea <- feols(first_full_gaus, cluster = "full_mun", data = sub.samples[["closesea"]])
m_distsea <- feols(first_full_gaus, cluster = "full_mun", data = sub.samples[["farsea"]])
m_corres <- feols(first_full_gaus, cluster = "full_mun", data = sub.samples[["corresponds"]])
m_nocorres <- feols(first_full_gaus, cluster = "full_mun", data = sub.samples[["nocorresponds"]])

#################################### # Succesfully treated

true_first_base <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(true_regs, collapse = " + "),
         # " + ", paste0(covs$base3, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))

true_m_base <- feols(true_first_base, cluster = "full_mun", data = sub.samples[["A"]])


true_first_full <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(true_regs, collapse = " + "),
         " + ", paste0(covs$base4, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))

true_m_full <- feols(true_first_full, cluster = "full_mun", data = sub.samples[["A"]])

true_treat_full <- as.formula(
  paste0("commercial_bank ~ ",
         paste0(true_regs, collapse = " + "),
         " + ", paste0(covs$base4, collapse = " + "),
         "|", paste0(FE2, collapse= " + ")))

true_m_treat <- feols(true_treat_full, cluster = "full_mun", data = sub.samples[["A"]])

####################################
plotES <- function(model, ylims, yvar) {
  ES <- broom::tidy(model, conf.int = TRUE) %>%
    dplyr::filter(grepl("rel_treat",term)) %>%
    dplyr::filter(!(grepl("rel_treat_9|`rel_treat_-6`", term))) %>%
    mutate(t = c(-5:-2, 0:8)) %>%
    bind_rows(tibble(t = -1, estimate = 0, conf.low = 0, conf.high = 0)) %>%
    mutate(group = as.factor(case_when(
      t < 0 ~ 1,
      t >= 0 ~ 2
    ))) %>% 
    # plot
    ggplot(aes(x = t, y = estimate)) + 
    geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
    geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
    # geom_vline(xintercept = -.5,  linetype = "solid", size=20, color = "gray", alpha=.4) + 
    geom_rect(aes(xmin=-.8, xmax=-.2, ymin=-Inf, ymax=Inf), fill = "gray", alpha = .1) +
    # geom_text()
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  linetype = "solid", show.legend = FALSE, 
                  color="darkgray", width=.1, size=1) + 
    geom_point(fill = "black", shape = 21, size=2) + geom_line(size=1) + 
    ggtitle("") + 
    ylim(ylims) +
    labs(y = yvar, x = "Years Relative to Plan Award") + 
    scale_x_continuous(breaks = seq(-4, 8, by = 2)) + 
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title=element_text(size=8))
  
  return(ES)
}

figs$ES1 <- plotES(m_base, ylims=c(-.6,2.2), yvar='Commercial licenses, 1000 inhab.')
figs$ES2 <- plotES(m_full_tria, ylims=c(-.6,2), yvar='Commercial licenses, 1000 inhab.')
figs$ES3 <- plotES(m_full_gaus, ylims=c(-.6,2), yvar='Commercial licenses, 1000 inhab.')
figs$ES4 <- plotES(m_interact, ylims=c(-.6,1.5), yvar='Commercial licenses, 1000 inhab.')
figs$ES3 <- plotES(m_treat, ylims=c(-.1,1), yvar='Commercial Bank branch')

figs$ES5 <- plotES(m_corres, ylims=c(-.6,1.5), yvar='Commercial licenses, 1000 inhab.')
figs$ES6 <- plotES(m_nocorres, ylims=c(-.8,2.5), yvar='Commercial licenses, 1000 inhab.')

figs$ES7 <- plotES(m_closesea, ylims=c(-1,3), yvar='Commercial licenses, 1000 inhab.')
figs$ES8 <- plotES(m_distsea, ylims=c(-.8,1.5), yvar='Commercial licenses, 1000 inhab.')


################################################################################
#### Heterogeneity: License Regressions ####
################################################################################
license_types <- 1:8
results <- c()
tmp_objects <- list()

labels <- list(
  comm_lic_1_p1000c = "Cat. 1: Foodstuff licenses",
  comm_lic_2_p1000c = "Cat. 2: Textile licenses",
  comm_lic_3_p1000c = "Cat. 3: Woodwork licenses",
  comm_lic_4_p1000c = "Cat. 4: Leatherwork licenses",
  comm_lic_5_p1000c = "Cat. 5: Chemical licenses",
  comm_lic_6_p1000c = "Cat. 6: Construction licenses",
  comm_lic_7_p1000c = "Cat. 7: Steelwork licenses",
  comm_lic_8_p1000c = "Cat. 8: Electricity licenses"
)

plotES_lic <- function(model, depvar) {
  ES <- broom::tidy(model, conf.int = TRUE) %>%
    dplyr::filter(grepl("rel_treat",term)) %>%
    mutate(t = lagsleads) %>%
    bind_rows(tibble(t = -1, estimate = 0, conf.low = 0, conf.high = 0)) %>%
    mutate(group = as.factor(case_when(
      t < 0 ~ 1,
      t >= 0 ~ 2
    ))) %>% 
    # plot
    ggplot(aes(x = t, y = estimate)) + 
    geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
    geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
    geom_rect(aes(xmin=-.8, xmax=-.2, ymin=-Inf, ymax=Inf), fill = "gray", alpha = .1) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  linetype = "solid", show.legend = FALSE, 
                  color="darkgray", width=.1, size=1) + 
    geom_point(fill = "black", shape = 21, size=2) + geom_line(size=1) + 
    ggtitle(labels[[depvar]]) + 
    labs(x = "Years Relative to Plan Award") + 
    scale_x_continuous(breaks = seq(-4, 8, by = 2), limits =c(-5.1, 8.1)) +
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0.5))
  
  return(ES)
}

for (spec in license_types) {
  tmp_a <- paste("comm_lic_", spec, "_p1000c", sep="")
  tmp_ <- paste("comm_lic_", spec, "_p1000c ~ ", sep="")
  
  tmp_base0 <- as.formula(
    paste0(tmp_,
           paste0(regs, collapse = " + "),
           "|", paste0(FE2, collapse= " + ")))
  
  tmp_full <- as.formula(
    paste0(tmp_,
           paste0(regs, collapse = " + "),
           " + ", paste0(covs$base4, collapse = " + "),
           "|", paste0(FE2, collapse= " + ")))
  
  tmp_lab0 <- paste("cl_", spec, "_base", sep="")
  tmp_labfull <- paste("cl_", spec, "_full", sep="")
  
  results[[tmp_lab0]] <- try(feols(tmp_base0, cluster = "full_mun", data = sub.samples[["A"]]))
  results[[tmp_labfull]] <- try(feols(tmp_full, cluster = "full_mun", data = sub.samples[["A"]]))
  tmp_objects[[tmp_lab0]] <- plotES_lic(results[[tmp_lab0]], tmp_a)  
  tmp_objects[[tmp_labfull]] <- plotES_lic(results[[tmp_labfull]], tmp_a)  
}
tmp_objects$heterog_base1 <- tmp_objects[["cl_1_base"]] + tmp_objects[["cl_2_base"]] 
tmp_objects$heterog_base3 <- tmp_objects[["cl_3_base"]] + tmp_objects[["cl_4_base"]] 
tmp_objects$heterog_base5 <- tmp_objects[["cl_5_base"]] + tmp_objects[["cl_6_base"]] 
tmp_objects$heterog_base7 <- tmp_objects[["cl_7_base"]] + tmp_objects[["cl_8_base"]]

tmp_objects$heterog_full1 <- tmp_objects[["cl_1_full"]] + tmp_objects[["cl_2_full"]] 
tmp_objects$heterog_full3 <- tmp_objects[["cl_3_full"]] + tmp_objects[["cl_4_full"]] 
tmp_objects$heterog_full5 <- tmp_objects[["cl_5_full"]] + tmp_objects[["cl_6_full"]] 
tmp_objects$heterog_full7 <- tmp_objects[["cl_7_full"]] + tmp_objects[["cl_8_full"]]

figs$heterog_base <- tmp_objects$heterog_base1 / tmp_objects$heterog_base3 / tmp_objects$heterog_base5 / tmp_objects$heterog_base7
figs$heterog_full <- tmp_objects$heterog_full1 / tmp_objects$heterog_full3 / tmp_objects$heterog_full5 / tmp_objects$heterog_base7

figs$heterog_base
figs$heterog_full


################################################################################
#### Callaway and Sant'Anna CATT ####
################################################################################
data_cs <- sub.samples$A %>% 
  mutate(treat_year = replace_na(treat_year, 0)) %>%
  filter_at(vars(commercial_cost, lag_gaussian_20_num_commercial_banks_p1000c, savings_cost), all_vars(!is.na(.)))
  as_tibble()

# run
out_base0 <- att_gt(yname = "comm_lic_total_p1000c",
                    data = data_cs,
                    tname = "year",
                    idname = "num_id",
                    gname = "treat_year",
                    control_group = "nevertreated",
                    est_method = "reg",
                    print_details = FALSE,
                    bstrap = T,
                    cband = T,
                    clustervars = "num_id",
                    panel=T,
                    biters=1000
)

out_base1 <- att_gt(yname = "comm_lic_total_p1000c",
                    data = data_cs,
                    tname = "year",
                    idname = "num_id",
                    gname = "treat_year",
                    xformla = ~ log(pops) +
                      pop_growth,
                    control_group = "nevertreated",
                    est_method = "reg",
                    print_details = FALSE,
                    bstrap = T,
                    cband = T,
                    clustervars = "num_id",
                    panel=T,
                    biters=1000
)

out_full <- att_gt(yname = "comm_lic_total_p1000c",
                   data = data_cs,
                   tname = "year",
                   idname = "num_id",
                   gname = "treat_year",
                   xformla = ~ log(pops) +
                     pop_growth +
                     longitude +
                     latitude +
                     correspondents +
                     log(altitude) +
                     log(dist_sea) +
                     tourism_idx +
                     commercial_cost +
                     savings_cost +
                     lag_gaussian_20_num_commercial_banks_p1000c +
                     log(1 + mun_budget_p1000c) +
                     log(1 + cultural_show_tax_p1000c)
                   ,
                   control_group = "nevertreated",
                   est_method = "reg",
                   print_details = FALSE,
                   bstrap = T,
                   cband = T,
                   clustervars = "num_id",
                   panel=T,
                   biters=1000)

m_CS_base0 <- aggte(out_base0, type="dynamic",min_e = -5, max_e = 8)
m_CS_base <- aggte(out_base1, type="dynamic",min_e = -5, max_e = 8)
m_CS_full <- aggte(out_full, type="dynamic",min_e = -5, max_e = 8)

# function to produce CS coef plots
plotCS <- function(CS_object) {
  
  es1 <- aggte(CS_object, type="dynamic",min_e = -5, max_e = 8)
  
  source(here::here(dirname(rstudioapi::getSourceEditorContext()$path), "aggte_anyF.R"))
  es1_pre <- aggte_anyF(CS_object, type = "dynamic", min_e = -5, max_e = 9, min_e2 = -5, max_e2 = -1)
  
  pre_att <- format(round(es1_pre$overall.att, 3), nsmall = 3)
  pre_p <- format(round(pnorm(abs(es1_pre$overall.att/es1_pre$overall.se), lower.tail = FALSE)*2, 3), nsmall = 3)
  
  post_att <- format(round(es1$overall.att, 3), nsmall = 3)
  post_p <- format(round(pnorm(abs(es1$overall.att/es1$overall.se), lower.tail = FALSE)*2, 3), nsmall = 3)
  
  # make the text that goes into the plot
  text_pre <- bquote(widehat(delta^'Pre') ==.(pre_att)~"; "~p^'Pre'==.(pre_p))
  text_post <- bquote(widehat(delta^'Post') ==.(post_att)~"; "~p^'Post'==.(post_p))
  
  ES_CS_plot <- tibble(
    t = es1$egt,
    estimate = es1$att.egt,
    se = es1$se.egt,
    conf.low = estimate - 1.96*se,
    conf.high = estimate + 1.96*se
  ) %>% 
    filter(!t == 9) %>%
    # make two different periods for the connection
    mutate(group = as.factor(case_when(
      t < 0 ~ 1,
      t >= 0 ~ 2
    ))) %>% 
    # plot
    ggplot(aes(x = t, y = estimate)) + 
    geom_rect(aes(xmin=-.8, xmax=-.2, ymin=-Inf, ymax=Inf), fill = "gray", alpha = .1) +
    geom_point(fill = "black", shape = 21, size=2) + geom_line(size=1) + 
    ggtitle("") + 
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group=group, color = group),
                  linetype = "solid", show.legend = FALSE, 
                  color="darkgray", width=.1, size=1) +
    scale_color_manual(values = c("#4B5F6C", "#A7473A")) + 
    geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
    geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
    labs(y = "Commercial Licenses, 1000 inhab.", x = "Years Relative to Plan Award") + 
    scale_x_continuous(breaks = seq(-4, 8, by = 2)) + 
    scale_y_continuous(breaks = seq(0, 2, by = 1)) + 
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title=element_text(size=8))
  
  return(ES_CS_plot)
}

figs$CS1 <- plotCS(out_base1)
figs$CS2 <- plotCS(out_full)

################################################################################
#### Visual Event Study Plots ####
################################################################################
group_estimates <- tibble(
  g = out_base1$group,
  t = out_base1$t,
  estimate = out_base1$att,
  se = out_base1$se,
  conf.low = estimate - 1.96*se,
  conf.high = estimate + 1.96*se) %>%
  mutate(period = as.factor(case_when(
    t < g ~ "Pre",
    t >= g ~ "Post"
  )))

treat_counts <- sub.samples$A %>%
  distinct(full_mun, .keep_all=TRUE) %>%
  group_by(treat_year) %>%
  count()

figs$group <- list() 
for (i in unique(group_estimates$g)) {
  yr_count <- treat_counts %>% filter(treat_year == i) %>% pull(n)
  figs$group[[as.character(i)]] <- group_estimates %>% filter(g==i) %>%
    ggplot(aes(x = t, y = estimate)) + 
    geom_point(fill = "black", shape = 21, size=2) + geom_line(size=1) + 
    ggtitle(paste("Group ", as.character(i), " (n = ", yr_count, ")", sep = "")) + 
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, group=period, color = period),
                  linetype = "solid", show.legend = FALSE, 
                  color="darkgray", width=.1, size=1) +
    scale_color_manual(values = c("#4B5F6C", "#A7473A")) + 
    geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") +
    geom_rect(aes(xmin=-Inf, xmax=g, ymin=-Inf, ymax=Inf), fill = "gray", alpha = .01) +
    geom_rect(aes(xmin=g, xmax=Inf, ymin=-Inf, ymax=Inf), fill = "darkorange", alpha = .01) +
    labs(y = "Comm. Lic.", x = "Years Relative to Plan Award") + 
    theme_classic() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title=element_text(size=8),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.x=element_blank())
}

figs$group_att <- figs$group$"1965" / figs$group$"1966" / figs$group$"1967" / figs$group$"1968" / figs$group$"1969" / figs$group$"1970" / figs$group$"1971" 

################################################################################
#### Baseline Matching ####
################################################################################
library(MatchIt)

A_1964 <- sub.samples$A %>% filter(year == 1964) %>% mutate(will_treat = if_else(category=="A", 1, 0))

vars <- c("surface", "pops", "telephones_p1000c", "trucks_p1000c", "cultural_show_tax_p1000c", "mun_budget_p1000c",
          "num_commercial_banks", "num_savings_banks", "comm_lic_1_p1000c", "comm_lic_2_p1000c", "comm_lic_3_p1000c",
          "comm_lic_4_p1000c", "comm_lic_5_p1000c", "comm_lic_6_p1000c", "comm_lic_7_p1000c", "comm_lic_8_p1000c",
          "comm_lic_9_p1000c", "comm_lic_total_p1000c", "tourism_idx", "longitude", "latitude", "altitude", "dist_sea")

# Baseline means and p_value for test H0: equal means (true diff =0)
means_tab <- A_1964 %>% 
  group_by(category) %>% 
  select(one_of(vars)) %>% 
  summarise_all(funs(mean(., na.rm=T))) %>%
  gather(variable, value, -category) %>%
  spread(category, value) %>%
  rowwise() %>%
  mutate(p_value = round(t.test(unlist(A_1964[, variable]) ~ unlist(A_1964[, 'category']))$p.value, 2)) %>% 
  rename(prevar = variable, preA = A, preNone=None, prep_value=p_value)

# Parameters to match on
obs <- c("pops", "longitude", "latitude", "altitude", "dist_sea", "tourism_idx", 
         "comm_lic_total_p1000c", "num_commercial_banks", "num_savings_banks", "mun_budget_p1000c", "cultural_show_tax_p1000c")

match_fml <- as.formula(paste0("will_treat ~ ", paste0(obs, collapse= " + ")))
mod_match <- matchit(match_fml,
                     method = "optimal", data = A_1964, mahvars="pops")

# Obtain list of municipalities that were matched
mtch_mun <- match.data(mod_match) %>% distinct(full_mun) %>% pull()
dta_m <- match.data(mod_match)
dim(dta_m)

# Compare means
match_tab <- dta_m %>% 
  group_by(category) %>% 
  select(one_of(vars)) %>% 
  summarise_all(funs(mean(., na.rm=T))) %>%
  gather(variable, value, -category) %>%
  spread(category, value) %>%
  rowwise() %>%
  mutate(p_value =round(t.test(unlist(dta_m[, variable]) ~ unlist(dta_m[, 'category']))$p.value, 2)) %>%
  rename(postvar = variable, postA = A, postNone=None, postp_value=p_value)

#### ES using matched data
match_data <- sub.samples$A %>% filter(full_mun %in% mtch_mun)

m_match <- feols(first_full_gaus, cluster = "full_mun", data = match_data)
figs$match <- plotES(m_full_gaus, ylims=c(-0.7,1.7), yvar='Commercial licenses, 1000 inhab.')



################################################################################
#### Stack and Matching ####
################################################################################
make_match_dt_full <- function(tyr) {
  tmp_dt <- sub.samples$A %>% 
    # keep firms in the adopt year, never treateds, or treated after tyr + 5
    filter(treat_year == tyr | treat_year > tyr + 3 | is.na(treat_year)) %>% 
    # keep just years t - 5  to t + 5
    filter(year %>% between(tyr - 5, tyr + 9)) %>% 
    # replace adopt year to NA to make dummies
    mutate(rel_treat = if_else(treat_year == tyr, rel_treat, NA_real_)) %>% 
    select(full_mun, year, treatment, rel_treat, province, comm_lic_total_p1000c, correspondents, pops, pop_growth, savings_bank, longitude, latitude, rel_year, altitude, dist_sea, tourism_idx, dist_polo, init_dist_bank, mun_budget_p1000c, cultural_show_tax_p1000c) %>% 
    mutate(dt = as.character(tyr))
  
  samp <- tmp_dt %>% mutate(will_treat = lead(if_else((treatment==1 & year==tyr), 1, 0))) %>% filter(year == tyr - 1)
  
  mod_match <- matchit(will_treat ~ pops + savings_bank + altitude + latitude + dist_sea + tourism_idx + mun_budget_p1000c,
                       method = "optimal", data = samp)
  
  mtch_mun <- match.data(mod_match) %>% distinct(full_mun) %>% pull()
  match_data <- tmp_dt %>% filter(full_mun %in% mtch_mun)
  
  return(match_data)
}

stack_data_match <- map_dfr(treats, make_match_dt_full) %>%
  dummy_cols(select_columns = "rel_treat", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_treat_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(full_mun, "_", dt))

stack_regs <- c(paste0("`", "rel_treat_", c(-5:-2, 0:9), "`"))

mod_stack_match <- as.formula(
  paste0("comm_lic_total_p1000c ~ ",
         paste0(stack_regs, collapse = " + "),
         " + ", paste0(covs$base3, collapse = " + "),
         "|", paste0(list("province^dt", "rel_year^dt", "full_mun^dt"), collapse= " + ")))

m_stack_match <- feols(mod_stack_match, cluster = "cluster", data = stack_data_match)
summary(m_stack_match)
plotES(m_stack_match, ylims=c(-1.5,2.5), yvar='Commercial licenses, 1000 inhab.')

################################################################################
#### Permutation Tests ####
################################################################################
set.seed(42)

muns <- sub.samples$A %>% distinct(full_mun)
treat_counts <- sub.samples$A %>% distinct(full_mun, .keep_all=T) %>%
  group_by(treat_year) %>%
  count %>%
  ungroup %>%
  mutate(total = sum(n), prob = n / total)

lagsleads <- c(-5:-2, 0:9)
regs <- c(paste0("`", "rel_treat_", lagsleads, "`"))

iter_list <- list()
for (i in seq(1:1000)) {
  muns$assign <- sample(treat_counts$treat_year,size = nrow(muns),prob = treat_counts$prob, replace = T)
  tmp <- sub.samples$A %>%
    left_join(., muns) %>%
    select(-(starts_with("rel_treat"))) %>%
    mutate(rel_treat = year - assign) %>%
    dummy_cols(select_columns = "rel_treat", remove_selected_columns = FALSE,
               ignore_na = TRUE) %>%
    mutate(across(starts_with("rel_treat_"), ~replace_na(., 0)))
  
  tmp_full <- as.formula(
    paste0("comm_lic_total_p1000c ~ ",
           paste0(regs, collapse = " + "),
           " + ", paste0(covs$base3, collapse = " + "),
           "|", paste0(FE2, collapse= " + ")))
  
  m_tmp_full <- feols(tmp_full, cluster = "full_mun", data = tmp)
  summary(m_tmp_full)
  tmp_res <- broom::tidy(m_tmp_full) %>% filter(grepl("rel_treat_", term))
  tmp_res$i <- i
  iter_list[[i]] <- tmp_res
}
simul_results <- do.call(rbind, iter_list)
# ################################################################################

actual_res <- broom::tidy(m_full_tria, conf.int = TRUE) %>%
  dplyr::filter(grepl("rel_treat",term)) %>%
  mutate(t = lagsleads) %>%
  filter(!t==9) %>%
  bind_rows(tibble(term = "`rel_treat_-1`", t = -1, estimate = 0, conf.low = 0, conf.high = 0)) %>%
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  )))

figs$simul_plot <- simul_results %>%
  group_by(i) %>%
  mutate(t = lagsleads) %>%
  filter(!t==9) %>%
  group_modify(~ add_row(.x,term = "`rel_treat_-1`",t = -1, estimate = 0, .before=0)) %>%
  ungroup %>%
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>%
  # plot
  ggplot() +
  geom_hline(yintercept = 0,  linetype = "longdash", color = "black") +
  geom_vline(xintercept = 0,  linetype = "longdash", color = "black") +
  geom_line(aes(x = t, y = estimate, group = i), color = "gray", alpha=.1) +
  geom_line(data = actual_res, aes(x = t, y = estimate), color = "black") +
  # geom_vline(xintercept = -.5,  linetype = "solid", size=20, color = "gray", alpha=.4) +
  geom_rect(aes(xmin=-.8, xmax=-.2, ymin=-Inf, ymax=Inf), fill = "gray", alpha = .01) +
  # geom_text()
  geom_errorbar(data = actual_res, aes(x = t, y = estimate, ymin = conf.low, ymax = conf.high),
                linetype = "solid", show.legend = FALSE,
                color="darkgray", width=.1, size=1) +
  geom_point(data = actual_res, aes(x = t, y = estimate), fill = "black", shape = 21, size=2) +
  labs(y = "Commercial Licenses, 1000 inhab.", x = "Years Relative to Plan Award") +
  scale_x_continuous(breaks = seq(-4, 8, by = 2)) +
  theme_classic() +
  theme(
    axis.title=element_text(size=8))

# Table
actual_res <- actual_res %>% rename(actual_estimate = estimate) %>% select(term, actual_estimate)

simul_table <- simul_results %>%
  group_by(i) %>%
  left_join(., actual_res) %>%
  dplyr::filter(grepl("rel_treat_[0-9]",term)) %>%
  mutate(eff_size = if_else(estimate - actual_estimate > 0, 1, 0)) %>%
  group_by(term) %>%
  summarize(mean_larg_eff = mean(eff_size)) %>%
  ungroup %>%
  spread(term, mean_larg_eff)

################################################################################
################################################################################
################################################################################

m5_full <- feols(comm_lic_total_p1000c ~ `rel_treat_-5` + `rel_treat_-4` +
                   `rel_treat_-3` + `rel_treat_-2` + rel_treat_0 +
                   rel_treat_1 + rel_treat_2 + rel_treat_3 + rel_treat_4 + rel_treat_5 +
                   rel_treat_6 + rel_treat_7 + rel_treat_8 + rel_treat_9 + log(pops) +
                   pop_growth + longitude*rel_year + latitude*rel_year + log(altitude)*rel_year +
                   log(dist_sea)*rel_year + tourism_idx + dist_polo + savings_bank + init_dist_bank*rel_year +
                   commercial_cost + 
                   savings_cost +
                   lag_gaussian_20_num_commercial_banks_p1000c +
                   log(1 + mun_budget_p1000c) + log(1 + cultural_show_tax_p1000c) |
                   rel_year^province + full_mun,
                 conley(5, distance = "spherical"),
                 data = sub.samples[["A"]])

m10_full <- feols(comm_lic_total_p1000c ~ `rel_treat_-5` + `rel_treat_-4` +
                    `rel_treat_-3` + `rel_treat_-2` + rel_treat_0 +
                    rel_treat_1 + rel_treat_2 + rel_treat_3 + rel_treat_4 + rel_treat_5 +
                    rel_treat_6 + rel_treat_7 + rel_treat_8 + rel_treat_9 + log(pops) +
                    pop_growth + longitude*rel_year + latitude*rel_year + log(altitude)*rel_year +
                    log(dist_sea)*rel_year + tourism_idx + dist_polo + savings_bank + init_dist_bank*rel_year +
                    commercial_cost + 
                    savings_cost +
                    lag_gaussian_20_num_commercial_banks_p1000c +
                    log(1 + mun_budget_p1000c) + log(1 + cultural_show_tax_p1000c) |
                    rel_year^province + full_mun,
                  conley(10, distance = "spherical"),
                  data = sub.samples[["A"]])

m25_full <- feols(comm_lic_total_p1000c ~ `rel_treat_-5` + `rel_treat_-4` +
                    `rel_treat_-3` + `rel_treat_-2` + rel_treat_0 +
                    rel_treat_1 + rel_treat_2 + rel_treat_3 + rel_treat_4 + rel_treat_5 +
                    rel_treat_6 + rel_treat_7 + rel_treat_8 + rel_treat_9 + log(pops) +
                    pop_growth + longitude*rel_year + latitude*rel_year + log(altitude)*rel_year +
                    log(dist_sea)*rel_year + tourism_idx + dist_polo + savings_bank + init_dist_bank*rel_year +
                    commercial_cost + 
                    savings_cost +
                    lag_gaussian_20_num_commercial_banks_p1000c +
                    log(1 + mun_budget_p1000c) + log(1 + cultural_show_tax_p1000c) |
                    rel_year^province + full_mun,
                  conley(25, distance = "spherical"),
                  data = sub.samples[["A"]])

m50_full <- feols(comm_lic_total_p1000c ~ `rel_treat_-5` + `rel_treat_-4` +
                    `rel_treat_-3` + `rel_treat_-2` + rel_treat_0 +
                    rel_treat_1 + rel_treat_2 + rel_treat_3 + rel_treat_4 + rel_treat_5 +
                    rel_treat_6 + rel_treat_7 + rel_treat_8 + rel_treat_9 + log(pops) +
                    pop_growth + longitude*rel_year + latitude*rel_year + log(altitude)*rel_year +
                    log(dist_sea)*rel_year + tourism_idx + dist_polo + savings_bank + init_dist_bank*rel_year +
                    commercial_cost + 
                    savings_cost +
                    lag_gaussian_20_num_commercial_banks_p1000c +
                    log(1 + mun_budget_p1000c) + log(1 + cultural_show_tax_p1000c) |
                    rel_year^province + full_mun,
                  conley(50, distance = "spherical"),
                  data = sub.samples[["A"]])

m100_full <- feols(comm_lic_total_p1000c ~ `rel_treat_-5` + `rel_treat_-4` +
                     `rel_treat_-3` + `rel_treat_-2` + rel_treat_0 +
                     rel_treat_1 + rel_treat_2 + rel_treat_3 + rel_treat_4 + rel_treat_5 +
                     rel_treat_6 + rel_treat_7 + rel_treat_8 + rel_treat_9 + log(pops) +
                     pop_growth + longitude*rel_year + latitude*rel_year + log(altitude)*rel_year +
                     log(dist_sea)*rel_year + tourism_idx + dist_polo + savings_bank + init_dist_bank*rel_year +
                     commercial_cost + 
                     savings_cost +
                     lag_gaussian_20_num_commercial_banks_p1000c +
                     log(1 + mun_budget_p1000c) + log(1 + cultural_show_tax_p1000c) |
                     rel_year^province + full_mun,
                   conley(100, distance = "spherical"),
                   data = sub.samples[["A"]])


m200_full <- feols(comm_lic_total_p1000c ~ `rel_treat_-5` + `rel_treat_-4` +
                     `rel_treat_-3` + `rel_treat_-2` + rel_treat_0 +
                     rel_treat_1 + rel_treat_2 + rel_treat_3 + rel_treat_4 + rel_treat_5 +
                     rel_treat_6 + rel_treat_7 + rel_treat_8 + rel_treat_9 + log(pops) +
                     pop_growth + longitude*rel_year + latitude*rel_year + log(altitude)*rel_year +
                     log(dist_sea)*rel_year + tourism_idx + dist_polo + savings_bank + init_dist_bank*rel_year +
                     commercial_cost + 
                     savings_cost +
                     lag_gaussian_20_num_commercial_banks_p1000c +
                     log(1 + mun_budget_p1000c) + log(1 + cultural_show_tax_p1000c) |
                     rel_year^province + full_mun,
                   conley(200, distance = "spherical"),
                   data = sub.samples[["A"]])


############################

library(car)
library(foreign)
library(ggrepel)
library(fields)
library(santoku)
library(spdep)

study_file <- sub.samples$A %>% dplyr::rename(X = longitude, Y=latitude, id=num_id) %>% mutate(id = as.factor(id))
Residuals <- m_base$residuals
study_file$residuals <- Residuals

n_years=length(unique(study_file$year))
n_id=length(unique(study_file$id))
set_years=sort(unique(study_file$year))[-c(1,2)]
set_id=sort(unique(study_file$id))

morans_z_list <- c()
morans_p_list <- c()

for (i in set_years) {
  study_file_yr=study_file %>% filter(year==i) %>%  select(X,Y,residuals)%>% na.omit() 
  locs=study_file_yr %>% select(X,Y) %>% as.matrix()
  study_file_spatial=SpatialPoints(coords=locs)
  proj4string(study_file_spatial)=CRS("+proj=longlat +datum=WGS84")
  nearest=knn2nb(knearneigh(study_file_spatial,k=20,longlat = T)) 
  nearest=nb2listw(nearest,style="W")
  moran_z=as.vector(moran.test(study_file_yr$residuals,listw=nearest, alternative='two.sided')$statistic)
  moran_p=as.vector(moran.test(study_file_yr$residuals,listw=nearest, alternative='two.sided')$p.value)
  morans_z_list <- c(morans_z_list, moran_z)
  morans_p_list <- c(morans_p_list, moran_p)
}

median(morans_z_list)
median(morans_p_list)

################################################################################
################################################################################
################################# TABLES #######################################
################################################################################
################################################################################
options(knitr.kable.NA = '')
library(kableExtra)

# function to get significance stars
make_stars <- function(t, dof) {
  if (2 * pt(-t, df=dof) < 0.01) {
    ptstar <- "***"
  } else if (abs(2 * pt(-t, df=dof)) < 0.05) {
    ptstar <- "**"
  } else if (abs(2 * pt(-t, df=dof)) < 0.1) {
    ptstar <- "*"
  } else {
    ptstar <- ""
  }
  return(ptstar)
}

# function to get info from models
get_DID_info <- function(est, modelname, type, variable) {
  bind_cols(
    broom::tidy(est) %>% 
      filter(term == variable) %>% 
      select(estimate, std.error, statistic),
    broom::glance(est) %>% 
      select(nobs, adj.r.squared, within.r.squared) %>%
      mutate(mod = modelname, type = type),
    num_id = length(unique(est$fixef_id$full_mun)),
    mdv = mean(est$fitted.values + est$residuals)
  )
}

get_ES_info <- function(est, modelname, type){
  bind_cols(
    broom::tidy(est) %>%
      dplyr::filter(grepl("rel_treat",term)) %>%
      mutate(t = lagsleads),
    broom::glance(est) %>% 
      select(nobs, adj.r.squared, within.r.squared) %>%
      mutate(mod = modelname, type = type),
    num_id = length(unique(est$fixef_id$full_mun)),
    mdv = mean(est$fitted.values + est$residuals)
  )[1:13,] %>%
    add_row(estimate = 0, std.error = 0, term = "`rel_treat_-1`", statistic = 0, t = -1, type=type, mod = modelname, .before = 5)
}

################################################################################
#### TABLE 1 ####
################################################################################

T1 <- list()

T1$DID <- bind_rows(
  get_DID_info(did_base, "base0", "DID_B", "treatment"),
  get_DID_info(did_full, "full", "DID_F", "treatment"),
  get_DID_info(did_treat, "treat", "DID_T", "treatment")
) %>% 
  rowwise() %>% 
  mutate(estimate = paste0(as.character(format(round(estimate, 2), nsmall = 2)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  select(estimate, std.error, mod) %>%
  gather(variable, value, -mod) %>%
  spread(mod, value) %>%
  add_row(.before = 1) %>%
  mutate(names = c("\\textit{Panel A: Difference-In-Differences Model}", "Awarded in Plan", "")) %>%
  select(names, treat, base0, full)

T1$ES <- bind_rows(
  get_ES_info(m_base, "base0", "ES_B"),
  get_ES_info(m_full_tria, "full", "ES_F"),
  get_ES_info(m_treat, "treat", "ES_T")
) %>% 
  rowwise() %>% 
  mutate(estimate = paste0(as.character(format(round(estimate, 2), nsmall = 2)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  select(estimate, std.error, mod, t) %>%
  gather(variable, value, -c(mod,t)) %>%
  spread(mod, value) %>%
  add_row(.before = 1) %>%
  mutate(names = c("\\textit{Panel B: Event Study Model}", 
                   "Year -5", "", "Year -4", "","Year -3", "","Year -2", "","Year -1 (Omitted)", "",
                   "Year 0", "","Year 1", "","Year 2", "","Year 3", "","Year 4", "",
                   "Year 5", "","Year 6", "","Year 7", "","Year 8", "")) %>%
  select(names, treat, base0, full)

T1$ES[10,2] <- ""
T1$ES[10,3] <- ""
T1$ES[10,4] <- ""
T1$ES[11,2] <- ""
T1$ES[11,3] <- ""
T1$ES[11,4] <- ""

T1$bottoms <- bind_rows(
  get_ES_info(m_base, "base0", "ES_B"),
  get_ES_info(m_full_tria, "full", "ES_F"),
  get_ES_info(m_treat, "treat", "ES_T")
) %>%
  mutate(nobs = scales::comma_format()(nobs),
         num_id = scales::comma_format()(num_id),
         mdv = as.character(format(round(mdv, 2), nsmall = 2))
  ) %>%
  # pivot longer and then wider
  select(nobs, num_id, mdv, mod, t) %>%
  gather(variable, value, -c(mod,t)) %>%
  spread(mod, value) %>%
  filter(t==-5) %>%
  mutate(names = c("Mean dependent variable", "Number of observations", "Number of municipalities")) %>%
  select(names, treat, base0, full)

Tab1 <- bind_rows(
  T1$DID,
  T1$ES,
  tribble(
    ~names, ~treat, ~base0, ~full,
    "Demographic Controls", "Yes", "No", "Yes",
    "Spatial Controls",  "Yes", "No", "Yes",
    "Economic Controls", "Yes", "No", "Yes",
    "Municipality FE",  "Yes", "Yes", "Yes",
    "Year x Province FE",  "Yes","Yes","Yes"
  ),
  T1$bottoms
) %>%
  # Table
  kable("latex", align = 'lccc', booktabs = T, linesep = c(""),
        col.names = NULL,
        escape=F,
        label = "table4", 
        caption = "The effect of bank branch deregulation on local commercial activity") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position", "scale_down")) %>%
  add_header_above(c(" ", "(1)" =1, "(2)"=1, "(3)"=1)) %>%
  add_header_above(c("\\\\textit{Dependent Variable:}" = 1, "New Bank Branch" = 1, "Commercial Licenses, per 1000 inhab." = 2), escape=F) %>%
  row_spec(3, extra_latex_after = "\\linesep") %>%
  row_spec(32, extra_latex_after = "\\midrule") %>%
  row_spec(35, extra_latex_after = "\\midrule") %>%
  row_spec(37, extra_latex_after = "\\midrule") %>% 
  footnote(
    general_title = "",
    general = c(
      "\\\\footnotesize \\\\textit{Note:} The table shows the author's estimation of equation (3). Panel A 
      displays the original difference-in-differences estimates for the post-expansion period. Panel B shows the event study coefficients,
      which report the lead and lag effects of the Banking Banking Expansion Plans on the number of commercial licenses, 1000 inhab, 
      used by the author as a proxy measure for entrepreneurship and commercial engagement. See the text for a detailed description of the control variables.   \\\\\\\\
      ***, **, and *
      indicate significance at the 0.01, 0.05 and 0.10 levels, respectively, using two-tailed tests. Robust standard errors are clustered
      at the municipality level."
    ),
    threeparttable = T ,
    footnote_as_chunk=T,
    escape = F
  )

write_lines(Tab1, file = paste(tables, "table1.tex", sep = ""))

################################################################################
#### Table 2 ####
################################################################################

T2 <- list()

T2$CSatt <- bind_rows(bind_cols(t="", 
                                 estimate=m_CS_base$overall.att,
                                 std.error=m_CS_base$overall.se,
                                 type="base"),
                       bind_cols(t="",
                                 estimate=m_CS_full$overall.att,
                                 std.error=m_CS_full$overall.se,
                                 type="full")) %>%
  mutate(statistic = estimate/std.error) %>%
  rowwise() %>% 
  mutate(estimate = paste0(as.character(format(round(estimate, 2), nsmall = 2)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  select(t, estimate, std.error, type) %>%
  gather(estimate, value, -c(type, t)) %>%
  spread(type, value) %>%
  add_row(.before = 1) %>%
  mutate(names = c("\\textit{Panel A: Overall ToT}", "Awarded in Plan", ""),
         sunab = c("","",""),
         stack = c("","","")) %>%
  select(names, base, full)




T2$CSegt <- bind_rows(bind_cols(t=m_CS_base$egt,
                                 estimate=m_CS_base$att.egt,
                                 std.error=m_CS_base$se.egt,
                                 type="base"),
                       bind_cols(t=m_CS_full$egt,
                                 estimate=m_CS_full$att.egt,
                                 std.error=m_CS_full$se.egt,
                                 type="full")) %>%
  mutate(statistic = estimate/std.error) %>%
  rowwise() %>% 
  mutate(estimate = paste0(as.character(format(round(estimate, 2), nsmall = 2)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  select(t, estimate, std.error, type) %>%
  gather(estimate, value, -c(type, t)) %>%
  spread(type, value) %>%
  add_row(.before = 1) %>%
  mutate(names = c("\\textit{Panel B: Event Study}", "Year -5", "", "Year -4", "","Year -3", "","Year -2", "","Year -1", "",
                   "Year 0", "","Year 1", "","Year 2", "","Year 3", "","Year 4", "",
                   "Year 5", "","Year 6", "","Year 7", "","Year 8", "")) %>%
  select(names, base, full)

T2$AS <- AS_data_full[1:13,] %>%
  mutate(statistic = estimate/std.error,
         type = "sunab") %>%
  add_row(estimate = 0, std.error = 0, statistic = 0, t = -1, type="sunab", .before = 5) %>%
  rowwise() %>% 
  mutate(estimate = paste0(as.character(format(round(estimate, 2), nsmall = 2)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  select(t, estimate, std.error, type) %>%
  gather(estimate, value, -c(type, t)) %>%
  spread(type, value) %>%
  add_row(.before = 1) %>%
  mutate(names = c("", "Year -5", "", "Year -4", "","Year -3", "","Year -2", "","Year -1", "",
                   "Year 0", "","Year 1", "","Year 2", "","Year 3", "","Year 4", "",
                   "Year 5", "","Year 6", "","Year 7", "","Year 8", "")) %>%
  select(sunab)

T2$AS[10,1] <- ""
T2$AS[11,1] <- ""

T2$SR <- get_ES_info(m_stack_full, "full", "stack") %>% 
  rowwise() %>% 
  mutate(estimate = paste0(as.character(format(round(estimate, 2), nsmall = 2)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  select(estimate, std.error, type, t) %>%
  gather(estimate, value, -c(type,t)) %>%
  spread(type, value) %>%
  add_row(.before = 1) %>%
  mutate(names = c("", "Year -5", "", "Year -4", "","Year -3", "","Year -2", "","Year -1", "",
                   "Year 0", "","Year 1", "","Year 2", "","Year 3", "","Year 4", "",
                   "Year 5", "","Year 6", "","Year 7", "","Year 8", "")) %>%
  select(stack)

T2$SR[10,1] <- ""
T2$SR[11,1] <- ""

T2$altES <- bind_cols(T2$CSegt, "sunab"=T2$AS, "stack"=T2$SR)

Tab2 <- bind_rows(
  T2$CSatt,
  T2$altES,
  tribble(
    ~names, ~base, ~full, ~sunab, ~stack,
    # "Demographic Controls", "Yes", "Yes", "Yes", "Yes",
    # "Spatial Controls",  "No", "Yes", "Yes", "Yes",
    # "Economic Controls", "No", "Yes", "Yes", "Yes",
    "Controls", "No", "Yes", "Yes", "Yes",
    "Municipality FE",  "Yes", "Yes", "Yes", "No",
    "Year x Province FE",  "Yes","Yes","Yes", "No",
    "Stack x Municipality FE", "No","No","No", "Yes",
    "Stack x Year FE", "No","No","No", "Yes",
    "Stack x Province FE", "No","No","No", "Yes"
  )
) %>%
  # Table
  kable("latex", align = 'lcccc', booktabs = T, linesep = c(""),
        col.names = NULL,
        escape=F,
        label = "apptable3", 
        caption = "Alternative event study estimators") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position")) %>%
  add_header_above(c(" "=1, "Callaway & Sant'Anna" =2, "Sun & Abraham"=1, "Cengiz"=1)) %>%
  add_header_above(c("\\\\textit{Dependent Variable:}" = 1, "Commercial Licenses, per 1000 inhab." = 4), escape=F) %>%
  row_spec(32, extra_latex_after = "\\midrule") %>%
  row_spec(33, extra_latex_after = "\\midrule") %>%
  footnote(
    general_title = "",
    general = c(
      "\\\\footnotesize \\\\textit{Note:} The table shows the author's estimation of equation (3) using a series of alternative methods proposed
      in recent years to improve identification and inference of treatment effects in the context of heterogeneous outcomes and staggered designs. 
      \\\\citep{CS20} use a series of group-time ATT estimates weighted by group-specific propensity scores, a generalization of \\\\citep{AB2005}. 
      \\\\citep{SA20} instead estimate the dynamic effect for each treatment cohort, which are then weighted by their sample share and aggregated.
      \\\\citep{CEN19} create group-specific datasets and discard soon-to-be-treated observations. The specification is identical to (3), but now fixed effects
      are saturated with indicators for the specific stacked datasets. \\\\\\\\
      ***, **, and *
      indicate significance at the 0.01, 0.05 and 0.10 levels, respectively, using two-tailed tests."
    ),
    threeparttable = T ,
    footnote_as_chunk=T,
    escape = F
  )

write_lines(Tab2, file = paste(tables, "table2.tex", sep = ""))

################################################################################
#### Appendix Table Permutation ####
################################################################################
Tab5 <- simul_table %>%
  mutate_at(vars(everything()), ~paste0(as.character(format(round(., 2), nsmall = 2)))) %>%
  mutate(names = "Percent more than baseline") %>%
  select(names, everything(), -rel_treat_9) %>%
  # Table
  kable("latex", align = 'lcccccccc', booktabs = T, linesep = c(""),
        col.names = NULL,
        escape=F,
        label = "apptablepermut", 
        caption = "p-values of permutation tests on the baseline event study model") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position")) %>%
  add_header_above(c(" "=1, "t=0" =1,"t=1" =1,"t=2" =1,"t=3" =1,"t=4" =1,"t=5" =1,"t=6" =1,"t=7" =1,"t=8" =1)) %>%
  footnote(
    general_title = "",
    general = c(
      "\\\\footnotesize \\\\textit{Note:} Estimates include demographic, spatial and economic covariates as well as time-trended province fixed effects
      and municipality fixed effects. The table shows the proportion of times the estimates from the permutation tests are more significant than the
      baseline estimate, over 1,000 simulations."
    ),
    threeparttable = T ,
    footnote_as_chunk=T,
    escape = F
  )

write_lines(Tab5, file = paste(tables, "permut.tex", sep = ""))

################################################################################
#### Appendix Table Conley ####
################################################################################

TabConley <- bind_rows(
  get_ES_info(m5_full, "full", "5"),
  get_ES_info(m10_full, "full", "10"),
  get_ES_info(m25_full, "full", "25"),
  get_ES_info(m50_full, "full", "50"),
  get_ES_info(m100_full, "full", "100"),
  get_ES_info(m200_full, "full", "200")
) %>% 
  rowwise() %>% 
  mutate(estimate = paste0(as.character(format(round(estimate, 2), nsmall = 2)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  select(estimate, std.error, type, t) %>%
  gather(variable, value, -c(type,t)) %>%
  spread(type, value) %>%
  mutate(names = c("Year -5", "", "Year -4", "","Year -3", "","Year -2", "","Year -1 (Omitted)", "",
                   "Year 0", "","Year 1", "","Year 2", "","Year 3", "","Year 4", "",
                   "Year 5", "","Year 6", "","Year 7", "","Year 8", "")) %>%
  select(names, "5", "10", "25", "50", "100", "200")

TabConley[9,2] <- ""
TabConley[9,3] <- ""
TabConley[9,4] <- ""
TabConley[9,5] <- ""
TabConley[9,6] <- ""
TabConley[9,7] <- ""
TabConley[10,2] <- ""
TabConley[10,3] <- ""
TabConley[10,4] <- ""
TabConley[10,5] <- ""
TabConley[10,6] <- ""
TabConley[10,7] <- ""

TabConley <- TabConley %>%
  # Table
  kable("latex", align = 'lcccccc', booktabs = T, linesep = c(""),
        col.names = NULL,
        escape=F,
        label = "appConley", 
        caption = "The effects of banking deregulation account for spatial dependence") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position")) %>%
  add_header_above(c("Cut-off Value"=1, "5 km." =1,  "10 km." =1,  "25 km." =1,  "50 km." =1,  "100 km." =1,  "200 km." =1)) %>%
  add_header_above(c("Dependent Variable"=1, "Commercial Licenses, 1000 inhab." = 6)) #%>%
# row_spec(13, extra_latex_after = "\\midrule")

write_lines(TabConley, file = paste(tables, "app_conley.tex", sep = ""))

################################################################################
#### Appendix Table Heterog ####
################################################################################

ATHet <- list()

ATHet$ES <- bind_rows(
  get_ES_info(m_corres, "full", "corres"),
  get_ES_info(m_nocorres, "full", "nocorres"),
  get_ES_info(m_closesea, "full", "coastal"),
  get_ES_info(m_distsea, "full", "inland")) %>% 
  rowwise() %>% 
  mutate(estimate = paste0(as.character(format(round(estimate, 2), nsmall = 2)), make_stars(statistic, 10000)),
         std.error = paste0("(", as.character(format(round(std.error, 2), nsmall = 2)), ")")) %>%
  select(estimate, std.error, type, t) %>%
  gather(variable, value, -c(type,t)) %>%
  spread(type, value) %>%
  mutate(names = c("Year -5", "", "Year -4", "","Year -3", "","Year -2", "","Year -1 (Omitted)", "",
                   "Year 0", "","Year 1", "","Year 2", "","Year 3", "","Year 4", "",
                   "Year 5", "","Year 6", "","Year 7", "","Year 8", "")) %>%
  select(names, corres, nocorres, coastal, inland)

ATHet$ES[9,2] <- ""
ATHet$ES[9,3] <- ""
ATHet$ES[9,4] <- ""
ATHet$ES[9,5] <- ""
ATHet$ES[10,2] <- ""
ATHet$ES[10,3] <- ""
ATHet$ES[10,4] <- ""
ATHet$ES[10,5] <- ""

ATHet$bottoms <- bind_rows(
  get_ES_info(m_corres, "full", "corres"),
  get_ES_info(m_nocorres, "full", "nocorres"),
  get_ES_info(m_closesea, "full", "coastal"),
  get_ES_info(m_distsea, "full", "inland")) %>% 
  mutate(nobs = scales::comma_format()(nobs),
         num_id = scales::comma_format()(num_id),
         mdv = as.character(format(round(mdv, 2), nsmall = 2))
  ) %>%
  # pivot longer and then wider
  select(nobs, num_id, mdv, type, t) %>%
  gather(variable, value, -c(type,t)) %>%
  spread(type, value) %>%
  filter(t==-5) %>%
  mutate(names = c("Mean dependent variable", "Number of observations", "Number of municipalities")) %>%
  select(names, corres, nocorres, coastal, inland)

ATabHet <- bind_rows(
  ATHet$ES,
  tribble(
    ~names, ~corres, ~nocorres, ~coastal, ~inland,
    
    "Controls", "Yes", "Yes", "Yes", "Yes",
    "Fixed Effects",  "Yes","Yes","Yes","Yes"
  ),
  ATHet$bottoms
) %>%
  # Table
  kable("latex", align = 'lcccc', booktabs = T, linesep = c(""),
        col.names = NULL,
        escape=F,
        label = "apptabhet", 
        caption = "Heterogeneous effects of banking branch deregulation") %>% 
  kable_styling(position = "center", latex_options = c("HOLD_position")) %>%
  add_header_above(c(" ", "Presence" =1, "Absence"=1, "Coastal"=1, "Inland"=1)) %>%
  add_header_above(c(" ", "Correspondents" =2, "Distance to Sea"=2)) %>%
  add_header_above(c("\\\\textit{Dependent Variable:}" = 1, "Commercial Licenses, per 1000 inhab." = 4), escape=F) %>%
  row_spec(28, extra_latex_after = "\\midrule") %>%
  row_spec(30, extra_latex_after = "\\midrule") %>%
  
  footnote(
    general_title = "",
    general = c(
      "\\\\footnotesize \\\\textit{Note:} The table shows the author's estimation of equation (3) using different samples of municipalities. The first two
      columns show heterogeneous effects based on the presence of banking alternatives prior to a municipality being included in a Plan. These alternatives,
      labelled correspondents, represent agents that acted as intermediary for neighboring branches. The last two columns show heterogeneous effects based on 
      distance to the sea, with a cutoff of 25 kilometers. Saturated controls and fixed effects are used in all regressions. \\\\\\\\
      ***, **, and *
      indicate significance at the 0.01, 0.05 and 0.10 levels, respectively, using two-tailed tests. Robust standard errors are clustered
      at the municipality level."
    ),
    threeparttable = T ,
    footnote_as_chunk=T,
    escape = F
  )

write_lines(ATabHet, file = paste(tables, "app_heterog.tex", sep = ""))

################################################################################
################################################################################
################################# FIGURES ######################################
################################################################################
################################################################################
ES_plot <- cowplot::plot_grid(figs$ES3, figs$ES1, figs$ES2,
                              nrow = 3, labels=c("First Stage", "Baseline", "Saturated"),
                              label_x = , label_y = , label_fontface = )

ggsave(ES_plot, filename = paste(figures, "ES.png", sep = ""), dpi = 800,
       width = 6, height = 7.25)

ES2_plot <- cowplot::plot_grid(figs$ES5, figs$ES6,
                               nrow = 1, labels=c("Correspondents", "No Correspondents"),
                               label_x = , label_y = , label_fontface = )

ggsave(ES2_plot, filename = paste(figures, "ES2.png", sep = ""), dpi = 800,
       width = 9, height = 3)

################################################################################
CS_plot <- cowplot::plot_grid(figs$CS1, figs$CS2,
                              nrow = 2, labels=c("Baseline", "Saturated"),
                              label_x = , label_y = , label_fontface = )

ggsave(CS_plot, filename = paste(figures, "CS.png", sep = ""), dpi = 800,
       width = 6, height = 5.50)

################################################################################
ggsave(figs$group_att, filename = paste(figures, "ESyear.png", sep = ""), dpi = 1200,
       width = 6, height = 8)

figs$group_att <- figs$group$"1965" / figs$group$"1966" / figs$group$"1967" / figs$group$"1968" / figs$group$"1969" / figs$group$"1970" / figs$group$"1971" 
ATT_year1 <- cowplot::plot_grid(figs$group$"1965", figs$group$"1966",
                                nrow=1, label_x = , label_y = , label_fontface = )
ATT_year2 <- cowplot::plot_grid(figs$group$"1967", figs$group$"1968",
                                nrow=1, label_x = , label_y = , label_fontface = )
ATT_year3 <- cowplot::plot_grid(figs$group$"1969", figs$group$"1970",
                                nrow=1, label_x = , label_y = , label_fontface = )

ATT_year <- cowplot::plot_grid(ATT_year1, ATT_year2, ATT_year3, nrow=3, label_x = , label_y = , label_fontface = )

ggsave(ATT_year, filename = paste(figures, "ESyear.png", sep = ""), dpi = 1200,
       width = 9, height = 4.5)

################################################################################
ggsave(figs$heterog_base, filename = paste(figures, "HetBase.png", sep = ""), dpi = 800,
       width = 6, height = 8)

################################################################################
ggsave(figs$heterog_full, filename = paste(figures, "HetFull.png", sep = ""), dpi = 800,
       width = 6, height = 8)

################################################################################
ggsave(figs$simul_plot, filename = paste(figures, "permut.png", sep = ""), dpi = 800,
       width = 6, height = 2.75)

################################################################################