# Visualise the outputs

options(digits = 2)

# Load all packages
source("analysis/settings_packages.R")

# Read in data
predicted_count <- read_csv("output/st03_05_subgroup_predicted_value.csv")


# 1. Previous COVID admission: ----
hos_stratum <- read_csv("output/st03_05_subgroup_hospitalisation.csv") %>% 
      arrange(model) %>% relocate(stratum) %>% 
      dplyr::select(stratum, model, estimate, lci, hci) %>% 
      mutate(`OR (95% CI)` = sprintf("%.2f (%.2f - %.2f)",estimate, lci, hci)) %>% 
      mutate(`RR (95% CI)` = `OR (95% CI)` ) %>% 
      mutate(`   First part      `  = " ",
             `   Second part      ` = " ") %>% 
      relocate(`   First part      `, .before = `OR (95% CI)`) %>% 
      relocate(`   Second part      `, .before = `RR (95% CI)`)

hos_part_1 <- hos_stratum %>% filter(model=="binomial") %>% 
      dplyr::select(stratum,estimate,lci,hci,`   First part      `,`OR (95% CI)`)

hos_part_2 <- hos_stratum %>% filter(model=="Positive Negative Bionomial") %>% 
      dplyr::select(stratum,estimate,lci,hci,`   Second part      `,`RR (95% CI)`) %>% 
      rename(estimate2=estimate, lci2=lci, hci2=hci)

hos_plot_data <- inner_join(hos_part_1, hos_part_2, by = c("stratum" = "stratum"))

# Plot forest plot
tm <- forest_theme(core=list(bg_params=list(fill = c("#FFFFFF"))))

hos_forest <- forest(
      data = hos_plot_data[,c(1,5,6,10,11)],
      est = list(hos_plot_data$estimate, hos_plot_data$estimate2),
      lower = list(hos_plot_data$lci, hos_plot_data$lci2),
      upper = list(hos_plot_data$hci,hos_plot_data$hci2),
      ci_column = c(2, 4),
      ref_line = 1,
      theme = tm)
plot(hos_forest)

# Average visits:
hos_visits <- predicted_count %>% filter(group == "Previous hospitalisation") %>% 
      mutate(stratum = ifelse(stratum == "FALSE", "Not admitted to hospital due to COVID", "Admitted to hospitals due to COVID"))

colours <- c("Long covid exposure" = "#FFC20A", "Comparator"="#0C7BDC")

hos_bar <- ggplot(data = hos_visits, aes(x = stratum, y = visits, fill = exposure)) + 
      geom_bar(position="dodge", stat="identity", width = 0.5) + 
      geom_errorbar(position=position_dodge(0.5), aes(ymin = lci, ymax = hci), width = 0.2) + 
      theme_bw() +  coord_flip() + scale_fill_manual(values = colours) +
      ylab("Average healthcare visit frequency") + xlab(" ") +
      guides(fill=guide_legend(title="Exposure group"))

# Combine plots: 
hos_plots <- ggarrange(hos_forest, hos_bar, ncol = 1)

ggsave(hos_plots, file = "output/st03_05_stratum_hos_visits.png",
       width=10, height=5, units = "in", dpi = 300)


# 2. Sex stratum -----
sex_stratum <- read_csv("output/st03_05_subgroup_sex.csv") %>% 
      arrange(model) %>% relocate(stratum) %>% 
      dplyr::select(stratum, model, estimate, lci, hci) %>% 
      mutate(`OR (95% CI)` = sprintf("%.2f (%.2f - %.2f)",estimate, lci, hci)) %>% 
      mutate(`RR (95% CI)` = `OR (95% CI)` ) %>% 
      mutate(`   First part      `  = " ",
             `   Second part      ` = " ") %>% 
      relocate(`   First part      `, .before = `OR (95% CI)`) %>% 
      relocate(`   Second part      `, .before = `RR (95% CI)`)

sex_part_1 <- sex_stratum %>% filter(model=="binomial") %>% 
      dplyr::select(stratum,estimate,lci,hci,`   First part      `,`OR (95% CI)`)

sex_part_2 <- sex_stratum %>% filter(model=="Positive Negative Bionomial") %>% 
      dplyr::select(stratum,estimate,lci,hci,`   Second part      `,`RR (95% CI)`) %>% 
      rename(estimate2=estimate, lci2=lci, hci2=hci)

sex_plot_data <- inner_join(sex_part_1, sex_part_2, by = c("stratum" = "stratum"))

# forest plots
sex_forest <- forest(
      data = sex_plot_data[,c(1,5,6,10,11)],
      est = list(sex_plot_data$estimate, sex_plot_data$estimate2),
      lower = list(sex_plot_data$lci, sex_plot_data$lci2),
      upper = list(sex_plot_data$hci,sex_plot_data$hci2),
      ci_column = c(2, 4),
      ref_line = 1,
      theme = tm)
plot(sex_forest)

# Average visits:
sex_visits <- predicted_count %>% filter(group == "Sex") %>% 
      mutate(stratum = ifelse(stratum == "female", "Female", "Male"))

sex_bar <- ggplot(data = sex_visits, aes(x = stratum, y = visits, fill = exposure)) + 
      geom_bar(position="dodge", stat="identity", width = 0.5) + 
      geom_errorbar(position=position_dodge(0.5), aes(ymin = lci, ymax = hci), width = 0.2) + 
      theme_bw() +  coord_flip() + scale_fill_manual(values = colours) +
      ylab("Average healthcare visit frequency") + xlab(" ") +
      guides(fill=guide_legend(title="Exposure group"))

# Combine plots: 
sex_plots <- ggarrange(sex_forest, sex_bar, ncol = 1)

ggsave(sex_plots, file = "output/st03_05_stratum_sex_visits.png",
       width=8, height=5, units = "in", dpi = 300)


# 3. Age categories: ------
age_stratum <- read_csv("output/st03_05_subgroup_age.csv")%>% 
      arrange(model) %>% relocate(stratum) %>% 
      dplyr::select(stratum, model, estimate, lci, hci) %>% 
      mutate(`OR (95% CI)` = sprintf("%.2f (%.2f - %.2f)",estimate, lci, hci)) %>% 
      mutate(`RR (95% CI)` = `OR (95% CI)` ) %>% 
      mutate(`   First part      `  = " ",
             `   Second part      ` = " ") %>% 
      relocate(`   First part      `, .before = `OR (95% CI)`) %>% 
      relocate(`   Second part      `, .before = `RR (95% CI)`)

age_part_1 <- age_stratum %>% filter(model=="binomial") %>% 
      dplyr::select(stratum,estimate,lci,hci,`   First part      `,`OR (95% CI)`)

age_part_2 <- age_stratum %>% filter(model=="Positive Negative Bionomial") %>% 
      dplyr::select(stratum,estimate,lci,hci,`   Second part      `,`RR (95% CI)`) %>% 
      rename(estimate2=estimate, lci2=lci, hci2=hci)

age_plot_data <- inner_join(age_part_1, age_part_2, by = c("stratum" = "stratum"))

# forest plots
age_forest <- forest(
      data = age_plot_data[,c(1,5,6,10,11)],
      est = list(age_plot_data$estimate, age_plot_data$estimate2),
      lower = list(age_plot_data$lci, age_plot_data$lci2),
      upper = list(age_plot_data$hci,age_plot_data$hci2),
      ci_column = c(2, 4),
      ref_line = 1,
      theme = tm)
plot(age_forest)

# Average visits:
age_visits <- predicted_count %>% filter(group == "Age group") 
age_visits$stratum <- factor(age_visits$stratum, levels = c("70+","60-69", "50-59", "40-49","30-39", "18-29"))

age_bar <- ggplot(data = age_visits, aes(x = stratum, y = visits, fill = exposure)) + 
      geom_bar(position="dodge", stat="identity", width = 0.5) + 
      geom_errorbar(position=position_dodge(0.5), aes(ymin = lci, ymax = hci), width = 0.2) + 
      theme_bw() +  coord_flip() + scale_fill_manual(values = colours) +
      ylab("Average healthcare visit frequency") + xlab(" ") +
      guides(fill=guide_legend(title="Exposure group"))

# Combine plots: 
age_plots <- ggarrange(age_forest, age_bar, ncol = 1)

ggsave(age_plots, file = "output/st03_05_stratum_age_visits.png",
       width=8, height=5, units = "in", dpi = 300)