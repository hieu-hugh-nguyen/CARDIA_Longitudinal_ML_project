

eval_time_for_plot <- seq(0,17, 0.1)
prob_risk_test <- predictRisk.rsf(trained_model
                                  , newdata = test_data
                                  , times = eval_time_for_plot
)

prob_surv = 1-prob_risk_test
prob_surv_with_ID_df <- data.frame(cbind(test_id, prob_surv))
names(prob_surv_with_ID_df) <- c('ID', eval_time_for_plot)

prob_surv_test_with_ID_long <-  prob_surv_with_ID_df %>% 
  tidyr::pivot_longer(-c(ID), values_to = "Survival", names_to = "Year") %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::mutate(Survival = as.numeric(Survival)) %>%
  dplyr::mutate(ID = as.character(ID))


#ggplot(data = prob_surv_test_with_ID_long[1:(length(eval_time_for_plot)*300),])+

ggplot(data = prob_surv_test_with_ID_long)+
  geom_line(aes(Year, Survival, group = ID, colour =  "steelblue1"), alpha = 1, size = 1) +
  xlab("Time (Years)") +
  # ylab(var_oi) +
  # scale_x_continuous(breaks=seq(5,17,2))+
  # ylim(min(longi_data_median$mean_)*0.9, max(longi_data_median$mean_)*1.1) + 
  #ylim(0.5, 1) + 
  scale_color_manual(values = 'steelblue') +
  
  theme_minimal() + 
  theme(axis.text=element_text(size=25),
        #plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_text(size=15),
        #axis.ticks.y=element_blank(),
        axis.title=element_text(size=30),
        # legend.title = element_text(color = "black", size = 18),
        # legend.text = element_text(color = "black", size = 18),
        legend.position = "none") 
  