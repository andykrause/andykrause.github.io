#***************************************************************************************************
#*  
#*  
#*  
#***************************************************************************************************

### Setup ------------------------------------------------------------------------------------------

  # Load libraries
  library(tidyverse)

  # Set Data Path
  data_path = '/Volumes/googledrive/shared drives/Valuation'
  
### Data Loading and Prep --------------------------------------------------------------------------
  
  # Load Raw data (from <insert link>)
  raw_df <- read.csv(file.path(data_path, 'data', 'responsiveness_test.csv'))

  # Remove duplicates sale pairs
  sp_df <- raw_df %>%
    dplyr::select(-X) %>%
    dplyr::arrange(desc(recordingdate)) %>%
    dplyr::distinct(zpid, .keep_all = TRUE)
  
  # Remove January sale pairs as they might have correct info
  sp_df <- sp_df %>%
    dplyr::filter(recordingdate >= '2022-02-01' & recordingdate <= '2022-08-31')  

  # Add Error and feature diff columns
  sp_df <- sp_df %>%
    dplyr::mutate(error = (pred_05 - salespriceamount)/salespriceamount) %>%
    dplyr::mutate(bedroom_diff = bedroomcnt_post - bedroomcnt_pre,
                  bedroom_diff = ifelse(is.na(bedroom_diff), 0, bedroom_diff),
                  sqft_diff = (finishedsquarefeet_post/finishedsquarefeet_pre) - 1,
                  sqft_diff = ifelse(is.na(sqft_diff), 0, sqft_diff),
                  bathroom_diff = bathroomcnt_post - bathroomcnt_pre,
                  bathroom_diff = ifelse(is.na(bathroom_diff), 0, bathroom_diff),
                  yearbuilt_diff = yearbuilt_post - yearbuilt_pre,
                  yearbuilt_diff = ifelse(is.na(yearbuilt_diff), 0, yearbuilt_diff),
                  effectiveyearbuilt_pre = ifelse(is.na(effectiveyearbuilt_pre), 
                                                  yearbuilt_pre, effectiveyearbuilt_pre),
                  effectiveyearbuilt_post = ifelse(is.na(effectiveyearbuilt_post), 
                                                  yearbuilt_post, effectiveyearbuilt_post),
                  effectiveyearbuilt_diff = effectiveyearbuilt_post - effectiveyearbuilt_pre,
                  effectiveyearbuilt_diff = ifelse(is.na(effectiveyearbuilt_diff), 0, 
                                                   effectiveyearbuilt_diff))

  # Remove a few extreme values
  
  sp_df <- sp_df %>%
    dplyr::filter(abs(error) < .9 & 
                  abs(bathroom_diff) < 10 & 
                  abs(sqft_diff) < 4 & 
                  abs(bedroom_diff) < 10 & 
                  abs(yearbuilt_diff) < 500 & 
                  abs(effectiveyearbuilt_diff) < 500)

### Classification and EDA -------------------------------------------------------------------------  
  
 # Summarize remaining data  
  summary(sp_df)

 # Classify homes into: 1) no change; 2) small correction; 3) adjust down; 4) adjust up; 5)renovation
  sp_df <- sp_df %>%
    dplyr::mutate(change_type = '(1) correction',
                  change_type = ifelse(bedroom_diff >= 2 & sqft_diff > .25 | bathroom_diff >= 1.5,
                                        '(4) renovation', change_type),
                  change_type = ifelse(yearbuilt_diff > 30, '(4) renovation', change_type),
                  change_type = ifelse(effectiveyearbuilt_diff > 20, '(4) renovation', change_type),
                  change_type = ifelse(bedroom_diff == 0 & bathroom_diff == 0 &  
                                        sqft_diff == 0 & yearbuilt_diff == 0 & 
                                          effectiveyearbuilt_diff == 0, '(0) none', change_type),
                  change_type = ifelse(change_type == '(1) correction' & 
                                          (sqft_diff > .05 | bathroom_diff > 0.5),
                                        '(3) adjust up', change_type),
                  change_type = ifelse(change_type == '(1) correction' & 
                                         (sqft_diff < -.05 | bathroom_diff < -0.5),
                                       '(2) adjust down', change_type))

  # Summarize results
  summ_df <- sp_df %>% 
    dplyr::group_by(change_type) %>%
    dplyr::summarize(count = dplyr::n(),
                     mde = round(mean(error), 3),
                     mdpe = round(median(error), 3),
                     mape = round(mean(abs(error)),3),
                     mdape = round(median(abs(error)), 3),
                     pe5 = round(length(which(abs(error)< .05)) / count, 3),
                     pe30 = round(length(which(abs(error) < .3)) / count, 3))

  # Summarize Results by month
  summ_mdf <- sp_df %>% 
    dplyr::mutate(month = substr(recordingdate, 1, 7)) %>%
    dplyr::group_by(change_type, month) %>%
    dplyr::summarize(count = dplyr::n(),
                     mde = round(mean(error), 3),
                     mdpe = round(median(error), 3),
                     mape = round(mean(abs(error)),3),
                     mdape = round(median(abs(error)), 3),
                     pe5 = round(length(which(abs(error)< .05)) / count, 3),
                     pe30 = round(length(which(abs(error) < .3)) / count, 3))
  
### Plotting ---------------------------------------------------------------------------------------
  
  chg_cols = c('black', 'gray50', 'darkorange', 'springgreen', 'green4' )
  
  # Counts
  ggplot(summ_df, aes(x = change_type, y = count, fill = change_type)) + 
    geom_col() + 
    scale_fill_manual(values = chg_cols, name = 'Change Type') + 
    ylab('Count of Sale Pairs\n') + 
    xlab('\n Type of Change to Home Attributes') + 
    theme(legend.position = 'none') + 
    ggtitle('Count of Sales by Attribute Change Type\nJan-Aug 2022\n')
  
  # Bias by type
  ggplot(summ_df, aes(x = change_type, y = mdpe, fill = change_type)) + 
    geom_col() + 
    scale_fill_manual(values = chg_cols, name = 'Change Type') + 
    ylab('Bias (MdPE)\n') + 
    xlab('\n Type of Change to Home Attributes') + 
    theme(legend.position = 'none') + 
    ggtitle('Bias by Attribute Change Type\nJan-Aug 2022\n')
  
  # Accuracy by type
  ggplot(summ_df, aes(x = change_type, y = mdape, fill = change_type)) + 
    geom_col() + 
    scale_fill_manual(values = chg_cols, name = 'Change Type') + 
    ylab('Accuracy (MdAPE)\n') + 
    xlab('\n Type of Change to Home Attributes') + 
    theme(legend.position = 'none') + 
    ggtitle('Accuracy by Attribute Change Type\nJan-Aug 2022\n')
  
  # Bias over time
  ggplot(summ_mdf, aes(x = month, y = mdpe, group = change_type, color = change_type)) + 
    geom_line(size = 1.3) + 
    scale_color_manual(values = chg_cols, name = 'Change Type') + 
    ylab('Bias (MdPE)\n') + 
    xlab('\n Type of Change to Home Attributes') + 
    theme(legend.position = 'bottom') + 
    ggtitle('Bias by Attribute Change Type\nJan-Aug 2022\n')
  
  # Accuracy over time
  ggplot(summ_mdf, aes(x = month, y = mdape, group = change_type, color = change_type)) + 
    geom_line(size = 1.3) + 
    scale_color_manual(values = chg_cols, name = 'Change Type') + 
    ylab('Accuracy (MdPE)\n') + 
    xlab('\n Type of Change to Home Attributes') + 
    theme(legend.position = 'bottom') + 
    ggtitle('Bias by Attribute Change Type\nJan-Aug 2022\n')
  
  # Deep dive on SqFt changes
  ggplot(sp_df %>% 
           dplyr::filter(sqft_diff != 0) %>% 
           dplyr::filter(abs(sqft_diff) < .9), 
         aes(x = sqft_diff, y = error)) +
    geom_smooth() + 
    coord_cartesian(xlim = c(-.32, .32), ylim = c(-.03, .03)) +
    ylab('Bias (MdPE)\n') + 
    xlab('\nChange in SqFt (%)') +
    scale_x_continuous(breaks = seq(-.3, .3, by = .1),
                       labels = c('-30%', '-20%', '-10%', '0%', '10%', '20%', '30%')) + 
    ggtitle('Bias as a function of change in SqFt\n')
  
#***************************************************************************************************
#***************************************************************************************************  
  
  
  
  
ggplot(a, aes(x=month, y = mdpe, group = change_type, color = change_type)) + 
  geom_line()

ggplot(a, aes(x=month, y = mdape, group = change_type, color = change_type)) + 
  geom_line()

quantile(x_df$sqft_diff, seq(.1, .9, .1))


ggplot(x_df %>% 
         dplyr::filter(sqft_diff != 0) %>% 
         dplyr::filter(abs(sqft_diff) < .9),
       aes(x = sqft_diff)) + geom_density()
      
ggplot(x_df,
       aes(x = error, group = change_type, color = change_type)) + 
  geom_density() + 
  coord_cartesian(xlim = c(-.3, .3))



