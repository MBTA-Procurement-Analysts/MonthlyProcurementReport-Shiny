# File: server.r
# Created by: Mickey Guo
# Server Function of the Procurement Report Shiny App



# Server Function, make sure this stays at the bottom ---------------------


# The below functions should include all the references to plots and DataTables. 
# Format in a nutshell:
#   output$[variable name in ui.r] <- renderDT or renderPlotly ([object name defined in pages])
# A prefix of page number or section for the output variable is encouraged

function(input, output) {
  
  # Page 2, Overalls
  output$pg2_p1_line_all <- renderPlotly(plot_line_month_all)
  
  tbl1 <- DT::datatable(po_all_top10_table, 
                        rownames = FALSE,
                        options = list(dom = "t",
                                       columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                         list(className = 'dt-right', targets = 2))),
                        caption = "Top 10 POs from all") %>% 
    formatCurrency(c("Sum Amount"))
  
  output$pg2_d1_all <- renderDT(tbl1)
  
  output$pg2_p2_line_98pc <- renderPlotly(plot_line_month_98perc)
  output$pg2_d2_98pc <- renderDT(DT::datatable(po_98perc_top10_table, 
                                               rownames = FALSE,
                                               options = list(dom = "t",
                                                              columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                                                list(className = 'dt-right', targets = 2))),
                                               caption = "Top 10 POs from 98% percentile.") %>% 
                                   formatCurrency(c("Sum Amount")))
  
  output$pg2_d3_prev2mo <- renderDT(DT::datatable(po_prev2mos_all_table,
                                                  rownames = FALSE, 
                                                  options = list(dom = "t")) %>% 
                                      formatCurrency(c("Sum")) %>% 
                                      formatStyle('Unit', 
                                                  target = 'row',
                                                  fontWeight = styleEqual(c("May Total", "Jun Total"), c('bold', 'bold')),
                                                  backgroundColor = styleEqual(c("May Total", "Jun Total"), c("#dedede", "#dedede"))))
  
  output$pg2_p3_pie_spend_bunit <- renderPlotly(plot_pie_count_bunit)
  
  output$pg2_d4_ytd_bunit <- renderDT(DT::datatable(po_spend_bunit_all, 
                                                    rownames = FALSE, 
                                                    options = list(dom = "t")) %>%
                                        formatCurrency(c("Sum")) %>% 
                                        formatStyle('Unit',
                                                    target = 'row',
                                                    fontWeight = styleEqual(c("Grand Total"), c('bold')), 
                                                    backgroundColor = styleEqual(c("Grand Total"), c("#dedede"))))
  
  # Pg4, Thresholds and Purchasing Platforms
  output$pg4_plot_thresholds <- renderPlotly(pg4_plot_thresholds)
  output$pg4_plot_threshold_sum_cnt <- renderPlotly(pg4_plot_threshold_sum_cnt)
  output$pg4_threshold_sum_cnt_dt <- renderDT(DT::datatable(pg4_threshold_table,
                                                            rownames = FALSE, 
                                                            options = list(dom = 't')) %>% 
                                                formatCurrency(c("FY18 Spend")) %>% 
                                                formatPercentage(c("Percent of PO Count", "Percent of FY18 Spend"), digits = 2) %>%
                                                # this can be considered as a hack, since piping 2 formatStyle produces conflicts.
                                                formatStyle("FY18 Thresholds", 
                                                            target = 'row', 
                                                            fontStyle = styleEqual(c("Total", ""), c('italic', 'normal')), 
                                                            fontWeight = styleEqual(c("Total", ""), c('normal', 'bold')), 
                                                            backgroundColor = styleEqual(c("Total", ""), c("#e6e6e6", "dedede"))))
  
  output$pg4_2moago_threshold <- renderDT(DT::datatable(pg4_2moago_threshold_all_table, 
                                                        colnames = paste(prevmos[1], colnames(pg4_2moago_threshold_all_table)),
                                                        rownames = FALSE,
                                                        options = list(dom = 't',
                                                                       columnDefs = list(list(className = 'dt-left', targets = 0)))) %>% 
                                            formatCurrency(c("Sum")) %>% 
                                            formatStyle("Threshold", 
                                                        target = 'row', fontWeight = styleEqual(c("Grand Total"), c('bold')), 
                                                        backgroundColor = styleEqual(c("Grand Total"), c("#dedede"))))
  
  output$pg4_1moago_threshold <- renderDT(DT::datatable(pg4_1moago_threshold_all_table, 
                                                        colnames = paste(prevmos[2], colnames(pg4_1moago_threshold_all_table)),
                                                        rownames = FALSE,
                                                        options = list(dom = 't',
                                                                       columnDefs = list(list(className = 'dt-left', targets = 0)))) %>% 
                                            formatCurrency(c("Sum")) %>% 
                                            formatStyle("Threshold", 
                                                        target = 'row', fontWeight = styleEqual(c("Grand Total"), c('bold')), 
                                                        backgroundColor = styleEqual(c("Grand Total"), c("#dedede"))))
  
  output$pg4_plot_sub100_bunit_count <- renderPlotly(pg4_plot_sub1k_count_bunit)
  
}