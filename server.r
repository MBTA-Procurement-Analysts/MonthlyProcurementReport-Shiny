# File: server.r
# Created by: Mickey Guo
# Server Function of the Procurement Report Shiny App



# Server Function, make sure this stays at the bottom ---------------------


# The below functions should include all the references to plots and DataTables. 
# The variable names should match the references in the ui.r file.
# A prefix of page number or section for the output variable is encouraged

function(input, output) {
  
  output$pg2_p1_line_all <- renderPlotly(plot_line_month_all)
  output$pg2_d1_all <- renderDT(DT::datatable(po_all_top10_table, 
                                          rownames = FALSE,
                                          options = list(dom = "t",
                                                         columnDefs = list(list(className = 'dt-left', targets = 0:1),
                                                                           list(className = 'dt-right', targets = 2))),
                                          caption = "Top 10 POs from all") %>% 
                              formatCurrency(c("Sum Amount")))
  
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
}