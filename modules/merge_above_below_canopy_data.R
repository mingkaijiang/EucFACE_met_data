merge_above_below_canopy_data <- function(atDF, btDF) {
    
    #### above canopy data has shorter duration, and averaged over all rings
    #### below canopy data has two temperature profile, for each Ring
    
    #### merge
    mgDF <- merge(btDF, atDF, by = c("DateHour"))
    
    ### checking results
    #with(mgDF[mgDF$Ring==1,], plot(AAirT~DateHour, type="l"))
    #with(mgDF[mgDF$Ring==1,], lines(AirTC_1_Avg~DateHour, col=alpha("red", 0.2)))
    
    ### rename
    colnames(mgDF) <- c("DateHour", "Ring", "BT1", "BT2", "AT")
    
    ### check difference
    mgDF$diff1 <- mgDF$AT - mgDF$BT1
    mgDF$diff2 <- mgDF$AT - mgDF$BT2
    
    ### checking results
    #with(mgDF[mgDF$Ring==1,], plot(diff1~DateHour, type="l"))
    #with(mgDF[mgDF$Ring==1,], plot(diff2~DateHour, type="l"))
    
    #with(mgDF, plot(diff1~AT, type="p"))
    #with(mgDF, plot(diff2~AT, type="p"))
    
    
    p1 <- ggplot(mgDF, aes(x=AT, y=diff1)) +
        geom_bin2d(bins = 500) +
        theme_bw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0,
              legend.direction="vertical")+
        #geom_smooth(se=FALSE, method="loess", span=0.5)+
        xlab("Tair (above)") +
        ylab("Tair diff (above - below)")+
        scale_fill_continuous(type = "viridis")
    
    p2 <- ggplot(mgDF, aes(x=AT, y=diff2)) +
        geom_bin2d(bins = 500) +
        theme_bw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.text.align=0,
              legend.direction="vertical")+
        #geom_smooth(se=FALSE, method="loess", span=0.5)+
        xlab("Tair (above)") +
        ylab("Tair diff (above - below)")+
        scale_fill_continuous(type = "viridis")
    
    
    multi.panel.plot <-
        ggdraw() +
        draw_plot(p1, x = 0.0, y = .51, width = 1., height = .45) +
        draw_plot(p2, x = 0.0, y = .0, width = 1., height = .45)
    
    
    ggsave(filename = "output/T_canopy.pdf", 
           plot = multi.panel.plot,
           width = 89, 
           height = 160,
           units = "mm",
           dpi = 300)
    
}