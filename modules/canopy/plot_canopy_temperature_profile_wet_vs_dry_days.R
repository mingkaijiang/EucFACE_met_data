plot_canopy_temperature_profile_wet_vs_dry_days <- function(plotDF) {
    
    ### process the data
    plotDF$HOD <- hour(plotDF$DateHour)
    plotDF$Date <- as.Date(plotDF$DateHour)
    
    ### quality control - remove unrealistic temperatures
    plotDF <- plotDF[plotDF$SBTmp1 > -5, ]
    plotDF <- plotDF[plotDF$TargTmp1 > -5, ]
    plotDF <- plotDF[plotDF$TargTmp2 > -5, ]
    
    plotDF <- plotDF[plotDF$AirT_Avg < 50, ]
    plotDF <- plotDF[plotDF$SBTmp2 < 50, ]
    plotDF <- plotDF[plotDF$SBTmp4 < 50, ]
    plotDF <- plotDF[plotDF$TargTmp1 < 50, ]
    plotDF <- plotDF[plotDF$TargTmp2 < 50, ]
    plotDF <- plotDF[plotDF$TargTmp3 < 50, ]
    plotDF <- plotDF[plotDF$TargTmp4 < 50, ]
    
    
    ### calculate temperature difference for each four sensor individually, and for each ring
    plotDF$Tmp1Diff <- plotDF$SBTmp1 - plotDF$AirT_Avg#plotDF$TargTmp1
    plotDF$Tmp2Diff <- plotDF$SBTmp2 - plotDF$AirT_Avg#plotDF$TargTmp2
    plotDF$Tmp3Diff <- plotDF$SBTmp3 - plotDF$AirT_Avg#plotDF$TargTmp3
    plotDF$Tmp4Diff <- plotDF$SBTmp4 - plotDF$AirT_Avg#plotDF$TargTmp4
    
    ### calculate average temperature difference profile
    plotDF$TmpDiff_Avg <- rowMeans(data.frame(plotDF$Tmp1Diff,plotDF$Tmp2Diff,plotDF$Tmp3Diff,plotDF$Tmp4Diff), na.rm=T)

    ### plot 
    p1 <- ggplot(plotDF, aes(x=AirT_Avg, y=TmpDiff_Avg)) +
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
        xlab("Tair") +
        ylab("Tair - Tcanopy")+
        scale_fill_continuous(type = "viridis")
    
    plot(p1)
    
    ### create binned Tair profile
    plotDF2 <- data.frame(seq(275, 315, by=10), NA, NA, NA, NA)
    colnames(plotDF2) <- c("Tair", "Td_mean", "Td_sd", "Td_pos", "Td_neg")
    
    for (i in plotDF2$Tair) {
        plotDF2[plotDF2$Tair == i, "Td_mean"] <- mean(plotDF[plotDF$AT > (i - 5) & plotDF$AT <= (i + 5), "diff1"], na.rm=T)
        plotDF2[plotDF2$Tair == i, "Td_sd"] <- sd(plotDF[plotDF$AT > (i - 5) & plotDF$AT <= (i + 5), "diff1"], na.rm=T)
    }
    
    plotDF2$Td_pos <- plotDF2$Td_mean+plotDF2$Td_sd
    plotDF2$Td_neg <- plotDF2$Td_mean-plotDF2$Td_sd
    
    ### plotting
    p4 <- ggplot(plotDF2, aes(x=Tair, y=Td_mean)) +
        geom_point()+
        geom_errorbar(aes(x=Tair, ymin=Td_neg, ymax=Td_pos))+
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
        xlab("Tair (above)") +
        ylab("Tair diff (above - below)")+
        scale_fill_continuous(type = "viridis")
    
    
    plot(p1)
    
    
    
    
    
    dDF <- summaryBy(AT~Date, FUN=mean, data=plotDF, keep.names=T, na.rm=T)
    colnames(dDF) <- c("Date", "Daily_Mean_Tair")
    plotDF2 <- merge(plotDF, dDF, by="Date")
    
    ### subset days
    #test <- subset(plotDF2, Daily_Mean_Tair > 303)
    plotDF3 <- plotDF2[plotDF2$Date%in%c(as.Date("2016-12-29"),as.Date("2020-01-04")),]
    
    
    
    ### plotting
    p1 <- ggplot(plotDF3, aes(x=HOD, y=diff1, group=as.character(Date),
                              col=as.character(Date))) +
        geom_hline(yintercept=0, col="black", lwd=1.5)+
        geom_line()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12, family="Helvetica"), 
              axis.text.x = element_text(size=12, family="Helvetica"),
              axis.text.y=element_text(size=12, family="Helvetica"),
              axis.title.y=element_text(size=12, family="Helvetica"),
              legend.text=element_text(size=12, family="Helvetica"),
              legend.title=element_text(size=12, family="Helvetica"),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0,
              legend.direction="vertical")+
        xlab("HOD") +
        ylab("Tair diff (above - below)")+
        xlim(0, 24)
    
    
    ggsave(filename = "output/T_canopy_wet_vs_dry.pdf", 
           plot = p1,
           width = 89, 
           height = 89,
           units = "mm",
           dpi = 300)


}