TimePlot = function(multi_ob, start = NULL, 
                    end = NULL, line = TRUE, 
                    signif = TRUE, Type = FALSE) {
  score_frame = multi_ob$Full %>% ungroup() %>%
    arrange(as.numeric(gsub("Sample", "", Sample))) %>% 
    mutate(Sample = factor(Sample, levels = unique(Sample)))
  require(ggplot2)
  if (line) {
    
    if ( (signif == TRUE) & (Type == FALSE) ) {
      score_frame = score_frame %>% 
        group_by(Coordinate) %>% 
        filter(any(TAD_Score>3))
      
      #Pull out category and left join to long frame
      
      Cats = multi_ob$Consensus %>% dplyr::select(Coordinate, Category)
      
      score_frame =left_join(score_frame, Cats)
      
      sum_frame = score_frame %>% group_by(Sample) %>% 
        summarise(Mean_Scores = mean(TAD_Score))
      
     score_frame = score_frame %>% 
       mutate(Differential = ifelse(TAD_Score>3, "Differential",
                                    "Non-Differential"))
      
      ggplot(score_frame , 
             aes(x = Sample, y = Boundary)) +
        guides(group = FALSE) + geom_line(aes(y = TAD_Score, group = Coordinate),
                                          size = .6) + 
        geom_point(data = (score_frame %>% 
                             filter(TAD_Score>3)),
                   aes(x = Sample, 
                       y = TAD_Score, 
                       color = Differential), size = 2) +
        scale_color_manual(values = c("Differential" = "red", 
                                      "Non-Differential" = "black")) + 
        guides(color = FALSE) + geom_line(data = sum_frame, 
                                          aes(x = Sample, y = Mean_Scores, 
                                              group = 1),
                                          size = 4, color = "red") +
        scale_x_discrete(labels=unique(score_frame$Sample), 
                            expand=c(0,0.1)) +
        theme(text = element_text(size=32), 
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(size = 32),
              axis.text.y = element_text(size = 32))
    } else if ( (Signif == TRUE) & Type == TRUE ) {
      score_frame = score_frame %>% 
        group_by(Coordinate) %>% 
        filter(any(TAD_Score>3))
      
      #Pull out category and left join to long frame
      
      Cats = multi_ob$Consensus %>% dplyr::select(Coordinate, Category)
      
      score_frame =left_join(score_frame, Cats)
      
      sum_frame = score_frame %>% group_by(Sample) %>% 
        summarise(Mean_Scores = mean(TAD_Score))
      
      score_frame = score_frame %>% 
        mutate(Differential = ifelse(TAD_Score>3, "Differential",
                                     "Non-Differential"))
      
      ggplot(score_frame , 
             aes(x = Sample, y = Boundary)) +
        guides(group = FALSE) + geom_line(aes(y = TAD_Score, group = Coordinate, 
                                              color = Category),
                                          size = .6) + 
        geom_point(data = (score_frame %>% 
                             filter(TAD_Score>3)),
                   aes(x = Sample, 
                       y = TAD_Score, 
                       color = Differential), size = 2) +
        scale_color_manual(values = c("Differential" = "red", 
                                      "Non-Differential" = "black",
                                      "Late Transit TAD" = "blue",
                                      "Highly Common TAD" = "orange", 
                                      "Dynamic TAD" = "black",
                                      "Early Transit TAD" = "green")) + 
        guides(color = FALSE) + geom_line(data = sum_frame, 
                                          aes(x = Sample, y = Mean_Scores, 
                                              group = 1),
                                          size = 4, color = "red") +
        scale_x_discrete(labels=unique(score_frame$Sample), 
                         expand=c(0,0.1)) +
        theme(text = element_text(size=32), 
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(size = 32),
              axis.text.y = element_text(size = 32))
   } else {
  ggplot(score_frame %>% filter( (Coordinate<end) &  (Coordinate>start)), aes(x = Sample, y = Boundary)) +
    guides(group = FALSE) + geom_line(aes(y = TAD_Score, group = Coordinate) ) + 
    geom_point(data = (score_frame %>% 
                         filter(TAD_Score>3) %>%
                         filter((Coordinate<end) &  (Coordinate>start))),
               aes(x = Sample, 
                   y = TAD_Score, 
                   color = Differential)) +
    scale_color_manual(values = c("red", "black")) + guides(color = FALSE)
      
    }
  } else {
    
    annotation = data.frame(Type = multi_ob$Consensus$Category)
    rownames(annotation) = multi_ob$Consensus$Coordinate
    
    Type =  c("blue","orange", "black","green", "light blue", "red")
    names(Type) = c("Early Transit TAD", 
                    "Late Transit TAD",
                    "Highly Common TAD", 
                    "Dynamic TAD" , 
                    "Early Disappearing TAD", 
                    "Late Diseappearing TAD")
    
    
    anno_colors = list(Type = Type)
    #Subset to only include individual TAD scores
    consensus_frame = multi_ob$Consensus[,2:(ncol(multi_ob$Consensus)-1)]
    row.names(consensus_frame) = multi_ob$Consensus$Coordinate
    #Subset the annotation
    annotation = annotation[apply(consensus_frame, 1, function(x) any(x>3)),,
                            drop = FALSE]
    #Subset to include any rows with significant TADs
    consensus_frame = 
      consensus_frame[which(apply(consensus_frame, 1, function(x) any(x>3))),]
    
    #Make rows full numbers and not scientific
    
    consensus_frame = consensus_frame[,-ncol(consensus_frame)]
    row.names(consensus_frame) = as.integer(row.names(consensus_frame))
    row.names(annotation) = as.integer(row.names(annotation))
    
    
    pheatmap::pheatmap(consensus_frame, 
                       clustering_method = "ward.D2",
                       cluster_cols = FALSE, cluster_rows = TRUE,
                       scale = "row", 
                       annotation_row = annotation)
    
  }
}
