#Running this script assumes that you have downloaded the google doc "Collated Martes Survey Results.xlsx" and placed it in the project folder

#Load Packages
library(tidyverse)
library(readxl)
library(Cairo)

#Load Data
sheets <- excel_sheets("Collated Martes Survey Results.xlsx")

df1 <- read_excel("Collated Martes Survey Results.xlsx", sheet = sheets[1])
colnames(df1) <- c("Question","Count_Online","Count_Conference","Threat_Action","CMP_Category","CMP_SubCategory","Orig_Category","Online_Comments")
glimpse(df1)

df2 <- read_excel("Collated Martes Survey Results.xlsx", sheet = sheets[3])
glimpse(df2)
colnames(df2) <- c("Question","Threat_Action","CMP_Category","CMP_SubCategory","Orig_Category","InPerson_Comments")


# Graphs df1
df1_Q <- df1 %>% count(Question)
df1_Q$Short <- c("Martes Wins", "Information Needs - Jurisdiction", "Obstacles - Jurisdiction", "Threats - Globe", "Threats - Jurisdiction", "Actions Needed - Jurisdiction")
df1_Qcat <- df1 %>% group_by(Question, Threat_Action,CMP_Category) %>% summarize(Conference = sum(Count_Conference), Online = sum(Count_Online))
df1_Qcat <- left_join(df1_Qcat, df1_Q %>% select(-n))

df1_Qcat <- df1_Qcat %>% pivot_longer(cols=c(Conference, Online), names_to = "Type", values_to = "Sum")

df1_Qcat %>% ungroup() %>% count(CMP_Category) %>% print(n=33)
df1_Qcat$Type <- factor(df1_Qcat$Type, levels = c("Online", "Conference"))

create_df1_plot <- function(Q1name=Q1name){
  df1_Q1 <- df1_Qcat %>% filter(Short %in% Q1name) %>% filter(Sum>0) %>% filter(CMP_Category!="NA") %>%
  mutate(CMP_Category = fct_reorder(CMP_Category, Sum)) %>%
  ggplot( aes(x=CMP_Category, y=Sum, fill=Type)) +
  geom_bar(position = "dodge", stat="identity", alpha=0.6, width=0.4) +
  scale_fill_manual(values = alpha(c("#137a63","#0a3a2a"))) +
  coord_flip() +
  xlab("") +
  ylab("Votes") +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank(), )+
  facet_wrap(~Short)
}

df1_Q5 <- create_df1_plot(Q1name=unique(df1_Qcat$Short)[5])

Cairo(file="df1_Q5_hist.PNG", 
      type="png",
      width=1600, 
      height=2000, 
      pointsize=12,
      bg="white",
      dpi=300)
df1_Q5
dev.off()

# Graphs df2
df2_Q <- df2 %>% count(Question)
df2_Q$Short <- c("Replicate & Scale Wins", "Research to Advance Actions", "Overcome Obstacles by...", "Actions to Take", "Emerging Themes")
df2_Qcat <- df2 %>% group_by(Question, Threat_Action) %>% count(CMP_Category)
df2_Qcat <- left_join(df2_Qcat, df2_Q %>% select(-n))


create_df2_plot <- function(Q2name=Q2name){
  df2_Q2 <- df2_Qcat %>% filter(Short %in% Q2name) %>% filter(n>0) %>% filter(CMP_Category!="NA") %>%
    mutate(CMP_Category = fct_reorder(CMP_Category, n)) %>%
    ggplot(aes(x=CMP_Category, y=n)) +
    geom_bar(stat="identity", alpha=0.6, width=0.4) +
    scale_fill_manual(values = alpha(c("#0a3a2a"))) +
    coord_flip() +
    xlab("") +
    ylab("Group Themes") +
    theme_bw() +
    facet_wrap(~Short)
}

df2_Q4 <- create_df2_plot(Q2name=unique(df2_Qcat$Short)[4])

Cairo(file="df2_Q4_hist.PNG", 
      type="png",
      width=1600, 
      height=2000, 
      pointsize=12,
      bg="white",
      dpi=300)
df2_Q4
dev.off()


df2_Qcat %>% group_by(CMP_Category) %>% summarise(Sum=sum(n))
df2_Qcat$CMP_Category <- factor(df2_Qcat$CMP_Category, levels = c("10. Institutional Development", "3. Awareness Raising",
                                                                  "8. Research & Monitoring","7. Legal & Policy Frameworks",
                                                                  "5. Livelihood, Economic & Moral Incentives", "2. Species Management",
                                                                  "NA"))
df2_Qcat$CMP_Category <- as.factor(df2_Qcat$CMP_Category)
levels(df2_Qcat$CMP_Category)


df2_all4 <- df2_Qcat %>% filter(Short != "Emerging Themes") %>% filter(n>0) %>% filter(CMP_Category!="NA") %>%
  ggplot(aes(x=fct_rev(CMP_Category), y=n)) +
  geom_bar(stat="identity", alpha=0.6, width=0.4) +
  scale_fill_manual(values = alpha(c("#0a3a2a"))) +
  coord_flip() +
  xlab("") +
  ylab("Group Themes") +
  theme_bw() +
  facet_wrap(~Short)

Cairo(file="df2_all4_hist.PNG", 
      type="png",
      width=2200, 
      height=2000, 
      pointsize=12,
      bg="white",
      dpi=300)
df2_all4
dev.off()
