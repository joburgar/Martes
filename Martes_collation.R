#Running this script assumes that you have downloaded the google doc "Collated Martes Survey Results.xlsx" and placed it in the project folder

#Load Packages
library(tidyverse)
library(readxl)
library(Cairo)
library(PNWColors)


#Load Data
sheets <- excel_sheets("Collated Martes Survey Results.xlsx")

df1 <- read_excel("Collated Martes Survey Results.xlsx", sheet = sheets[1])
colnames(df1) <- c("Question","Count_Online","Count_Conference","Threat_Action","CMP_Category","CMP_Category_OLD","CMP_SubCategory","CMP_SubCategory_OLD","Orig_Category","Online_Comments")
glimpse(df1)
df1 %>% summarise(sum(Count_Online), sum(Count_Conference, na.rm=T))

df2 <- read_excel("Collated Martes Survey Results.xlsx", sheet = sheets[3])
glimpse(df2)
colnames(df2) <- c("Question","Threat_Action","CMP_Category","CMP_Category_OLD","CMP_SubCategory","CMP_SubCategory_OLD","Orig_Category","InPerson_Comments")


# Graphs df1
df1_Q <- df1 %>% count(Question)
df1_Q$Short <- c("Achievements", "Information Needs - Jurisdiction", "Obstacles - Jurisdiction", "Threats - Global", "Threats - Jurisdiction", "Actions Needed - Jurisdiction")
df1_Qcat <- df1 %>% group_by(Question, Threat_Action,CMP_Category, CMP_SubCategory) %>% summarize(Conference = sum(Count_Conference), Online = sum(Count_Online))
df1_Qcat <- left_join(df1_Qcat, df1_Q %>% select(-n))

df1_Qcat <- df1_Qcat %>% pivot_longer(cols=c(Conference, Online), names_to = "Type", values_to = "Sum")

df1_Qcat %>% ungroup() %>% count(CMP_Category) %>% print(n=33)
df1_Qcat <- df1_Qcat %>% 
  mutate(Prop = case_when(Type == "Online" ~ (Sum/46)*100,
                          Type == "Conference" ~ (Sum/101)*100))

pal <- pnw_palette(name="Winter",n=2,type="discrete")

create_df1_plot <- function(Q1name=Q1name){
  df1_Q1 <- df1_Qcat %>% filter(Short %in% Q1name) %>% filter(CMP_Category!="NA") %>%
  mutate(CMP_Category = fct_reorder(CMP_Category, Prop)) %>%
  ggplot( aes(x=CMP_Category, y=Prop, fill=Type)) +
  geom_bar(position = "dodge", stat="identity", alpha=0.6, width=0.4) +
  # scale_fill_manual(values = alpha(c("#137a63","#0a3a2a"))) +
  scale_fill_manual(values = pal) +
  coord_flip() +
  xlab("") +
  ylab("Percent of Votes (%)") +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank(), )+
  facet_wrap(~Short)
}

unique(df1_Qcat$Short)

df1_Q1 <- create_df1_plot(Q1name=unique(df1_Qcat$Short)[1])

Cairo(file="df1_Q1_hist_perc.PNG", 
      type="png",
      width=1600, 
      height=2000, 
      pointsize=12,
      bg="white",
      dpi=300)
df1_Q1
dev.off()

df1 %>% count(Question)
df1 %>% filter(grepl("wins", Question)) %>% group_by(CMP_Category,CMP_SubCategory) %>% summarise(sum(Count_Conference))


df1_Qcat %>% ungroup() %>% count(Short)
# df1_Qcat_long <- pivot_longer(df1_Qcat, cols=c(Conference, Online), names_to = "Survey Type", values_to = "Count")
df1_Qcat <- df1_Qcat %>% mutate(Type = str_replace_all(Type, fixed("Conference"), "In-person"))

pal <- pnw_palette(name="Winter",n=2,type="discrete")

df1_Qcat %>% count(CMP_Category)
df1_Q4Q5 <- df1_Qcat %>% filter(grepl("threats", Question)) %>% filter(CMP_Category!="NA") %>%
  # ggplot(aes(fill=Type, x=fct_rev(CMP_Category), y=Sum)) +
  ggplot(aes(x = reorder(CMP_Category, Prop), y=Sum, fill=Type))+
  geom_bar(stat="identity", position="dodge", alpha=0.6, width=0.4) +
  scale_fill_manual(values = pal) +
  coord_flip() +
  xlab("") +
  ylab("Percent of Votes (%)") +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_blank()) +
  facet_wrap(~Short)

Cairo(file="df1_Q4Q5_hist_perc.PNG", 
      type="png",
      width=2200, 
      height=2000, 
      pointsize=12,
      bg="white",
      dpi=300)
df1_Q4Q5
dev.off()

df1_Qcat %>% filter(grepl("Actions|Obstacles", Short)) %>% filter(CMP_Category!="NA") %>% print(n=34) %>% select(Short, CMP_Category, Sum)

df1_Q3Q6 <- df1_Qcat %>% filter(grepl("Actions|Obstacles", Short)) %>% filter(CMP_Category!="NA") %>% filter(Sum!="NA") %>% 
  ggplot(aes(x = reorder(CMP_Category, Sum), y=Sum, fill=Type))+
  geom_bar(stat="identity", position="dodge", alpha=0.6, width=0.4) +
  scale_fill_manual(values = pal) +
  coord_flip() +
  xlab("") +
  ylab("Count of Responses") +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_blank()) +
  facet_wrap(~Short)

Cairo(file="df1_Q3Q6_hist.PNG", 
      type="png",
      width=2200, 
      height=2000, 
      pointsize=12,
      bg="white",
      dpi=300)
df1_Q3Q6
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
