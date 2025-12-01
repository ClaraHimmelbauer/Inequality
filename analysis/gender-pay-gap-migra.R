# Gender Pay Gap nach Migrationsmerkmalen ---------------------------------------------------------

# 1. PREP -----------------------------------------------------------------------------------------
rm(list = ls()); gc()
packages <- c("tidyverse", "labelled", "haven")
sapply(packages, library, character.only = T)

# 2. DATA -----------------------------------------------------------------------------------------

psilc <- read_sav("data/p_silc2022_ext.sav")

# 3. WRANGLING ------------------------------------------------------------------------------------

# alter
# sex
# P038004 - einkommen aus haupterwerbstaetigkeit monatsbetrag netto
# P110000nu - geburtsland
# ln = laufnummer
# hgew = gewicht

dfx <- psilc %>% 
  filter(P038004 >= 0) %>% 
  mutate(at = ifelse(P110000nu == 1, 1, 0)) %>% 
  group_by(sex, at) %>% 
  reframe(meanek = weighted.mean(P038004, hgew)) %>% 
  mutate(Geschlecht = factor(sex, c(1,2), c("Männer", "Frauen")),
         Geburtsland = factor(at, c(1, 0), c("Geburtsland Österreich", "Geburtsland nicht Österreich")),
         lab = paste0("€ ", format(round(meanek), big.mark = " ")))

p <- ggplot(dfx, aes(x = Geschlecht, y = meanek, fill = Geschlecht)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = lab),
            position = position_dodge(),
            vjust = -0.5) +
  facet_wrap(~ Geburtsland) +
  
  scale_fill_manual(values = c("#d7b772", "#5a78c8")) +
  scale_y_continuous(labels = scales::label_currency(prefix = "€"),
                     expand = expansion(mult = c(0, 0.15))) +
  
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  
  labs(title = "Nettomonatseinkommen nach Geschlecht und Geburtsland",
       subtitle = "Daten: EU-SILC 2022, eigene Darstellung")
p
ggsave("figures/intersektionalitaet/silc22_genderpaygap-migration.png",
       p,
       width = 2560,
       height = 1440,
       unit = "px",
       dpi = 400,
       bg = "white")
