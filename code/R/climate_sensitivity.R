read_csv("./trends/clim_sensitivity/results.csv") %>%
  mutate(WWF_MHTNAM= "Global")  %>%
  bind_rows(read_csv("./trends/clim_sensitivity/results.csv") %>%
            inner_join(read_csv("./tmp/ecoregions.csv") %>%
                            filter(!WWF_MHTNAM %in% c("Inland Water","Mangroves")) %>%
                            inner_join(read_csv("/home/marco/Dropbox/phenology/ecoregions/biomes.csv")) %>%
                            filter(is.na(out)))) %>%
  mutate(WWF_MHTNAM = factor(WWF_MHTNAM,
                        levels=c("Global","Tundra",
                        "Boreal Forests/Taiga",
                        "Temperate Conifer Forests",
                        "Temperate Broadleaf and Mixed Forests",
                        "Tropical and Subtropical Moist Broadleaf Forests",
                        "Tropical and Subtropical Dry Broadleaf Forests",
                        "Tropical and Subtropical Grasslands, Savannas and Shrublands")))->res


# bio1 comparison with all obs
(res %>%
  #filter(WWF_MHTNAM!="Global") %>%
  filter(term=="bio1") %>%
  filter(estimate > -50 & estimate < 50) %>%
  mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) |>
  ggplot(aes(x=estimate,fill=pcol))+
  geom_area(stat="bin",bins=30,alpha=.25,color="black",size=0.3)+
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))+
  theme(strip.text = element_text(size=15),legend.position = "none")) |>
  ggsave(filename = "./figures/paper/bio1_coefs_count.png",width = 13,height = 10,
       dpi = 400,bg = "white")

# bio1 only significant interactions
(res %>%
  #filter(WWF_MHTNAM!="Global") %>%
  filter(term=="bio1") %>%
  filter(estimate > -50 & estimate < 50) %>%
  filter(p.value < 0.05) %>%
  #mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) |>
  ggplot(aes(x=estimate,fill="red"))+geom_density(alpha=.25)+
  theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))+
  theme(strip.text = element_text(size=13),legend.position = "none")) |>
  ggsave(filename = "./figures/paper/bio1_coefs_dens.png",width = 13,height = 10,
       dpi = 400,bg = "white")
   

# bio12 comparison with all obs
(res %>%
    #filter(WWF_MHTNAM!="Global") %>%
    filter(term=="bio12") %>%
    filter(estimate > -50 & estimate < 50) %>%
    mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) |>
    ggplot(aes(x=estimate,fill=pcol))+
    geom_area(stat="bin",bins=30,alpha=.25,color="black",size=0.3)+
    scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
    theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))+
    theme(strip.text = element_text(size=15),legend.position = "none")) |>
  ggsave(filename = "./figures/paper/bio12_coefs_count.png",width = 13,height = 10,
         dpi = 400,bg = "white")

# bio12 only significant interactions
(res %>%
    #filter(WWF_MHTNAM!="Global") %>%
    filter(term=="bio12") %>%
    filter(estimate > -50 & estimate < 50) %>%
    filter(p.value < 0.05) %>%
    #mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) |>
    ggplot(aes(x=estimate,fill="red"))+geom_density(alpha=.25)+
    theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))+
    theme(strip.text = element_text(size=13),legend.position = "none")) |>
  ggsave(filename = "./figures/paper/bio12_coefs_dens.png",width = 13,height = 10,
         dpi = 400,bg = "white")


# bio4 cdomparison with all obs
(res %>%
    #filter(WWF_MHTNAM!="Global") %>%
    filter(term=="bio4") %>%
    filter(estimate > -50 & estimate < 50) %>%
    mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) |>
    ggplot(aes(x=estimate,fill=pcol))+
    geom_area(stat="bin",bins=30,alpha=.25,color="black",size=0.3)+
    scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
    theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))+
    theme(strip.text = element_text(size=15),legend.position = "none")) |>
  ggsave(filename = "./figures/paper/bio4_coefs_count.png",width = 13,height = 10,
         dpi = 400,bg = "white")

# bio4 only significant interactions
(res %>%
    #filter(WWF_MHTNAM!="Global") %>%
    filter(term=="bio4") %>%
    filter(estimate > -50 & estimate < 50) %>%
    filter(p.value < 0.05) %>%
    #mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) |>
    ggplot(aes(x=estimate,fill="red"))+geom_density(alpha=.25)+
    theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))+
    theme(strip.text = element_text(size=13),legend.position = "none")) |>
  ggsave(filename = "./figures/paper/bio4_coefs_dens.png",width = 13,height = 10,
         dpi = 400,bg = "white")


# bio15 cdomparison with all obs
(res %>%
    #filter(WWF_MHTNAM!="Global") %>%
    filter(term=="bio15") %>%
    filter(estimate > -50 & estimate < 50) %>%
    mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) |>
    ggplot(aes(x=estimate,fill=pcol))+
    geom_area(stat="bin",bins=30,alpha=.25,color="black",size=0.3)+
    scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
    theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))+
    theme(strip.text = element_text(size=15),legend.position = "none")) |>
  ggsave(filename = "./figures/paper/bio15_coefs_count.png",width = 13,height = 10,
         dpi = 400,bg = "white")

# bio15 only significant interactions
(res %>%
    #filter(WWF_MHTNAM!="Global") %>%
    filter(term=="bio15") %>%
    filter(estimate > -50 & estimate < 50) %>%
    filter(p.value < 0.05) %>%
    #mutate(pcol = ifelse(p.value < 0.05,"sig","nsig")) |>
    ggplot(aes(x=estimate,fill="red"))+geom_density(alpha=.25)+
    theme_minimal()+facet_wrap(~WWF_MHTNAM,scale="free",labeller = label_wrap_gen(width=30))+
    theme(strip.text = element_text(size=13),legend.position = "none")) |>
  ggsave(filename = "./figures/paper/bio15_coefs_dens.png",width = 13,height = 10,
         dpi = 400,bg = "white")