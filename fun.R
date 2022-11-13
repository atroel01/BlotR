#Main extractor function
extract_blot_data<-function(name, file_location){
   
    pdf<-name
    location<-file_location
    
    txt <- pdf_text(file_location) %>% str_squish()

    #Extraact blot type
    Blot_Type <- txt %>% str_extract("(?<=Test: ).{50}")

    #Extract identifiers
    ptname <- txt %>% str_extract("(?<=Patient name: ).+(?= Date of birth:)")
    DOB <- txt %>% str_extract("(?<= Date of birth:).+(?=Lab number:)")
    labno <- txt %>% str_extract("(?<=Lab number:).+(?=Strip number)")
    created<-txt %>% str_extract("(?<=Created on: ).{10}")
    incub <- txt %>% str_extract("(?<=Date of incubation: ).{10}")
    # stripno <- txt %>% str_extract("(?<=Strip number:).+(?=Antigen Intensity)")
    ptid<- txt %>% str_extract("(?<=Patient ID:).+(?=Test:)")

    #myositis blot values
    mi2a <- txt %>% str_extract("(?<=\\(Mi-2a\\) )[[:digit:]-]*")
    mi2b <- txt %>% str_extract("(?<=\\(Mi-2b\\) )[[:digit:]-]*")
    mi2b_old <- txt %>% str_extract("(?<=\\(Mi-2\\) )[[:digit:]-]*")
    tif1g <- txt %>% str_extract("(?<=\\(TIF1g\\) )[[:digit:]-]*")
    mda5 <- txt %>% str_extract("(?<=\\(MDA5\\) )[[:digit:]-]*")
    nxp2 <- txt %>% str_extract("(?<=\\(NXP2\\) )[[:digit:]-]*")
    sae1 <- txt %>% str_extract("(?<=\\(SAE1\\) )[[:digit:]-]*")
    ku <- txt %>% str_extract("(?<=\\(Ku\\) )[[:digit:]-]*")
    pm100 <- txt %>% str_extract("(?<=\\(PM100\\) )[[:digit:]-]*")
    pm75 <- txt %>% str_extract("(?<=\\(PM75\\) )[[:digit:]-]*")
    
    jo1<-if_else(str_detect(txt, "\\(Jo\\)"),
                  str_extract(txt,"(?<=\\(Jo\\) )[[:digit:]-]*"),
                  str_extract(txt,"(?<=\\(Jo-1\\) )[[:digit:]-]*"))
    
    srp <- txt %>% str_extract("(?<=\\(SRP\\) )[[:digit:]-]*")
    pl7 <- txt %>% str_extract("(?<=\\(PL-7\\) )[[:digit:]-]*")
    pl12 <- txt %>% str_extract("(?<=\\(PL-12\\) )[[:digit:]-]*")
    ej <- txt %>% str_extract("(?<=\\(EJ\\) )[[:digit:]-]*")
    oj <- txt %>% str_extract("(?<=\\(OJ\\) )[[:digit:]-]*")

    ro52<-if_else(str_detect(txt, "Ro52"),
                  str_extract(txt,"(?<=\\(Ro52\\) )[[:digit:]-]*"),
                  str_extract(txt,"(?<=\\(52\\) )[[:digit:]-]*"))
    ro52<-ifelse(is.na(ro52),
                 str_extract(txt,"(?<=\\(Ro-52\\) )[[:digit:]-]*"),
                 ro52)
    #ro52 <- txt %>% str_extract("(?<=\\(Ro52\\) )[[:digit:]-]*")
    #ro52 <- txt %>% str_extract("(?<=\\(52\\) )[[:digit:]-]*")

    #scleroderma blot values
    scl <- txt %>% str_extract("(?<=\\(Scl-70\\) )[[:digit:]-]*")
    cenpa <- txt %>% str_extract("(?<=\\(CA\\) )[[:digit:]-]*")
    cenpb <- txt %>% str_extract("(?<=\\(CB\\) )[[:digit:]-]*")
    rp11 <- txt %>% str_extract("(?<=\\(RP11\\) )[[:digit:]-]*")
    rp155 <- txt %>% str_extract("(?<=\\(RP155\\) )[[:digit:]-]*")
    fib <- txt %>% str_extract("(?<=\\(Fib\\) )[[:digit:]-]*")
    nor90 <- txt %>% str_extract("(?<=\\(NOR90\\) )[[:digit:]-]*")
    thto <- txt %>% str_extract("(?<=\\(Th/To\\) )[[:digit:]-]*")
    pdgfr <- txt %>% str_extract("(?<=\\(PDGFR\\) )[[:digit:]-]*")

    #EIB blot values
    rnpsm <- txt %>% str_extract("(?<=\\(RNP/Sm\\) )[[:digit:]-]*")
    sm <- txt %>% str_extract("(?<=Sm \\(Sm\\) )[[:digit:]-]*")
    ro60 <- txt %>% str_extract("(?<=\\(SSA\\) )[[:digit:]-]*")
    ssb <- txt %>% str_extract("(?<=\\(SSB\\) )[[:digit:]-]*")
    pcna <- txt %>% str_extract("(?<=\\(PCNA\\) )[[:digit:]-]*")
    dna <- txt %>% str_extract("(?<=\\(DNA\\) )[[:digit:]-]*")
    nuc <- txt %>% str_extract("(?<=\\(NUC\\) )[[:digit:]-]*")
    his <- txt %>% str_extract("(?<=\\(HI\\) )[[:digit:]-]*")
    ribp <- txt %>% str_extract("(?<=\\(RIB\\) )[[:digit:]-]*")
    ama <- txt %>% str_extract("(?<=\\(M2\\) )[[:digit:]-]*")
    dfs <- txt %>% str_extract("(?<=\\(DFS70\\) )[[:digit:]-]*")

    #EIB5 specific
    rnp70<- txt %>% str_extract("(?<=\\(70\\) )[[:digit:]-]*")
    rnpa<- txt %>% str_extract("(?<=\\(A\\) )[[:digit:]-]*")
    rnpc<- txt %>% str_extract("(?<=\\(C\\) )[[:digit:]-]*")

    #Liver blot
    bpo<-txt %>% str_extract("(?<=\\(M2-3E\\) )[[:digit:]-]*")
    sp100<-txt %>% str_extract("(?<=\\(Sp100\\) )[[:digit:]-]*")
    pml<-txt %>% str_extract("(?<=\\(PML\\) )[[:digit:]-]*")
    gp210<-txt %>% str_extract("(?<=\\(gp210\\) )[[:digit:]-]*")
    lkm<-txt %>% str_extract("(?<=\\(LKM-1\\) )[[:digit:]-]*")
    lc1<-txt %>% str_extract("(?<=\\(LC-1\\) )[[:digit:]-]*")
    sla<- txt %>% str_extract("(?<=\\(SLA/LP\\) )[[:digit:]-]*")

    #Neuronal blot values
    amp<- txt %>% str_extract("(?<=\\(Amp\\) )[[:digit:]-]*")
    cv2<- txt %>% str_extract("(?<=\\(CV2\\) )[[:digit:]-]*")
    ma2<- txt %>% str_extract("(?<=\\(Ma2/Ta\\) )[[:digit:]-]*")
    ri<- txt %>% str_extract("(?<=\\(Ri\\) )[[:digit:]-]*")
    yo<- txt %>% str_extract("(?<=\\(Yo\\) )[[:digit:]-]*")
    hu<- txt %>% str_extract("(?<=\\(Hu\\) )[[:digit:]-]*")

    #control values
    co<-if_else(str_detect(txt, "\\(Co\\)"),
                  str_extract(txt,"(?<=\\(Co\\) )[[:digit:]-]*"),
                  str_extract(txt,"(?<=\\(Ko\\) )[[:digit:]-]*"))

    #Aggregate and append to total results
    blot_result<-tibble(
      pdf = pdf,
      location = location,
      Blot = Blot_Type,
      Name = ptname,
      DOB = dmy(DOB),
      Lab.Number = labno,
      # Strip = stripno,
      Pt.ID = ptid,
      Incub.Date = dmy(incub),
      Created = dmy(created),
      Mi2a = mi2a,
      Mi2b = mi2b,
      TIF1g = tif1g,
      MDA5 = mda5,
      NXP2 = nxp2,
      SAE1 = sae1,
      Ku = ku,
      PM100 = pm100,
      PM75 = pm75,
      Jo1 = jo1,
      SRP = srp,
      PL7 = pl7,
      PL12 = pl12,
      EJ = ej,
      OJ = oj,
      Ro52 = ro52,
      Scl70 = scl,
      CentA = cenpa,
      CentB = cenpb,
      RP3.11 = rp11,
      RP3.155 = rp155,
      Fibrillarin = fib,
      Nor90 = nor90,
      `Th/To` = thto,
      PDGFR = pdgfr,
      `RNP/Sm` = rnpsm,
      RNP70 = rnp70,
      RNPA = rnpa,
      RNPC = rnpc,
      Sm = sm,
      Ro60 = ro60,
      La = ssb,
      PCNA = pcna,
      DNA = dna,
      Nucleosomes = nuc,
      Histones = his,
      RiboP = ribp,
      AMA = ama,
      DFS70 = dfs,
      BPO = bpo,
      Sp100 = sp100,
      PML= pml,
      Gp210 = gp210,
      `LKM-1` = lkm,
      LC1 = lc1,
      SLA= sla,
      Amphi = amp,
      CV2 = cv2,
      Ma2 = ma2,
      Ri = ri,
      Yo = yo,
      Hu = hu,
      Control = co
    )

  return(blot_result)
}
#Multiscanner strip paper Green detector wrapper function

#green strip detector
green_calculator<-function(file_location){
  pdf_convert(
    pdf = file_location,
    format = "png",
    antialias = F,
    dpi = 70,
    pages = 1,filenames = "temporary_pdf_image.png",verbose = F
    )
  
 im_rgb_raw<- image_read("temporary_pdf_image.png") %>% 
   image_data(channels = "rgb")
 
 im_rgb <- list(
   r = as.integer(im_rgb_raw[1,,]),
   g = as.integer(im_rgb_raw[2,,]),
   b = as.integer(im_rgb_raw[3,,])
 )
 
 im_rgb_means <- list(
   r = mean(im_rgb$r),
   g = mean(im_rgb$g),
   b = mean(im_rgb$b)
 )
 
  greenness <- (
   im_rgb_means$g - mean(
     c(
       im_rgb_means$r, 
       im_rgb_means$g, 
       im_rgb_means$b)
     )) / abs(im_rgb_means$r - im_rgb_means$b)
 
  file.remove("temporary_pdf_image.png")

  return(greenness)
}

#Turn CSV string to vector 
csv_to_filter <- function(filter_expressions){
  filter_terms<-toString(filter_expressions) %>%  StrSplit(split = ",") %>% str_squish()

  return(filter_terms)
}

#Filter to exclude values
exclude_filter <- function(blot_results,filter_terms){
  filtered_blot_results<-blot_results
  for (i in 1:length(filter_terms)) {
    filtered_blot_results<-filtered_blot_results %>% 
      filter(
        !str_detect(Name, filter_terms[i])
      )
  }
  return(filtered_blot_results)
}

#Filter to specify which values are qcs
qc_filter <- function(blot_results, filter_terms){
  for (i in 1:length(filter_terms)) {
    blot_results<-blot_results %>% 
      mutate(qc_sample = case_when(
        str_detect(Name, filter_terms[i]) ~ TRUE,
        TRUE ~ FALSE
        )
      )
  }
  return(blot_results)
}

#Other functions
signif_2<-function(x){
  y<- signif(x, digits = 2)
  return(y)
}

#Repeated Graph output
repeated_graph<-function(processed_data){
  processed_data_long<-processed_data %>% 
    as_tibble() %>% 
    pivot_longer(
      cols = Mi2a:Control,
      names_to = "antigen",
      values_to = "band_intensity",
      values_drop_na = T
    ) %>%
    mutate(
      scanner = "BlotOne",
      result = case_when(
        band_intensity >10 & scanner == "FlatBed" ~ "+",
        band_intensity >14 & scanner == "BlotOne" ~ "+",
        band_intensity >25 & scanner == "FlatBed" ~ "++",
        band_intensity >34 & scanner == "BlotOne" ~ "++",
        band_intensity >50 & scanner == "FlatBed" ~ "+++",
        band_intensity >69 & scanner == "BlotOne" ~ "+++",
        TRUE ~ "Negative"
      ),
      result_binary = case_when(
        result == "Negative" ~ 0,
        TRUE ~ 1
      )
    ) %>%
    type_convert() %>% 
    separate(Name,into = c("name1", "name2", "name3"), sep = " ", extra = "merge",remove = F) %>%
    group_by(name1, name2, DOB, antigen) %>% 
    filter(n()>1) %>% 
    mutate(
      run_date = case_when(
        !is.na(Incub.Date) ~ as_date(Incub.Date),
        TRUE ~ as_date(Created)
      )) %>% 
    arrange(run_date) %>%
    mutate(time_point = row_number(),
           index_time = run_date[1],
           assay_time = as.numeric(run_date -index_time),
           change = case_when(
             sum(result_binary) == max(time_point) ~ "Remained Positive",
             sum(result_binary) == 0 ~ "Remained Negative",
             TRUE ~ "Changed")) 

  if (str_detect(toString(processed_data_long$change), "Changed")) {
    
    gg_repeated<- processed_data_long %>% 
      group_by(name1, name2, DOB, antigen) %>% 
      filter(!sum(assay_time)<14) %>% 
      ungroup() %>% 
      group_by(antigen,change) %>% 
      summarise(n= n()) %>% 
      group_by(antigen) %>% 
      mutate(
        prop = n/sum(n) %>% signif(digits = 2),
        total = sum(n)
      ) %>% 
      select(antigen, change,prop,total,) %>% 
      pivot_wider(
        names_from = change,
        values_from = prop,
        values_fill = 0,
        values_fn = signif_2) %>% 
      filter(!is.na(Changed)) %>% 
      mutate(
        antigen_name = paste0(antigen, " - ", Changed*100, "%")
      ) %>% 
      select(antigen, antigen_name) %>% 
      right_join(processed_data_long)%>% 
      filter(change == "Changed", !sum(assay_time)<14, antigen != "Control") %>% 
      select(Name, DOB, time_point, assay_time, band_intensity, change,antigen_name) %>%
      ggplot()+
      aes(x = assay_time, y = band_intensity, group = Name, color = change)+
      geom_line(size =2,alpha = 0.6)+
      geom_line(inherit.aes = T,color = "black")+
      facet_wrap(vars(antigen_name),drop = T)+
      theme_pubr()+
      xlab("Days since first blot")+
      ylab("Raw Band Intensity")+
      scale_color_met_d("Hiroshige")+
      guides(color = guide_legend(title = ""))+
      labs(
        title = "Positive Bands - Change over time",
        caption =  "Removed values which remained consistently negative and results repeated within 14 days (spurious changes)"
      )+
      theme(
        axis.text = element_text(size = 7),
        strip.text = element_text(size = 7),
        panel.grid.major.y = element_line(size = 1,linetype = "dashed"))
    
    return(gg_repeated)
  }else{
    print("No repeated values changed")
    return()}

}
