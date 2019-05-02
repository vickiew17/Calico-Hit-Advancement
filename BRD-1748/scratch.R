library("pre")
airq <- airquality[complete.cases(airquality), ]
set.seed(42)
airq.ens <- pre(Ozone ~ ., data = airq)


d <- dplyr::rename(serexp.otherCERSexp.viab.aneu.19p.formodel,"CERS3AS1" = `CERS3-AS1`, "CERS6AS1" = `CERS6-AS1`) %>%
    dplyr::filter(!is.na(logviability))
viab.ens <- pre(logviability ~ ., data = d)
viab.ens
plot(viab.ens, nterms = 9, cex = 0.5)

importance(viab.ens,round = 4)
singleplot(viab.ens, varname = "CERS1")
#pairplot(viab.ens, varnames = c("CERS1","PSAT1"))
explain(viab.ens, newdata = d[1:4,], cex = .6)
corplot(viab.ens)

nullmods <- bsnullinteract(viab.ens)
int <- interact(viab.ens, nullmods = nullmods)

asdf <- bsnullinteract(airq.ens)
intasdf <- interact(airq.ens, nullmods = asdf)

#myData <- NULL
#myData <- int$fittedH2 %>% as.data.frame %>% set_colnames(c("mean")) %>% rownames_to_column(var = "gene")
myData <- int$nullH2 %>% dplyr::summarise_all(mean) %>% t %>%
    as.data.frame %>% 
    set_colnames(c("mean")) %>% 
    rownames_to_column(var = "gene")

myData$se <- int$nullH2 %>% dplyr::summarise_all(sd) %>% t
myData1 <- int$fittedH2 %>% as.data.frame %>% set_colnames(c("mean")) %>% rownames_to_column(var = "gene") %>% 
    dplyr::mutate(se = NA) %>% 
    rbind(myData) %>% dplyr::mutate(type = ifelse(is.na(se),"observed","null"))

myData <- myData1
limits <- aes(ymax = myData$mean + myData$se,
              ymin = myData$mean - myData$se)

p <- ggplot(data = myData, aes(x = factor(gene), y = mean,
                               fill = factor(type)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
    geom_errorbar(limits, position = position_dodge(0.9),
                  width = 0.25) #+
    labs(x = "No. Cylinders", y = "Miles Per Gallon") +
    ggtitle("Mileage by No. Cylinders\nand No. Gears") +
    scale_fill_discrete(name = "No. Gears")
