df <- data.frame(terms = c("cardiometabolic", "high\nblood\npressure", "pharmaco-\nepidemiology", 
                           "MACE", "prescribing\ncascades", "statins", 
                           "coronary\nartery\ndisease", "treatment-resistant\nhypertension", "hypertension", 
                           "blood\npressure", "patients", "dbp", 
                           "DOACs", "beta-\nblockers", "warfarin", 
                           "pharmacist", "SNRIs", "antihypertensive", 
                           "heart\nfailure", "treatment", "women", 
                           "plasma\nrenin\nactivity", "BP\nvariability"),
                 values = c(43, 112, 84, 
                            110, 48, 22,
                            38, 140, 160,
                            120, 24, 22,
                            42, 32, 50,
                            56, 14, 60,
                            42, 32, 39,
                            29, 32))

library(ggplot2)
library(packcircles)

df$packing <- circleProgressiveLayout(df$values, sizetype='area')

df.gg <- circleLayoutVertices(df$packing, npoints=50)

ggplot() + 
  geom_polygon(data = df.gg, aes(x, y, group = id, fill=id), alpha = 0.6)+
  scale_fill_viridis_c()+
  geom_text(data = df, aes(x=packing$x, y=packing$y, label = terms), size=5, color="black") +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()

ggsave("/Users/ssmithm/Downloads/cvmedlab_terms.png", width = 10, height = 7, units = "in")
