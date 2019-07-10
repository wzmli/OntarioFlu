library(ggplot2)

gg <- (ggplot(gammapred,aes(Diffdays, alpha=Pandemic, color=Diagnosis))
       + facet_wrap(Type~Diagnosis, scale="free_y")
       + geom_jitter(aes(y=dens))
       + geom_line(aes(y=pred))
       + scale_color_manual(values = c("red","dark green","black"))
       + scale_alpha_manual(values = c(0.3,1))
       + theme_bw()
       + theme(legend.position = "bottom"
          # , strip.background.x = element_blank()
          # , strip.text.x = element_blank()
       )
)

print(gg)

print(gg + scale_y_log10())
