library(ggplot2)


gg <- (ggplot(mppred,aes(Diffdays, color=Diagnosis, alpha=Pandemic))
       + facet_wrap(Type~Diagnosis, scale="free_y")
       + geom_jitter(aes(y=dens))
       + geom_line(aes(y=pred))
       + scale_color_manual(values = c("red","dark green","black"))
       + scale_alpha_manual(values = c(0.3,1))
       + theme_bw()
       + theme(legend.position = "bottom")
)

print(gg)

print(gg + scale_y_log10())
