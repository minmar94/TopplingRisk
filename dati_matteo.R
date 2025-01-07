################
# dati matteo  #
################

library(xtable)
dati_matteo <- read.table("dati_matteo.txt")
summary(dati_matteo)
bti.mod <- glm(bti.rib ~ bti.x + bti.y +bti.x:bti.y , data=dati_matteo, family = "binomial" )
xtable(summary(bti.mod))

fti.mod <- glm(fti.rib ~ fti.x + fti.y +fti.x:fti.y , data=dati_matteo, family = "binomial" )
xtable(summary(fti.mod))

s <- seq(0,2.5, by=0.01)
d <- seq(0,1, by=0.01)


#library(mgcv)
#modd <- gam(ribaltamento ~ te(X,Y), data=dati_matteo, family = "binomial" , method="REML")
#modd <- gam(ribaltamento ~ s(X)+s(Y),method="REML", data=dati_matteo, family = "binomial" )

bti.pred <- bti.pred.se <- fti.pred <- fti.pred.se  <-
  array(dim=c(length(s),length(d)))

i=0
for(x in s){
  print(i)
  i=i+1
  j=0
  for(y in d){
    j= j+1
    p <- predict(bti.mod, se.fit=T ,newdata = data.frame(bti.x=x,bti.y=y),type="link")
    bti.pred[i,j] <- p$fit
    bti.pred.se[i,j] <- p$se.fit
    p <- predict(fti.mod, se.fit=T ,newdata = data.frame(fti.x=x,fti.y=y),type="link")
    fti.pred[i,j] <- p$fit
    fti.pred.se[i,j] <- p$se.fit
    
  }
}

filled.contour(s, d, bti.pred,
               plot.axes = { axis(1); axis(2); points(dati_matteo$bti.x, dati_matteo$bti.y,pch=21, bg=dati_matteo$bti.rib, xlab="speed", ylab="depth" )}, main="BTI: log p/(1-p)")

filled.contour(s, d, bti.pred.se,
               plot.axes = { axis(1); axis(2); points(dati_matteo$bti.x, dati_matteo$bti.y,pch=21, bg=dati_matteo$bti.rib) },main="BTI: st.er. log p/(1-p)"
)

filled.contour(s, d, fti.pred,
               plot.axes = { axis(1); axis(2); points(dati_matteo$fti.x, dati_matteo$fti.y,pch=21, bg=dati_matteo$fti.rib) }, main="FTI: log p/(1-p)")

filled.contour(s, d, fti.pred.se,
               plot.axes = { axis(1); axis(2); points(dati_matteo$fti.x, dati_matteo$fti.y,pch=21, bg=dati_matteo$fti.rib) },main="FTI: st.er. log p/(1-p)"
)

##########################
# probs
##########################

bti.predp <- bti.pred.sep <- fti.predp <- fti.pred.sep  <-
  array(dim=c(length(s),length(d)))

i=0
for(x in s){
  print(i)
  i=i+1
  j=0
  for(y in d){
    j= j+1
    p <- predict(bti.mod, se.fit=T ,newdata = data.frame(bti.x=x,bti.y=y),type="response")
    bti.predp[i,j] <- p$fit
    bti.pred.sep[i,j] <- p$se.fit
    p <- predict(fti.mod, se.fit=T ,newdata = data.frame(fti.x=x,fti.y=y),type="response")
    fti.predp[i,j] <- p$fit
    fti.pred.sep[i,j] <- p$se.fit
    
  }
}

filled.contour(s, d, bti.predp,
               plot.axes = { axis(1); axis(2); points(dati_matteo$bti.x, dati_matteo$bti.y,pch=21, bg=dati_matteo$bti.rib) }, main="BTI: p")

filled.contour(s, d, bti.pred.sep,
               plot.axes = { axis(1); axis(2); points(dati_matteo$bti.x, dati_matteo$bti.y,pch=21, bg=dati_matteo$bti.rib) },main="BTI: st.er p"
)

filled.contour(s, d, fti.predp,
               plot.axes = { axis(1); axis(2); points(dati_matteo$fti.x, dati_matteo$fti.y,pch=21, bg=dati_matteo$fti.rib) }, main="FTI: p")

filled.contour(s, d, fti.pred.sep,
               plot.axes = { axis(1); axis(2); points(dati_matteo$fti.x, dati_matteo$fti.y,pch=21, bg=dati_matteo$fti.rib) },main="FTI: st.er. p"
)


plot(s, bti.predp[,20], type="l" ,ylim=c(0,1),xlab="speed",ylab="prob",lwd=2)
col=0
for(i in seq(20,100,by=20)){
  col=col+1
  lines(s, bti.predp[,i], type="l",col=col, lwd=2)  
}
legend("bottomright",lty=rep(1,5),
       lwd=rep(2,5), col=rep(1:5),
       legend=d[seq(20,100,by=20)],
       title="depth")

plot(d, bti.predp[20,], type="l" ,ylim=c(0,1),xlab="depth",ylab="prob",lwd=2)
col=0
for(i in seq(20,240,by=50)){
  col=col+1
  lines(d, bti.predp[i,], type="l",col=col, lwd=2)  
}
legend("topleft",lty=rep(1,5),
       lwd=rep(2,5), col=rep(1:5),
       legend=s[seq(20,240,by=50)],
       title="speed")






names(dati_matteo_incerti) <-c("X","Y", "ribaltamento")
dati_matteo_incerti$incert_pred <- predict(mod,newdata = dati_matteo_incerti,  type="response")

plot(dati_matteo_incerti$incert_pred ~ as.factor(dati_matteo_incerti$ribaltamento))

plot(dati_matteo_check$X,dati_matteo_check$Y,type="n",xlim=c(0.6,2))
text(dati_matteo_check$X,dati_matteo_check$Y,label=dati_matteo_check$obs)
text(dati_matteo_check$X+0.1,dati_matteo_check$Y,label=dati_matteo_check$pred,col=2)

#########################
# estrapolazione su 
# inondazione simulata
#########################

write.table(extraction_skipping_6_time297,"dat_time297.txt", sep="\t")
write.table(extraction_skipping_6_time99,"dat_time99.txt", sep="\t")
write.table(extraction_skipping_6_time0,"dat_time0.txt", sep="\t")

dat_time297 <- read.table("dat_time297.txt")
names(dat_time297) <- c("x","y", "bti.y","bti.x", "block")
pred_time297 <- predict(bti.mod,  newdata = dat_time297[dat_time297$block==0,], type="response")
pred_time297.fti <- predict(fti.mod,  newdata = dat_time297[dat_time297$block==0,], type="response")

dat_time99 <- read.table("dat_time99.txt")
names(dat_time99) <- c("x","y", "bti.y","bti.x", "block")
pred_time99 <- predict(bti.mod,  newdata = dat_time99[dat_time99$block==0,], type="response")

dat_time0 <- read.table("dat_time0.txt")
names(dat_time0) <- c("x","y", "bti.y","bti.x", "block")
pred_time0 <- predict(bti.mod,  newdata = dat_time0[dat_time0$block==0,], type="response")

library(tidyverse)
dat <- as.data.frame(cbind(dat_time297$x[dat_time297$block==0],dat_time297$y[dat_time297$block==0],pred_time297))
names(dat)[1:2] <- c("x","y")

dat <- as.data.frame(cbind(dat_time99$x[dat_time99$block==0],dat_time99$y[dat_time99$block==0],pred_time99))
names(dat)[1:2] <- c("x","y")

dat <- as.data.frame(cbind(dat_time0$x[dat_time0$block==0],dat_time0$y[dat_time0$block==0],pred_time0))
names(dat)[1:2] <- c("x","y")

dat %>% 
  pivot_longer(contains("pred"),
               names_to = "par", values_to = "value") %>%
  mutate(par = factor(par, levels = c("pred_time0", "pred_time99", "pred_time297"))) %>% 
  # filter(par != "pred_time0") %>% 
  ggplot(aes(x, y, fill = log(value/(1-value)))) +
  geom_raster() +
  scale_fill_viridis_c(option = "inferno", na.value = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "longitudine", fill = "logit(p)") +
  facet_wrap(~par, nrow = 1, scales = "free") +
  theme_bw() +
  theme(legend.position = "top")


dat_time99 %>% 
  pivot_longer(contains("bti"),
               names_to = "par", values_to = "value") %>% 
  filter(par == "bti.x") %>% 
  ggplot(aes(x, y, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "longitudine") +
  facet_wrap(~par, nrow = 1, scales = "free") +
  theme_bw() +
  theme(legend.position = "top")

library(magrittr)

dat %<>% 
  left_join(dat_time0 %>% rename(v0 = bti.x, h0 = bti.y),by=c("x","y")) %>% 
  left_join(dat_time99 %>% rename(v99 = bti.x, h99 = bti.y),by=c("x","y")) %>% 
  left_join(dat_time297 %>% rename(v297 = bti.x, h297 = bti.y),by=c("x","y"))

dat %>% 
  mutate(check=bti.x.x+bti.x.y+bti.y.x+bti.y.y+bti.x+bti.y) %>% 
  ggplot(aes(x,y,color=factor(check==0)))+geom_point()


#########################
# nuovi dati paper 2021 #
#########################

#extraction_skip_6_paper2021_levee <- read_excel("extraction_skip_6_paper2021_levee.xlsx", col_names = FALSE)
time500 <- extraction_skip_6_paper2021_levee
time1800 <- extraction_skip_6_paper2021_levee
time3600 <- extraction_skip_6_paper2021_levee
time7200 <- extraction_skip_6_paper2021_levee
names(time500) <- names(time1800) <- names(time3600) <- names(time7200) <- c("x","y", "speed", "deep", "block")

write.table(time500, "time500.txt", sep="\t")
write.table(time1800, "time1800.txt", sep="\t")
write.table(time3600, "time3600.txt", sep="\t")
write.table(time7200, "time7200.txt", sep="\t")

time500 <- read.table("time500.txt")
time1800 <- read.table("time1800.txt")
time3600 <- read.table("time3600.txt")
time7200 <- read.table("time7200.txt")

pred_time500 <- predict(bti.mod,  newdata = time500[time500$block==0,], type="response", se.fit=T )
pred_time1800 <- predict(bti.mod,  newdata = time1800[time1800$block==0,], type="response", se.fit=T)
pred_time3600 <- predict(bti.mod,  newdata = time3600[time3600$block==0,], type="response", se.fit=T)
pred_time7200 <- predict(bti.mod,  newdata = time7200[time7200$block==0,], type="response", se.fit=T)

pred_time500 <- predict(bti.mod,  newdata = time500[time500$block==0,], type="link", se.fit=T )
pred_time1800 <- predict(bti.mod,  newdata = time1800[time1800$block==0,], type="link", se.fit=T)
pred_time3600 <- predict(bti.mod,  newdata = time3600[time3600$block==0,], type="link", se.fit=T)
pred_time7200 <- predict(bti.mod,  newdata = time7200[time7200$block==0,], type="link", se.fit=T)

pred_time500.class <- factor(cut(pred_time500$fit, seq(0,1,by=0.25)))
pred_time1800.class <- factor(cut(pred_time1800$fit, seq(0,1,by=0.25)))
pred_time3600.class <- factor(cut(pred_time3600$fit, seq(0,1,by=0.25)))
pred_time7200.class <- factor(cut(pred_time7200$fit, seq(0,1,by=0.25)))

pred_time500.class <- factor(cut(pred_time500$fit, seq(-70,210,by=50)))
pred_time1800.class <- factor(cut(pred_time1800$fit, seq(-70,210,by=50)))
pred_time3600.class <- factor(cut(pred_time3600$fit, seq(-70,210,by=50)))
pred_time7200.class <- factor(cut(pred_time7200$fit, seq(-70,210,by=50)))

library(tidyverse)
library(magrittr)

p_fit <- bind_rows(time500[time500$block==0,] %>% mutate(pred=pred_time500$fit,type=500, se=pred_time500$se.fit),
          time1800[time1800$block==0,] %>% mutate(pred=pred_time1800$fit,type=1800,  se=pred_time1800$se.fit),
          time3600[time3600$block==0,] %>% mutate(pred=pred_time3600$fit,type=3600,  se=pred_time3600$se.fit),
          time7200[time7200$block==0,] %>% mutate(pred=pred_time7200$fit,type=7200,  se=pred_time7200$se.fit) )  %>% 
  # pivot_longer(c("pred", "se"),
  #              names_to = "par", values_to = "value") %>%
  # mutate(par = factor(par, levels = c("pred_time0", "pred_time99", "pred_time297"))) %>% 
  # filter(par != "pred_time0") %>% 
  #ggplot(aes(x, y, fill = log(pred/(1-pred)))) +
  ggplot(aes(x, y, fill = pred)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno" ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "longitude", fill = "prob (logit scale)", y = "latitude") +
  facet_wrap(~type, scales = "free", nrow = 1) +
  theme_bw() +
  theme(legend.position = "top")

p_fit

p_se <- bind_rows(time500[time500$block==0,] %>% mutate(pred=pred_time500$fit,type=500, se=pred_time500$se.fit),
          time1800[time1800$block==0,] %>% mutate(pred=pred_time1800$fit,type=1800,  se=pred_time1800$se.fit),
          time3600[time3600$block==0,] %>% mutate(pred=pred_time3600$fit,type=3600,  se=pred_time3600$se.fit),
          time7200[time7200$block==0,] %>% mutate(pred=pred_time7200$fit,type=7200,  se=pred_time7200$se.fit) )  %>% 
  # pivot_longer(c("pred", "se"),
  #              names_to = "par", values_to = "value") %>%
  # mutate(par = factor(par, levels = c("pred_time0", "pred_time99", "pred_time297"))) %>% 
  # filter(par != "pred_time0") %>% 
  #ggplot(aes(x, y, fill = log(pred/(1-pred)))) +
  ggplot(aes(x, y, fill = se)) +
  geom_raster(interpolate=T) +
  scale_fill_viridis_c(option = "inferno" ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "longitudine", fill = "se", y = "latitude") +
  facet_wrap(~type, scales = "free", nrow = 1) +
  theme_bw() +
  theme(legend.position = "top")

gridExtra::grid.arrange(p_fit, p_se, nrow = 2)


p_fit.class <- bind_rows(time500[time500$block==0,] %>% mutate(pred=pred_time500.class,type=500),
                   time1800[time1800$block==0,] %>% mutate(pred=pred_time1800.class,type=1800),
                   time3600[time3600$block==0,] %>% mutate(pred=pred_time3600.class,type=3600),
                   time7200[time7200$block==0,] %>% mutate(pred=pred_time7200.class,type=7200) )  %>% 
  # pivot_longer(c("pred", "se"),
  #              names_to = "par", values_to = "value") %>%
  # mutate(par = factor(par, levels = c("pred_time0", "pred_time99", "pred_time297"))) %>% 
  # filter(par != "pred_time0") %>% 
  #ggplot(aes(x, y, fill = log(pred/(1-pred)))) +
  ggplot(aes(x, y, fill = pred)) +
  geom_tile() +
  scale_fill_viridis_d(option = "inferno" ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "longitude", fill = "probability (logit scale)", y = "latitude") +
  facet_wrap(~type, scales = "free", nrow = 1) +
  theme_bw() +
  theme(legend.position = "top")

p_fit.class

gridExtra::grid.arrange(p_fit.class, nrow = 1)
