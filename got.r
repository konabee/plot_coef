## This code is written by Chia Liu on February 7th, 2023 ## 

library(tidyverse)
library(survival)
library(survminer)

## remember to run install.packages('whateverpackage') for whatever package you don't already have installed
## before you call the library function

data<-read.csv('C:/Users/ccl26/OneDrive - University of St Andrews/R_coef/clean_got.csv')

## How do we work with text data? A story for another time ##
# data %>% mutate(x=ifelse(grepl('^decapitat',dth_description,ignore.case=T)==T,1,0)) %>% filter(x==1)

## create survival object 
eha<-data %>% 
  mutate(event=dth_flag,
         dur=pmin(dth_time_hrs,censor_time_hrs),
         Survobj=Surv(dur,event))
fit<-survfit(Survobj~sex, data=eha)

ggsurvplot(fit,conf.int=T,xlab='Running time in hours',xlim=c(0,60),break.time.by=5)

c1<-coxph(Surv(dur,event)~sex,data=eha)
c2<-coxph(Surv(dur,event)~sex+social_status,data=eha)
c3<-coxph(Surv(dur,event)~sex+social_status+allegiance_switched,data=eha)
c4<-coxph(Surv(dur,event)~sex+social_status+allegiance_switched+allegiance_last,data=eha)
summary(c4)

## There are many ways to do this

## straight out of the box option: ggforest 
## only works for coxph class
## gives you the result in Hazard ratio, and already on log scale! 
## but not a lot of add on options. 
## You can try to tweak the function using trace(survminer::ggforest, edit = T) 
ggforest(c4,
         main='CoxPH hazard ratio of GoT deaths',
         fontsize=0.8,data=eha)

## but maybe we want to show coefficients of multiple models together
library(dotwhisker)
dwplot(list(c1,c2,c3),
       vline = geom_vline(
         xintercept = 1,
         colour = "grey60",
         linetype = 2)) %>%
         relabel_predictors(
         c(sexmale="Sex: Male",
           social_statuslowborn="Social status: Low born",
           allegiance_switchedyes="Switched allegiance"))+
  theme_bw(base_size=12)

## let's try another one 
library(sjPlot)
plot_models(c1,c2,c3,m.labels=c('Model 1','Model 2','Model 3'),vline.color = 'red')+
  theme_pubr()

## displayed in another way (less effective, in this case)
plot_models(c1,c2,c3,m.labels=c('Sex only','Sex & Status','Full model'),grid=T,
            vline.color='red',show.legend=F) +
  theme_bw()

## let's plot from scratch, using ggplot 
## The main ingredients we need: coefficient, lower & upper limit

## first, write a function to extract the information we need from the model objects
getm_f<-function(fit){
  a<-exp(fit$coefficients) %>% round(2)
  b<-exp(confint(fit)) %>% round(2)
  c<-data.frame(a,b) 
  colnames(c)<-c('coef','lower','upper')
  c$names <- rownames(c)
  c<-c[,c(4,1,2,3)]
  return(c)
}

## apply the function on fit c1 to c4 and add a column called model 
x1<-getm_f(c1) %>% mutate(model=1)
x2<-getm_f(c2) %>% mutate(model=2)
x3<-getm_f(c3) %>% mutate(model=3)
x4<-getm_f(c4) %>% mutate(model=4)

## stack them together so we're only dealing with one coefficient data frame
mydata<-rbind(x1,x2,x3,x4)

## make the labels a bit more readable
## this syntax specifically works with strings 
## but one can always use a series of if-else statements
data2<-mydata %>% 
  mutate(names=case_when(
    grepl('^sex',names)==T~'Male',
    grepl('^social_status',names)==T~'Lowborn',
    grepl('^allegiance_sw',names)==T~'Switched side',
    grepl('^allegiance_la',names)==T~substr(names,16,nchar(names)) 
  ))

## create 4 rows of reference categories 
names<-c('Female','Highborn','Did not switch side','Bolton')
coef<-1
lower<-1
upper<-1
model<-'ref'
ref<-data.frame(names,coef,lower,upper,model)

## stack the ref categories with our coefficient data frame 
## use factor to arrange the categories in the order we want them to appear in the plot
## otherwise it's alphabetic by default
data3<-data2 %>% 
  rbind(ref) %>% 
  mutate(model=ifelse(model=='ref','Reference',paste('Model',model)),
         names=factor(names,levels=c('Targaryen','Stark','Nights Watch','Lannister',
                                     'Greyjoy','Frey','Bolton','Switched side',
                                     'Did not switch side',
                                     'Lowborn','Highborn','Male','Female',
                                     'unknown','other')))

## making the plot 
fig1<-data3 %>% 
  filter(!names %in% c('unknown','other')) %>%
  ggplot(aes(x=names,y=coef,color=model))+
  geom_pointrange(mapping=aes(ymin=upper, ymax=lower), size=0.8, position=position_dodge(width=.9),alpha=.6)+
  geom_hline(yintercept = 1, color='darkred',linewidth=.8,na.rm=T)+
  scale_y_continuous(trans='log2',labels = scales::label_number(accuracy = 0.1), ## this converts the numeric scale to log scale 
                     breaks=seq(0,3,by=.5))+
  coord_flip()+
  labs(x = NULL, y = 'Hazard Ratio')+
  scale_colour_manual(values=c('forestgreen','pink2','gray70','dodgerblue','darkred'))+ ## use your fav colours here! 
  theme(panel.background = element_rect(fill='white',color='black'),
        axis.title.y=element_text(size=9),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(size=7),
        axis.text.y = element_text(face = rev(c('bold','plain','bold','plain','bold',
                                                'plain','bold','plain','plain',
                                                'plain','plain','plain','plain'))),
        text=element_text(family='arial',size=12),
        legend.background=element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.title=element_blank())

## if you want to save the plot, save it as an object (for example 'fig1') then ggsave fig1

ggsave(plot = fig1, width = 10, height = 4, dpi = 300, filename = "C:/Users/ccl26/OneDrive - University of St Andrews/R_coef/got_plot.png")



