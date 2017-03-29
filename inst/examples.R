library(magrittr)
library(wrangle)

data(events)
x <- events

#' Step 0.  Rename columns to remove semantic (non-syntactic) underscores.
#' Step 1.  De-interlace the data. 
x %<>% filter(CMT == 2) %>% select(-EVID,-CMT,-AMT)


#' Step 2.  Describe the (super) key. (Enforce as necessary).
x %<>% group_by(USUBJID, TIME)
x %>% status

#' Step 3.  Supply metadata as values or factors. You can hardcode these or merge from some other source.

x %<>% mutate(
         ID_LABEL = 'NONMEM ID',
          C_LABEL = 'comment flag',
    USUBJID_LABEL = 'universal subject identifier',
       TIME_LABEL = 'time',
         DV_LABEL = 'parent drug',
        BLQ_LABEL = 'below limit of quantitation',
       LLOQ_LABEL = 'lower limit of quantitation',
        TAD_LABEL = 'time since most recent dose',
        SEX_LABEL = 'sex',
         WT_LABEL = 'weight',
       PRED_LABEL = 'population prediction'
)
x %<>% mutate(
         C_GUIDE = factor(
        C,levels = c(NA,'C'),
          labels = c('not commented','commented'),
        exclude = NULL),
      TIME_GUIDE = 'h',
        DV_GUIDE = 'ng/mL',
       BLQ_GUIDE = factor(
      BLQ,levels = 0:1,
          labels = c('not quantifiable','quantifiable')),
      LLOQ_GUIDE = 'ng/mL',
       TAD_GUIDE = 'h',
       SEX_GUIDE = factor(
      SEX,levels = 0:1,
          labels = c('female','male')),
        WT_GUIDE = 'kg',
      PRED_GUIDE = 'ng/mL'
)

#' Step 4. Fold and unfold your data.
x %>% fold
x %>% fold %>% unfold
x %>% fold %>% unfold %>% fold(USUBJID,TIME) 
x %>% fold %>% unfold(PRED,TIME,WT)












