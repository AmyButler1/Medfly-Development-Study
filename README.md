# Medfly-Development-Study

# The effects of dietary adaptation to life history traits in the Mediterranean fruit fly (Ceratitis capitata)

## Abstract:
### Nutritional variation influences many traits including developmental times and adult phenotypes, including body mass. Responses to dietary variation is key to the evolution of dietary specialism and thus, speciation. This study assessed the effects of proximate diet versus the effects from evolved diet-mediated divergence between medflies reared on a low-calorie Starch larval diet and a high-calorie ASG larval diet. This study built on findings from earlier generations of the same fly lines by Leftwich et al. in 2019, to see whether new adaptations were shown. Different nutritional environments were found to cause divergence between the fly lines. Body mass, egg number and pupal showed evidence of diet-mediated adaptation. Development times and egg volume exhibited divergence between lines. A trade-off between egg number and egg volume was observed. Flies long reared on Starch were shown to have adapted to minimise negative effects of their low-calorie diet, benefitting from: faster development time and higher pupal survival. This furthers understanding of medfly trait plasticity, which could help improve mass-rearing programmes, eradicating populations of the agricultural pest. Understanding medfly adaptation to their nutritional environment could also help identify invasive risks of medfly populations.

# Method:

### Fly stocks and maintenance:
Derivatives of the Guatemalan TOLIMAN wildtype strain were raised on a wheat bran diet (24% wheat bran, 16% sugar, 8% yeast, 0.6% citric acid, 0.5% sodium benzoate) since, at least, 2015 with adults fed on a 3:1 sucrose: yeast hydrolysate mix [2]. These lines were then established, by Leftwich et al. in 2017, on two different larval diets, either an ASG (A) diet or a Starch (S) diet. 
Three experimental lines for each regime diet (A1, A2, A3, S1, S2, S3) were long reared on their respective larval diet and maintained separately by Leftwich et al. (2017 and 2019).
In this current study, three egg collections were taken for each experimental line (over 3 days) and raised separately. Temperature was controlled at 25˚C for both diets and they were maintained under a 12:12 hr light-dark cycle with 50% relative humidity. All adult flies were fed weekly and given water biweekly.
Reciprocal diet crosses were carried out between a Starch (S) line and an ASG (A) line (both long reared on a regime of their respective diet) to produce four diet regimes (regime larval diet, proximate larval diet: SS, SA, AA and AS) to allow effects of diet to be observed.

### Development times/pupae to adult survival

Pupae from first 3 egg collections were checked for eclosions and their sex at 9:00AM (at lights on), 12:00, 15:00 and 18:00 to see if a pattern in eclosion exists (Supplemental Figure 2 & Supplemental Table 1). Pupae from the other days were checked for eclosions and sex at 18:00.
At 3pm, eclosions in the petri dishes were checked and eclosed flies were transferred into cages until 20 females and 10 males were present in each. Flies were transferred by placing the petri dishes into ice until unconscious, then transferred with soft forceps by the wings. Flies were not on ice for longer than 7 minutes. First egg-laying was noted for each cage.

### Egg number

Eggs were collected on the fourth day of egg-laying for each cage and were counted under a dissecting microscope using a paint brush.

### Egg volume

30 eggs were aligned in the same orientation on damp filter paper, this was done for each of the lines on each diet. The programme ToupView (version x64, 3.7.5660) was used to take measurements and a Brunel microscope (model N-300M) at 4 X magnification was used and calibrated. Length was taken from the centre of the anterior end to the centre of the posterior end; width was taken at half the length. Egg volume was then calculated using the formula 1/6 π(L×W^2).

### Female weight
The dry weights of females from the development tests were taken by desiccating the flies at 60˚C for 24hrs in a drying oven. A plastic weigh boat was weighed, and the balance zeroed before 5 female flies from each cage were weighed individually in the tray on a AND BM-20 microanalytical balance.
### Data analysis
R programming software v4.0.2 [35] was used to run analyses on the data collected. Egg to adult eclosion development time was calculated for each individual fly and modelled using Poisson log-link generalised linear-mixed models (GLMMs) to compare across diet treatments and males and females. Fly replicate, F1 egg collection date, eclosion plate and eclosion well were treated as random effects. Egg to eclosion was then split into larval development time (egg to pupation) and pupae development time (pupation to eclosion) and modelled in the same way. 
A binomial log-link GLMM was used to compare pupae to adult survival across diet treatments. Female body mass, eggs per female and egg volume was compared across diets using linear-mixed models. The estimated marginal means (EMMs) for each model were then calculated.
All analyses were carried out in R (v4.0.2) [35] with the following packages: lme4 [36], lmerTest [37] and emmeans [38] for modelling and analysis; and tidyverse [39], and DHARMa [40] for checking model assumptions.


## Scripts
