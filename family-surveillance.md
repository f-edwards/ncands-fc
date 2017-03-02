<!-- add header with bibfile, bibstyle, make two versions, one MS, one diss -->
<!-- prep for diss style file -->

#Introduction

Contact with the criminal justice system has a host of consequences for families. Incarceration has a profound effect on family complexity and stability, and strains the emotional and material resources of children's caregivers in ways that are known to cause both short and long-term harm to children. While entanglement with the criminal justice system has known causal effects on child maltreatment and family inequality, criminal justice systems also act as a key point of entry into the child welfare system. Child welfare systems are responsible for the investigation of alleged child abuse and neglect and empowered to separate children from their families. Police are legally obliged to report suspected child abuse and neglect; about one fifth of all reports of child abuse and neglect originate from law enforcement. 

The organization and spacial distribution of policing may play an important role in determining whether a family comes into contact with the child welfare system, with likely serious consequences for family inequality. Child welfare system involvement is an important, but under-examined spillover consequence of criminal justice contact. By subjecting families to unequal levels of surveillance through the uneven distribution of criminal justice contact, police may affect the likelihood that an abused or neglected child comes to the attention of the state and the likelihood that a struggling family experiences additional state intrusion through a child welfare investigation. Patterns of over-policing and under-policing have direct implications not only for criminal justice, but also for child protection.

Some places with exceptionally high levels of child poverty have very low rates of abuse and neglect reporting by law enforcement. Figure 1 displays the proportions of children subject to a child abuse or neglect report by police in 2012 for those states that reported data to the National Child Abuse and Neglect Data System. In Florida, Arkansas, Michigan, Delaware, North Dakota and Alaska, more than 1,200 reports of child abuse and neglect per 100,000 children were filed by police in 2012. In Washington, Mississippi, Louisiana, Kansas, and Hawaii, police filed fewer than 500 reports of child abuse and neglect per 100,000 children. Within states, counties also vary dramatically in the volumes of reports filed by law enforcement. In this project, I seek to clarify how law enforcement practices and resources relate to family surveillance and the generation of child maltreatment reports. 

Using administrative data on investigated reports of child abuse and neglect in the U.S. from 2000 through 2014, I will explore whether the administrative features of police departments, the resources available to them, and volumes of arrests help to explain cross-sectional variation in the number of child abuse and neglect reports filed by law enforcement across counties. I will also evaluate whether short-term changes in patterns of policing help to explain longitudinal variation in counts of child abuse and neglect reports filed by police within counties.

Patterns of policing may play a direct role in magnifying childhood inequality through affecting which children and families become subject to child maltreatment investigations. Such investigations can be a vector for both critical services and serve as a source of intense family disruption. As front-line sentinels for child welfare systems, contact with police can trigger contact with a range of other state agencies and may result in additional interventions targeted at family members not directly subject to criminal suspicion. 

Family surveillance is an inherently multi-institutional effort. By investigating how the activities of police relate to child maltreatment reporting, this study seeks to clarify the role that criminal justice plays in structuring which families come under suspicion of child abuse and neglect and whether these patterns contribute to family inequality.


#The police and surveillance

In an era of social service retrenchment, police have increasingly become points of entry to interaction with human services and social welfare organizations (CITATIONS TO MENTAL HEALTH AND POLICING?)

<!-- HYPOTHESIS - AUSTERITY-> Increased police reporting, alt social services decline, cops take on increased role -->

Police conduct both active surveillance of individuals and families, driven by their direct observations and perceptions of suspicious or illegal behavior, and reactive surveillance, driven by third-party reports and complaints about suspicious, nuisance, or dangerous behavior. In so doing, they act as the central instrument of detection and intervention for the criminal justice system. We obtain official knowledge (hence actionable through policy) about the intensity and varieties of criminal activity through the activities of police organizations and community engagement with police organizations. Police serve as a central institution through which information about deviant or socially problematic behavior is systematically collected and directed to a variety of public and private organizations tasked with documenting and intervening in family and community life. 

Because the distribution of policing resources is distinctly non-random <!-- (say more about presence of police, styles of interaction across space, toward differently racialized people in different spaces) --> and the mode of police-public interaction across subjects is not uniform, the probability of detecting particular kinds of behavior of concern to criminal justice and welfare bureaucracies is uneven across places and across groups. 

While some forms of crime are highly unlikely to go undetected (such as homicide), many other behaviors or incidents police are tasked with detecting are sensitive to the social organization of policing and are unlikely to be brought to the state's attention unless police or those who report to the police actively look for and respond to suspicions. For example, if two places $a$ and $b$ have identical rates of a particular kind of criminal behavior j the *reported* rate of this criminal behavior behavior for places $a$ and $b$, $d_a, d_b$ will be a function of surveillance in places $a$ and place $b$. We can denote the likelihood of police detection of particular kinds of behavior in place $a$ through function $f(j)$ and in place $b$ through function $g(j)$, such that

$$d_a=f(j)$$
$$d_b=g(j)$$

This approach treats objective rates of a particular behavior of interest, $j$, as latent and unobservable. Reported rates of this behavior of interest $d$, vary across places as a function of the likelihood of detection. 

These reporting functions are sensitive to inputs from both the organization of active police surveillance (the organization of patrols, force size, enforcement priorities, bias), and from the propensity of community members to report suspicious or dangerous occurrences to the police (trust, organizational routines, bias). 

This model is a simplification of the real data-generating process. Can I bake labeling into the surveillance function? Assumes that there is some real objective incidence of child abuse and neglect, that it's potentially independent of social process (DOESN'T ACTUALLY ASSUME IT). BEHAVIOR->OBSERVATION->LABEL->REPORT. Multiple events are included in the function then. As a bernoulli event we get report~B(report|label, observation). Label, observation is a joint event with some probability distribution p(observation, label), and the report is conditional on this joint event. I'm then building a count model of sum(B(report)). Could I plausibly treat arrests as the exposure for p(report|observation)? Problem is I don't have micro-data for arrests within families.  

Police are not only tasked with detecting criminal behavior. Police increasingly act as catchall points-of-entry for a range of both coercive and welfarist institutions (CITE NEOLIB WELFARE STUFF). In all U.S. jurisdictions (CHECK HISTORY OF REPORTING LAW), they are also mandated reporters of child abuse and neglect. When they suspect that a child has been or may become a victim of child abuse or neglect, mandated reporters are required by law to notify state or local child protection offices, who are then tasked with investigating and responding to the allegations (often accompanied by police). Police may be particularly likely to report suspected abuse or neglect they detect as a result of their investigation and enforcement activities related to other potentially criminal offenses. As a result, police act as a major conduit through which states come to know about a case of child abuse or neglect, and through which families come into contact with child protection agencies. In 2014 X percent of all investigated reports of abuse and neglect originated from police. In this study, I investigate whether the organization and enforcement priorities of local police agencies influences the rates at which police report child abuse and neglect to state child protection systems. 

reporting function f(j) = \Beta_{drugenforcement}*DrugArrests or something - specify what goes into these functions, even if fairly general. think about between and within place variation with regard to race

Do I want to follow through with a formal specification for surveillance?

#Crime, criminal justice, and the family

#Analytic strategy

This paper addresses a series of empirical questions directed at clarifying whether child welfare system involvement is a spillover consequence of police contact using a variety of multilevel regression models. After controlling for variation in child well-being measured through exposure to poverty, infant mortality, family complexity, and other variables known to indicate or be closely correlated with child abuse and neglect:

1) Do police force budgets, numbers of sworn officers, and number of arrests predict the count of reports of child abuse and neglect filed by police officers at the county-year level? Do these relationships vary for reports about children and families of color?

2) Do shifts in enforcement priorities predict within-county changes in counts of reports of child abuse and neglect filed by police officers? Do these relationships vary for children and families of color? 
]

GOAL IS TO ESTIMATE `SURVEILLANCE FUNCTION` - contribution of police org to reporting rates - some kind of decomposition of the measured effect of policing on reporting? 

EXPLAIN INCLUSION OF EACH MEASURE - poverty and infmort as malt predictors y~B1poverty+B2infmort

maybe expect policing to work on malt in multiplicative fashion - i.e as f(maltreatment) - how does this change estimation strategy? 

assumption - malt~Poisson(poverty, infmort) - can check cor(infmort, NDEATHS in NCANDS), can check baseline  - can't do county-level with current NCANDS, need restricted file for that

EXPLAIN CLASSES OF OFFENSES - why do I group, why do I expect QOL and DRUGS to matter. Why use homicide?

explain QoL policing. Explain why viol likely subject to lowest surveillance effect. Think abt inclusion of crimes known data

WHY ARRESTS - DIFF B/W ARRESTS AND CRIMES KNOWN

SHOULD I INCLUDE SEX OFFENSES, OFFENSES AGAINST CHILDREN AND FAMILIES AS DISTINCT (200)?

NEW MODELS FOR TS CHECKS ON POL.RPTS OVER TIME AS FUNCTION OF BUDGETS? IS THERE A (+) NATL TIME TREND?

##Data and measures

###Child maltreatment reports data and measures
Outcomes for this study are constructed from the National Child Abuse and Neglect Data System (NCANDS) [CITATION AND DATA NOTE]. NCANDS records case-level information on all investigated reports of child maltreatment annually with data reported from state child welfare agencies. In 2014, 46 states voluntarily reported data on these abuse and neglect investigations and their outcomes. From these case-level data, I construct counts of reports of child abuse and neglect filed by police at the county-year level (the smallest unit of geography reported in NCANDS). NCANDS de-identifies counties with fewer than 1,000 reports of child abuse and neglect filed annually, resulting in an unbalanced panel including ```R length(unique(dat$FIPS))``` unique counties between 2000 and 2014, and ```R write code to do N in all years``` counties included in each of the included annual waves of NCANDS data. Of the included counties, ```R calculate percent of counties with more than 100k population included in full NCANDS``` THEN STATEMENT ON COVERAGE OF POPULOUS COUNTIES IN US. These data allow for novel insight into the effects of police activities and organizations on surveillance, as they include information on the sources of child abuse and neglect reports for millions of reports across a wide array of jurisdictions annually. 

MISSINGNESS IN RPTSRC. Missingness in data on the source of maltreatment sources is correlated with geography, with some places reporting complete data, and others reporting high-levels of missing data on sources of reports. In 2014, ```R calculate proportion complete``` counties reported no missing data on sources of maltreatment reports, and ```R calculate proportion with >5 percent missing``` percent of counties reported more than 5 percent of their cases with missing information on the source of the report. I DO X TO DEAL WITH THIS. SUBSETS, IMPUTATIONS, REPORT SENSITIVITY.

###Crime, policing, and arrests data and measures

Focal predictors are constructed from the Uniform Crime Reports (UCR) County-Level Detailed Arrest and Offense data and the UCR Arrests by Age, Sex, and Race annual data for 2000 through 2014 [CITATIONS]. The UCR, collected by the FBI and maintained by the National Archive of Criminal Justice Data, provides the only time series data on law enforcement activity available at the jurisdiction level and covering the period of interest for this study. The UCR County-Level Detailed Arrest and Offense data provide counts of arrests for a broad range of offenses (including, but not limited to index crimes), but do not provide demographic information on those arrested. The UCR Arrests by Age, Sex, and Race series provides data on arrests for age groups by sex and race at the police agency level, which can be aggregated to the county-level as sums of agencies within particular counties. Aggregated county counts from the Arrests by Age, Sex and Race data for all arrests generally agree with county arrest counts from the County-Level Detailed Arrest and Offense data, $r=$ ```R calculate correlation in tot.arrest across two datasets ```. Because the County-Level Detailed Arrest and Offense data are subject to agency imputations and corrections and may present more reasonable counts for marginal cases, I use those data for counts of all arrests at the county-year level, and the UCR Arrests by Age, Sex, and Race data series to construct counts of arrests by offense, race, and gender at the county-year level. 

I group offenses into sets that are subject to higher or lower levels of police discretion in enforcement, then create county-year sums for all arrests, and for arrests by gender and race. I treat violent offenses include murder, manslaughter, rate, robbery, aggravated assault, and other assaults, largely following the FBI's classification of violent offenses in the UCR index crime classification system. I also rely on the UCR's classification of drug offenses, a set which includes either posession or sale of opiates, marijuana, synthetic narcotics, and other dangerous non-narcotic drugs. Quality of life policing captures a more diffuse set of offenses that are generally low-level and subject to high levels of officer and agency discretion in enforcement. These include vandalism, liquor laws, drunkenness, disorderly conduct, vagrancy, general suspicion, curfew and loitering, and various gambling offenses. 

I obtain information on the number of officers employed by police agencies at the county-year level from the UCR Police Employee Data (LEOKA). These data present monthly counts of assaults and killings of law enforcement officers, and include counts of total officers and employees at the agency level for all years included in this analysis. I aggregate agencies at the county-year level to construct per capita rates of police officer employment. These rates provide a comparable measure of police force size both across places and within places over time.  

###Child and family well-being data and measures

Of course, rates of reported child abuse and neglect are sensitive to actual rates of child abuse and neglect. Poverty is among the consistently strongest predictors of risk for child abuse and neglect [CITATIONS], though some argue that many cases of neglect are simple proxies for family poverty [CITATIONS]. Few sources provide reliable time series estimates of child population well-being at the county-level for the period under consideration [NOTE ON 5yr ACS FOR BACK END, MAYBE ESTIMATE MODELS WITH THOSE DATA AND FULLER PANEL OF PREDICTORS]. Sample surveys are subject to substantial small-area problems for relatively small units of geography, like counties. The U.S. Census Bureau's Small Area Income and Poverty Estimates (SAIPE) provide model-based annual estimates of total population, total child population, persons in poverty, children in poverty, and median household income at the county-level, and provide estimates with far lower error than do estimates constructed from the American Community Survey (ACS). However, the SAIPE do not include information on child poverty by race, nor do they provide information on other risk factors for abuse and neglect.

Infant mortality is a widely used indicator of population well-being and health [CITES], and is both a strong predictor and partial subset of child abuse and neglect [CITES]. I rely on infant mortality data from the National Center for Health Statistics Detail Mortality data files, as compiled and reported through the Area Health Resources File. These data report the rate of infant deaths per thousand live births for a rolling five-year period at the county-level for all counties with more than 10 infant deaths per year. Infant mortality data reported through AHRF in the 2015-2016 release are available as White and Non-White infant mortality for five-year periods beginning with 1996 - 2000 and ending with 2010 - 2014. To maximize comparability with NCANDS data, I join the five-year data according to the final year of the reporting period, so 1996 - 2000 infant mortality data are joined with 2000 NCANDS data, and 2010 - 2014 infant mortality data are joined with 2014 NCANDS data. 

DO I WANNA USE 5-YR ACS FOR BY-RACE ESTIMATES? ON SUBSET OF YEARS? PROBABLY BEST WAY TO GO, CAN USE FAM STRUCTURE, EMPLOYMENT, OTHER MICRO-DATA AS NEEDED. 

###Other predictors

I calculate county population density by dividing the annual county total population from the Census SAIPE estimates by the total county land area using data from the U.S. Census TIGER geographic data. 

Pop density, vote share, other things? trying to deal with omitted variables

##Statistical models

Count models - think hard about specification. I think I'm suggesting an additive model for maltreatment occurrence, a multiplicative model for policing. 

something like y~B(maltreatment)*G(policing)

breaks down to interactions of policing vars with malt vars - model is that reporting rates = surveillance function applied to maltreatment rates = g(maltreatment) which can be written \gamma_i=(bPov+bInfMort), \alpha_i=(bArrest+bForce) - E[y]=\gamma_i*\alpha_i

Weights for pop? Dealing with weird errors? Dealing with missing? 

#Findings

Descriptives - what kinds of malt do police report? who do they report (demographics)? changes over time? notable geographic distinctions? region/urban? Maps probably not helpful for counties... 

Model results

cross-validation

Model predictions 

Decomposition, estimated effect of policing on malt reporting rates overall? Of reporting / malt detection by police?

#Discussion

Spillover CJ family implications

Strat/family implications

Surveillance studies implications

Soc of knowledge implications



#Conclusion



#References