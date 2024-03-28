
# Derived score to alternative derived score ------------------------------

der_to_der  <- function(score.old, old.metric, new.metric, dp = 2) {

  # specify old mean
  mean.old = if(old.metric == "index"){
    100
  } else if(old.metric == "ss") {
    10
  } else if(old.metric == "z") {
    0
  } else if(old.metric == "t") {
    50
  } else if(old.metric == "sten") {
    5.5
  } else if(old.metric == "stanine") {
    5
  }

  # specify old SD
  SD.old = if(old.metric == "index"){
    15
  } else if(old.metric == "ss") {
    3
  } else if(old.metric == "z") {
    1
  } else if(old.metric == "t") {
    10
  } else if(old.metric == "sten") {
    2
  } else if(old.metric == "stanine") {
    2
  }

  # specify new mean
  mean.new = if(new.metric == "index") {
    100
  } else if(new.metric == "ss") {
    10
  } else if(new.metric == "z") {
    0
  } else if(new.metric == "t") {
    50
  } else if(new.metric == "sten") {
    5.5
  } else if(new.metric == "stanine") {
    5
  }

  # specify new SD
  SD.new = if(new.metric == "index") {
    15
  } else if(new.metric == "ss") {
    3
  } else if(new.metric == "z") {
    1
  } else if(new.metric == "t") {
    10
  } else if(new.metric == "sten") {
    2
  } else if(new.metric == "stanine") {
    2
  }

  round(
    (SD.new/SD.old)*(score.old-mean.old)+mean.new, dp
  )
}



# z-score to percentile rank ----------------------------------------

z_to_rank <- function(derived_score, dp = 2) {
  round(pnorm(derived_score)*100, dp)
}


# Convert derived score to all scores -------------------------------------

der_to_all <- function(old.score, old.metric, dp = 2) {
  data.frame(
    "index" =   der_to_der(score.old = old.score,      old.metric = old.metric, new.metric = "index", dp = dp),
    "t" =      der_to_der(score.old = old.score,      old.metric = old.metric, new.metric = "t",      dp = dp),
    "z" =      der_to_der(score.old = old.score,      old.metric = old.metric, new.metric = "z",      dp = dp),
    "ss" =     der_to_der(score.old = old.score,      old.metric = old.metric, new.metric = "ss",     dp = dp)#,
    #"Sten" = der_to_der(score.old = 100,    old.metric = "index", new.metric = "sten")
  ) %>% mutate(
    rank = z_to_rank(z, dp = dp),
    AAN  = case_when(
      index >= 130 ~ "Exceptionally High Score",
      index >=  120 ~ "Above Average Score",
      index >=  110 ~ "High Average Score",
      index >=  90 ~ "Average Score",
      index >=  80 ~ "Low Average Score",
      index >  69 ~ "Below Average Score",
      .default = "Exceptionally Low Score"
    ),
  )

}


# Norms Table -------------------------------------------------------------

norms <-
data.frame(
  test = 1:150
) %>%
  mutate(
    index = der_to_der(old.metric = "index", dp = 5, new.metric = "index", score.old = test),
    z = der_to_der(old.metric     = "index", dp = 5, new.metric = "z", score.old = test),
    t = der_to_der(old.metric     = "index", dp = 5, new.metric = "t", score.old = test),
    ss = der_to_der(old.metric    = "index", dp = 5, new.metric = "ss", score.old = test),
    index = der_to_der(old.metric = "index", dp = 5, new.metric = "index", score.old = test)
  ) %>% mutate(
    rank = z_to_rank(z, dp = 5),
    AAN  = case_when(
      index >= 130 ~ "Exceptionally High Score",
      index >=  120 ~ "Above Average Score",
      index >=  110 ~ "High Average Score",
      index >=  90 ~ "Average Score",
      index >=  80 ~ "Low Average Score",
      index >  69 ~ "Below Average Score",
      .default = "Exceptionally Low Score"
    )
  ) %>%
  select(-test) %>%
  filter(index > 40) %>%
  arrange(desc(index))



# Descriptor Generator  -------------------------------------------------------

# generate_descriptor <- function(index_score, descriptor_system = "aan") {
#   if (descriptor_system == "aan") {
#     if (index_score >= 130) {
#       return("Exceptionally High Score")
#     } else if (index_score >= 120) {
#       return("Above Average Score")
#     } else if (index_score >= 110) {
#       return("High Average Score")
#     } else if (index_score >= 90) {
#       return("Average Score")
#     } else if (index_score >= 80) {
#       return("Low Average Score")
#     } else if (index_score > 69) {
#       return("Below Average Score")
#     } else if (index_score < 70) {
#       return("Exceptionally Low Score")
#     } else {
#       return(NA_character_)
#     }
#   } else if (descriptor_system == "wechsler") {
#     # Define conditions for descriptor system "wechsler"
#     # Adjust conditions and return values as needed
#     if (index_score >= 130) {
#       return("Very Superior Score")
#     } else if (index_score >= 120) {
#       return("Superior Score")
#     } else if (index_score >= 110) {
#       return("High Average Score")
#     } else if (index_score >= 90) {
#       return("Average Score")
#     } else if (index_score >= 80) {
#       return("Low Average Score")
#     } else if (index_score > 69) {
#       return("Borderline")
#     } else if (index_score < 70) {
#       return("Extremely Low")
#     } else {
#       return(NA_character_)
#     }
#   } else if (descriptor_system == "nepsy") {
#     # Define conditions for descriptor system "newsy"
#     # Adjust conditions and return values as needed
#     if (index_score >= 110) {
#       return("Above Expected Level")
#     } else if (index_score >= 90) {
#       return("At Expected Score")
#     } else if (index_score >= 75) {
#       return("Borderline Score")
#     } else if (index_score > 64) {
#       return("Below Expected Level")
#     } else if (index_score < 65) {
#       return("Well Below Expected Level")
#     } else {
#       return(NA_character_)
#     }
#   } else if (descriptor_system == "q.simple") {
#     # Define conditions for descriptor system "q.simple"
#     # Adjust conditions and return values as needed
#     if (index_score > 129) {
#       return("Very Superior Score")
#     } else if (index_score >= 120) {
#       return("Superior Score")
#     } else if (index_score >= 110) {
#       return("High Average Score")
#     } else if (index_score >= 90) {
#       return("Average Score")
#     } else if (index_score >= 85) {
#       return("Low Average Score")
#     } else if (index_score >= 77) {
#       return("Borderline Score")
#     } else if (index_score < 77) {
#       return("Abnormal Score")
#     } else {
#       return(NA_character_)
#     }
#   } else if (descriptor_system == "groth.marnat") {
#     # Define conditions for descriptor system "groth.marnat"
#     # Adjust conditions and return values as needed
#     if (index_score > 129) {
#       return("Upper Extreme Score")
#     } else if (index_score >= 120) {
#       return("Well Above Average Score")
#     } else if (index_score >= 110) {
#       return("High Average Score")
#     } else if (index_score >= 90) {
#       return("Average Score")
#     } else if (index_score >= 80) {
#       return("Low Average Score")
#     } else if (index_score >= 70) {
#       return("Well Below Average Score")
#     } else if (index_score < 70) {
#       return("Lower Extreme Score")
#     } else {
#       return(NA_character_)
#     }
#   } else {
#     stop("Invalid descriptor system. Choose 'aan', 'wechsler', 'nepsy', 'q.simple', or 'groth.marnat'.")
#   }
# }


generate_descriptor <- function(index_score, descriptor_system = "aan") {
  if (descriptor_system == "aan") {
    result <- case_when(
      index_score >= 130 ~ "Exceptionally High Score",
      index_score >= 120 ~ "Above Average Score",
      index_score >= 110 ~ "High Average Score",
      index_score >= 90 ~ "Average Score",
      index_score >= 80 ~ "Low Average Score",
      index_score > 69 ~ "Below Average Score",
      index_score < 70 ~ "Exceptionally Low Score",
      TRUE ~ NA_character_
    )
  } else if (descriptor_system == "wisc") {
    # Adjust conditions for descriptor system "wechsler" as needed
    result <- case_when(
      index_score >= 130 ~ "Extremely High",
      index_score >= 120 ~ "Very High",
      index_score >= 110 ~ "High Average",
      index_score >= 90 ~ "Average",
      index_score >= 80 ~ "Low Average",
      index_score > 69 ~ "Very Low",
      index_score < 70 ~ "Extremely Low",
      TRUE ~ NA_character_
    )
  } else if (descriptor_system == "wais") {
    # Adjust conditions for descriptor system "wechsler" as needed
    result <- case_when(
      index_score >= 130 ~ "Very Superior Score",
      index_score >= 120 ~ "Superior Score",
      index_score >= 110 ~ "High Average Score",
      index_score >= 90 ~ "Average Score",
      index_score >= 80 ~ "Low Average Score",
      index_score > 69 ~ "Borderline",
      index_score < 70 ~ "Extremely Low",
      TRUE ~ NA_character_
    )
  } else if (descriptor_system == "nepsy") {
    # Adjust conditions for descriptor system "nepsy" as needed
    result <- case_when(
      index_score >= 110 ~ "Above Expected Level",
      index_score >= 90 ~ "At Expected Score",
      index_score >= 75 ~ "Borderline Score",
      index_score > 64 ~ "Below Expected Level",
      index_score < 65 ~ "Well Below Expected Level",
      TRUE ~ NA_character_
    )
  } else if (descriptor_system == "q.simple") {
    # Adjust conditions for descriptor system "q.simple" as needed
    result <- case_when(
      index_score > 129 ~ "Very Superior Score",
      index_score >= 120 ~ "Superior Score",
      index_score >= 110 ~ "High Average Score",
      index_score >= 90 ~ "Average Score",
      index_score >= 85 ~ "Low Average Score",
      index_score >= 77 ~ "Borderline Score",
      index_score < 77 ~ "Abnormal Score",
      TRUE ~ NA_character_
    )
  } else if (descriptor_system == "groth.marnat") {
    # Adjust conditions for descriptor system "groth.marnat" as needed
    result <- case_when(
      index_score > 129 ~ "Upper Extreme Score",
      index_score >= 120 ~ "Well Above Average Score",
      index_score >= 110 ~ "High Average Score",
      index_score >= 90 ~ "Average Score",
      index_score >= 80 ~ "Low Average Score",
      index_score >= 70 ~ "Well Below Average Score",
      index_score < 70 ~ "Lower Extreme Score",
      TRUE ~ NA_character_
    )
  } else if (descriptor_system == "heaton") {
    # Adjust conditions for descriptor system "groth.marnat" as needed
    result <- case_when(
      index_score > 129 ~ "Very superior",
      index_score >= 115 ~ "Superior",
      index_score >= 107 ~ "Above average",
      index_score >= 92 ~ "Average",
      index_score >= 85 ~ "Below Average",
      index_score >= 77 ~ "Mildy Impaired",
      index_score >= 70 ~ "Mildy to Moderately Impaired",
      index_score >= 62 ~ "Moderately Impaired",
      index_score >= 55 ~ "Moderately to Severely Impaired",
      index_score >= 45 ~ "Severely Impaired",
      TRUE ~ NA_character_
    )
  } else {
    stop("Invalid descriptor system. Choose 'aan', 'wechsler', 'nepsy', 'q.simple', 'groth.marnat', or heaton.")
  }

  return(result)
}


# Test Names --------------------------------------------------------------

test_names <-

  tribble(
    ~Test, ~Test_Name_Full,
    # Adult
    "ACE III", "Addenbrookes Cogntive Evaluation (ACE III)",
    "AHI", "Annette Handedness Questionnaire",
    "BAI", "Becks Anxiety Inventory",
    "BDI", "Becks Depression Inventory",
    "BVMT R", "Brief Visuospatial Memory Test Revised (BVMT R)",
    "b Test", "The b Test",
    "BMIPB II", "BIRT Memory and Information Processing Battery (BMIPB II)",
    "BADS", "Behavioural Assessment of Dysexecutive Syndrome (BADS)",
    "BNT 2", "Boston Naming Test (BNT II)",
    "Coin In Hand", "Coin In Hand Test",
    "CVLT 2", "California Verbal Learning Test (CVLT II)",
    "CVLT 3", "California Verbal Learning Test (CVLT III)",
    "DKEFS", "Delis-Kaplan Executive Function System (DKEFS)",
    "Dot Counting", "Dot Counting Test",
    "FRSBE", "Frontal Systems Behavior Scale (FrSBe)",
    "GAD 7", "Generalised Anxiety Disorder Scale 7 (GAD 7)",
    "GNT", "Graded Naming Test",
    "Grooved Peg Board", NA,
    "HADS", "Hospital Anxiety and Depression Scale (HADS)",
    "Hayling & Brixton", "Hayling & Brixton Test",
    "HVLT R", "Hopkins Verbal Learning Test Revised (HVLT R)",
    "MAE", "Multilingual Aphasia Examination (MAE)",
    "PHQ 9", "Physical Health Questionnaire 9 (PHQ 9)",
    "RBANS", "The Repeatable Battery for the Assessment of Neuropsychological Status (RBANS)",
    "RCFT", "Rey Complex Figure Test (RCFT)",
    "Rey 15", "Rey Fifteen-Item Test (Rey 15)",
    "RAVLT", "Rey Auditory Verbal Learning Test (RAVLT)",
    "Stroop", "Stroop Test",
    "Sydney", "Sydney Language Battery",
    "SDMT", "Symbol Digit Modalities Test (SDMT)",
    "TEA", "Test of Everyday Attention (TEA)",
    "TOMM", "Test of Memory Malingering (TOMM)",
    "TOPF", "Test of Premorbid Functioning (TOPF)",
    "TMT", "Trail Making Test",
    "VOSP", "Visual Object and Space Perception Battery (VOSP)",
    "WAIS IV", "Wechsler Adult Intelligence Scale (WAIS IV)",
    "WASI II", "Wechsler Abbreviated Scale of Intelligence (WASI II)",
    "WMS IV", "Wechsler Memory Scale (WMS IV) ",
    "WCST", "Wisconsin Card Sorting Test (WCST)",
    "WMT", "Word Memory Test",

    "COWA", "Controlled Oral Word Association (COWA)",
    "NAB", "Neuropsychological Assessment Battery (NAB)",
    "MMPI-2-RF/3", "Minnesota Multiphasic Personality Inventory 2 - Restructured Form (MMPI 2)",
    "PAI", "Personality Assessment Battery (PAI)",

    "BASC 3", "Behavior Assessment System for Children (BASC 3)",
    "K TEA 3", "Kaufman Test of Educational Assessment (KTEA 3)",
    "CPT 3", "Continuous Performance Test (CPT 3)",

    # Child
    "Bayley III", "Bayley Scales Of Infant and Toddler Development (Bayley III)",
    "Beck Youth Inventory", NA,
    "Beery VMI 6", "Beery Buktenica Developmental Test of Visual Motor Integration (VMI 6)",
    "BRIEF 2 Parent", "Behavior Rating Inventory of Executive Function, Parent (BRIEF II)",
    "BRIEF 2 Self Report", "Behavior Rating Inventory of Executive Function, Self (BRIEF II)",
    "BRIEF 2 Teacher", "Behavior Rating Inventory of Executive Function, Teacher (BRIEF II)",
    "ChaMP", "Child and Adolescent Memory Profile (ChaMP)",
    "CMS", "Children's Memory Scale (CMS)",
    "CVLT C", "California Verbal Learning Test for Children (CVLT C)",
    "NEPSY II", NA,
    "Strengths and Difficulties", "Strengths and Difficulties Questionnaire",
    "TeaCh 2", "Test of Everyday Attention for Children (TeaCH II)",
    "TeaCh 2 Adolescent",  "Test of Everyday Attention for Children, Adolescents (TeaCH II)",
    "TeaCh 2 Junior",  "Test of Everyday Attention for Children, Juniors (TeaCH II)",
    "Vineland III", "Vineland Adaptive Behavior Scale (Vineland III)",
    "WIAT III", "Wechsler Individual Achievement Test (WIAT III)",
    "WISC V", "Wechsler Intelligence Scale for Children (WISC V)",
    "WPPSI IV", "Wechsler Preschool and Primary Scale of Intelligence (WPPSI IV)",
    "WRAT", "Wide Range Achievement Test (WRAT IV)",
    "WRAML 3", "Wide Range Assessment of Memory and Learning (WRAML III)"
  )

