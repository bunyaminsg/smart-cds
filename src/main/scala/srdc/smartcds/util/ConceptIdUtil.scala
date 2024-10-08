package srdc.smartcds.util

object ConceptIdUtil {
  // Observations
  val ACUTE_EXACERBATIONS = "AcuteExacerbations"
  val ACR = "ACR"
  val ADVANCED_RETINOPATHY = "AdvancedRetinopathy"
  val ALBUMIN_IN_URINE = "AlbuminInUrine"
  val ALBUMIN_IN_24_HOUR_URINE = "AlbuminIn24HourUrine"
  val ALCOHOL_USAGE = "AlcoholUsage"
  val ALTERED_MENTAL_STATUS_DUE_TO_HYPERCAPNIA = "AlteredMentalStatusDueToHypercapnia"
  val ALZHEIMER_SEVERITY = "AlzheimerSeverity"
  val AGITATION_ASSOCIATED_WITH_ANXIETY_OR_DELIRIUM = "AgitationAssociatedWithAnxietyOrDelirium"
  val ANKLE_BRACHIAL_INDEX = "AnkleBrachialIndex"
  val ARTERIAL_STIFFENING = "ArterialStiffening"
  val ASTHMATIC_FEATURES = "AsthmaticFeatures"
  val BARTHEL_INDEX = "BarthelIndex"
  val BEHAVIOUR_CHANGE = "BehaviourChange"
  val BEHAVIOURAL_PSYCHOLOGICAL_SYMPTOMS_DEMENTIA = "BehaviouralPsychologicalSymptomsDementia"
  val BMI = "BMI"
  val BP_UNDER_CONTROL = "BPUnderControl"
  val BLOOD_PH = "BloodPH"
  val CANNOT_MANAGE_AT_HOME = "CannotManageAtHome"
  val CARDIOVASCULAR_RISK = "CardiovascularRisk" // hs
  val CIRCINATE_OR_GROUP_EXUDATES = "CircinateOrGroupExudates"
  val CONTRAINDICATION_TO_ANTICOAGULATION = "ContraindicationToAnticoagulation"
  val CRITICAL_LIMB_ISCHAEMA = "CriticalLimbIschaemia"
  val COPD_SEVERITY = "COPDSeverity"
  val COPD_EXACERBATION_SEVERITY = "COPDExacerbationSeverity"
  val CONFUSION_LETHARGY_EVIDENCE_OF_HYPOVENTILATION = "ConfusionLethargyEvidenceOfHypoventilation"
  val CONTRAINDICATION_TO_ACHE_INHIBITORS = "ContraindicationToACHEInhibitors"
  val CREATININE_CLEARANCE = "CreatinineClearance"
  val DIALYSIS = "Dialysis"
  val DIASTOLIC_BP = "DiastolicBP"
  val DISCHARGED_IN_THE_PAST_ONE_MONTH = "DischargedInThePastOneMonth"
  val DIURESIS = "Diuresis"
  val ECG_LVH = "EcgLVH"
  val ECHOCARDIOGRAPHIC_LVH = "EchocardiographicLVH"
  val EF = "EF"
  val EGFR  = "Egfr"
  val EGFR_AT_RASA = "EgfrAtRasa"
  val EJECTION_FRACTION = "EjectionFraction"
  val ERRATIC_BLOOD_GLUCOSE_CONTROL = "ErraticBloodGlucoseControl"
  val EXACERBATION_DESPITE_NON_PHARMACOLOGICAL_MANAGEMENT_AND_BRONCHODILATOR =
    "ExacerbationDespiteNonPharmacologicalManagementAndBronchodilator"
  val EXERCISE_HABITS = "ExerciseHabits"
  val EXUDATE_OR_RETINAL_THICKENING = "ExudateOrRetinalThickening"
  val FASTING_GLUCOSE = "FastingGlucose"
  val FEATURES_SUGGESTING_STEROID_RESPONSIVENESS = "FeaturesSuggestingSteroidResponsiveness"
  val FEV1 = "FEV1"
  val FEV1_RATIO = "FEV1Ratio"
  val FEV1_FVC_RATIO = "FEV1FVCRatio"
  val FRAIL = "Frail"
  val FRAILTY_CATEGORY = "FrailtyCategory"
  val FUNCTIONAL_IMPAIRMENT_RELATED_TO_COGNITION = "FunctionalImpairmentRelatedToCognition"
  val GAIT_SPEED = "GaitSpeed"
  val GENETIC_CAUSE_OF_CKD = "GeneticCauseOfCKD"
  val GDS4 = "GDS4"
  val HADS = "HADS"
  val HBA1C = "HbA1C"
  val HDL = "HDL"
  val HEART_RATE = "HeartRate"
  val HEIGHT = "Height"
  val HEMATOCRIT = "Hematocrit"
  val HEMOGLOBIN = "Hemoglobin"
  val HIGH_RISK_COMORBIDITY_CONDITION = "HighRiskComorbidityCondition"
  val HIGH_GRADE_SINOATRIAL_OR_AV_BLOCK = "HighGradeSinoatrialOrAVBlock"
  val HIGH_PHYSICALLY_ACTIVE = "HighPhysicallyActive"
  val HIGH_RISK_DEVELOPING_CVD = "HighRiskDevelopingCVD"
  val HOSPITALIZATION_DUE_TO_EXACERBATIONS = "HospitalizationDueToExacerbations"
  val HOSPITALIZATION_NEED = "HospitalizationNeed"
  val HOSPITALIZATION_WITH_OR_WITHOUT_SURGERY = "HospitalizationWithOrWithoutSurgery"
  val HYPERTENSION_CONFIRMED = "HypertensionConfirmed" // hs
  val HYPERTENSION_EMERGENCY = "HypertensionEmergency" // hs
  val HYPERTENSION_GRADE = "HypertensionGrade"
  val IADL = "IADL"
  val INABILITY_EAT_OR_SLEEP_BECAUSE_OF_DYSPNEA = "InabilityEatOrSleepBecauseOfDyspnea"
  val INABILITY_TO_WALK_BETWEEN_ROOMS = "InabilityToWalkBetweenRooms"
  val INADEQUATE_RESPONSE_TO_AMBULATORY_MANAGEMENT = "InadequateResponseToAmbulatoryManagement"
  val INCREASE_IN_INTENSITY_OF_SYMPTOMS= "IncreaseInIntensityOfSymptoms"
  val IMMOBILIZATION_MORE_THAN_TWO_WEEKS = "ImmobilizationMoreThanTwoWeeks"
  val IMPROVE_WITH_CLINICALLY_ASSISTED_HYDRATION = "ImproveWithClinicallyAssistedHydration"
  val INTOLERANCE_TO_ACHE_INHIBITORS = "IntoleranceToACHEInhibitors"
  val LDL = "LDL"
  val LONG_TERM_CARE = "LongTermCare"
  val LOWER_EXTREMITY_STENOSIS = "LowerExtremityStenosis"
  val LVH = "LVH"
  val MEDICATION_SIDE_EFFECT = "MedicationSideEffect"
  val METFORMIN_TOLERATED = "MetforminTolerated"
  val MORE_THAN_TWO_MODERATE_EXACERBATIONS = "MoreThanTwoModerateExacerbations"
  val MCI = "MildCognitiveImpairmentObservation"
  val MILD_DEMENTIA = "MildDementia"
  val MOOD_SWINGS = "MoodSwings"
  val MUST = "MUST"
  val MMRC = "MMRC"
  val MNASF = "MNASF"
  val NEED_FOR_ACUTE_IMMOBILIZATION = "NeedForAcuteImmobilization"
  val NEED_FOR_PALLIATIVE_CARE = "NeedForPalliativeCare"
  val NEED_FOR_SOCIAL_CARE = "NeedForSocialCare"
  val NEW_VESSEL_TRANSFORMATION = "NewVesselTransformationInEye"
  val NON_HDL = "NonHDL"
  val NOISY_RESPIRATORY_SECRETIONS_IMPACT = "NoisyRespiratorySecretionsImpact"
  val NYHA_CLASS = "NYHAClass"
  val OGTT = "OGTT"
  val OTHER_CONDITIONS_FOR_BB = "OtherConditionsForBB"
  val PAO2 = "PaO2"
  val PACO2 = "PaCO2"
  val PERSISTENT_HYPOXEMIA = "PersistentHypoxemia"
  val PERSONALITY_CHANGE = "PersonalityChange"
  val PHYSICAL_FRAILTY_PHENOTYPE = "PhysicalFrailtyPhenotype"
  val POTASSIUM_IN_SERUM_OR_PLASMA = "PotassiumInSerumOrPlasma"
  val POTASSIUM_IN_BLOOD = "PotassiumInBlood"
  val RANDOM_PLASMA_GLUCOSE = "RandomPlasmaGlucose"
  val RECEIVING_PCEOF_CARE = "ReceivingPCEOFCare"
  val RECENT_ACUTE_CORONARY_SYNDROME = "RecentAcuteCoronarySyndrome"
  val RECENT_CORONARY_STENT = "RecentCoronaryStent"
  val REDUCED_LIFE_EXPECTANCY = "ReducedLifeExpectancy"
  val REDUCED_LVEF = "ReducedLVEF"
  val RENAL_ARTERY_STENOSIS = "RenalArteryStenosis"
  val REVERSIBLE_CAUSE_AGITATION_ANXIETY_DELIRIUM = "ReversibleCauseAgitatioAnxietyDelirium"
  val REVERSIBLE_CAUSE_BREATHLESSNESS = "ReversibleCauseBreathlessness"
  val RISK_FALL = "RiskFall"
  val RISK_OF_HARMING_THEMSELVES = "RiskOfHarmingThemselves"
  val RISK_SCORE = "RiskScore"
  val SAO2 = "SaO2"
  val SARCF = "SARCF"
  val SECONDARY_CVD = "SecondaryCVD"
  val SECONDARY_HYPERTENSION_CONFIRMED = "SecondaryHypertensionConfirmed" // hs
  val SELF_MONITORING_GLUCOSE = "SelfMonitoringGlucose"
  val SERUM_CREATININE = "Creatinine"
  val SEVERE_COGNITIVE_IMPAIRMENT = "SevereCognitiveImpairment"
  val SEVERE_DYSPNEA_RESPONDS_INADEQUATELY_TO_EMERGENCY_THERAPY = "SevereDyspneaRespondsInadequatelyToEmergencyTherapy"
  val SEVERE_DYSPNEA_WITH_RESPIRATOY_MUSCLE_FATIGUE = "SevereDyspneaWithRespiratoryMuscleFatigue"
  val SEVERE_LV_DYSFUNCTION = "SevereLVDysfunction"
  val SEVERE_RESPIRATORY_ACIDOSIS = "SevereRespiratoryAcidosis"
  val SIGN_OF_HMOD = "SignOfHMOD" // hs
  val SIGN_OF_PAIN = "SignOfPain" // hs
  val SIGNIFICANT_PLAQUE_ON_ANGIOGRAPHY = "SignificantPlaqueOnAngiography"
  val SIGNS_OF_LAST_DAYS = "SignsOfLastDays"
  val SLEEP_PROBLEMS = "SleepProblems"
  val SMOKING_STATUS= "SmokingStatus"
  val SPPB = "SPPB"
  val SPREADING_INFECTION = "SpreadingInfection"
  val SUSPICIOUS_ACUTE_ARTHROPATHY = "SuspicionOfAcuteArthropathy"
  val SYMPTOMATIC_BP = "SymptomaticBP"
  val SYMPTOMS_ADVERSELY_IMPACT_QUALITY_OF_LIFE = "SymptomsAdverselyImpactQualityOfLife"
  val BP = "BP"
  val SYSTOLIC_BP = "SystolicBP"
  val TIMED_UP_AND_GO_TEST = "TimedUpAndGoTest"
  val TOTAL_CHOLESTEROL = "TotalCholesterol"
  val UNCERTAIN_AGITATION_DELIRIUM = "UncertainAgitationDelirium"
  val UNCERTAIN_ALZHEIMER_DIAGNOSIS = "UncertainAlzheimerDiagnosis"
  val VIVIFRAIL_EXERCISE_OPTIONS = "VivifrailExerciseOptions"
  val VOLUME_DEPLETION = "VolumeDepletion"
  val WAIST_CIRCUMFERENCE = "WaistCircumference"
  val WEIGHT = "Weight"
  val WORSENING_HYPOXEMIA_OR_CORPULMONALE = "WorseningHypoxemiaOrCorpulmonale"
  val WORSENING_HYPERCAPNIA = "WorseningHypercapnia"

  // Conditions
  val ABNORMAL_SWEATING = "AbnormalSweating"
  val ACIDOSIS = "Acidosis"
  val ACUTE_CORONARY_SYNDROME = "AcuteCoronarySyndrome"
  val AGITATION = "Agitation"
  val ALCOHOLISM_WITH_COMPLICATIONS = "AlcoholismWithComplications"
  val ALZHEIMER = "Alzheimer"
  val AMPUTATION = "Amputation"
  val ANGINA_PECTORIS = "AnginaPectoris"
  val ANGIONEUROTIC_OEDAMA = "AngioneuroticOedama"
  val ANXIETY_EXTENDED = "AnxietyExtended"
  val AORTIC_ANEURYSM = "AorticAneurysm"
  val ARTHRITIS = "Arthritis"
  val ASCVD = "ASCVD"
  val ASTHMA = "Asthma"
  val ATHEROSCLEROSIS_OF_RENAL_ARTERY = "AtherosclerosisOfRenalArtery"
  val ATHLETES_PHYSICALLY_ACTIVE = "AthletesPhysicallyActive"
  val ATRIAL_FIBRILLATION = "AtrialFibrillation"
  val ANXIETY = "Anxiety"
  val BILATERAL_RENAL_ARTERY_STENOSIS = "BilateralRenalArteryStenosis"
  val BRADYCARDIA = "Bradycardia"
  val BREATHLESSNESS = "Breathlessness"
  val BRONCHIAL_ASTHMA = "BronchialAsthma"
  val CALLUS_OF_LIMB = "CallusOfLimb"
  val CANCER = "Cancer"
  val CARDIOVASCULAR_DISEASE_CAREPATH = "CardiovascularDiseaseCarepath"
  val CAROTID_ARTERY_DISEASE = "CarotidArteryDisease"
  val CEREBRAL_HAEMORRHAGE = "CerebralHaemorrhage"
  val CHF = "CHF"
  val CHF_EXT = "CHFExtended"
  val CHILD_BEARING = "ChildBearing"
  val CHRONIC_CORONARY_SYNDROMES = "ChronicCoronarySyndromes"
  val CHRONIC_COUGH = "ChronicCough"
  val CKD = "CKD"
  val CKD_STAGE_12 = "CKD1-2"
  val CKD_STAGE_23 = "CKD2-3"
  val CKD_STAGE_3 = "CKD3"
  val CKD_STAGE_345 = "CKD3-4-5"
  val CKD_STAGE_45 = "CKD4_5"
  val CKD_STAGE_5 = "CKD5"
  val COGNITIVE_IMPAIRMENT = "CognitiveImpairment"
  val COMA_ASSOCIATED_WITH_DIABETES = "ComaAssociatedWithDiabetes"
  val CONGESTIVE_HEART_FAILURE = "CongestiveHeartFailure"
  val CONSTIPATION = "Constipation"
  val COPD = "COPD"
  val COPD_WITH_EXACERBATION = "COPDWithExacerbation"
  val COR_PULMONALE = "CorPulmonale"
  val CORONARY_ARTERY_DISEASE = "ChronicIschemicHeartDisease"
  val CORONARY_REVASCULARIZATION = "CoronaryRevascularization"
  val COUGH = "Cough"
  val CVD = "CVD"
  val CYANOSIS = "Cyanosis"
  val DEFORMITY_OF_LIMB = "DeformityOfLimb"
  val DELIRIUM = "Delirium"
  val DELUSIONS = "Delusions"
  val DEPRESSION = "Depression"
  val DEPRESSION_EXTENDED = "DepressionExtended"
  val DETORIORATING_MOBILITY = "DetorioratingMobility"
  val DIABETES = "Diabetes"
  val DIABETIC_NEUROPATHY = "DiabeticNeuropathy"
  val DRY_MOUTH = "DryMouth"
  val DYSPHAGIA = "Dysphagia"
  val DYSPNEA = "Dyspnea"
  val EARLY_ONSET_MENOPAUSE = "EarlyOnsetMenopause"
  val EMPHYSEMA = "Emphysema"
  val ERECTILE_DYSFUNCTION = "ErectileDysfunction"
  val EXERTIONAL_BREATHLESSNESS = "ExertionalBreathlessness"
  val FALLS = "Falls"
  val FLASHES = "Flashes"
  val FRAILTY = "Frailty"
  val FREQUENT_WINTER_BRONCHITIS = "FrequentWinterBronchitis"
  val GANGRENE = "Gangrene"
  val GLUCOSE_INTOLERANCE = "GlucoseIntolerance"
  val GOUT = "Gout"
  val HAEMORRHAGIC_DISEASES = "HaemorrhagicDiseases"
  val HALLUCINATIONS = "Hallucinations"
  val HEART_FAILURE = "HeartFailure"
  val HEMATURIA = "RecurrentHaematuria"
  val HEPATOPATHY = "Hepatopathy"
  val HF_REDUCED = "HeartFailureReducedEjection"
  val HFPEF = "HFpEF"
  val HYPERCALCEMIA = "Hypercalcemia"
  val HYPERGLYCEMIC_CRISIS = "HyperglycemicCrisis"
  val HYPERCHOLESTEROLEMIA = "Hypercholesterolemia"
  val HYPERGLYCAEMIA = "Hyperglycaemia"
  val HYPERKALAEMIA = "Hyperkalaemia"
  val HYPERLIPIDEMIA = "Hyperlipidemia" // Dyslipidemia. They are the same.
  val HYPERSOMNIA = "Hypersomnia"
  val HYPERTENSION = "Hypertension"
  val HYPERTENSIVE_LVH = "HypertensiveLVH"
  val HYPOGLYCEMIA_DUE_TO_DIABETES = "HypoglycemiaDueToDiabetes"
  val HYPOKALAEMIA = "Hypokalaemia"
  val INCONTINENCE = "UrinaryIncontinenceOtherSpecified"
  val IRRITABILITY_AND_AGGRESSION = "IrritabilityAndAggression"
  val ISCHEMIC_ATTACK = "TransientIschemicAttack"
  val ISCHEMIC_STROKE = "IschaemicStroke"
  val LEG_OEDEMA = "LegOedema"
  val LIMB_ISCHAEMIA = "LimbIschaemia"
  val LOCALIZED_EDEMA = "LocalizedEdema"
  val LOSE_WARNING_SIGNS_OF_HYPOGLYCEMIA = "LoseWarningSignsOfHypoglycemia"
  val LV_DYSFUNCTION = "LVdysfunction"
  val MACRO_MICRO_ANGIOPATHY = "MacroMicroAngiopathy"
  val MACROVASCULAR_CONDITIONS = "MacrovascularConditions"
  val MALAISE_FATIGUE = "MalaiseFatigue"
  val MALNUTRITION = "Malnutrition"
  val MEMORY_LOSS = "MemoryLoss"
  val METABOLIC_SYNDROME = "MetabolicSyndrome"
  val METABOLIC_SYNDROME_SPECIFIED = "MetabolicSyndromeSpecified"
  val MICROVASCULAR_CONDITIONS = "MicrovascularConditions"
  val MYOCARDIAL_INFARCTION = "MyocardialInfarction"
  val MYOCARDIAL_REVASCULARIZATION = "MyocardialRevascularization"
  val NEUROPATHIC_PAIN = "NeuropathicPain"
  val NIGHT_TIME_CARDIOVASCULAR_EVENTS = "NightTimeCardiovascularEvents"
  val NOCTURIA = "Nocturia"
  val NOISY_RESPIRATORY_SECRETIONS = "NoisyRespiratorySecretions"
  val OEDEMA = "Oedema"
  val ORTHOSTATIC_HYPOTENSION = "OrthostaticHypotension"
  val OXYGEN_DEPENDENT_LUNG_DISEASE = "OxygenDependentLungDisease"
  val PALLIATIVE_CARE = "PalliativeCare"
  val PERIPHERAL_ARTERY_DISEASE = "PeripheralArteryDisease"
  val PERIPHERAL_ARTERY_DISEASE_UNSPECIFIED = "PeripheralArteryDiseaseUnspecified"
  val PERIPHERAL_NEUROPATHY = "PeripheralNeuropathy"
  val POSTURAL_HYPOTENSION = "PosturalHypotension"
  val POLYCYTHAEMIA = "Polycythaemia"
  val POOR_JUDGEMENT = "PoorJudgement"
  val PRIMARY_PULMONARY_HYPERTENSION = "PrimaryPulmonaryHypertension"
  val PRE_RETINAL_OR_VITREOUS_HAEMORRHAGE = "PreRetinalOrVitreousHaemorrhage"
  val PREDIABETES = "Prediabetes"
  val PREGNANCY = "Pregnancy"
  val RAISED_JUGULAR_VENOUS_PRESSURE = "RaisedJugularVenousPressure"
  val REGULAR_SPUTUM_PRODUCTION = "RegularSputumProduction"
  val RENAL_OUTFLOW_OBSTRUCTION = "RenalOutflowObstruction"
  val RENAL_REPLACEMENT_THERAPY = "RenalReplacementTherapy"
  val RESPIRATORY_FAILURE = "RespiratoryFailure"
  val RETINAL_DETACHMENT = "RetinalDetachment"
  val RHEUMATOID_ARTHRITIS = "RheumatoidArthritis"
  val RISK_OF_SUICIDE = "RiskOfSuicide"
  val RUBEOSIS_IRIDIS = "RubeosisIridis"
  val SARCOPENIA = "Sarcopenia"
  val SENT_HOME_FOR_DIAGNOSIS_CONFIRMATION = "SentHomeForDiagnosisConfirmation"
  val SEVERE_COGNITIVE_DISORDER = "SevereCognitiveDisorder"
  val SLEEP_DISORDERS = "SleepDisorders"
  val SINOATRIAL_BLOCK = "SinoatrialBlock"
  val SNORING = "Snoring"
  val STROKE = "Stroke"
  val SUDDEN_LOSS_OF_VISION = "SuddenLossOfVision"
  val SUDDEN_LOSS_OF_VISION_ICD10_CM = "SuddenLossOfVisionICD10CM"
  val SWOLLEN_LEGS = "SwollenLegs"
  val SYMPTOMATIC_HYPOXAEMIA = "SymptomaticHypoxaemia"
  val TACHYARRHYTHMIA = "Tachyarrhythmia"
  val THROMBOEMBOLIC_DISEASE = "ThromboembolicDisease"
  val TRANSIENT_ISCHEMIC_ATTACK = "TransientIschemicAttack"
  val TYPE1DIABETES  = "Type1Diabetes"
  val TYPE2DIABETES  = "Type2Diabetes"
  val TYPE2DIABETES_EXTENDED  = "Type2DiabetesExtended"
  val UNEXPLAINED_BLADDER_EMPTYING_PROBLEMS = "UnexplainedBladderEmptyingProblems"
  val UNEXPLAINED_DIARRHEA = "UnexplainedDiarrhea"
  val USING_SHORT_ACTING_BRONCHODILATOR = "UsingShortActingBronchodilator"
  val DIABETIC_ULCERATION = "DiabeticUlceration"
  val UNCONTROLLED_METASTATIC_CANCER = "UncontrolledMetastaticCancer"
  val UNEXPECTED_GASTRIC_BLOATING = "UnexplainedGastricBloating"
  val UNWANTED_SEDATION = "UnwantedSedation"
  val URINARY_RETENTION = "UrinaryRetention"
  val VISUAL_IMPAIRMENT = "VisualImpairment"
  val VOMITING = "Vomiting"
  val WHEEZING = "Wheezing"

  // Medications
  val ACEI = "ACEI"
  val ACHE_INHIBITORS = "Anticholinesterases"
  val ALPHA_BLOCKER = "AlphaBlocker"
  val ANTIDEPRESSANTS = "Antidepressants"
  val ANTIDIABETIC  = "Antidiabetic"
  val ANTIDIABETIC_DUAL_WITH_METFORMIN  = "AntidiabeticDualWithMetformin"
  val ANTIDIABETIC_DUAL_OTHER  = "AntidiabeticDualOther"
  val ANTIDIABETIC_TRIPLE_WITH_METFORMIN = "AntidiabeticTripleWithMetformin"
  val ANTIHYPERTENSIVES = "Antihypertensives"
  val ANTIPLATELET = "Antiplatelets"
  val ANTIPSYCHOTICS = "Antipsychotics"
  val APIXABAN = "Apixaban"
  val ARB = "ARB"
  val ASPIRIN = "Aspirin"
  val ATORVASTATIN = "Atorvastatin"
  val ATROPINE = "Atropine"
  val BETA_BLOCKER = "BetaBlocker"
  val BENZODIAZEPINE = "Benzodiazepine"
  val CCB = "CCB"
  val CLOPIDOGREL = "Clopidogrel"
  val CORTICOSTEROIDS = "Corticosteroids"
  val DAPAGLIFLOZIN = "Dapagliflozin"
  val DIGOXIN = "Digoxin"
  val DIURETICS = "Diuretics"
  val DPP4_INHIBITOR = "DPP4Inhibitor"
  val DHP_CCB = "DHPCCB"
  val DOAC = "DOAC"
  val ERYTHROMYCIN = "Erythromycin"
  val ERYTHROPIETIN = "Erythropoietin"
  val ESTROGENS = "Estrogens"
  val EZETIMIBE = "Ezetimibe"
  val GLIMEPIRIDE_AND_PIOGLITAZONE = "GlimepirideAndPioglitazone"
  val GLIMEPIRIDE_AND_ROSIGLITAZONE = "GlimepirideAndRosiglitazone"
  val GLP1 = "GLP1"
  val GLUCOCORTICOIDS = "Glucocorticoids"
  val GLYCOPYRRONIUM_BROMIDE = "GlycopyrroniumBromide"
  val GLYMIDINE = "Glymidine"
  val HYOSCINE_BUTYLBROMIDE = "HyoscineButylbromide"
  val HYOSCINE_HYDROBROMIDE = "HyoscineHydrobromide"
  val INHALED_CORTICOSTEROIDS = "InhaledCorticosteroids"
  val INSULIN = "Insulin"
  val INSULIN_DETEMIR = "InsulinDetemir"
  val IVABRADINE = "Ivabradine"
  val LABA_EXTENDED = "LABAExtended"
  val LAMA_EXTENDED = "LAMAExtended"
  val LOOP_DIURETIC = "LoopDiuretic"
  val METFORMIN = "Metformin"
  val METFORMIN_AND_SULFONYLUREAS = "MetforminAndSulfonylureas"
  val METOCLOPRAMIDE = "Metoclopramide"
  val NATEGLINIDE = "Nateglinide"
  val NON_DHP_CCB = "NonDHPCCB"
  val NPH_INTERMEDIATE_ACTING_INSULIN = "NPHIntermediateActingInsulin"
  val OAC = "OAC"
  val OPIOID = "Opioid"
  val PHENFORMIN_AND_SULFONYLUREAS = "PhenforminAndSulfonylureas"
  val PHOSPHODIESTERASE5_INHIBITOR = "Phosphodiesterase5Inhibitor"
  val PIOGLITAZONE = "Pioglitazone"
  val PPI = "PPI"
  val PRASUGREL = "Prasugrel"
  val RASA = "Rasa"
  val REPAGLINIDE = "Repaglinide"
  val ROSUVASTATIN = "Rosuvastatin"
  val SGLT2_INHIBITOR = "SGLT2Inhibitor"
  val SGLT2_INHIBITOR_EXTENDED = "SGLT2InhibitorExtended"
  val SHORT_ACTING_INSULIN = "ShortActingInsulin"
  val SIMVASTATIN = "Simvastatin"
  val SPIRONOLACTONE = "Spironolactone"
  val STATIN_EXTENDED = "StatinExtended"
  val SULFONYLUREAS = "Sulfonylureas"
  val SYMPATHOMIMETICS = "Sympathomimetics"
  val THIAZIDE_LIKE_DIURETIC = "ThiazideLikeDiuretic"
  val THIAZOLIDINEDIONES = "Thiazolidinediones"
  val TICAGRELOR = "Ticagrelor"
  val ULCERATION_THERAPY = "UlcerationTherapy"
  val VITAMIN_K_ANTAGONIST = "VitaminKAntagonist"

  // FamilyMemberHistory
  val FMH_HYPERCHOLESTEROLEMIA = "FMH-Hypercholesterolemia"

  // Other
  val EXTERNAL_CDS_GENDER = "ExternalCDSGender"
  val EXTERNAL_CDS_AGE = "ExternalCDSAge"
  val EXTERNAL_CDS_NATIONALITY = "ExternalCDSNationality"

  // Temporary GUI Observations
  val MODERATE_CKD = "ModerateCKD"
  val GENERAL_EXAMINATION = "GeneralExamination"

}
