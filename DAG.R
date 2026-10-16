#####################################################################################
#####################################################################################
########################## DAG: MASLD AND OFFSPRING NDD #############################
#####################################################################################
#####################################################################################

# Date: 2.10.2026

# Tool used: https://www.dagitty.net/
  # Online version ("Launch DAGitty online in your browser.")
  # Specification: "Adjustment (total effect)"

# Code

dag {
CalendarYearOfDelivery [pos="-3.000,3.000"]
MaternalAgeAtDelivery [pos="-3.000,-1.000"]
MaternalDM2 [pos="-2.000,2.000"]
MaternalDyslipidemia [pos="1.000,2.000"]
MaternalGDM [pos="-1.000,2.000"]
MaternalGHT [pos="0.000,2.000"]
MaternalLowEducationLevel [pos="0.100,-1.000"]
MaternalMASLD [exposure,pos="-2.000,1.000"]
MaternalNDD [pos="-2.000,-1.000"]
MaternalObesity [pos="-1.000,0.000"]
MaternalPsychiatricDisease [pos="-1.000,-1.000"]
MaternalSmoking [pos="1.000,-1.000"]
OffspringNDD [outcome,pos="2.000,1.000"]
OffspringSex [pos="2.000,-1.000"]
Parity [pos="-3.500,0.000"]
CalendarYearOfDelivery -> MaternalMASLD
CalendarYearOfDelivery -> OffspringNDD
MaternalAgeAtDelivery -> MaternalDM2
MaternalAgeAtDelivery -> MaternalDyslipidemia
MaternalAgeAtDelivery -> MaternalGDM
MaternalAgeAtDelivery -> MaternalGHT
MaternalAgeAtDelivery -> MaternalMASLD
MaternalAgeAtDelivery -> MaternalObesity
MaternalAgeAtDelivery -> OffspringNDD
MaternalDM2 -> MaternalMASLD
MaternalDM2 -> OffspringNDD
MaternalDyslipidemia -> MaternalMASLD
MaternalDyslipidemia -> OffspringNDD
MaternalGDM -> MaternalMASLD
MaternalGDM -> OffspringNDD
MaternalGHT -> MaternalMASLD
MaternalGHT -> OffspringNDD
MaternalLowEducationLevel -> MaternalMASLD
MaternalLowEducationLevel -> MaternalSmoking
MaternalLowEducationLevel -> OffspringNDD
MaternalMASLD -> OffspringNDD
MaternalMASLD <-> MaternalNDD
MaternalMASLD <-> MaternalPsychiatricDisease
MaternalNDD -> OffspringNDD
MaternalObesity -> MaternalDM2
MaternalObesity -> MaternalDyslipidemia
MaternalObesity -> MaternalGDM
MaternalObesity -> MaternalGHT
MaternalObesity -> MaternalMASLD
MaternalObesity -> OffspringNDD
MaternalPsychiatricDisease -> OffspringNDD
MaternalSmoking -> MaternalMASLD
MaternalSmoking -> OffspringNDD
OffspringSex -> OffspringNDD
Parity -> MaternalGDM
Parity -> MaternalGHT
Parity -> MaternalMASLD
Parity -> OffspringNDD
}
