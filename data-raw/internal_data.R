
## trust lookup
trust_lkp <- c(
  "R0D" = "UNIVERSITY HOSPITALS DORSET NHS FOUNDATION TRUST",
  "RA7" = "UNIVERSITY HOSPITALS BRISTOL AND WESTON NHS FOUNDATION TRUST",
  "RA9" = "TORBAY AND SOUTH DEVON NHS FOUNDATION TRUST",
  "RBD" = "DORSET COUNTY HOSPITAL NHS FOUNDATION TRUST",
  "RD1" = "ROYAL UNITED HOSPITALS BATH NHS FOUNDATION TRUST",
  "REF" = "ROYAL CORNWALL HOSPITALS NHS TRUST",
  "RH5" = "SOMERSET NHS FOUNDATION TRUST",
  "RH8" = "ROYAL DEVON UNIVERSITY HEALTHCARE NHS FOUNDATION TRUST",
  "RJ8" = "CORNWALL PARTNERSHIP NHS FOUNDATION TRUST",
  "RK9" = "UNIVERSITY HOSPITALS PLYMOUTH NHS TRUST",
  "RN3" = "GREAT WESTERN HOSPITALS NHS FOUNDATION TRUST",
  "RNZ" = "SALISBURY NHS FOUNDATION TRUST",
  "RTE" = "GLOUCESTERSHIRE HOSPITALS NHS FOUNDATION TRUST",
  "RVJ" = "NORTH BRISTOL NHS TRUST"
)

## specialty lookup
treatment_function_codes <- c(
  "(:?C_|[INA]P)?100" = "General Surgery",
  "(:?C_|[INA]P)?101" = "Urology",
  "(:?C_|[INA]P)?110" = "Trauma and Orthopaedic",
  "(:?C_|[INA]P)?120" = "Ear Nose and Throat",
  "(:?C_|[INA]P)?130" = "Ophthalmology",
  "(:?C_|[INA]P)?140" = "Oral Surgery",
  "(:?C_|[INA]P)?150" ="Neurosurgical",
  "(:?C_|[INA]P)?160" = "Plastic Surgery",
  "(:?C_|[INA]P)?170" = "Cardiothoracic Surgery",
  "C_300" = "General Internal Medicine",
  "(:?C_|[INA]P)?301" = "Gastroenterology",
  "(:?C_|[INA]P)?320" = "Cardiology",
  "(:?C_|[INA]P)?330" = "Dermatology",
  "(:?C_|[INA]P)?340" = "Respiratory Medicine",
  "(:?C_|[INA]P)?400" = "Neurology",
  "(:?C_|[INA]P)?410" = "Rheumatology",
  "(:?C_|[INA]P)?430" = "Elderly Medicine",
  "(:?C_|[INA]P)?502" = "Gynaecology",
  "X0[1-6]" = "Other",
  "C_999" = "Total"
)

usethis::use_data(
  trust_lkp,
  treatment_function_codes,
  internal = TRUE,
  overwrite = TRUE
)
