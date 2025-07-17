# org_name_lkp errors

    Code
      org_name_lkp(names = "BUCKSHAW HOSPITAL", type = "PROVIDER Org")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "NHS Region", "Provider Parent", "Provider Org", "Commissioner Parent", "Commissioner Org"

# filters_displays errors

    Code
      filters_displays(nhs_only = TRUE, trust_parents = "NHS LANCASHIRE AND SOUTH CUMBRIA INTEGRATED CARE BOARD",
        trusts = "FULWOOD HALL HOSPITAL", comm_parents = c(
          "NHS SOUTH YORKSHIRE INTEGRATED CARE BOARD",
          "NHS NORTH EAST LONDON INTEGRATED CARE BOARD"), comms = NULL, spec = "Total")
    Condition
      Error in `match.arg()`:
      ! 'arg' must be NULL or a character vector

