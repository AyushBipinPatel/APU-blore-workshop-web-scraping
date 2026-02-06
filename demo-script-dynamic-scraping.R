# goal -------------------------------------------------------------------

## Learn about dynamic webscraping using R9.3.1 of MGNREGA

## Notice how the link does not change when inputs are changed/provided

## URL: https://mnregaweb4.nic.in/netnrega/SocialAuditFindings/SAU_action_taken_report.aspx?lflag=eng&fin_year=2025-2026&source=national&labels=labels&rep_type=SoA&Digest=thAWvpUnSGmcxYqvoy+Drg

# libraries --------------------------------------------------------------

library(tidyverse) # for data manipulation, iteration and structuring
library(rvest) # for static webpage scraping
library(selenider) # for dynamic webpage scraping


# store url --------------------------------------------------------------

r931 <- "https://mnregaweb4.nic.in/netnrega/SocialAuditFindings/SAU_action_taken_report.aspx?lflag=eng&fin_year=2025-2026&source=national&labels=labels&rep_type=SoA&Digest=thAWvpUnSGmcxYqvoy+Drg"

## Visit this page to see possible options and arrive at a plan on how to scrape...

## There are two inputs

  ## Financial Year 
  ## Issue type

  # is there a way to programatically get all the options for a given input?
  # Are all options of a input valid across selection of all options of a different input?

## Take a manual walk through of the site.

## GOAL: GET STATE LEVEL REPORTS ACROSS ALL FY FOR ALL KINDS OF ISSUES


# start a selenider session ----------------------------------------------

nregs_session <- selenider_session(
  session = "chromote",
  timeout = 30,
  options = chromote_options(headless = FALSE) # what is this?
)


# navigate to the url of thr9.3.1 ----------------------------------------

open_url(r931)


# get the list of options for all inputs ---------------------------------

## FY year

  # Identify the element and get all children and appropriate values

  FY_opts <- s("#ctl00_ContentPlaceHolder1_ddl_finyr") |>
                elem_children()  # notice these are all the options for FY, though not how we wish

  FY_opts[-1] -> FY_opts # removed the "select" option

  # get the text as well as the values of the options

  tibble(
    fy_opts_text = map_chr(
                    FY_opts |>
                      as.list(),
                    elem_text
                    ),
    fy_opts_value = map_chr(
                    FY_opts |>
                      as.list(),
                    elem_attr,"value"
                    ) 
  ) -> fy_details


##  ISSUE type

  issue_opts <- s("#ctl00_ContentPlaceHolder1_ddl_issue") |>
                  elem_children()

  issue_opts[-1] -> issue_opts

  tibble(
    issues_opts_text = map_chr(
      issue_opts |>
        as.list(),
      elem_text
    ),
    issue_opts_value = map_chr(
      issue_opts |>
        as.list(),
      elem_attr,"value"
    )
  ) -> issues_details

## create a sequence of inputs

  expand.grid(fy_details$fy_opts_value, issues_details$issues_opts_text) |>
    tibble() -> input_sequence

## A detour: how to send/click/set values

   # elem_click(), elem_set_value(), and more

  ## I want to show how to set values for a dropdown

  s("#ctl00_ContentPlaceHolder1_ddl_finyr") |>
    elem_select(text = "2024-2025")

  s("#ctl00_ContentPlaceHolder1_ddl_finyr") |>
    elem_select(index = 5)
  
  ###

  s("#ctl00_ContentPlaceHolder1_ddl_issue") |>
    elem_select(text = "Financial Misappropriation")

  ### press the button

  s("#ctl00_ContentPlaceHolder1_btnSubmit") |>
    elem_click()

  nregs_session |>
    get_page_source() |>
    html_element("div.dataTables_scrollBody table") |>
    html_table()

nregs_session$driver$close()


## Wrap it nicely in a function

get_r931_tabs <- function(inp1, inp2){

  s("#ctl00_ContentPlaceHolder1_ddl_finyr") |>
    elem_select(text = inp1)
  
  s("#ctl00_ContentPlaceHolder1_ddl_issue") |>
    elem_select(text = inp2)

  s("#ctl00_ContentPlaceHolder1_btnSubmit") |>
    elem_click()
  
  Sys.sleep(10)
  
  s("div.dataTables_scrollBody table") |>
  read_html()|>
  html_table(header = T) |>
    pluck(1) |>
    mutate(
      tab_id = paste(inp1,inp2,"-")
    ) |>
    janitor::clean_names()-> tab

  return(tab)
}

get_r931_tabs("2023-2024", "Financial Deviation")

## running iterations

nregs_session <- selenider_session(
  session = "chromote",
  timeout = 30,
  options = chromote_options(headless = T)
)

  open_url(r931)

pmap(
  list(
    inp1 = input_sequence$Var1[1:3],
    inp2 = input_sequence$Var2[1:3]
  ),
  get_r931_tabs
)


# close session ----------------------------------------------------------

nregs_session$driver$close()
