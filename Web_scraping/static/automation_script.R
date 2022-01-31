# automate the script every 15 min ----------------------------------------

taskscheduleR::taskscheduler_create(
  taskname = "web_scraping_reddit_TEST",
  rscript = "C:/Users/LucaK/Desktop/GitHub/RStudio/Web_scraping/static/Web_scraping_in_R.R",
  schedule = "MINUTE",
  modifier = 15,
  starttime = "16:00",
  startdate = "31/01/2022",
  Rexe = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe")
  )