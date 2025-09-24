@echo on
cd /d "c:\Users\Dalon.White\OneDrive - USDA\Desktop\Projects\asf-risk-framework2\asf-risk-framework2\scripts"
echo Running Shiny app...
"C:\Program Files\R\R-4.5.0\bin\R.exe" -e "shiny::runApp('ASF risk map shiny app.R', launch.browser = TRUE)"
if %ERRORLEVEL% NEQ 0 (
    echo Failed to run Shiny app with default R path
    echo Trying alternative paths...
    
    if exist "C:\Program Files\R\R-4.4.2\bin\R.exe" (
        "C:\Program Files\R\R-4.4.2\bin\R.exe" -e "shiny::runApp('ASF risk map shiny app.R', launch.browser = TRUE)"
    ) else if exist "C:\Program Files\R\R-4.4.1\bin\R.exe" (
        "C:\Program Files\R\R-4.4.1\bin\R.exe" -e "shiny::runApp('ASF risk map shiny app.R', launch.browser = TRUE)"
    ) else (
        echo R not found in expected locations.
        echo To run the app, open RStudio and run:
        echo shiny::runApp('c:/Users/Dalon.White/OneDrive - USDA/Desktop/Projects/asf-risk-framework2/asf-risk-framework2/scripts/ASF risk map shiny app.R')
    )
)
echo.
echo Press any key to close this window...
pause > nul
