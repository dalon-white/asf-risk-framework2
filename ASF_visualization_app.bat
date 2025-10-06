@echo on
REM Set the working directory to the root project folder
cd /d "c:\Users\Dalon.White\OneDrive - USDA\Desktop\Projects\asf-risk-framework2\asf-risk-framework2"
echo Setting up working directory for proper file path handling...

REM Run the setup script to check and prepare the environment
echo Running setup script to prepare the environment...
"C:\Program Files\R\R-4.5.0\bin\Rscript.exe" setup_for_visualization.R
if %ERRORLEVEL% NEQ 0 (
    echo Setup script failed with default R path, trying alternative paths...
    
    if exist "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" (
        "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" setup_for_visualization.R
    ) else if exist "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" (
        "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" setup_for_visualization.R
    )
)

echo Running Shiny app...
REM Call to the R executable to run the Shiny app with the correct working directory
"C:\Program Files\R\R-4.5.0\bin\R.exe" -e "setwd('c:/Users/Dalon.White/OneDrive - USDA/Desktop/Projects/asf-risk-framework2/asf-risk-framework2'); shiny::runApp('scripts/ASF risk map shiny app.R', launch.browser = TRUE)"
REM if there is a failed launch, try alternative R versions
if %ERRORLEVEL% NEQ 0 (
    echo Failed to run Shiny app with default R path
    echo Trying alternative paths...
      if exist "C:\Program Files\R\R-4.4.2\bin\R.exe" (
        "C:\Program Files\R\R-4.4.2\bin\R.exe" -e "setwd('c:/Users/Dalon.White/OneDrive - USDA/Desktop/Projects/asf-risk-framework2/asf-risk-framework2'); shiny::runApp('scripts/ASF risk map shiny app.R', launch.browser = TRUE)"
    ) else if exist "C:\Program Files\R\R-4.4.1\bin\R.exe" (
        "C:\Program Files\R\R-4.4.1\bin\R.exe" -e "setwd('c:/Users/Dalon.White/OneDrive - USDA/Desktop/Projects/asf-risk-framework2/asf-risk-framework2'); shiny::runApp('scripts/ASF risk map shiny app.R', launch.browser = TRUE)"
    ) else if exist "C:\Program Files\R\R-4.3.2\bin\R.exe" (
        "C:\Program Files\R\R-4.3.2\bin\R.exe" -e "setwd('c:/Users/Dalon.White/OneDrive - USDA/Desktop/Projects/asf-risk-framework2/asf-risk-framework2'); shiny::runApp('scripts/ASF risk map shiny app.R', launch.browser = TRUE)"
    ) else if exist "C:\Program Files\R\R-4.3.1\bin\R.exe" (
        "C:\Program Files\R\R-4.3.1\bin\R.exe" -e "setwd('c:/Users/Dalon.White/OneDrive - USDA/Desktop/Projects/asf-risk-framework2/asf-risk-framework2'); shiny::runApp('scripts/ASF risk map shiny app.R', launch.browser = TRUE)"
    ) else (
        echo R not found in expected locations.
        echo To run the app, open RStudio and run:
        echo setwd('c:/Users/Dalon.White/OneDrive - USDA/Desktop/Projects/asf-risk-framework2/asf-risk-framework2')
        echo shiny::runApp('scripts/ASF risk map shiny app.R')
    )
)
echo.
echo Press any key to close this window...
pause > nul
