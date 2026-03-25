# OmicsStack Application
# ==============================================================================
# This file sources the modular components and runs the Shiny application.
# 
# File Structure:
#   - global.R  : Package loading, helper functions, global variables
#   - ui.R      : User interface definition
#   - server.R  : Server logic and reactive functions
#   - app.R     : This file (entry point)
# ==============================================================================

# Source the global setup (packages, helpers, global variables)
source("global.R", local = FALSE)

# Source the UI definition
source("ui.R", local = FALSE)

# Source the server logic
source("server.R", local = FALSE)

# Run the Shiny application
shinyApp(ui = ui, server = server)
