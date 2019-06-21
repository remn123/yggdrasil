
header <- dashboardHeader(
  title = "Yggdrasil"
)

sidebar <- dashboardSidebar(
  menuItem(
      "Yggdrasil",
      menuSubItem(
        "Create Initial Tree"
      ),
      menuSubItem(
        "Train Tree"
      ),
      menuSubItem(
        "Prune Tree"
      )
  ),
  menuItem(
      "Summary"
  )
)

body <- dashboardBody(
  selectWithBarsUI("select_x", label='1ª Variável'),
  verticalLayout(
    #selectWithBarsUI("select_y",label='2ª Variável'),
    actionButton("go", "Go"),
    yggUI('tree')
    #setBoxUI('settings')
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body 
)