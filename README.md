# R shiny app for optimization of monthy costs

## Allows input of:

1) Total monthly budget
  - Desired monthly savings
  - Minimum and maximum amounts for four expense categories:
    - Housing (rent and utilities)
    - Food
    - Transportation
    - Luxury/entertainment
      
2) Uses linear programming to optimize expense allocation to:
  - Minimize total expenses
  - Respect given constraints (min/max for each category)
  - Ensure the desired monthly savings

3) Displays results:
  - Textually - the optimal amount for each category
  - Graphically - a simple bar chart
