## Assignment 1. Nadav Kempinski. 1/6/2026

#### Function: Model energy production created by solar panels
## Inputs: A (solar panel area, m^2)
##         r (panel yield [0-1])
##         H (annual average solar radiation, kWh/m^2)
##         PR (performance ratio, [0-1])
## Output: E (energy produced, kWh)
model_energy = function(A, r = 0.2, H, PR = 0.75) {
  # Check to confirm all values are numerics
  numeric_check = is.numeric(A) && is.numeric(r) && is.numeric(H) && is.numeric(PR)
  
  if(!numeric_check) {
    message("Parameters are non-numeric. Please check your input and try again.")
    return(NULL)
  }
  
  E = A * r * H * PR
  return(E)
}
