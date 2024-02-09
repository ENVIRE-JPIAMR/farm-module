## Function for loading input variables from csv list

load_inputs <- function() {
  
  ## read input variables
  input_file <- here("inputs.csv")
  df_read <- read.csv(input_file, header = TRUE, sep = ';')
  
  ## parsing objects from input list
  input_objects = list(
    water_consum.min    = eval(parse(text = df_read$Value[df_read$Variable == "water_consum.min"])),
    water_consum.max    = eval(parse(text = df_read$Value[df_read$Variable == "water_consum.max"])),
    water_consum.mean   = eval(parse(text = df_read$Value[df_read$Variable == "water_consum.mean"])),
    weight              = eval(parse(text = df_read$Value[df_read$Variable == "weight"])),
    daily_gain          = eval(parse(text = df_read$Value[df_read$Variable == "daily_gain"])),
    daily_intake        = eval(parse(text = df_read$Value[df_read$Variable == "daily_intake"]))
  )
  
  ## parsing non-objects from input list
  idx_double <- df_read$Type != "OBJECT"
  df_double <-
    data.frame(id = df_read$Variable[idx_double],
               val = unlist(lapply(df_read$Value[idx_double], function(x)
                 eval(parse(
                   text = x
                 )))))
  
  named_vector <- with(df_double, setNames(val, id))
  input_doubles <- lapply(split(named_vector, names(named_vector)), unname)
  
  input_list <- c(input_objects, input_doubles) 
  
  return(input_list)
}

