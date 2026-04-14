## a simple function to check if a directory exists, and to create it if not

check_and_create_dir <- function(path){
  
  if(!dir.exists(path)){
    
    dir.create(path)
    
  }
  
}
