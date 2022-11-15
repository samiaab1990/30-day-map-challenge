make_gradient<-function(string_lab=NULL, start_color=NULL, end_color=NULL)
{
  if (is.null(start_color) & is.null(end_color))
  {
     stop("Function requires both start and end color.")
  } 
  
  if (is.null(start_color)|is.null(end_color))
  {
    stop("Function requires both start and end color.")
  }
  
  if(is.null(string_lab) | !is.character(string_lab))
  {
    stop("Function requires character string.")
  }
  
  
  pal = colorRampPalette(c(start_color,end_color))
  
  lab = string_lab
  
  pal_n<-pal(nchar(lab))
  
  make_lab<-""
  
  for(i in 1:nchar(lab))
  {
    j = i 
    
    if (substr(lab,i,i) !=" ")
    {
      make_lab = paste0(make_lab,"<span style='color:",pal_n[j],";'>",substr(lab,i,i),"</span>")
    } else
    {
      make_lab<-paste0(make_lab," ")
      j = i - 1
    }
    
  } 
  return(make_lab)
}