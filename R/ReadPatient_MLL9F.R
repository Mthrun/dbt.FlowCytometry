ReadPatient_MLL9F=function(Patient,Key,Filnames,Fullpaths){
  
  ind_pat=which(Key2Id_MLL9F(Key)==Patient)
  
  if(length(ind_pat)==3){
    P11=ReadLRN(Filenames[ind_pat[1]],Fullpaths[ind_pat[1]])
    P12=ReadLRN(Filenames[ind_pat[2]],Fullpaths[ind_pat[2]])
    P13=ReadLRN(Filenames[ind_pat[3]],Fullpaths[ind_pat[3]])
    
    return(list(PT1=P11,PT2=P12,PT3=P13))
  }
}