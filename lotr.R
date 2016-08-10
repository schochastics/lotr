########################################
#R script for the blog post about Lord of the Rings
#mildlyscientific.schochastics.net
########################################

library(XML)
library(stringr)
library(igraph)

####################################################################################################
#helper functions
####################################################################################################
#function to extract character names from the html code of the script
get.scene.char=function(text){
  m=regexpr("<b>[ ]{2,}[A-Z]{3,}",text)
  raw.characters=(regmatches(text,m))
  raw.characters=str_trim(gsub("<b>[ ]*","",raw.characters))
  idx=grep("(ANGLE|CUT|CLOSE|DISSOLVE|BLACK|FARMER|LATER|QUICK|CRANE|FADE)",raw.characters,invert=T)
  return(unique(raw.characters[idx]))
}

#function to create an edgelist of characters appearing in a scene
get.scene.interaction=function(char.list){
  if(length(char.list)==2){
    return(char.list)
  }
  else if(length(char.list)<2){
    return()
  }
  A=outer(char.list,char.list,function(x,y) paste(x,y,sep=" "))
  A=A[lower.tri(A)]
  el=matrix(unlist(strsplit(A," ")),ncol=2,nrow=length(A),byrow=T)
  return(el)
}

#function to look for INT/EXT description for scene starts
get.scene.starts=function(url){
  doc=htmlParse(url)
  text=getNodeSet(doc,"//pre")
  
  text.char <- sapply(text, xmlValue)
  text.clean=unlist(strsplit(text.char,"\n"))
  text.clean=str_trim(text.clean)
  scene.start=grep("(INT.|EXT.)",text.clean)
  return(scene.start)
}

#function to convert html code to plain text. Also recover some mispells
get.text.body=function(url){
  doc=htmlParse(url)
  text=getNodeSet(doc,"//pre")
  fl <- saveXML(text[[1]], tempfile())
  text.char=readLines(fl)
  text.char=gsub("FRO DO","FRODO",text.char)
  text.char=gsub("STRIDER","ARAGORN",text.char)
  text.char=gsub("CANDALF","GANDALF",text.char)
  text.char=gsub("DEAGOL","SMEAGOL",text.char)
  text.char=gsub("G AN DA LF","GANDALF",text.char)
  return(text.char)
}
#####################################################################
#####################################################################

#urls to the scripts of the movie. The second movie is not in the same format and thus handled differently
urls=c("http://www.imsdb.com/scripts/Lord-of-the-Rings-Fellowship-of-the-Ring,-The.html",
       "http://www.imsdb.com/scripts/Lord-of-the-Rings-The-Two-Towers.html",#brokenoO
       "http://www.imsdb.com/scripts/Lord-of-the-Rings-Return-of-the-King.html")
#select movie to analyse
movie=3

#get scene start indices
scene.start=get.scene.starts(urls[movie])

#get the script as a character vector
text.char=get.text.body(urls[movie])

#loop over scenes and create edgelist
el=matrix(0,0,2)
for(i in 1:(length(scene.start)-1)){
  start=scene.start[i]+1
  end=scene.start[i+1]-1
  scene.interactions=get.scene.interaction(get.scene.char(text.char[start:end]))
  if(length(scene.interactions)>1){
    el=rbind(el,scene.interactions)
  }
}

#create the graph and contract multiple edges to weighted edges
g=graph_from_edgelist(el,directed=F)
E(g)$weight=1
g=simplify(g,edge.attr.comb = "sum")
plot(g)
####################################################################################################
#lotr1 specific adjustments. Some names are not parsed correctly
V(g)$name[V(g)$name=="ORC"]="ORC OVERSEER"
V(g)$name[V(g)$name=="WITCH"]="WITCH KING"
V(g)$name[V(g)$name=="ODO"]="ODO PROUDFOOT"

#add the race and good/evil/neutral attribute (done manually)
race=c("hobbit","wizard","human","elf","dwarf","other","orc")
group=c("good","evil","neutral")
V(g)$race=race[c(6,4,1,1,2,1,1,1,1,2,3,3,3,7,4,6,4,3,3,4,5,4,3,7)]
V(g)$group=group[c(2,1,1,1,1,1,1,1,3,2,3,3,1,2,1,2,1,1,1,1,1,1,3,2)]
write.graph(g,"lotr1.graphml","graphml")
####################################################################################################
#lotr3 specific.  Some names are not parsed correctly
V(g)$name[V(g)$name=="ELF"]="ELF ESCORT"
V(g)$name[V(g)$name=="WITCH"]="WITCH KING"
V(g)$name[V(g)$name=="GATE"]="GATE GUARD"
V(g)$name[V(g)$name=="KING"]="KING OF THE DEATH"

#delete orc and rohan charactre since they are wrong
g=delete.vertices(g,V(g)$name=="ORC")
g=delete.vertices(g,V(g)$name=="ROHAN")

#add the race and good/evil/neutral attribute (done manually)
race=c("hobbit","wizard","human","elf","dwarf","other","orc")
group=c("good","evil","neutral")
V(g)$race=race[c(3,6,1,1,1,1,2,5,6,3,3,3,4,4,4,3,7,3,3,3,3,3,4,6,3,3,4,3,6,3,3,4,7,7,3,1)]
V(g)$group=group[c(3,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,2,1,1,1,1,3,3,1,1,2,2,1,1)]
write.graph(g,"lotr3.graphml","graphml")
####################################################################################################
####################################################################################################
####################################################################################################
#second movie script from www.fempiror.com/otherscripts/LordoftheRings2-TTT.pdf converted to txt file
text=readLines("LordoftheRings2-TTT.txt")

#scene starts are parsed differently here
scene.start=grep("(INT.|EXT.)",text)

#loop over scenes and create interactions
el=matrix(0,0,2)
for(i in 3:(length(scene.start)-1)){
  start=scene.start[i]+1
  end=scene.start[i+1]-1
  m=regexpr("^[A-Z-]+$",text[start:end])
  raw.characters=(regmatches(text[start:end],m))
  #the regex for names is not perfect, such that some words have to be deleted manually
  idx=grep("(ANGLE|CUT|CLOSE|DISSOLVE|BLACK|FARMER|LATER|QUICK|CRANE|FADE|SKY|HIGH|WIDE|CONTINUED|SMOKES|STRENGTH|SLOW|DELVINGS|SUPER|BOX|DOOM|INSERT|LOW|SUDDENLY)",raw.characters,invert=T)
  raw.characters=unique(raw.characters[idx])
  scene.interactions=get.scene.interaction(raw.characters)
  if("MERRY" %in% raw.characters & "ARAGORN" %in% raw.characters){ #there is no interaction between aragorn and pipin. it is just a flashback
    next()
  }
  if(length(scene.interactions)>1){
    el=rbind(el,scene.interactions)
  }
}

g=graph_from_edgelist(el,directed=F)
E(g)$weight=1
g=simplify(g,edge.attr.comb = "sum")
plot(g)

#add the race and good/evil/neutral attribute (done manually)
race=c("hobbit","wizard","human","elf","dwarf","other","orc")
group=c("good","evil","neutral")
V(g)$race=race[c(1,1,6,3,7,1,7,1,7,4,3,5,3,3,3,3,3,3,7,7,7,6,2,3,3,3,2,3,4,4,3,7,3,3,3,3,3,4,3)]
V(g)$group=group[c(1,1,2,3,2,1,2,1,2,1,1,1,3,3,3,1,1,2,2,2,2,1,1,1,3,3,2,1,1,1,1,3,3,3,1,1,3,1,3)]
write.graph(g,"lotr2.graphml","graphml")

