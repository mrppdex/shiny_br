full_code_fun <- function(LOApath, LOAfile) {
  sprintf(
  'f1 <-
  strsplit("%s", "/")

root_path <- paste0(f1[[1]][1:(length(f1[[1]])-2)], collapse="/")

R_code_path <- paste0(root_path, "/code/")
BRdata_path <- paste0(root_path, "/BRdata/")

# dataLOA 
BR_LOA <- read.csv("%s")

source(paste0(R_code_path, "util.R"))

##########
# binary #
##########

BR_LOA_binary <- BR_LOA %%>%% filter(endpoint_type == "binary")

ana_data_binary <- NULL
for(i in 1:nrow(BR_LOA_binary)){
  LOA_i <- BR_LOA_binary[i,]
  
  input.ards <- read.csv(paste0(LOA_i$tfl_location, LOA_i$tfl_filename, "_ards.csv"))
  input.attributes <- read.csv(paste0(LOA_i$tfl_location, LOA_i$tfl_filename, "_ards_attributes.csv"))
  
  source(paste0(R_code_path,"lightning_binary.R"))
  
  ana_data_binary <- rbind(ana_data_binary, extracted_data)
}

#write.csv(ana_data_binary, file = paste0(BRdata_path, "BRdata_binary.csv"))

##############
# continuous #
##############

BR_LOA_continuous <- BR_LOA %%>%% filter(endpoint_type == "continuous")

ana_data_continuous <- NULL
for(i in 1:nrow(BR_LOA_continuous)){
  LOA_i <- BR_LOA_continuous[i,]
  
  input.ards <- read.csv(paste0(LOA_i$tfl_location, LOA_i$tfl_filename, "_ards.csv"))
  input.attributes <- read.csv(paste0(LOA_i$tfl_location, LOA_i$tfl_filename, "_ards_attributes.csv"))
  
  source(paste0(R_code_path,"lightning_continuous.R"))
  
  ana_data_continuous <- rbind(ana_data_continuous, extracted_data)
}

#write.csv(ana_data_continuous, file = paste0(BRdata_path, "BRdata_continuous.csv"))', LOApath, LOAfile)
}
