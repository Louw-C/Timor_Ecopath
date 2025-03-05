#Code to include species names to Alex's 2016 fish survey.
#I asked Claude to create code for me to do this.

# Load required libraries
library(readxl)
library(dplyr)
library(writexl)

# Set your file paths
# Go up two levels from Timor_Ecopath, then into Potential data
input_path <- "../../Potential data/Alex data/RAW Data/Timor biomass survey 2016.xlsx"
output_path <- "../../Potential data/Alex data/RAW Data/Timor_biomass_survey_2016_with_species.xlsx"

# Create species code lookup table
species_lookup <- tribble(
  ~species_code, ~scientific_name,
  # Acanthuridae (Surgeonfish)
  "aca.bari", "Acanthurus bariene",
  "aca.bloc", "Acanthurus blochii",
  "aca.line", "Acanthurus lineatus",
  "aca.mata", "Acanthurus mata",
  "aca.ncus", "Acanthurus nigricans",
  "aca.nigr", "Acanthurus nigricauda",
  "aca.nuda", "Acanthurus nigrofuscus",
  "aca.pyro", "Acanthurus pyroferus",
  "aca.trio", "Acanthurus triostegus",
  "aca.xant", "Acanthurus xanthopterus",
  "cte.stri", "Ctenochaetus striatus",
  "cten.str", "Ctenochaetus striatus",
  "nas.brevi", "Naso brevirostris",
  "nas.hexa", "Naso hexacanthus",
  "nas.litu", "Naso lituratus",
  "nas.unic", "Naso unicornis",
  "nas.vlam", "Naso vlamingii",
  "zeb.scop", "Zebrasoma scopas",
  
  # Anthiidae (Anthias)
  "psa.disp", "Pseudanthias dispar",
  "psa.hutc", "Pseudanthias hutchii",
  "psa.squa", "Pseudanthias squamipinnis",
  
  # Apogonidae (Cardinalfish)
  "apo.spp", "Apogon species",
  
  # Balistidae (Triggerfish)
  "bal.viri", "Balistoides viridescens",
  "blp.undu", "Balistapus undulatus",
  "mel.nige", "Melichthys niger",
  "mel.vidu", "Melichthys vidua",
  "odo.nige", "Odonus niger",
  "suf.spp", "Sufflamen species",
  
  # Caesionidae (Fusiliers)
  "cae.caer", "Caesio caerulaurea",
  "cae.tere", "Caesio teres",
  "pte.caer", "Pterocaesio caerulaurea",
  "pte.diag", "Pterocaesio digramma",
  "pte.tile", "Pterocaesio tile",
  
  # Carangidae (Jacks)
  "crx.mela", "Caranx melampygus",
  "crx.tile", "Caranx tille",
  
  # Chaetodontidae (Butterflyfish)
  "Cha.raff", "Chaetodon rafflesii",
  "cha.adie", "Chaetodon adiergastos",
  "cha.auri", "Chaetodon auriga",
  "cha.baro", "Chaetodon baronessa",
  "cha.kle", "Chaetodon kleinii",
  "cha.kleI", "Chaetodon kleinii",
  "cha.klei", "Chaetodon kleinii",
  "cha.luna", "Chaetodon lunula",
  "cha.luns", "Chaetodon lunulatus",
  "cha.mela", "Chaetodon melannotus",
  "cha.meye", "Chaetodon meyer",
  "cha.ocel", "Chaetodon ocellicaudus",
  "cha.orna", "Chaetodon ornatissimus",
  "cha.raff", "Chaetodon rafflesii",
  "cha.spec", "Chaetodon speculum",
  "cha.tlis", "Chaetodon trifascialis",
  "cha.vaga", "Chaetodon vagabundus",
  "for.flav", "Forcipiger flavissimus",
  "forc.flav", "Forcipiger flavissimus",
  "hen.acum", "Heniochus acuminatus",
  "hen.chry", "Heniochus chrysostomus",
  "hen.vari", "Heniochus varius",
  "hmt.poly", "Hemitaurichthys polylepis",
  
  # Clupeidae (Herrings)
  "sar.dine", "Sardinella species",
  
  # Ephippidae (Batfish)
  "plx.orbi", "Platax orbicularis",
  
  # Haemulidae (Sweetlips)
  "dia.pict", "Diagramma pictum",
  "ple.gibb", "Plectorhinchus gibbosus",
  "ple.less", "Plectorhinchus lessoni",
  "ple.line", "Plectorhinchus lineatus",
  "ple.poly", "Plectorhinchus polytaenia",
  
  # Kyphosidae (Rudderfish)
  "kyp.bigi", "Kyphosus bigibbus",
  
  # Labridae (Wrasses)
  "Lab.dimi", "Labroides dimidiatus",
  "ana.twis", "Anampses twistii",
  "bod.dian", "Bodianus diana",
  "bod.meso", "Bodianus mesothorax",
  "che.tril", "Cheilinus trilobatus",
  "che.undu", "Cheilinus undulatus",
  "cho.anch", "Choerodon anchorago",
  "cir.aura", "Cirrhilabrus aurantidorsalis",
  "cir.exqu", "Cirrhilabrus exquisitus",
  "cor.auyg", "Coris aygula",
  "cor.batu", "Coris batuensis",
  "cor.gaim", "Coris gaimard",
  "dip.xant", "Diproctacanthus xanthurus",
  "epb.insi", "Epibulus insidiator",
  "gom.vari", "Gomphosus varius",
  "hal.chry", "Halichoeres chrysus",
  "hal.hort", "Halichoeres hortulanus",
  "hal.mage", "Halichoeres magentae",
  "hal.pros", "Halichoeres prosopeion",
  "hal.rich", "Halichoeres richmondi",
  "hal.timo", "Halichoeres timorensis",
  "hem.melt", "Hemigymnus melapterus",
  "hlg.doli", "Halichoeres dolitatus",
  "lab.bico", "Labroides bicolor",
  "lab.dimi", "Labroides dimidiatus",
  "lth.unil", "Labrichthys unilineatus",
  "mac.mele", "Macropharyngodon meleagris",
  "oxy.diag", "Oxycheilinus digramma",
  "stj.stri", "Stethojulis strigiventer",
  "tha.ambl", "Thalassoma amblycephalum",
  "tha.jans", "Thalassoma jansenii",
  "tha.luna", "Thalassoma lunare",
  "tha.lute", "Thalassoma lutescens",
  
  # Lethrinidae (Emperors)
  "gna.auri", "Gnathodentex aureolineatus",
  "let.atki", "Lethrinus atkinsoni",
  "let.eryp", "Lethrinus erythropterus",
  "let.hara", "Lethrinus harak",
  "let.obso", "Lethrinus obsoletus",
  "let.orna", "Lethrinus ornatus",
  "let.xant", "Lethrinus xanthochilus",
  "mon.gran", "Monotaxis grandoculis",
  
  # Lutjanidae (Snappers)
  "Lut.gibb", "Lutjanus gibbus",
  "aph.furc", "Aphareus furca",
  "lut.boha", "Lutjanus bohar",
  "lut.decc", "Lutjanus decussatus",
  "lut.decu", "Lutjanus decussatus",
  "lut.flma", "Lutjanus fulvimaculatus",
  "lut.fulv", "Lutjanus fulvus",
  "lut.gibb", "Lutjanus gibbus",
  "lut.lemn", "Lutjanus lemniscatus",
  "lut.lunu", "Lutjanus lunulatus",
  "lut.mono", "Lutjanus monostigma",
  "lut.rivu", "Lutjanus rivulatus",
  "lut.rufo", "Lutjanus russellii",
  "mcr.macu", "Macolor macularis",
  "mcr.nige", "Macolor niger",
  
  # Mullidae (Goatfish)
  "mul.flav", "Mulloidichthys flavolineatus",
  "par.barb", "Parupeneus barberinus",
  "par.bifa", "Parupeneus bifasciatus",
  "par.cycl", "Parupeneus cyclostomus",
  "par.indi", "Parupeneus indicus",
  
  # Nemipteridae (Threadfin breams)
  "sco.bili", "Scolopsis bilineata",
  "sco.marg", "Scolopsis margaritifera",
  "sco.vosm", "Scolopsis vosmeri",
  
  # Pempheridae (Sweepers)
  "pem.spp", "Pempheris species",
  
  # Pomacanthidae (Angelfish)
  "apo.trim", "Apolemichthys trimaculatus",
  "cen.bico", "Centropyge bicolor",
  "cen.flav", "Centropyge flavicauda",
  "cen.tibi", "Centropyge tibicen",
  "cen.vrol", "Centropyge vrolikii",
  "poc.annu", "Pomacanthus annularis",
  "poc.impe", "Pomacanthus imperator",
  "poc.nava", "Pomacanthus navarchus",
  "poc.semi", "Pomacanthus semicirculatus",
  "pyg.diac", "Pygoplites diacanthus",
  
  # Pomacentridae (Damselfish)
  "abu.vaig", "Abudefduf vaigiensis",
  "acn.poly", "Acanthochromis polyacanthus",
  "amb.aure", "Amblyglyphidodon aureus",
  "amb.cura", "Amblyglyphidodon curacao",
  "amb.leuc", "Amblyglyphidodon leucogaster",
  "amp.spp", "Amphiprion species",
  "chr.alis", "Chromis alpha",
  "chr.atri", "Chromis atripectoralis",
  "chr.caud", "Chromis caudalis",
  "chr.lepi", "Chromis lepidolepis",
  "chr.line", "Chromis lineata",
  "chr.marg", "Chromis margaritifer",
  "chr.retr", "Chromis retrofasciata",
  "chr.tern", "Chromis ternatensis",
  "chr.viri", "Chromis viridis",
  "chr.webe", "Chromis weberi",
  "chr.xanc", "Chromis xanthochira",
  "chr.xant", "Chromis xanthura",
  "chy.rex", "Chrysiptera rex",
  "chy.roll", "Chrysiptera rollandi",
  "chy.talb", "Chrysiptera talboti",
  "das.arua", "Dascyllus aruanus",
  "das.reti", "Dascyllus reticulatus",
  "das.trim", "Dascyllus trimaculatus",
  "dis.pros", "Dischistodus prosopotaenia",
  "lpz.tapa", "Lepidozygus tapeinosoma",
  "neg.cros", "Neoglyphidodon crossi",
  "neg.mela", "Neoglyphidodon melas",
  "neg.nigr", "Neoglyphidodon nigroris",
  "neg.thor", "Neoglyphidodon thoracotaeniatus",
  "neo.azys", "Neopomacentrus azysron",
  "pgy.lacr", "Plectroglyphidodon lacrymatus",
  "pom.ambo", "Pomacentrus amboinensis",
  "pom.auri", "Pomacentrus auriventris",
  "pom.aurv", "Pomacentrus auriventris",
  "pom.bank", "Pomacentrus bankanensis",
  "pom.brac", "Pomacentrus brachialis",
  "pom.coel", "Pomacentrus coelestis",
  "pom.lepi", "Pomacentrus lepidogenys",
  "pom.molu", "Pomacentrus moluccensis",
  "pom.phil", "Pomacentrus philippinus",
  "pom.reid", "Pomacentrus reidi",
  "pom.sims", "Pomacentrus simsiang",
  "pom.vaiu", "Pomacentrus vaiuli",
  "pom.ward", "Pomacentrus wardii",
  "ste.nigr", "Stegastes nigricans",
  
  # Scaridae (Parrotfish)
  "cal.caro", "Calotomus carolinus",
  "cet.bico", "Cetoscarus bicolor",
  "chl.sord", "Chlorurus sordidus",
  "chs.blee", "Chlorurus bleekeri",
  "chs.capi", "Chlorurus capistratoides",
  "chs.japa", "Chlorurus japanensis",
  "chs.micr", "Chlorurus microrhinos",
  "chs.sord", "Chlorurus sordidus",
  "hip.long", "Hipposcarus longiceps",
  "sca.alti", "Scarus altipinnis",
  "sca.dimi", "Scarus dimidiatus",
  "sca.fest", "Scarus festivus",
  "sca.fors", "Scarus forsteri",
  "sca.ghob", "Scarus ghobban",
  "sca.nigr", "Scarus niger",
  "sca.pras", "Scarus prasiognathos",
  "sca.psit", "Scarus psittacus",
  "sca.quoy", "Scarus quoyi",
  "sca.rivu", "Scarus rivulatus",
  "sca.rubr", "Scarus rubroviolaceus",
  "sca.schl", "Scarus schlegeli",
  "sca.spin", "Scarus spinus",
  "sca.tric", "Scarus tricolor",
  
  # Serranidae (Groupers)
  "aet.roga", "Aethaloperca rogaa",
  "cep.argu", "Cephalopholis argus",
  "cep.leop", "Cephalopholis leopardus",
  "cep.urod", "Cephalopholis urodeta",
  "epi.fasc", "Epinephelus fasciatus",
  "epi.macr", "Epinephelus macrospilos",
  "epi.macu", "Epinephelus maculatus",
  "epi.merr", "Epinephelus merra",
  "epi.merra", "Epinephelus merra",
  "epi.poly", "Epinephelus polyphekadion",
  "gra.albi", "Gracila albomarginata",
  "gra.albo", "Gracila albomarginata",
  "var.lout", "Variola louti",
  
  # Siganidae (Rabbitfish)
  "sig.arg", "Siganus argenteus",
  "sig.arge", "Siganus argenteus",
  "sig.doli", "Siganus doliatus",
  "sig.line", "Siganus lineatus",
  "sig.puel", "Siganus puellus",
  "sig.punc", "Siganus punctatus",
  "sig.vulp", "Siganus vulpinus",
  
  # Sphyraenidae (Barracuda)
  "sph.jell", "Sphyraena jello",
  
  # Tetraodontidae (Pufferfish)
  "aro.nigr", "Arothron nigropunctatus",
  
  # Zanclidae (Moorish Idol)
  "zan.corn", "Zanclus cornutus",
  
  # Fix duplicate family capitalizations
  "haemulidae:dia.pict", "Diagramma pictum",
  "labridae:che.tril", "Cheilinus trilobatus",
  "labridae:epb.insi", "Epibulus insidiator",
  "lutjanidae:lut.gibb", "Lutjanus gibbus",
  "pomacentridae:pom.molu", "Pomacentrus moluccensis",
  
  # Unknown
  "unknown", "Unidentified species"
)

# Try to read the file and catch any errors
tryCatch({
  # Print current working directory for verification
  cat("Current working directory:", getwd(), "\n\n")
  
  # Read the Excel file
  cat("Reading Excel file...\n")
  data <- read_excel(input_path)
  
  # Add scientific names
  cat("Adding scientific names...\n")
  data <- data %>%
    left_join(species_lookup, by = c("species" = "species_code")) %>%
    # If no match found, keep the original species code
    mutate(scientific_name = ifelse(is.na(scientific_name), species, scientific_name))
  
  # Write the updated data back to Excel
  cat("Saving updated file...\n")
  write_xlsx(data, output_path)
  
  # Print summary of changes
  n_matched <- sum(!is.na(data$scientific_name) & data$scientific_name != data$species)
  n_total <- nrow(data)
  
  cat("\nProcessing complete!\n")
  cat(sprintf("- Total records processed: %d\n", n_total))
  cat(sprintf("- Species names matched: %d\n", n_matched))
  cat(sprintf("- Species names not matched: %d\n", n_total - n_matched))
  
}, error = function(e) {
  cat("\nError occurred:\n")
  cat(sprintf("- %s\n", e$message))
  cat("\nPlease check:\n")
  cat("1. File paths are correct\n")
  cat("2. Excel file exists and isn't open in another program\n")
  cat("3. You have read/write permissions for the folder\n")
})

#checking which species are not named yet

# Load required libraries
library(readxl)
library(dplyr)
library(writexl)

# Set your file paths
input_path <- "../../Potential data/Alex data/RAW Data/Timor biomass survey 2016.xlsx"

# Read the data
cat("Reading Excel file...\n")
data <- read_excel(input_path)

# Create summary of unmatched species
cat("\n=== MISSING SPECIES CODES BY FAMILY ===\n\n")

# Group by Family and species, count occurrences
unmatched_summary <- data %>%
  group_by(Family, species) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(Family, species)

# Print by family
current_family <- ""
for(i in 1:nrow(unmatched_summary)) {
  if(current_family != unmatched_summary$Family[i]) {
    current_family <- unmatched_summary$Family[i]
    cat(sprintf("\n%s:\n", current_family))
  }
  cat(sprintf("  %s (appears %d times)\n", 
              unmatched_summary$species[i], 
              unmatched_summary$count[i]))
}

# Save to file for reference
output_file <- "../../Potential data/Alex data/RAW Data/missing_species_codes.txt"
sink(output_file)
cat("=== MISSING SPECIES CODES BY FAMILY ===\n\n")
current_family <- ""
for(i in 1:nrow(unmatched_summary)) {
  if(current_family != unmatched_summary$Family[i]) {
    current_family <- unmatched_summary$Family[i]
    cat(sprintf("\n%s:\n", current_family))
  }
  cat(sprintf("  %s (appears %d times)\n", 
              unmatched_summary$species[i], 
              unmatched_summary$count[i]))
}
sink()

cat(sprintf("\nA complete list has been saved to: %s\n", output_file))




