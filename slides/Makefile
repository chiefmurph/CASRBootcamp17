####################
# Makefile

RMD_DIR = slide_input
HTML_DIR = slide_output
HANDOUT_DIR = handouts
SCRIPT_DIR = scripts
DATA_DIR = data

RMD_SOURCE = $(wildcard $(RMD_DIR)/*.Rmd)
HANDOUT_SOURCE = $(wildcard $(HANDOUT_DIR)/*.Rmd)

HTML_OUT = $(patsubst $(RMD_DIR)/%.Rmd,$(HTML_DIR)/%.html,$(RMD_SOURCE))
HANDOUT_OUT = $(HANDOUT_SOURCE:.Rmd=.docx)
SCRIPT_OUT = $(patsubst $(RMD_DIR)/%.Rmd,$(SCRIPT_DIR)/%.R,$(RMD_SOURCE))

TUFTE_IN = $(addprefix $(RMD_SOURCE), 1P01_Data_Analysis.Rmd 1P02_Advanced_Visualization.Rmd)
TUFTE_OUT = $(TUFTE_IN:.Rmd=.pdf)
# TUFTE_OUT = $(patsubst $(TUFTE_OUT))

KNIT_SLIDE = Rscript -e "rmarkdown::render('$<', output_dir = '$(HTML_DIR)')"
KNIT_HANDOUT = Rscript -e "rmarkdown::render('$<', output_format = 'word_document')"
KNIT_BEAMER = Rscript -e "rmarkdown::render('$<', output_dir = '$(HTML_DIR)')"
KNIT_TUFTE = Rscript -e "rmarkdown::render('$<', output_format = 'tufte::tufte_handout', output_dir = '$(HTML_DIR)')"

all: slides handouts scripts

slides: $(HTML_OUT)

handouts: $(HANDOUT_OUT)

scripts: $(SCRIPT_OUT)

# tufte: $(TUFTE_OUT)

#$(TUFTE_OUT):$(TUFTE_IN)
#	$(KNIT_TUFTE)

$(HTML_DIR)/%.html:$(RMD_DIR)/%.Rmd
	$(KNIT_SLIDE)

$(HANDOUT_DIR)/%.docx:$(HANDOUT_DIR)/%.Rmd
	$(KNIT_HANDOUT)

$(SCRIPT_DIR)/%.R:$(RMD_DIR)/%.Rmd
	Rscript -e "knitr::purl('$<', output = '$@')"

#===========================
# Additional dependencies here
$(RMD_DIR)/GLMs.Rmd:$(DATA_DIR)/glm.rda

$(DATA_DIR)/glm.rda:$(DATA_DIR)/glm.R
	Rscript -e "source('./data/glm.R')"

clean:
	rm -f -v $(HTML_OUT)
	rm -f -v $(HANDOUT_OUT)
	rm -f -v $(SCRIPT_OUT)
