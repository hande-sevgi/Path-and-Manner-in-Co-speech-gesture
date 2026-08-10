# Path and Manner in Written English and Co-speech Gesture

Data, experimental materials, and analysis code for a series of studies investigating how **path** and **manner** information contribute to event interpretation in written English and co-speech gesture.

This project accompanies Chapter 3 of:

> Sevgi, Hande. 2026. *Manner Modification Across Modalities: Insights from Gesture, Sign, and Spoken Language*. Doctoral dissertation, Harvard University.

## Project overview

Motion events contain several components, including the trajectory of movement (**path**) and the way movement unfolds (**manner**). This project examines whether path and manner contribute to interpretation in the same way, particularly in affirmative and negative environments.

The studies compare information expressed through:

* Written linguistic modification
* Co-speech gesture
* Animated visual event contexts

Together, the experiments investigate how semantic content, grammatical form, visual context, and polarity interact during event interpretation.

## Research questions

The project addresses the following questions:

1. Do path and manner modifiers contribute symmetrically to judgments about motion events?
2. Does sentence polarity affect the interpretation of path and manner information?
3. Can differences between path and manner be explained by modifier order?
4. Do co-speech gestures conveying path and manner contribute differently to interpretation?
5. How does gestural information behave when an utterance occurs under negation?

## Studies

### Experiment 1: Written English — path before manner

Participants evaluated written English sentences paired with animated events. The experiment manipulated:

* Modification type: path, manner, or conflated
* Event type: path, manner, conflated, or no motion
* Polarity: affirmative or negative

A total of 150 participants were recruited. After the documented exclusions, 140 participants were included in the analysis.

### Experiment 2: Written English — manner before path

This experiment tested whether the ordering of path and manner modifiers explained the patterns observed in Experiment 1.

A total of 120 participants were recruited. After the documented exclusions, 114 participants were included in the analysis.

### Baseline Study 1: Event interpretation without gesture

Participants evaluated spoken utterances without accompanying gestures in animated event contexts. This study established whether event type independently affected judgments before gesture was introduced.

Forty-one participants were recruited, and 40 were included in the analysis.

### Baseline Study 2: Gesture interpretation without event context

Participants evaluated spoken utterances accompanied by gestures without an explicit animated event context. This study established whether the gestures were interpretable and acceptable independently of the event manipulation.

Forty participants were included in the study.

### Experiment 3: Co-speech gesture and event interpretation

Participants evaluated spoken English utterances accompanied by path, manner, or conflated gestures in animated event contexts. The experiment manipulated:

* Gesture type: path, manner, or conflated
* Event type: path, manner, conflated, or no motion
* Polarity: affirmative or negative

A total of 150 participants were recruited. After the documented exclusions, 142 participants were included in the analysis.

## Methods

Participants were recruited through Prolific and completed the experiments through Qualtrics. Judgments were recorded using a continuous response scale ranging from 0 to 100.

The analyses use beta mixed-effects regression for bounded response data. Models include participant- and scenario-level random intercepts to account for repeated observations.

The principal analyses were conducted in R using packages including:

* `tidyverse`
* `glmmTMB`
* `emmeans`
* `ggeffects`
* `performance`
* `DHARMa`

## Repository contents

```text
Path-and-Manner-in-Co-speech-gesture/
├── README.md
├── code/
│   ├── legacy/
│   │   └── Sevgi_Chapter3.R
│   └── scripts/
│       ├── 01_validate_inputs.R
│       ├── 02_prepare_processed_data.R
│       ├── 03_analyze_experiment_1.R
│       ├── 04_exp1_sensitivity_analysis.R
│       ├── 05_analyze_experiment_2.R
│       ├── 06_exp2_sensitivity_analysis.R
│       ├── 07_analyze_baseline_1.R
│       ├── 08_analyze_baseline_2.R
│       └── 09_analyze_experiment_3.R
├── data/
│   ├── input/
│   ├── processed/
│   └── codebook/
├── materials/
│   └── stimuli/
├── output/
│   ├── figures/
│   ├── tables/
│   └── models/
└── .gitignore
```

### `data/`

Contains documentation and analysis-ready datasets for the five studies.

### `materials/`

Contains or documents the written, animated, audio-visual, and gestural materials used in the experiments. Large archival materials may be maintained on OSF rather than duplicated in GitHub.

### `code/legacy/`

Preserves the original Chapter 3 R analysis unchanged for provenance.


### `code/scripts/`

Contains the ordered validation, preparation, dissertation-reproduction, and sensitivity-analysis scripts.

| Script           | Purpose         |
| ---------------- | ------------------------------ |
| 01_validate_inputs.R    | Validates the five source datasets without modifying them. |
| 02_prepare_processed_data.R     | Standardizes variables, applies documented exclusions, and creates five analysis-ready datasets. |
| 03_analyze_experiment_1.R | Reproduces the Experiment 1 beta-regression analyses reported in Chapter 3.    |
| 04_exp1_sensitivity_analysis.R | Fits a joint ordered-beta sensitivity model for Experiment 1.|
| 05_analyze_experiment_2.R     | Reproduces the Experiment 2 beta-regression analyses reported in Chapter 3. |
| 06_exp2_sensitivity_analysis.R    |Fits a joint ordered-beta sensitivity model for Experiment 2. |
| 07_analyze_baseline_1.R   |Analyses the no-gesture baseline and documents the motion-event inferential subset. |
| 08_analyze_baseline_2.R   |Analyses the complete four-condition gesture baseline without event context. |
| 09_analyze_experiment_3.R  |Reproduces the co-speech gesture analyses for motion and no-motion conditions. |

#### Reproduction and sensitivity analyses

`code/scripts/03_analyze_experiment_1.R` reproduces the beta-regression analyses reported for Experiment 1 in Chapter 3 of the dissertation, including the models presented in Tables 3.2 and 3.3.

`code/scripts/04_exp1_sensitivity_analysis.R` is an additional post-dissertation sensitivity analysis. It uses ordered-beta regression to retain ratings at exactly 0 and 100, models polarity jointly, and treats the five stimulus scenarios as fixed controls. This analysis supplements rather than replaces the dissertation analysis.

The sensitivity analysis supports the principal directional findings reported in the dissertation. The interaction between modification type, event type, and polarity was significant, p < .001. Global residual uniformity, dispersion, and outlier tests were satisfactory, although the residual-versus-predicted quantile test indicated some remaining conditional model misfit. Additional Holm-adjusted pairwise comparisons identify within-condition differences that were not directly tested by the original treatment-coded coefficient tables. A small number of secondary comparisons involving conflated modification vary across model specifications; these differences and the remaining diagnostic limitation do not alter the central Experiment 1 pattern.

`code/scripts/05_analyze_experiment_2.R` reproduces the beta-regression analyses reported for Experiment 2 in Chapter 3 of the dissertation, including the models presented in Appendix Tables B.4 and B.5. Trials involving no-motion events are excluded to match the original inferential analysis.

`code/scripts/06_exp2_sensitivity_analysis.R` is an additional post-dissertation sensitivity analysis. It uses ordered-beta regression to retain ratings at exactly 0 and 100, models polarity jointly, and treats the five stimulus scenarios as fixed controls. This analysis supplements rather than replaces the dissertation analysis.

The sensitivity analysis supports the principal Experiment 2 finding: matching modification–event combinations receive higher ratings in affirmative contexts and substantially lower ratings under negation. The interaction between modification type, event type, and polarity was significant, χ²(4) = 225.44, p < .001. Global residual uniformity, dispersion, and outlier tests were satisfactory, although the residual-versus-predicted quantile test indicated some remaining conditional model misfit. This limitation does not alter the central Experiment 2 pattern, which remains consistent across the reproduction and sensitivity analyses.

### `output/`

Contains figures, tables, model summaries, and other files generated by the computational workflow.

## Data files

| Study            | Analysis-ready dataset         |
| ---------------- | ------------------------------ |
| Experiment 1     | `exp1_written_path_manner.csv` |
| Experiment 2     | `exp2_written_manner_path.csv` |
| Baseline Study 1 | `baseline1_no_gesture.csv`     |
| Baseline Study 2 | `baseline2_no_event.csv`       |
| Experiment 3     | `exp3_cospeech_gesture.csv`    |

Variable definitions and coded values are documented in `data/codebook/`.

## Open Science Framework archive

The companion OSF project contains the archived research materials, including organized datasets, experimental stimuli, and the original Chapter 3 analysis:

**[View the OSF project](https://osf.io/buftx/)**

GitHub is used for the documented computational workflow. OSF serves as the archival location for research materials and large stimulus files.

## Reproducibility

This repository provides an implemented, end-to-end workflow for validating the five source datasets, preparing analysis-ready data, reproducing the dissertation analyses, conducting additional sensitivity analyses, and generating documented outputs.

The workflow:

1. validates filenames, variables, participant counts, condition labels, missing values, and rating ranges;
2. preserves the source datasets without modification;
3. standardizes variable names and experimental condition labels;
4. applies the documented participant-exclusion criteria;
5. creates five analysis-ready datasets;
6. fits the dissertation-reproduction and sensitivity models;
7. runs model diagnostics;
8. generates statistical tables and publication-ready figures;
9. saves fitted model objects, metadata, and R session information.

### Running the workflow

Open the repository in RStudio and ensure that the repository root is the active project directory.

Install the required packages if they are not already available:

```r
install.packages(c(
  "glmmTMB",
  "emmeans",
  "ggplot2",
  "DHARMa"
))
```

Run the scripts from the repository root in numerical order:

```r
source("code/scripts/01_validate_inputs.R")
source("code/scripts/02_prepare_processed_data.R")
source("code/scripts/03_analyze_experiment_1.R")
source("code/scripts/04_exp1_sensitivity_analysis.R")
source("code/scripts/05_analyze_experiment_2.R")
source("code/scripts/06_exp2_sensitivity_analysis.R")
source("code/scripts/07_analyze_baseline_1.R")
source("code/scripts/08_analyze_baseline_2.R")
source("code/scripts/09_analyze_experiment_3.R")
```

Scripts 03, 05, 07, 08, and 09 reproduce the dissertation analyses. Scripts 04 and 06 are additional post-dissertation sensitivity analyses and supplement rather than replace the original models.

The source files under `data/input/` remain unchanged. Analysis-ready datasets are written to `data/processed/`, and generated results are written to `output/`.

Do not use `setwd()` inside the scripts. All file paths are defined relative to the repository root.

## AI-assisted workflow disclosure

The development of this reproducible computational workflow was assisted by OpenAI’s ChatGPT and Codex. AI assistance was used to help reorganize the original dissertation materials, refactor the legacy R analysis into numbered scripts, troubleshoot code, design validation checks, structure generated outputs, and improve repository documentation.

The research questions, experimental designs, data collection, original dissertation analyses, and substantive scholarly conclusions are the work of the author. The author reviewed the AI-assisted code, ran the scripts locally, checked participant and observation counts, evaluated model convergence and diagnostics, and compared the reproduced results with the dissertation. AI tools did not independently collect data or make final decisions about data exclusion, statistical interpretation, or reporting.

This disclosure is provided for transparency. Responsibility for the accuracy of the repository and its scholarly interpretation remains with the author.

## Participant data and responsible use

The datasets contain behavioral judgments collected for academic research. Users should:

* Treat participant identifiers as pseudonymous research identifiers.
* Avoid attempting to identify participants.
* Avoid combining the data with external information for re-identification.
* Follow the applicable ethical, institutional, and data-use requirements.
* Cite the project when using its data, materials, or code.

## Citation

If you use this project, please cite:

```text
Sevgi, Hande. 2026.
Manner Modification Across Modalities:
Insights from Gesture, Sign, and Spoken Language.
Doctoral dissertation, Harvard University.
```

## Author

**Hande Sevgi**
Linguist working on sign language, semantics, event structure, gesture, and multimodal communication.

* [Academic website](https://hande-sevgi.github.io/)
* [OSF project](https://osf.io/buftx/)
