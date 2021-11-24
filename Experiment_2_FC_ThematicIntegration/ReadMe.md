# General Details
- Authors: Tibor Kiss, Alicia Katharina Börner, Jutta Pieper
- Affiliation: Linguistic Data Science Lab (LDSL), Ruhr-Universität Bochum, Germany
- Publication: Word order constraints on event-internal modifiers
- Date: 
- Abstract: 
> In this study, we tested subject-related comitatives (COM(S)) and instrumentals (INSTR). We assume that subject-oriented event-internal modifiers may occupy any position that is c-commanded by the subject if no further constraints apply. These further constraints are the subject matter of Experiment 2: we hypothesize that event-internal adverbials can modify the argument structure of the event by introducing further thematic roles. In the case of instrumentals, the thematic role will uniquely be that of instrument, but comitatives are more flexible: the role depends on the antecedent of the comitative. We further assume that the introduction of new roles is constrained by the lexical semantics of the preposition: both comitatives and instrumentals allow affirmative and privative readings, which are indicated by different lexicalizations: prepositions like mit (‘with’) and über (‘by means of’) provide affirmative interpretations, including an existential presupposition of their object, the preposition ohne (‘without’) provides a privative interpretation. In addition, the preposition ohne does not provide an existential presupposition of its object (but neither a negated existential presupposition). We summarize these properties by the concept of Thematic Integration: For the prepositions mit (‘with’) and über (‘by means of’), Thematic Integration results in the introduction of a thematic role, for the preposition ohne (‘without’), non-integration precludes the introduction of a further thematic role. Thematic roles influence the ordering of the elements bearing them: phrases bearing highly ranked thematic roles are assumed to occupy positions on the left, phrases bearing lower thematic roles are assumed to occupy positions further on the right of the German Mittelfeld. Without Thematic Integration, the internal argument of the PP will not bear a role in the event structure of the modified verb. Subject-oriented comitatives copy the role of the subject in case of Thematic Integration, typically agent, subject-oriented instrumentals provide the role of instrument. Agents, of course, are highly ranked, while instruments are placed much lower on the hierarchy of thematic roles. We thus predict an interaction of the type of the modifier with the integration of thematic roles: highly ranked integrated roles lead to a positioning of the PP in the left of the Mittelfeld, lower ranked integrated roles lead to a positioning of the PP closer to the verb. If Thematic Integration does not apply (in the case of ohne), the position of the phrase is determined by other extrinsic factors, i.e., by its syntactic category and Anaphoricity. 

# Study Details
- Study Title: ExPrep_Phase2_Int_ThematicIntegration
- Date (conducted on):  2021-05-17
- Recruitment: Prolific (https://prolific.co)
- Prescreening: Native Speakers of German
- Payment:   2.80£ (ca.  3.27€)
- Estimated Time Required: 20 minutes
- Used Software: 
	+ jspysch (https://www.jspsych.org)
 	+ jatos (https://www.jatos.org/Whats-JATOS.html - Version 3.5.11)	

# Survey Details

> Note: **column_names** are printed in boldface, *level_names* in italics

- Task: Two Alternative Forced Choice
- Design: **ANSWER** (*PP>OBJ*, *OBJ>PP*) ~ **ADVERBIAL_TYPE** (*COM(S)*,*INSTR*) x **PRESENCE** (*yes*,*no*) (== **P**(mit/über,ohne))
- Lists: 1
- List length: 72
- **ITEM_GROUP**s:
	+ *test*: 24
	+ *filler*: 48
	+ thereof (**ITEM_FUNCTION**): 
		+ *calibration*: 6
		+ *control*: 20
			+ thereof (**ITEM_SUBGROUP**)
			+ *related*: 10 
			+ *unrelated*: 10
		+ *attention*: 6
		+ (other) *filler*: 16
- randomization
	- the presentation order of each stimulus-pair is randomized individually for each trial for each participant (see **PRESENTATION_ORDER**)
	- each survey is randomized individually for each participant according to the following conditions: 
		+ the survey starts with calibration items in random order 
		+ control items are (otherwise) spread over the whole survey
		+ no test item should be adjacent to any other test item
		+ no control item should be adjacent to any other control item
		+ the survey ends with a filler 


# Participants
- Accepted: 33
	+ Gender: 11 male, 21 female, 1 diverse
	+ Age: 19 - 57 (Ø26,67, ±7.93)
- Rejected: 16 
	+ Gender: 8 male, 5 female, 3 NA
	+ Age: 18 - 61 (Ø29,77, ±12.75)
	+ Reasons:
		- failed on control items: 10  (i.e. picked the unacceptable option of at least *one* control item)
		- highly distracted: 2 
		- left before the survey started: 4

# VARIABLES: ForcedChoice (Experimental Eliciation, Survey)
- id columns:
	- **workerId**: id of participant
	- **ENCODING**: id of item's lexicalizations
	- **itemId**: unique identifier of the item (i.e. **ENCODING**_**OPTION_0_CONDITION_NO**_**OPTION_1_CONDITION_NO**)
- answer-related columns:
	- **rt**: reaction / response time: time (ms) needed to answer this trial (from appearance of trial till continue-button pressed)
	- **ANSWER**: KEY_CONDITION of the chosen option (i.e. OPTION_[0|1]_KEY_CONDITION of the stimulus)
	- **ANSWER_CONDITION(_NO)**: condition (number) of chosen option
    > Note: for *filler* items, conditions are coded as followed: 
        1 -> grammatical 
        2 -> * ungrammatical
        3 -> ? odd / marked
    > **KEY_CONDITION**s of the stimuli may give further details
- presentation-related columns:
	- **PRESENTION_ORDER** (+ PRESENTATION_ORDER_CONDITION): *a >> b* means that a has been presented above b 
	- **ANSWER_POSITION** (*above*/*below*): chosen answer has been presented above/below the other option
- survey-related columns:
	- **trial_index**: trial has been shown as the (trial_index+1)th trial of the survey (first trial has trial_index 0)
	- **time_elapsed**: time (ms) since the survey started (first trial shown)


# Variables: Participants 	
## From our Demographic Questionnaire 
- **PROLIFIC_PID**: DELETED, required for Payment
- **AGE**: DropDown Menu: *18-75*,*75+*, no specification (*keine Angabe*)
- **GENDER**: *male*, *female*, *diverse*, *no specification* (*männlich*, *weiblich*, *divers*, *keine Angabe*)
- **EDUCATION**
	> "highest educational attainment" ("höchster schulischer Bildungsabschluss")
	- available options:
		+ *in education* (*noch in schulischer Ausbildung*),
		+ *lower secondary school graduation* (*Haupt-(Volks-)schulabschluss*),
		+ *secondary school graduation* (*Realschul- oder gleichwertiger Abschluss*),
		+ *(specialized) university-level graduation* (*Fachhochschul- oder Hochschulreife*), 
		+ *none* (*ohne allgemeinen Schulabschluss*),
		+ *no specification* (*keine Angabe*)
- **EDUCATION_PROFESSIONAL**
	> "highest professional attainment" ("höchster beruflicher Bildungsabschluss")
	- available options:
		+ *in training* (*noch in Ausbildung*),
		+ *apprenticeship (dual)* (*Lehre/Berufsausbildung im dualen System*),
		+ *technical degree* (*Fachschulabschluss*),
		+ *polytechnic degree* (*Fachhochschulabschluss*),
		+ *university degree* (*Hochschulabschluss*),
		+ *bachelor* (*Bachelor*),
		+ *master* (*Master*),
		+ *diploma* (*Diplom*),
		+ *doctorate* (*Promotion*),
		+ *none* (*ohne Ausbildung*),
		+ *no specification* (*keine Angabe*)
- miscellaneous
	> "Which of the following statements applies to you personally? Check all that apply." ("Welche der folgenden Aussagen trifft auf Sie zu?  Bitte kreuzen Sie zutreffende Aussagen an.")
	- multiple selections possible (except for *nothing applies* and *no specification*),
	stored in different boolean variables:
		+ **MISC_MULTILINGUAL**
			> "I have been brought up speaking more than one language." ("Ich bin mehrsprachig aufgewachsen."),
		+ **MISC_NON_NAIVE** 
			> "I have knowledge in linguistics." ("Ich verfüge über sprachwissenschaftliche Kenntnisse."),
		+ **MISC_ABROAD** 
			> "I currently reside in a non-german-speaking country." ("Ich halte mich zurzeit im nicht-deutschsprachigen Ausland auf."),
		+ **MISC_NONE**
			> "Nothing of the above applies." ("Nichts davon trifft zu."),
		+ **MISC_NO_SPEC** 
			> "I do not want to provide any information to this end." ("Ich möchte dazu keine Angaben machen.")
## from a questionnaire after the survey
- **DEVICE**  (optional)
	> "Which device did you use (predominantly) throughout the survey?" ("Welches Eingabegerät haben Sie während des Experiments (vorwiegend) verwendet?")
	- available options: 
		+ *touchscreen* (*Touchscreen*)
		+ *touchpad*  (*Touchpad*)
		+ *mouse* (*Maus*)
		+ *keyboard*  (*Tastatur*)
		+ *other*  (*andere*)
## Demographic Information received from Prolific 
- **Country.of.Birth**
- **Current.Country.of.Residence**
- **Nationality**
