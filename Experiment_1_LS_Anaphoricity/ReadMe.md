# General Details
- Authors: Tibor Kiss, Alicia Katharina Börner, Jutta Pieper
- Affiliation: Linguistic Data Science Lab (LDSL), Ruhr-Universität Bochum, Germany
- Publication: Word order constraints on event-internal modifiers
- Date: 
- Abstract:   
> In this study, we have tested object-oriented internal locative PPs (ILOC(O)), object-oriented comitative PPs (COM(O)), and instrumental PPs (INSTR) to the left and to the right of an object realized as a wh-indefinite. We predict that the first two adverbials pattern alike but differ from the behavior of instrumentals. The reason is the orientation of the adverbials: object-related PPs should be realized below the object, while a realization of subject-related PPs could be possible at any position below the subject, including a position between the subject and the object. This hypothesis differs from Frey & Pittner’s (1998), who assume that ILOC(O) and COM(O) should be found to the right of the object, but INSTR to the immediate right of the subject. 

# Study Details
- StudyTitle: ExPrep_Phase2_Int_Anaphoricity
- Date (conducted on): 2021-05-10, 2021-05-11, 2021-05-12
- Recruitment: Prolific (https://prolific.co)
- Prescreening: Native Speakers of German
- Payment:   4.10£ (ca.  4.80€)
- Estimated Time Required: 30 minutes
- Used Software: 
	+ jspysch (https://www.jspsych.org)
 	+ jatos (https://www.jatos.org/Whats-JATOS.html - Version 3.5.11)	

# Survey Details

> Note: **column_names** are printed in boldface, *level_names* in italics

- Task: LikertSkala
- Design: **ANSWER** (*1*, *2*, *3*, *4*, *5*) ~ **ADVERBIAL_TYPE** (*INSTR*, *ILOC*, *COM(O)*) x **KEY_CONDITION** (*PP>OBJ*, *OBJ>PP*)
- Scale Labels (**ANSWER_LABEL**): *vollkommen unnatürlich* - *eher unnatürlich* - *keine Tendenz* - *eher natürlich* - *vollkommen natürlich*
- Lists: 2
- List length: 108
- **ITEM_GROUP**s:
	- *filler*: 72
	 - thereof **ITEM_FUNCTION**s:
		- *calibration*: 6
		- *control*: 24
		- thereof **ITEM_SUBGROUP**:
			- *unrelated*:  12
			- *related*:  12
		- *filler*: 30
		- *attention*: 12
	- *test*: 36
- randomization
	- participants are assigned to lists randomly
	- each survey is randomized individually for each participant according to the following conditions: 
		+ the survey starts with calibration items in random order 
		+ control items are (otherwise) spread over the whole survey 
		+ no test item should be adjacent to any other test item
		+ no control item should be adjacent to any other control item
		+ the survey ends with a filler 


# Participants
- Accepted: 51
	+ Gender: 24 male, 27 female
	+ Age: 18 - 55 (Ø27.25, ±7.121)
	+ Lists: 28 - 23
- Rejected: 28
	+ Gender: 16 male, 5 female
	+ Age: 19 - 45 (Ø27.14, ±8.639)
	+ Lists: 8 - 16
	+ Reasons (multiple reasons for one participants possible):
		- failed on attention items: 10
		- failed on control items: 12
		- highly distracted: 4
		- incomplete submission: 2
		- task not started: 7

# Variables: LikertSkala (Experimental Eliciation, Survey)
- id columns:
	- **workerId**: id of participant
	- **ENCODING**: id of item's lexicalization (i.e. associated to all conditions of an item!)
	- **CONDITION**: experimental condition of the item presented here. 
	> **itemId**: unique identifier of the item (i.e. **ENCODING**_**CONDITION_NO**)
	> Note: different **CONDITION**s of the same test item (**ENCODING**) are spread across lists. 
	> Each lists contains the same filler items!
	> For *filler* items, conditions are coded as followed: 
		- 1 -> grammatical 
        	- 2 -> * ungrammatical
        	- 3 -> ? odd / marked
   	 > **KEY_CONDITION**s of the stimuli may give further details
- answer-related columns:
	- **rt**: reaction / response time: time (ms) needed to answer this trial (from appearance of trial till continue-button pressed)
	- **ANSWER_LABEL**: chosen scale label 
	- **ANSWER**: given rating coded as numbers (1,2,3,4,5)
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
