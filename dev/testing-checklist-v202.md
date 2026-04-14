# Testing checklist for v2.0.2

### NEWS
- [X] Has `NEWS` file been updated?

### How to use the tool tab
- [X] Is **“How to use this tool”** section still correct?
- [X] Has **“Future developments”** been updated?

### Scenario planner tab

### Results tab (after running Scenario planner)
- [X] Does each chart display?
- [X] Does clicking on each chart provide a value box?

**Data table**
- [X] Column names readable?

From **Data table**:
- [X] **Copy table to clipboard** → correct column names when pasted into Excel
- [X] **Download table to CSV** → correct column names when opened

**Generate report**
- [X] Downloads a `.docx` (if download is a `.html`, there is an error in the docx generation code)
- [X] All intended charts appear?

### Steady state tab
After clicking **Calculate Steady State**:

- [X] All column headers readable?
- [X] Charts appear when clicking on a row?
- [X] **Download results as CSV** → readable column headers
- [X] **Download waiting list detail** → readable column headers
- [X] **Create PowerPoint of results** → final slide(s) correct

### Definitions tab
- [X] Any new definitions to add?

### Tutorials
- [ ] Any new tutorials to add?

### Acknowledgements
- [X] Timeline updated?
- [X] Collaborators updated?

### Links
- [X] Any new links?

## BUGS with 2.0.2

documenting the bugs and when they have been resolved, to see if any further testing checklist criteria should be added.

FIXED in commit "vectorise X01 specialty condition"
1) Steady state (SS) – when 1 trust and multiple specialties are selected, then “Download selections above as a template” pressed --> error

FIXED in commit "stop description.x being downloaded"
2) SS – when uploading data with “description” column, then run “calculate steady state”, 
   a. then “download results as csv”, the description column comes through to csv (as “description.x” and “description.y”)
   b.	create ppt  description comes through to the final table

FIXED in commit "steady state blank cols - bad"
3) SS – when I uploaded a file with trust and specialty columns, but specialty was empty, the tool crashed when I pressed “Calculate steady state” (not sure how realistic a scenario this is, but thought it might work given that “description” gets bunged into the “trust” column for the steady state calculations)

FIXED (check Seb happy)  in commit "steady state blank cols - bad"
4) SS – not sure what to expect here, but same as the previous point, but instead of blank specialty I put Cardiology. The 4 columns towards the end of the steady state table aren’t populated, even though we have built Cardiology activity into the tool. Would a user expect to take advantage of the tool in the data, or should the tool provide a ‘blanket empty columns’ rule when uploading local data? Perhaps the latter to be clearer about what is going on

FIXED in commit "type values as expected"
5) SS – I deleted a row of referral data (see attached file – between row 5 and 6, and the data were uploaded correctly, but tool falls over them “Calculate steady state” is pressed. Important to check all the correct data is loaded (I don’t know how well I do this in Scenario Planner))

FIXED in commit "Consistency with referrals - Peter data example"
6) Peter's problem (gynae manual upload)