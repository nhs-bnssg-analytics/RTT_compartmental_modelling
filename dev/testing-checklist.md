# Manual Testing Checklist

Perhaps this could be automated using shinytest2

## Release / QA Checklist

### NEWS
- [ ] Has `NEWS` file been updated?

### How to use the tool tab
- [ ] Is **“How to use this tool”** section still correct?
- [ ] Has **“Future developments”** been updated?

### Scenario planner tab
- [ ]

### Results tab (after running Scenario planner)
- [ ] Does each chart display?
- [ ] Does clicking on each chart provide a value box?

**Data table**
- [ ] Column names readable?

From **Data table**:
- [ ] **Copy table to clipboard** → correct column names when pasted into Excel
- [ ] **Download table to CSV** → correct column names when opened

**Generate report**
- [ ] Downloads a `.docx` (if download is a `.html`, there is an error in the docx generation code)
- [ ] All intended charts appear?

### Steady state tab
After clicking **Calculate Steady State**:

- [ ] All column headers readable?
- [ ] Charts appear when clicking on a row?
- [ ] **Download results as CSV** → readable column headers
- [ ] **Download waiting list detail** → readable column headers
- [ ] **Create PowerPoint of results** → final slide(s) correct

### Definitions tab
- [ ] Any new definitions to add?

### Tutorials
- [ ] Any new tutorials to add?

### Acknowledgements
- [ ] Timeline updated?
- [ ] Collaborators updated?

### Links
- [ ] Any new links?