# exams2ilias 0.0.2

* Fix invalid QTI XML for numeric questions with a tolerance and a box prompt.
* Keep solution metadata inside its text question when collapsing QTI XML.
* Validate the ILIAS export path against ILIAS 9.20.
* Confirm imports into ILIAS 10.12 with the existing template. Manually verify
  cloze scoring, numeric tolerance, multiple-choice partial credit, and
  automatic keyword scoring for text questions with `metasolution = TRUE`.
* Write compact ILIAS Cloze metadata fields so ILIAS imports `fixedTextLength`,
  `identicalScoring`, `feedback_mode`, and `combinations` reliably.
* Enable Identical Scoring by default for Cloze questions, matching the intended
  scoring behavior for repeated correct gap answers.
* Add focused tests and a repeated-select Cloze fixture for ILIAS metadata and
  repeated correct answers.
* Embed item supplement images as data URIs when exporting with base64 support.
* Add conservative table handling for ILIAS exports.
