# exams2ilias 0.0.2

* Validate the ILIAS export path against ILIAS 9.20.
* Write compact ILIAS Cloze metadata fields so ILIAS imports `fixedTextLength`,
  `identicalScoring`, `feedback_mode`, and `combinations` reliably.
* Enable Identical Scoring by default for Cloze questions, matching the intended
  scoring behavior for repeated correct gap answers.
* Add focused tests and a repeated-select Cloze fixture for ILIAS metadata and
  repeated correct answers.
* Embed item supplement images as data URIs when exporting with base64 support.
* Add conservative table handling for ILIAS exports.
