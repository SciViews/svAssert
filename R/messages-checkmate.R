#' Messages for Translation: checkmate
#'
#' Messages and regular expressions required to identify error strings
#' to translate using [svAssert::translate()] for 'checkmate'.
#'
#' @format ## `checkmate_msgs`
#' A character vector with messages and corresponding regular
#' expressions as names.
#' @export
#' @source <https://github.com/mllg/checkmate> (release v2.3.3)
checkmate_msgs <- (\() {
  gettext <- c # Do not translate now, just catch messages in .po files
  c(
  "^Variable '([^']*)': (.+)$" =
    gettext("Variable {.var %s}: %s."),
  "^Unknown type '([^']*)' to specify check for names\\. Supported are 'unnamed', 'named', 'unique' and 'strict'\\.$" =
    gettext("Unknown option '%s' to specify check for names. Supported are 'unnamed', 'named', 'unique' and 'strict'."),
  "^Unknown class identifier '([^']*)'$" =
    gettext("Unknown class identifier {.cls %s}."),
  "^Timezones of 'x' and 'upper' must match$" =
    gettext("Timezones of {.arg x} and {.arg upper} must match."),
  "^Timezones of 'x' and 'lower' must match$" =
    gettext("Timezones of {.arg x} and {.arg lower} must match."),
  "^Rule may not be NA$" =
    gettext("Rule may not be {.code NA}."),
  "^Must store numerics$" =
    gettext("Must store numerics."),
  "^Must store logicals$" =
    gettext("Must store logicals."),
  "^Must store integers$" =
    gettext("Must store integers."),
  "^Must store integerish values$" =
    gettext("Must store integerish values."),
  "^Must store doubles$" =
    gettext("Must store doubles."),
  "^Must store complexs$" =
    gettext("Must store complexs."),
  "^Must store characters$" =
    gettext("Must store characters."),
  "^Must store a list$" =
    gettext("Must store a list."),
  "^Must have unique (.+), but element ([0-9]+) is duplicated$" =
    gettext("Must have unique %s, but element %i is duplicated."),
  "^Must have length 1$" =
    gettext("Must have length 1."),
  "^Must have length >= (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*), but has length (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*)$" =
    gettext("Must have length >= %g, but has length %g."),
  "^Must have length <= (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*), but has length (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*)$" =
    gettext("Must have length <= %g, but has length %g."),
  "^Must have length (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*), but has length (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*)$" =
    gettext("Must have length %g, but has length %g."),
  "^Must have exactly ([0-9]+) rows, but has ([0-9]+) rows$" =
    gettext("Must have exactly %i rows, but has %i rows."),
  "^Must have exactly ([0-9]+) cols, but has ([0-9]+) cols$" =
    gettext("Must have exactly %i cols, but has %i cols."),
  "^Must have at most ([0-9]+) rows, but has ([0-9]+) rows$" =
    gettext("Must have at most %i rows, but has %i rows."),
  "^Must have at most ([0-9]+) cols, but has ([0-9]+) cols$" =
    gettext("Must have at most %i cols, but has %i cols."),
  "^Must have at least ([0-9]+) rows, but has ([0-9]+) rows$" =
    gettext("Must have at least %i rows, but has %i rows."),
  "^Must have at least ([0-9]+) cols, but has ([0-9]+) cols$" =
    gettext("Must have at least %i cols, but has %i cols."),
  "^Must have >=([0-9]+) dimensions, but has dimension ([0-9]+)$" =
    gettext("Must have >=%i dimensions, but has dimension %i."),
  "^Must have <=([0-9]+) dimensions, but has dimension ([0-9]+)$" =
    gettext("Must have <=%i dimensions, but has dimension %i."),
  "^Must have (.+), but is NA at position ([0-9]+)$" =
    gettext("Must have %s, but is {.code NA} at position %i."),
  "^Must have (.+), but element ([0-9]+) is empty$" =
    gettext("Must have %s, but element %i is empty."),
  "^Must have (.+) according to R's variable naming conventions, but element ([0-9]+) does not comply$" =
    gettext("Must have %s according to R's variable naming conventions, but element %i does not comply."),
  "^Must have (.+)$" =
    gettext("Must have %s."),
  "^Must be sorted$" =
    gettext("Must be sorted."),
  "^Must be of type 'integerish'(.+), not '([^']*)'$" =
    gettext("Must be of type 'integerish'%s, not {.cls %s}."),
  "^Must be of type 'integerish', not 'NULL'$" =
    gettext("Must be of type 'integerish', not {.code NULL}."),
  "^Must be of type 'integerish', but element (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*) is not in integer range$" =
    gettext("Must be of type 'integerish', but element %g is not in integer range."),
  "^Must be of type 'integerish', but element (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*) is not close to an integer$" =
    gettext("Must be of type 'integerish', but element %g is not close to an integer."),
  "^Must be of type 'integerish', but element (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*) has an imaginary part$" =
    gettext("Must be of type 'integerish', but element %g has an imaginary part."),
  "^Must be of type '([^']*)'(.+), not '([^']*)'$" =
    gettext("Must be of type {.cls %s}%s, not {.cls %s}."),
  "^Must be of type '([^']*)', not 'NULL'$" =
    gettext("Must be of type {.cls %s}, not {.code NULL}."),
  "^Must be of type '([^']*)', not '([^']*)'$" =
    gettext("Must be of type {.cls %s}, not {.cls %s}."),
  "^Must be of length (.+) ([0-9]+), but has length (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*)$" =
    gettext("Must be of length %s %i, but has length %g."),
  "^Must be of class '([^']*)', not '([^']*)'$" =
    gettext("Must be of class {.cls %s}, not {.cls %s}."),
  "^Must be finite$" =
    gettext("Must be finite."),
  "^Must be atomic$" =
    gettext("Must be atomic."),
  "^Must be a character vector$" =
    gettext("Must be a character vector."),
  "^Must be a ([0-9]+)-d array, but has dimension ([0-9]+)$" =
    gettext("Must be a %i-d array, but has dimension %i."),
  "^Must be >= ([0-9]+)$" =
    gettext("Must be >= %i."),
  "^May not have (.+)$" =
    gettext("May not have %s."),
  "^May not contain missing values, first at position ([0-9]+)$" =
    gettext("May not contain missing values, first at position %i."),
  "^May not contain missing values, first at column ([0-9]+), element ([0-9]+)$" =
    gettext("May not contain missing values, first at column %i, element %i."),
  "^May not contain missing values, first at column '([^']*)', element ([0-9]+)$" =
    gettext("May not contain missing values, first at column {.field %s}, element %i."),
  "^May not be NA$" =
    gettext("May not be {.code NA}."),
  "^Invalid length definition: (.+)$" =
    gettext("Invalid length definition: %s."),
  "^Invalid bound definition, missing opening '.' or '\\[': (.+)$" =
    gettext("Invalid bound definition, missing opening '(' or '[': %s."),
  "^Invalid bound definition, error parsing upper bound or missing closing '.' or '\\]': (.+)$" =
    gettext("Invalid bound definition, error parsing upper bound or missing closing ')' or ']': %s."),
  "^Invalid bound definition, error parsing lower bound, missing separator ',' or missing closing '.' or '\\]': (.+)$" =
    gettext("Invalid bound definition, error parsing lower bound, missing separator ',' or missing closing ')' or ']': %s."),
  "^Invalid argument 'mode'\\. Must be one of 'logical', 'integer', 'integerish', 'double', 'numeric', 'complex', 'character', 'list' or 'atomic'$" =
    gettext("Invalid argument {.arg mode}. Must be one of 'logical', 'integer', 'integerish', 'double', 'numeric', 'complex', 'character', 'list' or 'atomic'."),
  "^Empty rule$" =
    gettext("Empty rule."),
  "^Element ([0-9]+) is not >= (.+)$" =
    gettext("Element %i is not >= %s."),
  "^Element ([0-9]+) is not >= (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*)$" =
    gettext("Element %i is not >= %g."),
  "^Element ([0-9]+) is not <= (.+)$" =
    gettext("Element %i is not <= %s."),
  "^Element ([0-9]+) is not <= (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*)$" =
    gettext("Element %i is not <= %g."),
  "^Contains only missing values$" =
    gettext("Contains only missing values."),
  "^Contains missing values .row ([0-9]+), col ([0-9]+).$" =
    gettext("Contains missing values (row %i, col %i)."),
  "^Contains missing values .element ([0-9]+).$" =
    gettext("Contains missing values (element %i)."),
  "^Contains missing values .column '([^']*)', row ([0-9]+).$" =
    gettext("Contains missing values (column {.field %s}, row %i)."),
  "^Contains missing values$" =
    gettext("Contains missing values."),
  "^Contains duplicated values, position ([0-9]+)$" =
    gettext("Contains duplicated values, position %i."),
  "^Cannot handle length >= ([0-9]+)$" =
    gettext("Cannot handle length >= %i."),
  "^Cannot check for negative length$" =
    gettext("Cannot check for negative length."),
  "^Bound checks only possible for numeric variables, strings and factors, not (.+)$" =
    gettext("Bound checks only possible for numeric variables, strings and factors, not %s."),
  "^Argument 'x' must be a list or data\\.frame$" =
    gettext("Argument {.arg x} must be a list or data.frame."),
  "^Argument 'upper' must be provided as single POSIXct time$" =
    gettext("Argument {.arg upper} must be provided as single {.cls POSIXct} time."),
  "^Argument 'rules' must be a string$" =
    gettext("Argument {.arg rules} must be a string."),
  "^Argument 'lower' must be provided as single POSIXct time$" =
    gettext("Argument {.arg lower} must be provided as single {.cls POSIXct} time."),
  "^All elements must have exactly ([0-9]+) characters, but element ([0-9]+) has ([0-9]+) chararacters$" =
    gettext("All elements must have exactly %i characters, but element %i has %i characters."),
  "^All elements must have at most ([0-9]+) characters, but element ([0-9]+) has ([0-9]+) characters$" =
    gettext("All elements must have at most %i characters, but element %i has %i characters."),
  "^All elements must have at least ([0-9]+) characters, but element ([0-9]+) has ([0-9]+) characters$" =
    gettext("All elements must have at least %i characters, but element %i has %i characters."),
  "^All elements must have (.+) (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*) chars$" =
    gettext("All elements must have %s %g chars."),
  "^All elements must be (.+) Inf$" =
    gettext("All elements must be %s {.code Inf}."),
  "^All elements must be (.+) (-?[0-9]+.?[0-9]*E?[-+]?[0-9]*)$" =
    gettext("All elements must be %s %g."),
  "^All elements must be (.+) -Inf$" =
    gettext("All elements must be %s {.code -Inf}."),
  "^Additional chars found in rule!$" =
    gettext("Additional chars found in rule!")
  )
})()
