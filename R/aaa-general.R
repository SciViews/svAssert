# Translation of general messages -----------------------------------------

# # Comment everything below this line, except to update the general messages
# #
# # A list with messages to translate and regular expressions to identify them in
# # error strings and extract variables. It must be updated manually when needed.
# .gen_msgs <- c(
#   "Mean absolute difference: %g",
#   "Mean scaled difference: %g",
#   "Mean relative difference: %g"
# )
# # Corresponding regular expressions to extract variables from messages
# names(.gen_msgs) <-  c(
#   "^Mean absolute difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$",
#   "^Mean scaled difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$",
#   "^Mean relative difference: (-?[0-9]+\\.?[0-9]*[eE]?[-+]?[0-9]*)$"
# )
#
# # Do not run in the package, only when updating messages manually!
# # Write messages into R/messages-general.R
# create_messages_script(.gen_msgs, "general")
# # Clean up
# rm(.gen_msgs)

# all(NULL), or all(logical(0)) is TRUE, thus, it is a simpler way than to
# write length(x) == 0L || x == TRUE
# However in this case, it is not explicit enough, so we define:
all_or_length0_null <- all

# any(NULL), or any(logical(0)) is FALSE, thus, it is a simpler way than to
# write length(x) > 0L && x == TRUE
# The more explicit any is:
any_not_length0_null <- any
