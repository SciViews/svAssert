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
