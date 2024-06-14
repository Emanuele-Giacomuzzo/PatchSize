# system_type_input = "Medium-Medium isolated"
# for (disturbance_input in c("low", "high")) {
#   culture_IDs_first_patch_type = ID_combinations %>%
#     filter(disturbance == disturbance_input,
#            metaecosystem_type == system_type_input) %>%
#     pull(ID_first_patch) %>%
#     unique()
#   
#   culture_ID_second_patch_type = ID_combinations %>%
#     filter(disturbance == disturbance_input,
#            metaecosystem_type == system_type_input) %>%
#     pull(ID_second_patch) %>%
#     unique()
#   
#   culture_IDs = c(culture_IDs_first_patch_type,
#                   culture_ID_second_patch_type) %>%
#     unique()
#   
#   n_combination_sets = length(culture_IDs)
#   n_combinations_per_set = floor(length(culture_IDs) /2)
#   culture_IDs_repeated = rep(culture_IDs, 2)
#   
#   if(length(culture_IDs) %% 2 == 1) {
#     odd_number_of_patches = TRUE
#   } else  {
#     odd_number_of_patches = FALSE
#   }
#   
#   for (combination_set_input in 1:n_combination_sets) {
#     for (combination_input in 1:n_combinations) {
#       
#       if(odd_number_of_patches == TRUE){patch_to_leave_out = culture_IDs[combination_input]}
#       
#       ID_combinations_w_combination_sets = ID_combinations_w_combination_sets %>%
#         add_row(
#           ID_first_patch = culture_IDs_repeated[combination_input],
#           ID_second_patch = culture_IDs_repeated[combination_input  + combination_set_input - 1],
#           combination_set = combination_set_input,
#           disturbance = disturbance_input,
#           metaecosystem_type = system_type_input
#         )
#       
#       print(culture_IDs_repeated[combination_input])
#       print(culture_IDs_repeated[combination_input  + combination_set_input - 1])
#       print("next")
#     }
#   }
# }