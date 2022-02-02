  # Official Wordle dictionary (https://www.powerlanguage.co.uk/wordle/main.e65ce0a5.js)

guess_dictionary <- scan(file = "/wordle_dictionary.txt", 
                        what = "character", 
                        sep = ",")
answer_dictionary <- guess_dictionary[1:grep("shave", guess_dictionary, value = F)]

guess_dictionary <- strsplit(guess_dictionary, "")
answer_dictionary <- strsplit(answer_dictionary, "")
