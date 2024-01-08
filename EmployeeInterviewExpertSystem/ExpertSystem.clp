/*
** This module is an expert system which simulates the interviewing process for a GPU designer. It is an initial screening
** that will check for red flags.
** 
** @author Leo Tuckey
** @version 5/9/2023
*/

(clear)
(reset)
(batch "utilities_v4.clp")

(deftemplate traits (slot attribute) (slot result)) ;the attribute is an aspect of the potential employee, and the result is either a y or n, telling if the quality is true or not.
(do-backward-chaining traits)

(defglobal ?*NAME* = Unknown) ;the potential employee's name

/*
** Checks if two strings are equal
** 
** @return true if the two strings are equal
**    otherwise, false
** 
** @param ?str1 the first string
** @param ?str2 the second string
*/
(deffunction str-eq (?str1 ?str2)
   (return (= (str-compare (str-cat ?str1) (str-cat ?str2)) 0))
)

/*
** Reads a text file and converts each line to an index in a list
** 
** @param ?fileName the name of the file
** 
** @return the list
*/
(deffunction readFile (?fileName)
   (open ?fileName r)
   (bind ?endOfFile TRUE)
   (bind ?resumeList (create$))
   (while ?endOfFile
      (bind ?inputRead (readline r)) 
      (if (not (str-eq ?inputRead "EOF")) then
         (bind ?resumeList (insert$ ?resumeList (+ 1 (length$ ?resumeList)) ?inputRead))
      else
         (bind ?endOfFile FALSE)
      )
   )
   (return ?resumeList)
)

/*
** parses through the list and checks for a specific word
** 
** @param ?word the specific word
** 
** @return TRUE if the word is found
**    otherwise, FALSE
*/
(deffunction checkForWord (?word)
   (bind ?resumeList (readFile "SampleResume.txt"))
   (bind ?out FALSE)
   (for (bind ?i 1) (<= ?i (length$ ?resumeList)) (++ ?i)
      (bind ?line (nth$ ?i ?resumeList))
      (for (bind ?j 1) (<= ?j (- (str-length ?line) (str-length ?word))) (++ ?j)
         (if (str-eq (sub-string ?j (+ ?j (- (str-length ?word) 1)) ?line) ?word) then
            (bind ?out TRUE)
         )
      )
   )
   (return ?out)
)

/*
** Checks if an input is an integer
** 
** @param ?input the given input
** @return true if the input is an integer
**    otherwise, false
*/
(deffunction isInputValidInt (?input)
   (while (not (integerp ?input))
      (printout t "Invalid Input. Type in an integer. ")
      (bind ?input (ask ""))
   )
   (return ?input)
)

/*
** Checks if an input is a yes or no
** 
** @param ?input the given input
** @return true if the input is a yes or no
**    otherwise, false
*/
(deffunction isInputValidYesNo (?input)
   (bind ?input (lowcase (sub-string 1 1 ?input)))
   (while (not (or (= ?input y) (= ?input n)))
      (printout t "Invalid Input. Type in a yes or no. ")
      (bind ?input (lowcase (sub-string 1 1 (ask ""))))
   )
   (return ?input)
)

/*
** Checks if an input is an a, b, c, or d
** 
** @param ?input the given input
** @return true if the input is an a, b, c, or d
**    otherwise, false
*/
(deffunction isInputValidMultChoice (?input)
   (while (not (or (= ?input a) (= ?input b) (= ?input c) (= ?input d)))
      (printout t "Invalid Input. Type in one of the following: a, b, c, or d ")
      (bind ?input (ask ""))
   )
   (return ?input)
)

/*
** If an input is not a number from 1 to 10, it will continuously ask for a new input that is a number from 1 to 10.
** 
** @param ?input the given input
** @param ?parameter the parameter of being between 1 and 10
** @return the user input that is a number from 1 to 10
**    otherwise, false
*/
(deffunction isInputValidOneTen (?input ?parameter)
   (while (not ?parameter)
      (printout t "Invalid Input. Type in a number from 1 to 10. ")
      (bind ?input (ask ""))
      (bind ?parameter (scaleOneTen ?input))
   )
   (return ?input)
)

/*
** Checks if an input is a number from 1 to 10
** 
** @param ?input the given input
** @return true if the input is a number from 1 to 10
**    otherwise, false
*/
(deffunction scaleOneTen (?input)
   (if (and (integerp ?input) (and (< ?input 11) (> ?input 0)))then
      (bind ?out TRUE)
   else
      (bind ?out FALSE)
   )
   (return ?out)
)

(defrule beginInterview "starts the interview"
    (declare (salience 100))
    =>
    (printout t "We will now begin the interview process. What is your name? ")
    (bind ?*NAME* (ask ""))
    (printout t (str-cat "Hello " ?*NAME* "!") crlf)
) ;(defrule beginInterview "starts the interview"

(defrule successful "the rule for successes"
   (traits (attribute resume)           (result y))
   (traits (attribute outgoing)         (result y))
   (traits (attribute teamwork)         (result y))
   (traits (attribute workLifeBalance)  (result y))
   (traits (attribute workValues)       (result y))
   (traits (attribute questionOne)      (result y))
   (traits (attribute questionTwo)      (result y))
   (traits (attribute questionThree)    (result y))
   =>
   (printout t (str-cat "Congratulations " ?*NAME* "! You have passed this initial screening, and we will send you a follow-up email within the following days."))
   (halt)
) ;(defrule successful "the rule for successes"

(defrule unsuccessfulEight "a rule for unsuccessful attempts"
   (traits (attribute questionThree)      (result n))
   =>
   (printout t (str-cat "I'm sorry, " ?*NAME* ", but you have not passed this initial screening."))
   (halt)
) ;(defrule unsuccessfulEight "a rule for unsuccessful attempts"

(defrule unsuccessfulSeven "a rule for unsuccessful attempts"
   (traits (attribute questionTwo)      (result n))
   =>
   (printout t (str-cat "I'm sorry, " ?*NAME* ", but you have not passed this initial screening."))
   (halt)
) ;(defrule unsuccessfulSeven "a rule for unsuccessful attempts"

(defrule unsuccessfulSix "a rule for unsuccessful attempts"
   (traits (attribute questionOne)      (result n))
   =>
   (printout t (str-cat "I'm sorry, " ?*NAME* ", but you have not passed this initial screening."))
   (halt)
) ;(defrule unsuccessfulSix "a rule for unsuccessful attempts"

(defrule unsuccessfulFive "a rule for unsuccessful attempts"
   (traits (attribute workValues)      (result n))
   =>
   (printout t (str-cat "I'm sorry, " ?*NAME* ", but you have not passed this initial screening."))
   (halt)
) ;(defrule unsuccessfulFive "a rule for unsuccessful attempts"

(defrule unsuccessfulFour "a rule for unsuccessful attempts"
   (traits (attribute workLifeBalance)      (result n))
   =>
   (printout t (str-cat "I'm sorry, " ?*NAME* ", but you have not passed this initial screening."))
   (halt)
) ;(defrule unsuccessfulFour "a rule for unsuccessful attempts"

(defrule unsuccessfulThree "a rule for unsuccessful attempts"
   (traits (attribute teamwork)      (result n))
   =>
   (printout t (str-cat "I'm sorry, " ?*NAME* ", but you have not passed this initial screening."))
   (halt)
) ;(defrule unsuccessfulThree "a rule for unsuccessful attempts"

(defrule unsuccessfulTwo "a rule for unsuccessful attempts"
   (traits (attribute outgoing)      (result n))
   =>
   (printout t (str-cat "I'm sorry, " ?*NAME* ", but you have not passed this initial screening."))
   (halt)
) ;(defrule unsuccessfulTwo "a rule for unsuccessful attempts"

(defrule unsuccessfulOne "a rule for unsuccessful attempts"
   (traits (attribute resume)      (result n))
   =>
   (printout t (str-cat "I'm sorry, " ?*NAME* ", but you have not passed this initial screening."))
   (halt)
) ;(defrule unsuccessfulOne "a rule for unsuccessful attempts"

(defrule questionThree "the rule for question three"
   (need-traits (attribute questionThree) (result ?))
   =>
   (printout t "Question 3: What is the difference between raster and vector graphics?" crlf)
   (printout t "(a) Raster graphics are heptagon-based, as vector graphics are octagon-based")
   (printout t "     (b) Raster graphics are arranged in a grid, and vector graphics are geometric primitives")
   (printout t "     (c) Raster graphics are high definition, and vector graphics are lower definition")
   (printout t "     (d) Raster graphics have a lower dimension in its matrix, and vector graphics take up more storage" crlf)
   (bind ?input (ask ""))
   (bind ?input (isInputValidMultChoice ?input))
   (if (= ?input "b") then
      (assert (traits (attribute questionThree) (result y)))
      (printout t "That is correct!")
   else
      (assert (traits (attribute questionThree) (result n)))
      (printout t "That is incorrect.")
   )
   (printout t crlf)
) ;(defrule questionThree "the rule for question three"

(defrule questionTwo "the rule for question two"
   (need-traits (attribute questionTwo) (result ?))
   =>
   (printout t "Question 2: What are the best techniques for optimizing graphics for performance?" crlf)
   (printout t "  1. Minimizing bottlenecks through shaders.")
   (printout t "        2. Reducing the number of polygons in 3D models.")
   (printout t "        3. Using texture compression." crlf)
   (printout t "(a) 2 and 3   (b) 1, 2, and 3   (c)   1 and 3  (d) none of the above" crlf)
   (bind ?input (ask ""))
   (bind ?input (isInputValidMultChoice ?input))
   (if (= ?input "a") then
      (assert (traits (attribute questionTwo) (result y)))
      (printout t "That is correct!")
   else
      (assert (traits (attribute questionTwo) (result n)))
      (printout t "That is incorrect.")
   )
   (printout t crlf)
) ;(defrule questionTwo "the rule for question two"

(defrule questionOne "the rule for question one"
   (need-traits (attribute questionOne) (result ?))
   =>
   (printout t "I will now ask you a series of some Gpu design related and non-related questions." crlf)
   (printout t "Question 1: Why is using catch (exception) always a bad idea? Type in the letter of your answer. (e.g. a,b,c,d)" crlf)
   (printout t "  1. As there is no variable defined, it is not possible to read the exception.")
   (printout t "        2. Catch drastically will slow down runtime through the scanner's lack of declarations.")
   (printout t "        3. It’s good to use an exception when you have known exception types." crlf)
   (printout t "(a) 1 and 2   (b) 1, 2, and 3   (c)   1 and 3  (d) none of the above" crlf)
   (bind ?input (ask ""))
   (bind ?input (isInputValidMultChoice ?input))
   (if (= ?input "c") then
      (assert (traits (attribute questionOne) (result y)))
      (printout t "That is correct!")
   else
      (assert (traits (attribute questionOne) (result n)))
      (printout t "That is incorrect.")
   )
   (printout t crlf)
) ;(defrule questionOne "the rule for question one"

(defrule workValues
   (need-traits (attribute workValues) (result ?))
   =>
   (printout t "Nvidia promotes a fun, yet professional and authentic work environment with an emphasis on working as a team.
   Does this sound like a proper fit to you? ")
   (bind ?input (ask ""))
   (bind ?input (isInputValidYesNo ?input))
   (eval (str-cat "(assert (traits (attribute workValues) (result " ?input ")))"))
   (printout t crlf)
) ;(defrule workValues

(defrule workLifeBalance
   (need-traits (attribute workLifeBalance) (result ?))
   =>
   (printout t "How many hours a week are you willing to put into this job? ")
   (bind ?input (ask ""))
   (bind ?input (isInputValidInt ?input))
   (if (< ?input 35) then
      (assert (traits (attribute workLifeBalance) (result n)))
   else 
      (assert (traits (attribute workLifeBalance) (result y)))
   )
   (printout t crlf)
) ;(defrule workLifeBalance

(defrule teamwork
   (need-traits (attribute teamwork) (result ?))
   =>
   (printout t "From a scale of 1-10, please rate how well you can work with others. ")
   (bind ?input (ask ""))
   (bind ?input (isInputValidOneTen ?input (scaleOneTen ?input)))
   (if (> ?input 7) then
      (assert (traits (attribute teamwork) (result y)))
   else 
      (assert (traits (attribute teamwork) (result n)))

   )
   (printout t crlf)
) ;(defrule teamwork

(defrule outgoing
   (need-traits (attribute outgoing) (result ?))
   =>
   (printout t "From a scale of 1-10, please rate how outgoing you are. ")
   (bind ?input (ask ""))
   (bind ?input (isInputValidOneTen ?input (scaleOneTen ?input)))
   (if (> ?input 6) then
      (assert (traits (attribute outgoing) (result y)))
   else 
      (assert (traits (attribute outgoing) (result n)))

   )
   (printout t crlf)
) ;(defrule outgoing

(defrule resume
   (need-traits (attribute resume) (result ?))
   =>
   (if (and (or (checkForWord "Master's") (checkForWord "Ph. D.")) (checkForWord "GPU")) then
      (assert (traits (attribute resume) (result y)))
      (printout t crlf "Your resume is acceptable for this initial screening." crlf)
   else
      (printout t crlf "Your resume is not acceptable for this initial screening." crlf)
      (assert (traits (attribute resume) (result n)))
   )
   (printout t crlf)
) ;(defrule resume

(run)