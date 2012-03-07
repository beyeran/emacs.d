
;;;; remember templates ;;;;;
(setq org-remember-templates
      '(("university" ?u "* TODO %? %^g\n %i\n " "~/Documents/org/uni.org" "University")
        ("home" ?h "* TODO %? %^g\n %i\n " "~/Documents/org/home.org" "Home")
        ("free time" ?f "* TODO %? %^g\n %i\n " "~/Documents/org/freetime.org" "Leisure Time")
        ("Journal" ?j "\n* %^{topic} %T :JOURNAL: \n%i%?\n" "~/Documents/journal.org")
        ("Book" ?b "\n* %^{Book Title} %t :READING: \n%[~/Documents/booktemp.txt]\n" 
         "~/Documents/journal.org")
        ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Documents/org/finances.org")
        ("Film" ?m "** %^{Film Title} %t :FILM: \n%[~/Documents/.film_template.txt]\n" 
         "~/Documents/journal.org")
        ("Someday"   ?s "** %^{Someday Heading} %U\n%?\n"  "~/Documents/org/someday.org")
        ("Vocab"   ?v "** %^{Word?}\n%?\n"  "~/Documents/org/vocab.org")
        ("Daily Review" ?a "** %t :COACH: \n%[~/Documents/.daily_review.txt]\n" 
         "~/Documents/org/journal.org")))

(provide 'beyeran-org-remember)
