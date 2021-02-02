+------------------------------------------------------------------------------------------------------------------------
+--                                            Reasons To Terminate Search 
+--
+-- If parser encounters one of these words, stop looking forward. 
+--
+------------------------------------------------------------------------------------------------------------------------
+reasonsToTerminate :: HashSet String
+reasonsToTerminate = fromList [
+        "access"
+    ,   "after"
+    ,   "alias"
+    ,   "all"
+    ,   "architecture"
+    ,   "array"
+    ,   "assert"
+    ,   "attribute"
+    ,   "begin"
+    ,   "block"
+    ,   "body"
+    ,   "buffer"
+    ,   "bus"
+    ,   "case"
+    ,   "component"
+    ,   "configuration"
+    ,   "constant"
+    ,   "disconnect"
+    ,   "downto"
+    ,   "end"
+    ,   "entity"
+    ,   "exit"
+    ,   "file"
+    ,   "for"
+    ,   "function"
+    ,   "generate"
+    ,   "generic"
+    ,   "group"
+    ,   "guarded"
+    ,   "impure"
+    ,   "in"
+    ,   "inertial"
+    ,   "inout"
+    ,   "is"
+    ,   "label"
+    ,   "library"
+    ,   "linkage"
+    ,   "literal"
+    ,   "loop"
+    ,   "map"
+    ,   "new"
+    ,   "next"
+    ,   "null"
+    ,   "of"
+    ,   "on"
+    ,   "open"
+    ,   "others"
+    ,   "out"
+    ,   "package"
+    ,   "port"
+    ,   "postponed"
+    ,   "procedure"
+    ,   "pure"
+    ,   "range"
+    ,   "record"
+    ,   "register"
+    ,   "reject"
+    ,   "return"
+    ,   "select"
+    ,   "severity"
+    ,   "signal"
+    ,   "shared"
+    ,   "subtype"
+    ,   "then"
+    ,   "to"
+    ,   "transport"
+    ,   "type"
+    ,   "unaffected"
+    ,   "units"
+    ,   "until"
+    ,   "use"
+    ,   "variable"
+    ,   "wait"
+    ,   "when"
+    ,   "while"
+    ,   "with"
+    ,   ","
+    ,   ":"
+    ,   ";"
+    ,   "=>"
+    ]
+
+
+------------------------------------------------------------------------------------------------------------------------
+--                                              Reasons To Skip 1 Word 
+--
+-- If parser encounters one of these, skip that one word and continue searching. 
+--
+------------------------------------------------------------------------------------------------------------------------
+reasonsToScrapeFormulaInputsBrainsTail1x :: HashSet String
+reasonsToScrapeFormulaInputsBrainsTail1x = fromList [
+        "("
+    ,   ")"
+    ,   "/="
+    ,   "**"
+    ,   "="
+    ,   "<"
+    ,   ">"
+    ,   ">="
+    ,   "("
+    ,   ")"
+    ,   "+"
+    ,   "-"
+    ,   "*"
+    ,   "/"
+    ,   "else"
+    ,   "elsif"
+    ,   "if"
+    ,   "mod"
+    ,   "not"
+    ,   "process"
+    ]
+
+
+------------------------------------------------------------------------------------------------------------------------
+--                                              Reasons To Skip 2 Words 
+--
+-- If parser encounters one of these, skip two words and continue searching. 
+--
+------------------------------------------------------------------------------------------------------------------------
+reasonsToScrapeFormulaInputsBrainsTail2x :: HashSet String
+reasonsToScrapeFormulaInputsBrainsTail2x = fromList [
+    ,   "rol"
+    ,   "ror"
+    ,   "sla"
+    ,   "sli"
+    ,   "sra"
+    ,   "srl"
+    ]
+
+
+------------------------------------------------------------------------------------------------------------------------
+--                                              Reasons To Skip 3 Words 
+--
+-- If parser encounters one of these, skip three words and continue searching. 
+--
+------------------------------------------------------------------------------------------------------------------------
+reasonsToScrapeFormulaInputsBrainsTail3x :: HashSet String
+reasonsToScrapeFormulaInputsBrainsTail3x = fromList [
+    ,   "and"
+    ,   "nand"
+    ,   "nor"
+    ,   "or"
+    ,   "xnor"
+    ,   "xor"
+    ]
+
+
+------------------------------------------------------------------------------------------------------------------------
+--                                     Reasons To Search for Closing Parentheses 
+--
+-- If parser encounters one of these, bite off enough tokens to reach the closing parenthesis, filter out VHDL reserved 
+-- words, operators, and numbers, and return what's left. 
+--
+------------------------------------------------------------------------------------------------------------------------
+reasonsToUntilClosingParenRemoveTokens :: HashSet String
+reasonsToUntilClosingParenRemoveTokens = fromList [
+    ,   "abs"
+    ,   "shift_left"
+    ,   "shift_right"
+    ,   "rotate_left"
+    ,   "rotate_right"
+    ,   "shift_right"
+    ,   "rotate_left"
+    ,   "rotate_right"
+    ,   "sll"
+    ,   "srl"
+    ,   "resize"
+    ,   "to_integer"
+    ,   "to_signed"
+    ,   "to_unsigned"
+    ,   "std_match"
+    ,   "unsigned"
+    ,   "signed"
+    ]
+
+
+------------------------------------------------------------------------------------------------------------------------
+--                                  Reasons To Take The Left Token and Right Token 
+------------------------------------------------------------------------------------------------------------------------
+reasonsToOneToTheLeftOneToTheRight :: HashSet String
+reasonsToOneToTheLeftOneToTheRight = fromList [
+    ,   "rem"
+    ,   "mod"
+    ]
+
 ------------------------------------------------------------------------------------------------------------------------
 --                                                  Brain Function 
 --
 -- This helper function does the heavy lifting and the recursion. Don't call this though. Call the non-brain version 
 -- at the bottom. 
 --
+    ,   "sll"
+    ,   "srl"
+    ,   "resize"
+    ,   "to_integer"
+    ,   "to_signed"
+    ,   "to_unsigned"
+    ,   "std_match"
+    ,   "unsigned"
+    ,   "signed"
+    ]
+
+
+------------------------------------------------------------------------------------------------------------------------
+--                                  Reasons To Take The Left Token and Right Token 
+------------------------------------------------------------------------------------------------------------------------
+reasonsToOneToTheLeftOneToTheRight :: HashSet String
+reasonsToOneToTheLeftOneToTheRight = fromList [
+    ,   "rem"
+    ,   "mod"
+    ]
+
 ------------------------------------------------------------------------------------------------------------------------
 --                                                  Brain Function 
 --
 -- This helper function does the heavy lifting and the recursion. Don't call this though. Call the non-brain version 
 -- at the bottom. 
 --
+-- NOTE: Input scraping from port maps not supported at this time.
+-- There is simply no way to do it without a priori knowledge of port direction. 
+--
 ------------------------------------------------------------------------------------------------------------------------
 scrapeFormulaInputsBrains :: [String] -> [String]
 scrapeFormulaInputsBrains [] = []
 scrapeFormulaInputsBrains los
     | ((length los) < 1) = []
-    | ((head los) == "(") = scrapeFormulaInputsBrains (tail los)
-    | ((head los) == ")") = scrapeFormulaInputsBrains (tail los)
 
     -- Skeleton from py script starts here:
     | (((length los) > 3) && ((los !! 0) == "abs")) = stopAtClosingParen los 0 0
-    | ((los !! 0) == "access") = []
-    | ((los !! 0) == "after") = [] -- Technically, an input could inform an "after" statement, but since that is nearly nonexistent we do not supportthat.
-    | ((los !! 0) == "alias") = []
-    | ((los !! 0) == "all") = []
-    | (((length los) > 1) && ((los !! 1) == "and")) = [los !! 0, los!! 2] ++ (recurIfMore (tail (tail (tail los))))
-    | ((los !! 0) == "architecture") = []
-    | ((los !! 0) == "array") = []
-    | ((los !! 0) == "assert") = []
-    | ((los !! 0) == "attribute") = []
-    | ((los !! 0) == "begin") = []
-    | ((los !! 0) == "block") = []
-    | ((los !! 0) == "body") = []
-    | ((los !! 0) == "buffer") = []
-    | ((los !! 0) == "bus") = []
-    | ((los !! 0) == "case") = [los!! 1]
-    | ((los !! 0) == "component") = []
-    | ((los !! 0) == "configuration") = []
-    | ((los !! 0) == "constant") = []
-    | ((los !! 0) == "disconnect") = []
-    | ((los !! 0) == "downto") = [] -- Technically, an input could drive a downto, but we do not support that at this ime. 
-    | (((length los) > 1) && ((los !! 0) == "else")) = scrapeFormulaInputsBrains (tail los)
-    | (((length los) > 1) && ((los !! 0) == "elsif")) = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "end") = []
-    | ((los !! 0) == "entity") = []
-    | ((los !! 0) == "exit") = []
-    | ((los !! 0) == "file") = []
-    | ((los !! 0) == "for") = []
-    | ((los !! 0) == "function") = []
-    | ((los !! 0) == "generate") = []
-    | ((los !! 0) == "generic") = []
-    | ((los !! 0) == "group") = []
-    | ((los !! 0) == "guarded") = []
-    | (((length los) > 1) && ((los !! 0) == "if")) = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "impure") = []
-    | ((los !! 0) == "in") = []
-    | ((los !! 0) == "inertial") = []
-    | ((los !! 0) == "inout") = []
-    | ((los !! 0) == "is") = []
-    | ((los !! 0) == "label") = []
-    | ((los !! 0) == "library") = []
-    | ((los !! 0) == "linkage") = []
-    | ((los !! 0) == "literal") = []
-    | ((los !! 0) == "loop") = []
-    | ((los !! 0) == "map") = []
-    | (((length los) > 1) && ((los !! 0) == "mod")) = scrapeFormulaInputsBrains (tail los)
-    | (((length los) > 1) && ((los !! 0) == "nand")) = [los !! 0, los!! 2] ++ (recurIfMore (tail (tail (tail los))))
-    | ((los !! 0) == "new") = []
-    | ((los !! 0) == "next") = []
-    | (((length los) > 1) && ((los !! 0) == "nor")) = [los !! 0, los!! 2] ++ (recurIfMore (tail (tail (tail los))))
-    | (((length los) > 1) && ((los !! 0) == "not")) = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "null") = []
-    | ((los !! 0) == "of") = []
-    | ((los !! 0) == "on") = []
-    | ((los !! 0) == "open") = []
-    | (((length los) > 1) && ((los !! 0) == "or")) = [los !! 0, los!! 2] ++ (recurIfMore (tail (tail (tail los))))
-    | ((los !! 0) == "others") = []
-    | ((los !! 0) == "out") = []
-    | ((los !! 0) == "package") = []
-    | ((los !! 0) == "port") = []
-    | ((los !! 0) == "postponed") = []
-    | ((los !! 0) == "procedure") = []
-    | (((length los) > 1) && ((los !! 0) == "process")) = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "pure") = []
-    | ((los !! 0) == "range") = []
-    | ((los !! 0) == "record") = []
-    | ((los !! 0) == "register") = []
-    | ((los !! 0) == "reject") = []
-    | ((los !! 0) == "return") = []
-    | (((length los) > 1) && ((los !! 1) == "rol")) = [head los] ++ scrapeFormulaInputsBrains (tail (tail los))
-    | (((length los) > 1) && ((los !! 1) == "ror")) = [head los] ++ scrapeFormulaInputsBrains (tail (tail los))
-    | ((los !! 0) == "select") = []
-    | ((los !! 0) == "severity") = []
-    | ((los !! 0) == "signal") = []
-    | ((los !! 0) == "shared") = []
-    | (((length los) > 1) && ((los !! 1) == "sla")) = [head los] ++ scrapeFormulaInputsBrains (tail (tail los))
-    | (((length los) > 1) && ((los !! 1) == "sli")) = [head los] ++ scrapeFormulaInputsBrains (tail (tail los))
-    | (((length los) > 1) && ((los !! 1) == "sra")) = [head los] ++ scrapeFormulaInputsBrains (tail (tail los))
-    | (((length los) > 1) && ((los !! 1) == "srl")) = [head los] ++ scrapeFormulaInputsBrains (tail (tail los))
-    | ((los !! 0) == "subtype") = []
-    | ((los !! 0) == "then") = []
-    | ((los !! 0) == "to") = [] -- Technically an input could drive this, but that is not supported presently. 
-    | ((los !! 0) == "transport") = []
-    | ((los !! 0) == "type") = []
-    | ((los !! 0) == "unaffected") = []
-    | ((los !! 0) == "units") = []
-    | ((los !! 0) == "until") = []
-    | ((los !! 0) == "use") = []
-    | ((los !! 0) == "variable") = []
-    | ((los !! 0) == "wait") = []
-    | ((los !! 0) == "when") = []
-    | ((los !! 0) == "while") = []
-    | ((los !! 0) == "with") = []
-    | (((length los) > 1) && ((los !! 1) == "xnor")) = [los !! 0, los!! 2] ++ (recurIfMore (tail (tail (tail los))))
-    | (((length los) > 1) && ((los !! 1) == "xor")) = [los !! 0, los!! 2] ++ (recurIfMore (tail (tail (tail los))))
+
 
     -- Scrape Inputs From Assignment Operators:
-    | (((indexOf ";" los 0) > 0) && ((indexOf "<=" los 0) > 0) && ((indexOf ";" los 0) > (indexOf "<=" los 0))) =
+    -- TODO: First check if head los == "<=" before running the following:
+    -- Make sure the less than or equal to operator passes through. 
+    | ((head los == "<=") && ((indexOf ";" los 0) > 0) && ((indexOf "<=" los 0) > 0) && ((indexOf ";" los 0) > (indexOf "<=" los 0))) =
         [x | x <- (untilKeyword (afterKeyword los ["<="]) [";"] []), not (isVhdlKeyword x), not (isVhdlToken x), not (isVhdlNumber x)] ++
         (scrapeFormulaInputsBrains (afterKeyword los [";"]))
 
-    | (((indexOf ";" los 0) > 0) && ((indexOf ":=" los 0) > 0) && ((indexOf ";" los 0) > (indexOf ":=" los 0))) =
+    | ((head los == ":=") && ((indexOf ";" los 0) > 0) && ((indexOf ":=" los 0) > 0) && ((indexOf ";" los 0) > (indexOf ":=" los 0))) =
         [x | x <- (untilKeyword (afterKeyword los [":="]) [";"] []), not (isVhdlKeyword x), not (isVhdlToken x), not (isVhdlNumber x)] ++
         (scrapeFormulaInputsBrains (afterKeyword los [";"]))
 
 
-    -- NOTE: Input scraping from port maps not supported at this time.
-    -- There is simply no way to do it without a priori knowledge of port direction. 
-    | ((los !! 0) == "=>") = [] 
-    | ((los !! 0) == "/=") = scrapeFormulaInputsBrains (tail los)
-
-    -- NOTE: Comments should be impossible after operator input is tokenized. 
-    -- That is why this branch is commented out. 
-    -- Besides, after line endings have been removed, there is simply no way to know
-    -- when a comment ends. 
-    --
-    -- DO NOT delete this commented out line. It serves as a warning to those who,
-    -- in a moment of weakness, might flirt with thoughts of transgression:
-    -- | (((length los) > 1) && ((los !! 0) == "--")) = []
-
-    | ((los !! 0) == "**") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "=") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "<") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == ">") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == ">=") = scrapeFormulaInputsBrains (tail los)
-    
-    | ((los !! 0) == "(") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == ")") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "+") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "-") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "*") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == "/") = scrapeFormulaInputsBrains (tail los)
-    | ((los !! 0) == ",") = []
-    | ((los !! 0) == ":") = []
-    | ((los !! 0) == ";") = []
-
-    -- Functions & operators defined in IEEE.NUMERIC_STD:
-    | (((length los) > 1) && ((los !! 0) == "abs")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "rem")) = oneToTheLeftOneToTheRight los
-    | (((length los) > 1) && ((los !! 0) == "mod")) = oneToTheLeftOneToTheRight los
-
-    -- TODO: Add support for less than operator. This function has to figure out whether it's being used as less than or assignment.     
+reasonsToTerminate :: HashSet String
+reasonsToScrapeFormulaInputsBrainsTail1x :: HashSet String
+reasonsToScrapeFormulaInputsBrainsTail2x :: HashSet String
+reasonsToScrapeFormulaInputsBrainsTail3x :: HashSet String
+reasonsToUntilClosingParenRemoveTokens :: HashSet String
+reasonsToOneToTheLeftOneToTheRight :: HashSet String
+    -- TODO: Check for membership in reasonsToScrape...3x and recur, using tail thrice:
+    -- | (((length los) > 3) && ((los !! 1) == "xor")) = [los !! 0, los!! 2] ++ (recurIfMore (tail (tail (tail los))))
+
+
+    -- TODO: Check for membership in reasonsToScrape...2x and recur, using tail twice, like this:
+    -- | (((length los) > 1) && ((los !! 1) == "srl")) = [head los] ++ scrapeFormulaInputsBrains (tail (tail los))
+
+
+    -- TODO: check for membership in reasonsToScrape... and call scrapeFormulaInputsBrains
+    -- | ((los !! 0) == "/") = scrapeFormulaInputsBrains (tail los)
+
+    -- TODO: Check for membership in reasonsToOneToTheLeft... and call like this:
+    -- | (((length los) > 1) && ((los !! 0) == "mod")) = oneToTheLeftOneToTheRight los
+
     -- | (((length los) > 1) && ((los !! 0) == "<=")) = []
-    | (((length los) > 1) && ((los !! 0) == ">=")) = oneToTheLeftOneToTheRight los
-    | (((length los) > 1) && ((los !! 0) == "shift_left")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "shift_right")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "rotate_left")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "rotate_right")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "sll")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "srl")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "resize")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "to_integer")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "to_signed")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "to_unsigned")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "std_match")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "unsigned")) = untilClosingParenRemoveTokens los
-    | (((length los) > 1) && ((los !! 0) == "signed")) = untilClosingParenRemoveTokens los
+    -- | (((length los) > 1) && ((los !! 0) == "signed")) = untilClosingParenRemoveTokens los
 
     -- TODO: Take every keyword that involves some kind of a length requirement, and filter those out of the results. 
     | (isVhdlNumber (head los)) = scrapeFormulaInputsBrains (tail los)

