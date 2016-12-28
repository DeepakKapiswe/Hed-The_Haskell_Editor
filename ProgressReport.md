--------------------------------------------------------------------------------------------------------------------------------------------------
4-12-2016

now what is the current state of the editor

now i have made an interface through which i could represent the state of the actual text object in an abstract manner and also the required minimum functions to transform the textObj

>> next work

now i have to think that how can i display my text object to the terminal so that the user could modify it interctively, for that what i need is
 1>> show cursor at correct position --DONE
 2>> make the current line always visible i.e support scrolling vertically -- DONE 
 3>> i have to display such that the longer lines should be broken and incomplete lines should not be shown to the user they should be replaced with vaccant lines while displaying --DONE


now i have done the 3rd above mentioned point but now a new porblem i am facing of cursor position misplacement and now i have to comeup with some formula to calculate correctly the cursor position

--------------------------------------------------------------------------------------------------------------------------------------------------

6-12-2016 03:30 pm
 now the problem has been solved of correctness of cusor positon i.e now I am able to display cusor at correct position also supporting soft line break

6-12-2016 08:45 pm

now i have separated all the functions according to their group of functionality and made a modular model to start work and have focus only on current working problem this would help me for dealing with separate problems separately

>> next work

next I have to Implement separate section model for the UI like editor part , status part and Commandbar part and provide a mechanism so that user could change the focus to the desired tab and status bar should show always the current status like file name, current line number , cursor position.

--------------------------------------------------------------------------------------------------------------------------------------------------
10-12-2016 03:45 pm

now I have implemented the above mentioned point completely

>> Next Work

To implement seperate commandLine Interpreter which will interpret simple commands like delete,yank,insert etc 

now I have implemented all above mentioned commands but for interpretation parser is not quite well I have to improve it.

-- ------------------------------------------------------------------------------------------------------------

20/12/2016  09:00 pm

>> Next Work 

to add simple editing feauters with key strokes so that it could be used by me as an editor !!
ok we will add features of an editor ...so list them specifically as per need:~
__+
 1) copy line                   ==
 2) copy till line end          ==
 3) copy till line begining     ==
 4) copy current word
 5) copy previous word
 6) copy next word
 7) paste after cursor          ==
 8) paste before cusor          ==
 9) paste in newline below      ==
10) delete current word
11) delete till end of the word
12) delete till begin of the word
13) delete next word
14) delete prev word
15) delete current line         ==
16) pageUP
17) pageDown
18) breakline after end of line ==
19) open a file for editing     ==
20) writing changes to file(save) ==
__c__c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c_c

28/12/2016 08:00 pm

now I have implemented above features which are marked with (==) sign.now I have to implement rest features as time permits




