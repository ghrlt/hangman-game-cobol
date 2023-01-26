      ******************************************************************
      * Author: Gaëtan Hrlt
      * Purpose: The game of the Hangman, in COBOL! (Completely useless)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HANGMAN-GAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
           77 a PIC x(1).
           77 g PIC x(1).
           77 trash PIC x(1).
           77 i PIC 9(2).
           77 goodGuess PIC 9. *> 0 is False, 1 is True

           77 red PIC 99.
           77 white PIC 99.
           77 black PIC 99.

           77 playing PIC 9. *> 0 is False, 1 is True
           77 loseLevel PIC 9.

           77 hangLine1 PIC x(40).
           77 hangLine2 PIC x(40).
           77 hangLine3 PIC x(40).
           77 hangLine4 PIC x(40).
           77 hangLine5 PIC x(40).
           77 hangLine6 PIC x(40).
           77 hangLine7 PIC x(40).
           77 hangLine8 PIC x(40).
           
           77 hangGuessed PIC x(20). *> No word length > 20
           77 hangWord PIC x(20).

           1 hangWords.
             2 word1 PIC X(20) VALUE "hello".
             2 word2 PIC X(20) VALUE "world".
             2 word3 PIC X(20) VALUE "goodbye".
             2 word4 PIC X(20) VALUE "programming".

           77 random-index PIC 9(1) COMP.

       SCREEN SECTION.
           1 titre.
               *> Display "HANGMAN GAME!" in ASCII Art

               2 BLANK SCREEN.
               2 LINE 2 COL 1 VALUE
                   '  _  _   _   _  _  ___ __  __   _   _  _    ___   _'
                   BACKGROUND-COLOR 0
                   FOREGROUND-COLOR 15.
               2 VALUE '   __  __ ___   _ '
                   BACKGROUND-COLOR 0
                   FOREGROUND-COLOR 15.

               2 LINE 3 COL 1 VALUE
                   ' | || | /_\ | \| |/ __|  \/  | /_\ | \| |  / __| /_'
                   BACKGROUND-COLOR 0
                   FOREGROUND-COLOR 15.
               2 VALUE '\ |  \/  | __| | |'
                   BACKGROUND-COLOR 0
                   FOREGROUND-COLOR 15.

                   2 LINE 4 COL 1 VALUE
                   ' | __ |/ _ \| .` | (_ | |\/| |/ _ \| .` | | (_ |/ _'
                   BACKGROUND-COLOR 0
                   FOREGROUND-COLOR 15.
               2 VALUE ' \| |\/| | _|  |_|'
                   BACKGROUND-COLOR 0
                   FOREGROUND-COLOR 15.

               2 LINE 5 COL 1 VALUE
                   ' |_||_/_/ \_\_|\_|\___|_|  |_/_/ \_\_|\_|  \___/_/ '
                   BACKGROUND-COLOR 9
                   FOREGROUND-COLOR 15.
               2 VALUE '\_\_|  |_|___| (_)'
                   BACKGROUND-COLOR 0
                   FOREGROUND-COLOR 15.

           1 menu.
               *> Display the home menu, ask for action to exec

               2 LINE 7 COL 7 VALUE
                   '1) Start a game'
                   BACKGROUND-COLOR 0
                   FOREGROUND-COLOR 15.

               2 LINE 9 COL 1 VALUE 'What do you want to do? '.
               2 action PIC x(1) TO a REQUIRED.

           1 start-game.
               *> Start the game and all its logic

               2 LINE 6 COL 15 VALUE 'The game has started!'.
               
               2 LINE 8 COL  2 FROM hangLine1.
               2 LINE 9 COL  2 FROM hangLine2.
               2 LINE 10 COL 2 FROM hangLine3.
               2 LINE 11 COL 2 FROM hangLine4.
               2 LINE 12 COL 2 FROM hangLine5.
               2 LINE 13 COL 2 FROM hangLine6.
               2 LINE 14 COL 2 FROM hangLine7.
               2 LINE 15 COL 2 FROM hangLine8.
               
               2 LINE 11 COL 35 VALUE 'Current guess:'.
               2 LINE 13 COL 40 FROM hangGuessed.
               
               
               2 LINE 17 COL 1 VALUE 'Another guess? '.
               2 guess PIC x(1) TO g REQUIRED.
               
               
           1 you-won.
               *> Display win screen

               2 BLANK SCREEN.
               2 LINE 2 COL 1 VALUE 'YOU WON!'.
           
           1 you-lose.
               *> Display lose screen

               2 BLANK SCREEN.
               2 LINE 2 COL 1 VALUE 'YOU LOSE :('.
               

           1 invalid-action.
               *> Warn user and reset

               2 BLANK SCREEN.
               2 LINE 10 COL 10 VALUE
                   'Invalid action!'
                   BACKGROUND-COLOR red
                   FOREGROUND-COLOR white.

               2 LINE 20 COL 1 VALUE
                   'Press any key to continue...'
                   BACKGROUND-COLOR black
                   FOREGROUND-COLOR white.

               2 PIC x(1) TO trash REQUIRED.


       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               *> Assign all colors int to their respective variables
               MOVE 12 TO red.
               MOVE 15 TO white.
               MOVE 0 TO black.
                              
               *> Start the game
               PERFORM UNTIL action = '1'
                   INITIALIZE action

                   DISPLAY titre
                   DISPLAY menu
                   ACCEPT action
                   EVALUATE action
                     WHEN '1'
                       MOVE 1 TO playing
                       
                       *> Obtain word to guess
                       MOVE FUNCTION RANDOM() TO random-index
                           *> Not sure how to get a random INTEGER
                           *> between 1 and hangWords length
                           *> Actually, it returns 0
                       ADD 1 TO random-index

                       MOVE hangWords(random-index:) TO hangWord                       

                       PERFORM VARYING i FROM 1 BY 1
                           UNTIL i > LENGTH OF hangWord
                           
                           IF hangWord(i:1) = '' THEN
                               CONTINUE
                           ELSE
                               MOVE '_' TO hangGuessed(i:1)
                           END-IF
                       END-PERFORM

                       *> Start the game loop
                       PERFORM UNTIL playing = 0
                           MOVE 0 TO goodGuess

                           *> Assign hang man parts
                           IF loseLevel = 0 THEN
                               MOVE ' ' TO hangLine1
                               MOVE ' ' TO hangLine2
                               MOVE ' ' TO hangLine3
                               MOVE ' ' TO hangLine4
                               MOVE ' ' TO hangLine5
                               MOVE ' ' TO hangLine6
                               MOVE ' ' TO hangLine7
                               MOVE ' ' TO hangLine8
                           END-IF
                           
                           IF loseLevel = 1 THEN
                               MOVE '     ----------------             '
                                    TO hangLine8
                           END-IF
                           
                           IF loseLevel = 2 THEN
                               MOVE '            ||                    '
                                   TO hangLine2
                               MOVE '            ||                    '
                                   TO hangLine3
                               MOVE '            ||                    '
                                   TO hangLine4
                               MOVE '            ||                    '
                                   TO hangLine5
                               MOVE '            ||                    '
                                   TO hangLine6
                               MOVE '            ||                    '
                                   TO hangLine7
                           END-IF
                           
                           IF loseLevel = 3 THEN
                               MOVE '        --------------------      '
                                   TO hangLine1
                           END-IF
                           
                           IF loseLevel = 4 THEN
                               MOVE '            ||            |       '
                                   TO hangLine2
                           END-IF
                           
                           IF loseLevel = 5 THEN
                               MOVE '            ||            O       '
                                   TO hangLine3
                           END-IF
                           
                           IF loseLevel = 6 THEN
                               MOVE '            ||           /|\      '
                                   TO hangLine4
                           END-IF
                           
                           IF loseLevel = 7 THEN
                               MOVE '            ||           / \      '
                                   TO hangLine5
                           END-IF


                           DISPLAY titre
                           DISPLAY start-game
                           ACCEPT guess
                           
                           PERFORM VARYING i FROM 1 BY 1
                               UNTIL i > LENGTH OF hangWord

                               IF hangWord(i:1) = guess THEN
                                   MOVE guess to hangGuessed(i:1)
                                   MOVE 1 to goodGuess
                               END-IF
                           END-PERFORM

                           IF goodGuess = 0 THEN
                               ADD 1 TO loseLevel
                               
                               IF loseLevel = 8 THEN
                                   MOVE 0 TO playing
                               END-IF
                           ELSE
                               IF hangGuessed = hangWord THEN
                                   MOVE 0 TO playing
                               END-IF
                           END-IF
                       END-PERFORM

                       IF hangGuessed = hangWord THEN
                           DISPLAY you-won
                       ELSE
                           DISPLAY you-lose
                       END-IF

                     WHEN OTHER
                       DISPLAY invalid-action

                  END-EVALUATE
               END-PERFORM

               STOP RUN.

       END PROGRAM HANGMAN-GAME.
