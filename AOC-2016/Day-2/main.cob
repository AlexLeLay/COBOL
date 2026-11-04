
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "example.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile.

       01 FileLine PIC X(1024).

       WORKING-STORAGE SECTION.

       01 LinePointer PIC 9(4) VALUE 1.
       01 FileStatus PIC XX.
       01 Instruction PIC X.

       01 Keypad.
           05 KeypadRow OCCURS 5 TIMES.
               10 KeypadCol OCCURS 5 TIMES.
                   15 KeypadValue PIC X.

       01 CursorX PIC 9 VALUE 1.
       01 CursorY PIC 9 VALUE 3.
       01 ProposedX PIC 9.
       01 ProposedY PIC 9.
       01 RowIndex PIC 9.
       01 ColIndex PIC 9.

       PROCEDURE DIVISION.

           PERFORM InitializeTable.
           PERFORM DisplayTable.

           OPEN INPUT InputFile

           IF FileStatus = "00"
               PERFORM UNTIL FileStatus = "10"
                   READ InputFile
                       AT END
                           CONTINUE
                       NOT AT END
                           MOVE 1 TO LinePointer

                           PERFORM UNTIL LinePointer > FUNCTION LENGTH(
                               FUNCTION TRIM(FileLine TRAILING))

                               MOVE FileLine(LinePointer:1)
                                   TO Instruction

                               MOVE CursorX TO ProposedX
                               MOVE CursorY TO ProposedY

                               IF Instruction = "U"
                                   SUBTRACT 1 FROM ProposedY
                               END-IF

                               IF Instruction = "D"
                                   ADD 1 TO ProposedY
                               END-IF

                               IF Instruction = "L"
                                   SUBTRACT 1 FROM ProposedX
                               END-IF

                               IF Instruction = "R"
                                   ADD 1 TO ProposedX
                               END-IF

                               IF ProposedX >= 1
                                   AND ProposedX <= 5
                                   IF ProposedY >= 1
                                       AND ProposedY <= 5
                                       IF KeypadValue
                                          (ProposedY, ProposedX) 
                                          NOT = SPACE
                                           MOVE ProposedX TO CursorX
                                           MOVE ProposedY TO CursorY
                                       END-IF
                                   END-IF
                               END-IF

                               ADD 1 TO LinePointer

                           END-PERFORM

                           DISPLAY KeypadValue(CursorY, CursorX)
                           WITH NO ADVANCING

                   END-READ
               END-PERFORM

               CLOSE InputFile
           ELSE
               DISPLAY "Error opening: " FileStatus
           END-IF

           DISPLAY SPACE

           STOP RUN.

       InitializeTable.
           INITIALIZE Keypad

           MOVE "1" TO KeypadValue(1, 3)
           MOVE "2" TO KeypadValue(2, 2)
           MOVE "3" TO KeypadValue(2, 3)
           MOVE "4" TO KeypadValue(2, 4)
           MOVE "5" TO KeypadValue(3, 1)
           MOVE "6" TO KeypadValue(3, 2)
           MOVE "7" TO KeypadValue(3, 3)
           MOVE "8" TO KeypadValue(3, 4)
           MOVE "9" TO KeypadValue(3, 5)
           MOVE "A" TO KeypadValue(4, 2)
           MOVE "B" TO KeypadValue(4, 3)
           MOVE "C" TO KeypadValue(4, 4)
           MOVE "D" TO KeypadValue(5, 3).

       DisplayTable.
           PERFORM VARYING RowIndex FROM 1 BY 1
           UNTIL RowIndex > 5
               PERFORM VARYING ColIndex FROM 1 BY 1
               UNTIL ColIndex > 5
                   DISPLAY KeypadValue(RowIndex, ColIndex)
                       WITH NO ADVANCING
                   DISPLAY SPACE WITH NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM.
