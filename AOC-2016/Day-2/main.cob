       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "input.txt"
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
           05 KeypadRow OCCURS 3 TIMES.
               10 KeypadCol OCCURS 3 TIMES.
                   15 KeypadValue PIC 9.
       01 CursorX PIC 9(1) VALUE 2.
       01 CursorY PIC 9(1) VALUE 2. 
       01 RowIndex PIC 9(1).
       01 ColIndex PIC 9(1).
       01 Counter PIC 9 VALUE 0.

       PROCEDURE DIVISION.
           
           PERFORM InitializeTable.
      *     PERFORM DisplayTable.
           OPEN INPUT InputFile

           IF FileStatus = "00"
               PERFORM UNTIL FileStatus = "10"
                   READ InputFile
                       AT END
                           CONTINUE
                       NOT AT END
                           MOVE 1 TO LinePointer

                           PERFORM UNTIL LinePointer >
                               FUNCTION LENGTH(
                                   FUNCTION TRIM(FileLine TRAILING))

                               MOVE FileLine(LinePointer:1) 
                               TO Instruction

      *                         DISPLAY Instruction
      *                         WITH NO ADVANCING


                               
                               IF Instruction = "U"
                                   SUBTRACT 1 FROM CursorY
                               END-IF
                               IF Instruction = "D"
                                   ADD 1 TO CursorY
                               END-IF
                               IF Instruction = "L"
                                   SUBTRACT 1 FROM CursorX
                               END-IF
                               IF Instruction = "R"
                                   ADD 1 TO CursorX
                               END-IF
                               IF CursorY < 1
                                   MOVE 1 TO CursorY
                               END-IF
                               IF CursorY > 3
                                   MOVE 3 TO CursorY
                               END-IF
                               IF CursorX < 1
                                   MOVE 1 TO CursorX
                               END-IF
                               IF CursorX > 3
                                   MOVE 3 TO CursorX
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
      *     DISPLAY KeypadValue(CursorY, CursorX)
                      

       STOP RUN.

       InitializeTable.
           PERFORM VARYING RowIndex FROM 1 BY 1
           UNTIL RowIndex > 3
               PERFORM VARYING ColIndex FROM 1 BY 1
               UNTIL ColIndex > 3
                   ADD 1 TO COUNTER
                   MOVE Counter TO KeypadValue(RowIndex, ColIndex)
               END-PERFORM
           END-PERFORM.
       DisplayTable.
           PERFORM VARYING RowIndex FROM 1 BY 1
           UNTIL RowIndex > 3
               PERFORM VARYING ColIndex FROM 1 BY 1
               UNTIL ColIndex > 3
                   DISPLAY KeypadValue(RowIndex, ColIndex)
                   WITH NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM.
