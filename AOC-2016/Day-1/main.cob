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
       01 FileLine PIC X(256).

       WORKING-STORAGE SECTION.
       01 LinePointer PIC 9(4) VALUE 1.
       01 FileStatus PIC XX.
       01 I PIC 9(4) VALUE 1.
       01 X PIC S9(4) VALUE 0.
       01 Y PIC S9(4) VALUE 0.
       01 Direction PIC S9 VALUE 0.
       01 Instruction PIC X(10).
       01 TurnDirection PIC X.
       01 StepsText PIC X(9).
       01 Steps PIC 9(4).
       01 Distance PIC 9(5).
       01 FoundRepeat PIC X VALUE "N".

       01 PositionsNumber PIC 9(4) VALUE 1.
       01 Positions.
          05 Pos OCCURS 1 TO 1000 TIMES
             DEPENDING ON PositionsNumber.
             10 PosX PIC S9(4).
             10 PosY PIC S9(4).

       PROCEDURE DIVISION.
           MOVE 0 TO PosX(1)
           MOVE 0 TO PosY(1)

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
                                   FUNCTION TRIM(
                                       FileLine TRAILING))

                               MOVE SPACES TO Instruction
                               MOVE SPACES TO StepsText

                               UNSTRING FileLine
                                   DELIMITED BY ","
                                   INTO Instruction
                                   WITH POINTER LinePointer
                               END-UNSTRING

                               MOVE FUNCTION TRIM(Instruction)
                                   TO Instruction

                               MOVE Instruction(1:1)
                                   TO TurnDirection
                               MOVE Instruction(2:)
                                   TO StepsText

                               IF TurnDirection = "R"
                                   ADD 1 TO Direction
                                   IF Direction > 3
                                       MOVE 0 TO Direction
                                   END-IF
                               END-IF

                               IF TurnDirection = "L"
                                   SUBTRACT 1 FROM Direction
                                   IF Direction < 0
                                       MOVE 3 TO Direction
                                   END-IF
                               END-IF

                               COMPUTE Steps =
                                   FUNCTION NUMVAL(StepsText)

                               PERFORM Steps TIMES
                                   IF Direction = 0
                                       ADD 1 TO Y
                                   END-IF
                                   IF Direction = 1
                                       ADD 1 TO X
                                   END-IF
                                   IF Direction = 2
                                       SUBTRACT 1 FROM Y
                                   END-IF
                                   IF Direction = 3
                                       SUBTRACT 1 FROM X
                                   END-IF

                                   IF FoundRepeat = "N"
                                       PERFORM Check-Position
                                   END-IF

                                   IF PositionsNumber < 1000
                                       ADD 1 TO PositionsNumber
                                       MOVE X TO
                                           PosX(PositionsNumber)
                                       MOVE Y TO
                                           PosY(PositionsNumber)
                                   ELSE
                                       DISPLAY
                                           "Position array full"
                                       MOVE "Y" TO FoundRepeat
                                   END-IF
                               END-PERFORM
                           END-PERFORM
                   END-READ
               END-PERFORM

               CLOSE InputFile
           ELSE
               DISPLAY "Error opening: " FileStatus
           END-IF

           DISPLAY "Final position: " X "," Y
           COMPUTE Distance =
               FUNCTION ABS(X) + FUNCTION ABS(Y)
           DISPLAY "Distance: " Distance

           STOP RUN.

       Check-Position.
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > PositionsNumber
               IF PosX(I) = X AND PosY(I) = Y
                   DISPLAY "First repeated position: " X "," Y
                   COMPUTE Distance =
                       FUNCTION ABS(X) + FUNCTION ABS(Y)
                   DISPLAY "Distance to repeated position: "
                       Distance
                   MOVE "Y" TO FoundRepeat
                   EXIT PERFORM
               END-IF
           END-PERFORM.
