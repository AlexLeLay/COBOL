
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "INPUT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.

       01 FILE-LINE PIC X(1024).

       WORKING-STORAGE SECTION.

       01 FILE-STATUS PIC XX.
       01 TRIANGLES.
           05 TRIANGLE-ROW OCCURS 3 TIMES.
               10 TRIANGLE-COL OCCURS 3 TIMES PIC 9(3).
       01 ROW-COUNT PIC 9 VALUE 1.
       01 COL-COUNT PIC 9 VALUE 1.

       01 VALID-TRIANGLES PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.

           OPEN INPUT INPUT-FILE

           IF FILE-STATUS = "00"
               PERFORM UNTIL FILE-STATUS = "10"
                   READ INPUT-FILE
                       AT END
                           CONTINUE
                       NOT AT END
                           UNSTRING FUNCTION TRIM(FILE-LINE)
                               DELIMITED BY ALL SPACE
                               INTO TRIANGLE-COL(ROW-COUNT, 1)
                                    TRIANGLE-COL(ROW-COUNT, 2)
                                    TRIANGLE-COL(ROW-COUNT, 3)
                           END-UNSTRING

                           ADD 1 TO ROW-COUNT

                           IF ROW-COUNT > 3
                               PERFORM VARYING COL-COUNT FROM 1 BY 1
                               UNTIL COL-COUNT > 3
                                   IF  TRIANGLE-COL(1, COL-COUNT)
                                     + TRIANGLE-COL(2, COL-COUNT)
                                     > TRIANGLE-COL(3, COL-COUNT)
                                   AND TRIANGLE-COL(1, COL-COUNT)
                                     + TRIANGLE-COL(3, COL-COUNT)
                                     > TRIANGLE-COL(2, COL-COUNT)
                                   AND TRIANGLE-COL(2, COL-COUNT)
                                     + TRIANGLE-COL(3, COL-COUNT)
                                     > TRIANGLE-COL(1, COL-COUNT)
                                       ADD 1 TO VALID-TRIANGLES
                                   END-IF                                   
                               END-PERFORM
                               MOVE 1 TO ROW-COUNT
                           END-IF
                   END-READ
               END-PERFORM

               CLOSE INPUT-FILE
           ELSE
               DISPLAY "ERROR OPENING: " FILE-STATUS
           END-IF

           DISPLAY VALID-TRIANGLES

           STOP RUN.
