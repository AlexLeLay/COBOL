
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
       01 TRIANGLE.
           05 SIDE-A PIC 9(3).
           05 SIDE-B PIC 9(3).
           05 SIDE-C PIC 9(3).

       01 VALID-TRIANGLES PIC 9(4) VALUE 0.
       01 FIELD-COUNT PIC 9 VALUE 0.

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
                               INTO SIDE-A SIDE-B SIDE-C
                               TALLYING IN FIELD-COUNT
                           END-UNSTRING

                           DISPLAY "STATUS: " FILE-STATUS
                           DISPLAY "[" SIDE-A "]"
                           DISPLAY "[" SIDE-B "]"
                           DISPLAY "[" SIDE-C "]"

                           IF  SIDE-A + SIDE-B > SIDE-C 
                           AND SIDE-A + SIDE-C > SIDE-B
                           AND SIDE-B + SIDE-C > SIDE-A
                               ADD 1 TO VALID-TRIANGLES
                           END-IF
                   END-READ
               END-PERFORM

               CLOSE INPUT-FILE
           ELSE
               DISPLAY "ERROR OPENING: " FILE-STATUS
           END-IF

           DISPLAY VALID-TRIANGLES

           STOP RUN.
