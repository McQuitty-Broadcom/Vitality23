000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. TROY4.                                               00000200
      ************************************************                  00000300
      ************************************************                  00000310
      ************************************************                  00000400
      ************************************************                  00000410
      ************************************************                  00000420
      ************************************************                  00000430
      ************************************************                  00000440
000300 ENVIRONMENT DIVISION.                                            00000500
000400 INPUT-OUTPUT SECTION.                                            00000600
000500 FILE-CONTROL.                                                    00000700
000600     SELECT REPORT-FILE ASSIGN U-T-SYSOUT.                        00000800
000700     SELECT INPUT-FILE ASSIGN U-T-INPUT.                          00000900
000800 DATA DIVISION.                                                   00001000
000900 FILE SECTION.                                                    00001100
001000 FD REPORT-FILE                                                   00001200
001100     LABEL RECORDS ARE OMITTED                                    00001300
001200     RECORDING MODE IS F                                          00001400
001300     RECORD CONTAINS 133 CHARACTERS                               00001500
001400     DATA RECORD IS OUTPUT-RECORD.                                00001600
001500   01 OUTPUT-RECORD.                                              00001700
001600     02 CARRIAGE-CONTROL PIC X.                                   00001800
001700     02 OUTPUT-LINE      PIC X(132).                              00001900
001800 FD INPUT-FILE                                                    00002000
001900     LABEL RECORDS ARE STANDARD                                   00002100
002000     RECORD CONTAINS 80 CHARACTERS                                00002200
002100     DATA RECORD IS INPUT-FILE-RECORD.                            00002300
002200   01 INPUT-FILE-RECORD.                                          00002400
002300     02 INPUT-FIELD1           PIC 9(8).                          00002500
002400     02 INPUT-FIELD2           PIC X(10).                         00002600
002500     02 INPUT-FIELD3           PIC X(20).                         00002700
002600     02 INPUT-FIELD4           PIC 9(6).                          00002800
002700     02 INPUT-FIELD5           PIC 9(6).                          00002900
002800     02 INPUT-FIELD6           PIC X(6).                          00003000
002900     02 FILLER                 PIC X(24).                         00003100
003000******************************************************************00003200
003100 WORKING-STORAGE SECTION.                                         00003300
003200 COPY HEADER1.                                                    00003400
003300 01  PROGRAM-WORK-FIELDS.                                         00003500
003400     02  INPUT-SWITCH      PIC X(3).                              00003600
003500         88  END-OF-FILE   VALUE 'EOF'.                           00003700
003600     02  LINES-WRITTEN     PIC 9(3).                              00003800
003700         88  NEW-PAGE      VALUE 30.                              00003900
003800     02  PAGE-COUNT        PIC 9(3).                              00004000
003900 COPY PAGING.                                                     00004100
004000 01  DATA-LINE.                                                   00004200
004100     02  FILLER                PIC X(5).                          00004300
004200     02  OUTPUT-FIELD1         PIC ZZ,ZZZ,ZZ9.                    00004400
004300     02  FILLER                PIC X(5).                          00004500
004400     02  OUTPUT-FIELD2         PIC X(10).                         00004600
004500     02  FILLER                PIC X(5).                          00004700
004600     02  OUTPUT-FIELD3         PIC X(20).                         00004800
004700     02  FILLER                PIC X(5).                          00004900
004800     02  OUTPUT-FIELD4         PIC ZZZ,ZZ9.                       00005000
004900     02  FILLER                PIC X(5).                          00005100
005000     02  OUTPUT-FIELD5         PIC ZZZ,ZZ9.                       00005200
005100     02  FILLER                PIC X(5).                          00005300
005200     02  OUTPUT-FIELD6         PIC ZZZ,ZZ9.                       00005400
005300     02  FILLER                PIC X(5).                          00005500
005400     02  OUTPUT-TOTAL          PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.           00005600
005500     02  FILLER                PIC X(19).                         00005700
005600******************************************************************00005800
005700 PROCEDURE DIVISION.                                              00005900
005800     OPEN INPUT  INPUT-FILE                                       00006000
005900          OUTPUT REPORT-FILE.                                     00006100
006000     PERFORM GET-INPUT.                                           00006200
006100     PERFORM WRITE-AND-READ                                       00006300
006200        UNTIL END-OF-FILE.                                        00006400
006300     CALL 'FINARS01'.                                             00006500
006400     CALL 'FINARS02'.                                             00006600
006500     CLOSE INPUT-FILE                                             00006700
006600           REPORT-FILE.                                           00006800
006700     GOBACK.                                                      00006900
006800******************************************************************00007000
006810******************************************************************00007100
006900 GET-INPUT.                                                       00007200
007000     READ INPUT-FILE AT END                                       00007300
007100         MOVE 'EOF' TO INPUT-SWITCH.                              00007400
007200******************************************************************00007500
007300 WRITE-AND-READ.                                                  00007600
007400     PERFORM WRITE-REPORT-LINE.                                   00007700
007500     PERFORM GET-INPUT.                                           00007800
007600******************************************************************00007900
007700 WRITE-REPORT-LINE.                                               00008000
007800     IF NEW-PAGE                                                  00008100
007900        PERFORM WRITE-HEADER                                      00008200
008000        MOVE 2 TO LINE-SPACING.                                   00008300
008100        MOVE LINE-SPACING TO CARRIAGE-CONTROL.                    00008400
008200     MOVE INPUT-FIELD1 TO OUTPUT-FIELD1.                          00008500
008300     MOVE INPUT-FIELD2 TO OUTPUT-FIELD2.                          00008600
008400     MOVE INPUT-FIELD3 TO OUTPUT-FIELD3.                          00008700
008500     MOVE INPUT-FIELD4 TO OUTPUT-FIELD4.                          00008800
008600     MOVE INPUT-FIELD5 TO OUTPUT-FIELD5.                          00008900
008700     MOVE INPUT-FIELD6 TO OUTPUT-FIELD6.                          00009000
008800     COMPUTE OUTPUT-TOTAL =                                       00009100
008900         INPUT-FIELD4 + INPUT-FIELD5 + INPUT-FIELD1.              00009200
009000     MOVE DATA-LINE TO OUTPUT-LINE.                               00009300
009100     WRITE OUTPUT-RECORD.                                         00009400
009200     ADD 1 TO LINES-WRITTEN.                                      00009500
009300     MOVE 1 TO LINE-SPACING.                                      00009600
009400     MOVE LINE-SPACING TO CARRIAGE-CONTROL.                       00009700
009500******************************************************************00009800
009600 WRITE-HEADER.                                                    00009900
009700     MOVE PAGE-SPACING TO CARRIAGE-CONTROL.                       00010000
009800     ADD 1 TO PAGE-COUNT                                          00010100
009900     MOVE '     FINANCIAL REPORT         '   TO REPORT-TITLE.     00010200
010000     MOVE PAGE-COUNT TO PAGE-NUMBER.                              00010300
010100     MOVE HEADER-RECORD TO OUTPUT-LINE.                           00010400
010200     WRITE OUTPUT-RECORD.                                         00010500
