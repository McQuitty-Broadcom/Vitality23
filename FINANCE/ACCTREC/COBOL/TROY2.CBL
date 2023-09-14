000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID. TROY2.                                               00000200
000300 ENVIRONMENT DIVISION.                                            00000300
000400 INPUT-OUTPUT SECTION.                                            00000400
000500 FILE-CONTROL.                                                    00000500
000600     SELECT REPORT-FILE ASSIGN U-T-SYSOUT.                        00000600
000700     SELECT INPUT-FILE ASSIGN U-T-INPUT.                          00000700
000800 DATA DIVISION.                                                   00000800
000900 FILE SECTION.                                                    00000900
001000 FD REPORT-FILE                                                   00001000
001100     LABEL RECORDS ARE OMITTED                                    00001100
001200     RECORDING MODE IS F                                          00001200
001300     RECORD CONTAINS 133 CHARACTERS                               00001300
001400     DATA RECORD IS OUTPUT-RECORD.                                00001400
001500   01 OUTPUT-RECORD.                                              00001500
001600     02 CARRIAGE-CONTROL PIC X.                                   00001600
001700     02 OUTPUT-LINE      PIC X(132).                              00001700
001800 FD INPUT-FILE                                                    00001800
001900     LABEL RECORDS ARE STANDARD                                   00001900
002000     RECORD CONTAINS 80 CHARACTERS                                00002000
002100     DATA RECORD IS INPUT-FILE-RECORD.                            00002100
002200   01 INPUT-FILE-RECORD.                                          00002200
002300     02 INPUT-FIELD1           PIC 9(8).                          00002300
002400     02 INPUT-FIELD2           PIC X(10).                         00002400
002500     02 INPUT-FIELD3           PIC X(20).                         00002500
002600     02 INPUT-FIELD4           PIC 9(6).                          00002600
002700     02 INPUT-FIELD5           PIC 9(6).                          00002700
002800     02 INPUT-FIELD6           PIC X(6).                          00002800
002900     02 FILLER                 PIC X(24).                         00002900
003000******************************************************************00003000
003100 WORKING-STORAGE SECTION.                                         00003100
003200 COPY HEADER1.                                                    00003200
003300 01  PROGRAM-WORK-FIELDS.                                         00003300
003400     02  INPUT-SWITCH      PIC X(3).                              00003400
003500         88  END-OF-FILE   VALUE 'EOF'.                           00003500
003600     02  LINES-WRITTEN     PIC 9(3).                              00003600
003700         88  NEW-PAGE      VALUE 30.                              00003700
003800     02  PAGE-COUNT        PIC 9(3).                              00003800
003900 COPY PAGING.                                                     00003900
004000 01  DATA-LINE.                                                   00004000
004100     02  FILLER                PIC X(5).                          00004100
004200     02  OUTPUT-FIELD1         PIC ZZ,ZZZ,ZZ9.                    00004200
004300     02  FILLER                PIC X(5).                          00004300
004400     02  OUTPUT-FIELD2         PIC X(10).                         00004400
004500     02  FILLER                PIC X(5).                          00004500
004600     02  OUTPUT-FIELD3         PIC X(20).                         00004600
004700     02  FILLER                PIC X(5).                          00004700
004800     02  OUTPUT-FIELD4         PIC ZZZ,ZZ9.                       00004800
004900     02  FILLER                PIC X(5).                          00004900
005000     02  OUTPUT-FIELD5         PIC ZZZ,ZZ9.                       00005000
005100     02  FILLER                PIC X(5).                          00005100
005200     02  OUTPUT-FIELD6         PIC ZZZ,ZZ9.                       00005200
005300     02  FILLER                PIC X(5).                          00005300
005400     02  OUTPUT-TOTAL          PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.           00005400
005500     02  FILLER                PIC X(19).                         00005500
005600******************************************************************00005600
005700 PROCEDURE DIVISION.                                              00005700
005800     OPEN INPUT  INPUT-FILE                                       00005800
005900          OUTPUT REPORT-FILE.                                     00005900
006000     PERFORM GET-INPUT.                                           00006000
006100     PERFORM WRITE-AND-READ                                       00006100
006200        UNTIL END-OF-FILE.                                        00006200
006300     CALL 'FINARS01'.                                             00006300
006400     CALL 'FINARS02'.                                             00006400
006500     CLOSE INPUT-FILE                                             00006500
006600           REPORT-FILE.                                           00006600
006700     GOBACK.                                                      00006700
006800******************************************************************00006800
006900 GET-INPUT.                                                       00006900
007000     READ INPUT-FILE AT END                                       00007000
007100         MOVE 'EOF' TO INPUT-SWITCH.                              00007100
007200******************************************************************00007200
007300 WRITE-AND-READ.                                                  00007300
007400     PERFORM WRITE-REPORT-LINE.                                   00007400
007500     PERFORM GET-INPUT.                                           00007500
007600******************************************************************00007600
007610******************************************************************00007610
007700 WRITE-REPORT-LINE.                                               00007700
007800     IF NEW-PAGE                                                  00007800
007900        PERFORM WRITE-HEADER                                      00007900
008000        MOVE 2 TO LINE-SPACING.                                   00008000
008100        MOVE LINE-SPACING TO CARRIAGE-CONTROL.                    00008100
008200     MOVE INPUT-FIELD1 TO OUTPUT-FIELD1.                          00008200
008300     MOVE INPUT-FIELD2 TO OUTPUT-FIELD2.                          00008300
008400     MOVE INPUT-FIELD3 TO OUTPUT-FIELD3.                          00008400
008500     MOVE INPUT-FIELD4 TO OUTPUT-FIELD4.                          00008500
008600     MOVE INPUT-FIELD5 TO OUTPUT-FIELD5.                          00008600
008700     MOVE INPUT-FIELD6 TO OUTPUT-FIELD6.                          00008700
008800     COMPUTE OUTPUT-TOTAL =                                       00008800
008900         INPUT-FIELD4 + INPUT-FIELD5 + INPUT-FIELD1.              00008900
009000     MOVE DATA-LINE TO OUTPUT-LINE.                               00009000
009100     WRITE OUTPUT-RECORD.                                         00009100
009200     ADD 1 TO LINES-WRITTEN.                                      00009200
009300     MOVE 1 TO LINE-SPACING.                                      00009300
009400     MOVE LINE-SPACING TO CARRIAGE-CONTROL.                       00009400
009500******************************************************************00009500
009600 WRITE-HEADER.                                                    00009600
009700     MOVE PAGE-SPACING TO CARRIAGE-CONTROL.                       00009700
009800     ADD 1 TO PAGE-COUNT                                          00009800
009900     MOVE '     FINANCIAL REPORT         '   TO REPORT-TITLE.     00009900
010000     MOVE PAGE-COUNT TO PAGE-NUMBER.                              00010000
010100     MOVE HEADER-RECORD TO OUTPUT-LINE.                           00010100
010200     WRITE OUTPUT-RECORD.                                         00010200
