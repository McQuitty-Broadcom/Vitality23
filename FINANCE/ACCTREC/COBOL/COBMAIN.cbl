       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID.     COBMAIN.                                         00020000
       AUTHOR.         THOMAS.                                          00030000
       ENVIRONMENT DIVISION.                                            00040000
       CONFIGURATION SECTION.                                           00050000
       SOURCE-COMPUTER.        IBM-370.                                 00060000
       OBJECT-COMPUTER.        IBM-370.                                 00070000
      ******************************************************************00090000
       INPUT-OUTPUT SECTION.                                            00100000
       FILE-CONTROL.                                                    00110000
       DATA DIVISION.                                                   00130000
      ******************************************************************00140000
       WORKING-STORAGE SECTION.                                         00160000
       01 TEST-1.                                                       00170000
             05 COMP-CODE   PIC S9999 COMP.                             00180000
       PROCEDURE DIVISION.                                              00190000
           DISPLAY 'IN COBMAIN'.                                        00200000
           DISPLAY 'HELLO KITTY!!!!!'.                                  00200100
           MOVE 5 TO COMP-CODE.                                         00201000
           GOBACK.                                                      00220000
