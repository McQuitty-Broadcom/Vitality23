000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. FINARS04.                                                    
000300 ENVIRONMENT DIVISION.                                                    
000400 INPUT-OUTPUT SECTION.                                                    
000500 FILE-CONTROL.                                                            
000600     SELECT PRINTOUT ASSIGN U-T-SYSOUT.                                   
000700 DATA DIVISION.                                                           
       FILE SECTION.                                                            
       FD  PRINTOUT                                                             
           BLOCK CONTAINS 0 RECORDS                                             
           RECORDING MODE IS F                                                  
           LABEL RECORDS ARE STANDARD                                           
           DATA RECORD IS PRINT-OUT.                                            
       01  PRINT-OUT          PIC X(133).                                       
000700 WORKING-STORAGE SECTION.                                                 
000700 01  WS-DATA  PIC X(02) VALUE 'XX'.                                       
       LINKAGE  SECTION.                                                        
         COPY FINARC01.                                                         
      ******************************************************************        
022000 PROCEDURE DIVISION.                                                      
      ******************************************************************        
162300 STOP-RUN.                                                                
162300     STOP RUN.                                                            
