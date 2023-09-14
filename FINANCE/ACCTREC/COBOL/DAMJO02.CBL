000100 IDENTIFICATION DIVISION.                                                 
000200 PROGRAM-ID. FINAPS01.                                                    
         AUTHOR. DAMJO02.                                                       
      *****************************                                             
      ******DAMJO02  CORPORATION A*                                             
      *****************************                                             
      ******JORGEN DAM                                                          
      * local change          *****                                             
      * MF    change                                                            
      * local                                                                   
      * 03-03                                                                   
      * 03-03-02             *****                                              
      *****************************                                             
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
000700 01  WS-OUT   PIC X(02) VALUE 'YY'.                                       
       LINKAGE  SECTION.                                                        
          COPY FINARC01.                                                        
      ******************************************************************        
022000 PROCEDURE DIVISION.                                                      
022100     MOVE '** CONGRATULATIONS, FINAPS01 SUCCESSFULLY EXECUTED **'         
022200         TO COPY1-BOOK.                                                   
162300 STOP-RUN.                                                                
162300     GOBACK.                                                              
