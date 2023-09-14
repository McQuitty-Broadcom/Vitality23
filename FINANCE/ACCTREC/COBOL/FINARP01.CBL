000100 IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. FINARP01.                                                    
000300 ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
000500 FILE-CONTROL.                                                            
           SELECT REPORT-FILE ASSIGN U-T-SYSOUT.                                
           SELECT INPUT-FILE ASSIGN U-T-INPUT.                                  
000700 DATA DIVISION.                                                           
000800 FILE SECTION.                                                            
       FD REPORT-FILE                                                           
           LABEL RECORDS ARE OMITTED                                            
           RECORDING MODE IS F                                                  
           RECORD CONTAINS 133 CHARACTERS                                       
           DATA RECORD IS OUTPUT-RECORD.                                        
         01 OUTPUT-RECORD.                                                      
           02 CARRIAGE-CONTROL PIC X.                                           
           02 OUTPUT-LINE      PIC X(132).                                      
       FD INPUT-FILE                                                            
           LABEL RECORDS ARE STANDARD                                           
           RECORD CONTAINS 80 CHARACTERS                                        
           DATA RECORD IS INPUT-FILE-RECORD.                                    
         01 INPUT-FILE-RECORD.                                                  
           02 INPUT-FIELD1           PIC 9(8).                                  
           02 INPUT-FIELD2           PIC X(10).                                 
           02 INPUT-FIELD3           PIC X(20).                                 
           02 INPUT-FIELD4           PIC 9(6).                                  
           02 INPUT-FIELD5           PIC 9(6).                                  
           02 INPUT-FIELD6           PIC X(6).                                  
           02 FILLER                 PIC X(24).                                 
      ******************************************************************        
       WORKING-STORAGE SECTION.                                                 
       COPY HEADER1.                                                            
       01  PROGRAM-WORK-FIELDS.                                                 
           02  INPUT-SWITCH      PIC X(3).                                      
               88  END-OF-FILE   VALUE 'EOF'.                                   
           02  LINES-WRITTEN     PIC 9(3).                                      
               88  NEW-PAGE      VALUE 30.                                      
           02  PAGE-COUNT        PIC 9(3).                                      
       COPY PAGING.                                                             
       01  DATA-LINE.                                                           
           02  FILLER                PIC X(5).                                  
           02  OUTPUT-FIELD1         PIC ZZ,ZZZ,ZZ9.                            
           02  FILLER                PIC X(5).                                  
           02  OUTPUT-FIELD2         PIC X(10).                                 
           02  FILLER                PIC X(5).                                  
           02  OUTPUT-FIELD3         PIC X(20).                                 
           02  FILLER                PIC X(5).                                  
           02  OUTPUT-FIELD4         PIC ZZZ,ZZ9.                               
           02  FILLER                PIC X(5).                                  
           02  OUTPUT-FIELD5         PIC ZZZ,ZZ9.                               
           02  FILLER                PIC X(5).                                  
           02  OUTPUT-FIELD6         PIC ZZZ,ZZ9.                               
           02  FILLER                PIC X(5).                                  
           02  OUTPUT-TOTAL          PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.                   
           02  FILLER                PIC X(19).                                 
      ******************************************************************        
022000 PROCEDURE DIVISION.                                                      
           OPEN INPUT  INPUT-FILE                                               
                OUTPUT REPORT-FILE.                                             
           PERFORM GET-INPUT.                                            **'    
           PERFORM WRITE-AND-READ                                               
              UNTIL END-OF-FILE.                                                
           CALL 'FINARS01'.                                                     
           CALL 'FINARS02'.                                                     
           CLOSE INPUT-FILE                                                     
                 REPORT-FILE.                                                   
           GOBACK.                                                              
      ******************************************************************        
       GET-INPUT.                                                               
           READ INPUT-FILE AT END                                               
               MOVE 'EOF' TO INPUT-SWITCH.                                      
      ******************************************************************        
       WRITE-AND-READ.                                                          
           PERFORM WRITE-REPORT-LINE.                                           
           PERFORM GET-INPUT.                                                   
      ******************************************************************        
       WRITE-REPORT-LINE.                                                       
           IF NEW-PAGE                                                          
              PERFORM WRITE-HEADER                                              
              MOVE 2 TO LINE-SPACING.                                           
              MOVE LINE-SPACING TO CARRIAGE-CONTROL.                            
           MOVE INPUT-FIELD1 TO OUTPUT-FIELD1.                                  
           MOVE INPUT-FIELD2 TO OUTPUT-FIELD2.                                  
           MOVE INPUT-FIELD3 TO OUTPUT-FIELD3.                                  
           MOVE INPUT-FIELD4 TO OUTPUT-FIELD4.                                  
           MOVE INPUT-FIELD5 TO OUTPUT-FIELD5.                                  
           MOVE INPUT-FIELD6 TO OUTPUT-FIELD6.                                  
           COMPUTE OUTPUT-TOTAL =                                               
               INPUT-FIELD4 + INPUT-FIELD5 + INPUT-FIELD1.                      
           MOVE DATA-LINE TO OUTPUT-LINE.                                       
           WRITE OUTPUT-RECORD.                                                 
           ADD 1 TO LINES-WRITTEN.                                              
           MOVE 1 TO LINE-SPACING.                                              
           MOVE LINE-SPACING TO CARRIAGE-CONTROL.                               
      ******************************************************************        
       WRITE-HEADER.                                                            
           MOVE PAGE-SPACING TO CARRIAGE-CONTROL.                               
           ADD 1 TO PAGE-COUNT                                                  
           MOVE '     FINANCIAL REPORT         '   TO REPORT-TITLE.             
           MOVE PAGE-COUNT TO PAGE-NUMBER.                                      
           MOVE HEADER-RECORD TO OUTPUT-LINE.                                   
           WRITE OUTPUT-RECORD.                                                 
