//BROTR01E JOB (55800000),                                                      
//      'ENDEVOR JOB',MSGLEVEL=(1,1),CLASS=B,REGION=0M,MSGCLASS=A,              
//      NOTIFY=&SYSUID                                                          
//IDCAMS   EXEC PGM=IDCAMS                                                      
//SYSPRINT DD   SYSOUT=*                                                        
//SYSIN    DD   *                                                               
  DELETE BROTR01.C1TEMPR1.QCA23.MSGS  NOSCRATCH                                 
  DELETE BROTR01.C1TEMPR1.ECA23.MSGS  NOSCRATCH                                 
  DELETE BROTR01.C1TEMPR2.QCA23.MSGS  NOSCRATCH                                 
  DELETE BROTR01.C1TEMPR2.ECA23.MSGS  NOSCRATCH                                 
/*                                                                              
