//BROTR01A  JOB ,                                                               
//      'ENDEVOR JOB',MSGLEVEL=(1,1),CLASS=B,REGION=0M,MSGCLASS=A,              
//      NOTIFY=&SYSUID                                                          
//*-------------------------------------------------------------------*         
//*                                                                   *         
//*   COPYRIGHT (C) 1986-2013 CA. ALL RIGHTS RESERVED.                *         
//*                                                                   *         
//* NAME: ENBE1000                                                    *         
//*                                                                   *         
//* PURPOSE: THIS SAMPLE JCL IS USED TO EXECUTE THE BATCH ENVIRONMENT *         
//*  ADMINISTRATION JCL.  BEFORE EXECUTING THIS JCL, VERIFY THE       *         
//*  FOLLOWING:                                                       *         
//*    1) THE CONLIB DD REFERS TO THE CORRECT DATA SET.               *         
//*    2) THE ENESCLIN DD STATEMENT REFERS TO THE CONTROL STATEMENT   *         
//*       DATA SET.                                                   *         
//*    3) ADD A VALID JOB CARD STATEMENT                              *         
//*                                                                   *         
//*-------------------------------------------------------------------*         
//ENBE1000 EXEC PGM=NDVRC1,                                                     
//             PARM='ENBE1000',                                                 
//             REGION=4096K                                                     
//STEPLIB  DD DISP=SHR,DSN=CAPRD.NDVR.V180CA06.CSIQAUTU      * NDVR R18         
//         DD DISP=SHR,DSN=CAPRD.NDVR.V180CA06.CSIQAUTH      * NDVR R18         
//         DD DISP=SHR,DSN=CAPRD.NDVR.V180CA06.CSIQLOAD      * NDVR R18         
//CONLIB   DD DISP=SHR,DSN=CAPRD.NDVR.V180CA06.CSIQLOAD      * NDVR R18         
//SYSPRINT DD SYSOUT=*                                                          
//SYSOUT   DD SYSOUT=*                                                          
//SYSTERM  DD SYSOUT=*                                                          
//*-------------------------------------------------------------------*         
//* UNCOMMENT THE C1MSGS2 DD STATEMENT TO CREATE A SEPARATE SUMMARY   *         
//* REPORT.                                                           *         
//*-------------------------------------------------------------------*         
//C1MSGS1  DD SYSOUT=*                                                          
//*C1MSGS2  DD SYSOUT=*                                                         
//*-------------------------------------------------------------------*         
//* THE ENESCLIN DD STATEMENT CONTAINS THE BATCH ADMINISTRATION       *         
//* FACILITY CONTROL STATEMENTS.                                      *         
//*-------------------------------------------------------------------*         
//SYSUDUMP DD SYSOUT=*                                                          
//SYMDUMP  DD DUMMY                                                             
//*SCLOUT1  DD DSN=BROTR01.ENDEVOR.SCL(DEVCBL1),DISP=SHR                        
//*SCLOUT2  DD DSN=BROTR01.ENDEVOR.SCL(DEVCBL2),DISP=SHR                        
//*SCLOUT3  DD DSN=BROTR01.ENDEVOR.SCL(QACBL1),DISP=SHR                         
//*SCLOUT4  DD DSN=BROTR01.ENDEVOR.SCL(QACBL2),DISP=SHR                         
//*SCLOUT5  DD DSN=BROTR01.ENDEVOR.SCL(PRDCBL1),DISP=SHR                        
//*SCLOUT6  DD DSN=BROTR01.ENDEVOR.SCL(PRDCBL2),DISP=SHR                        
//ENESCLIN DD *                                                                 
BUILD SCL FOR SYSTEM "FINANCE"                                                  
    FROM ENVIRONMENT "*"                                                        
    INCLUDE SUBORDINATES                                                        
    TO DSNAME "BROTR01.ENDEVOR.SCL" MEMBER "MODEL" REPLACE.                     
//*                                                                             
