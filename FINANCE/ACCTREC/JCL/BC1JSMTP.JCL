//BROTR01E JOB (55800000),                                                      
//      'ENDEVOR JOB',MSGLEVEL=(1,1),CLASS=B,REGION=0M,MSGCLASS=A,              
//      NOTIFY=&SYSUID                                                          
//*-------------------------------------------------------------------*         
//*                                                                   *         
//*   (C) 2002 COMPUTER ASSOCIATES INTERNATIONAL, INC.                *         
//*                                                                   *         
//* NAME: BC1JSMTP                                                    *         
//*                                                                   *         
//* PURPOSE: BC1JSMTP IS USED TO ASSEMBLE AND LINK EDIT THE ENDEVOR   *         
//*          EMAIL USERID / EMAIL ID TABLE. THE TABLE IS NAMED        *         
//*          ESMTPTBL.                                                *         
//*                                                                   *         
//*-------------------------------------------------------------------*         
//* STEP 1: ASSEMBLE THE ENDEVOR EMAIL MAINFRAME ID / EMAIL ID TABLE  *         
//*-------------------------------------------------------------------*         
//ASM      EXEC PGM=ASMA90,                                                     
//             REGION=3072K,                                                    
//             PARM='NODECK,OBJECT,NOTERM,LIST,XREF(SHORT)'                     
//SYSLIB   DD  DISP=SHR,DSN=CAMAINT.END40.SOURCE                                
//SYSLIN   DD  DSN=&&SYSLIN,                                                    
//             UNIT=SYSDA,                                                      
//             SPACE=(TRK,(3,5)),                                               
//             DISP=(NEW,PASS,DELETE),                                          
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)                             
//SYSPUNCH DD  DUMMY                                                            
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(5,15))                                    
//SYSPRINT DD  SYSOUT=*                                                         
//SYSIN    DD  *                                                                
ESMTPTBL TITLE 'ENDEVOR SMTP EMAIL TABLE'                                       
***********************************************************************         
*  (C) 2000,2002 COMPUTER ASSOCIATES INTERNATIONAL, INC.              *         
*                                                                     *         
*  NAME:        ESMTPTBL                                              *         
*                                                                     *         
*  DESCRIPTION: ENDEVOR SMTP EMAIL ADDRESS TABLE.                     *         
*                                                                     *         
*  FUNCTION:    THIS TABLE CONTAINS MAINFRAME USER ID TO EMAIL        *         
*               ID INFORMATION.                                       *         
*                                                                     *         
*  INSTRUCTIONS:                                                      *         
*               1. TAILOR THIS TABLE AS NEEDED.                       *         
*                  FIRST $ESMTP Invocation Defines:                   *         
*                     A.  SMTP HOST NAME                              *         
*                     B.  CORPORATE EMAIL DOMAIN NAME (@CA.COM)       *         
*                     C.  URL FOR ENDEVOR ECCM OR ENDEVOR WEBSTATION  *         
*                  SECOND THRU N $ESMTP INVOCATION DEFINES:           *         
*                     A.  MAINFRAME APPROVER GROUP ID OR TSO USERID   *         
*                     B.  ASSOCIATED EMAIL ADDRESS (NAME BEFORE THE @)*         
*                     C.  OPTIONAL DOMAIN (IF DIFFERENT FROM 1ST DEF) *         
*                     D.  OPTIONAL URL    (IF DIFFERENT FROM 1ST DEF) *         
*                                                                     *         
*               2. ASSEMBLE AND LINKEDIT THE TABLE USING THE MEMBER   *         
*                  BC1JSMTP CONTAINED IN THE EXAMPLE SOURCE LIBRARY.  *         
*                                                                     *         
*  ASSEMBLY RETURN CODES:                                             *         
*                0 - NORMAL                                           *         
*               12 - PARAMETER ERROR, TABLE SHOULD NOT BE LINKED.     *         
*                    CONTACT ENDEVOR SUPPORT.                         *         
*                                                                     *         
***********************************************************************         
* FIRST INVOCATION - DEFINE "GLOBAL" VALUES                                     
*---------------------------------------------------------------------*         
         $ESMTP HOSTNAME=ca06.ca.com,DFTDOMAIN=ca06.ca.com             X        
               DFTURL='http://endevor0/ccm1002/webpages/login.jsp'              
***********************************************************************         
* SUBSEQUENT INVOCATIONS - DEFINE APPROVER GROUP NAME OR USERIDS                
*---------------------------------------------------------------------*         
         $ESMTP MFID=BROTR01,EMAILID=BROTRO1@CA.COM                             
         $ESMTP MFID=WALJO11,EMAILID=WALJO11@CA.COM                             
         $ESMTP MFID=ZEMPA01,EMAILID=ZEMPA01@CA.COM                    X        
               DOMAIN=CA.COM,URL='HTTP://WWW.YAHOO.COM'                         
         $ESMTP MFID=TWEMA01,EMAILID=TWEMA01@CA.COM                             
***********************************************************************         
* LAST INVOCATION - END THE TABLE GENERATION                                    
*---------------------------------------------------------------------*         
         $ESMTP CALL=END                                                        
//*-------------------------------------------------------------------*         
//* STEP 2: LINK EDIT THE TABLE.                                      *         
//*  THE SYSLMOD DD STATEMENT DEFINES THE LIBRARY FOR ESMTPTBL.  THIS *         
//*  STEP PLACES THE ESMTPTBL LOAD MODULE INTO THE USER AUTHLIB DATA  *         
//*  SET.                                                             *         
//*-------------------------------------------------------------------*         
//LINK     EXEC PGM=IEWL,                                                       
//             REGION=2048K,                                                    
//             PARM='LIST,NCAL,XREF,LET,RENT,REUS',                             
//             COND=(0,NE)                                                      
//SYSPRINT DD  SYSOUT=*                                                         
//SYSLIN   DD  DSN=&&SYSLIN,                                                    
//             DISP=(OLD,DELETE,DELETE)                                         
//SYSLMOD  DD  DISP=SHR,DSN= CAPRD.END40.AUTHLIB(ESMTPTBL)                      
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(5,15))                                    
