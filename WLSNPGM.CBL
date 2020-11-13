 CBL XOPTS(SP)                                                                  
      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    WLSNPGM.                                                  
       AUTHOR.        DAVE L CLARK I.                                           
       DATE-WRITTEN.  APRIL 2019.                                               
       DATE-COMPILED.                                                           
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.                              
       SECURITY.      NONE.                                                     
      *REMARKS.       WINSUPPLY LDAP SIGNON APPLICATION.                        
      *                                                                         
      * note that this program also sets vuserid attributes upon signon.        
      *     any changes here must also be reflected in program VUSRATTR         
      *                                         and in program VUSRAUDT.        
      *                                                                         
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 04/11/2019 DLC ORIGINAL PROGRAM.                                        
      * END OF HISTORY ------------------------------------------------         
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    ENVIRONMENT DIVISION                                        *        
      *                                                                *        
      ******************************************************************        
       ENVIRONMENT DIVISION.                                                    
                                                                                
      ******************************************************************        
      *    CONFIGURATION SECTION                                       *        
      ******************************************************************        
       CONFIGURATION SECTION.                                                   
                                                                                
       SOURCE-COMPUTER. IBM-2086-A04-140.                                       
       OBJECT-COMPUTER. IBM-2096-N03.                                           
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    DATA DIVISION                                               *        
      *                                                                *        
      ******************************************************************        
       DATA DIVISION.                                                           
                                                                                
      ******************************************************************        
      *    WORKING-STORAGE SECTION                                     *        
      ******************************************************************        
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  CONTROL-FIELDS.                                                      
      *                                                                         
      * FIELDS TO MANAGE PROGRAM AND MAP RESOURCE NAMES                         
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='WLSN'==              
                                       =='PROGRAM'== BY =='WLSNPGM'==.          
         03  WLSNMS                    PIC  X(08)   VALUE 'WLSNMS  '.           
         03  WLSNMAP                   PIC  X(08)   VALUE 'WLSNMAP '.           
         03  WARNWIN                   PIC  X(08)   VALUE 'WARNWIN '.           
      *                                                                         
      * FIELDS TO MANAGE REFERENCED FILE NAMES                                  
         03  VUSERID                   PIC  X(08)   VALUE 'VUSERID '.           
         03  IESLDUM                   PIC  X(08)   VALUE 'IESLDUM '.           
      *                                                                         
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES                      
         03  IESLDGAC                  PIC  X(08)   VALUE 'IESLDGAC'.           
         03  IESLDSOC                  PIC  X(08)   VALUE 'IESLDSOC'.           
         03  MAILWRTR                  PIC  X(08)   VALUE 'MAILWRTC'.           
      *                                                                         
      * FIELDS TO MANAGE ERROR HANDLING                                         
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.               
           88  NO-ERRORS-FOUND                      VALUE ZEROES.               
           88  ERROR-AT-CURSOR                      VALUE -1.                   
           88  ERRORS-FOUND                         VALUES -9 THRU -1.          
         03  LOCKED-SWITCH             PIC  X(1)    VALUE 'N'.                  
           88  USER-NOT-LOCKED                      VALUE 'N'.                  
           88  USER-IS-LOCKED                       VALUE 'Y'.                  
         03  SIGNON-SWITCH             PIC  X(1)    VALUE 'Y'.                  
           88  SIGNON-BY-NETUSER                    VALUE 'Y'.                  
           88  SIGNON-BY-VSEUSER                    VALUE 'N'.                  
         03  WARNING-SWITCH            PIC  X(1)    VALUE 'N'.                  
           88  WARNING-SENT                         VALUE 'N'.                  
           88  SEND-WARNING                         VALUE 'Y'.                  
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.        
      *                                                                         
         03  DSP-DATE                  PIC  X(10).                              
         03  DSP-TIME                  PIC  X(08).                              
                                                                                
         03  WORD1                     PIC  X(20)   VALUE SPACES.               
         03  WORD2                     PIC  X(20)   VALUE SPACES.               
         03  WORD3                     PIC  X(20)   VALUE SPACES.               
         03  WORD4                     PIC  X(20)   VALUE SPACES.               
         03  WORD5                     PIC  X(20)   VALUE SPACES.               
                                                                                
         03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.        
         03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.        
      *                                                                         
      * TO CONVERT DATES AND TIMES                                              
         03  NUM-DATE                  PIC  9(8).                               
         03  WRK-DATE                  REDEFINES    NUM-DATE.                   
           05  DTE-GRMO                PIC  99.                                 
           05  DTE-GRDA                PIC  99.                                 
           05  DTE-GRYR                PIC  9999.                               
         03  WS-ABSTIME                PIC S9(15)   COMP-3 VALUE ZEROES.        
         03  WS-WRKDATE.                                                        
           05  WS-MODATE               PIC  9(02).                              
           05                          PIC  X.                                  
           05  WS-DADATE               PIC  9(02).                              
           05                          PIC  X.                                  
           05  WS-CYDATE               PIC  9(04).                              
         03  WS-WRKTIME.                                                        
           05  WS-HRSTIME              PIC  9(02).                              
           05                          PIC  X(03).                              
           05  WS-XXXTIME              PIC  X(03).                              
                                                                                
       01  WIN-LOGO-CORRECTION.                                                 
         03  SET-BUFFER-ADDRESS-1      PIC  X(3) VALUE x'11C2F3'.               
         03  SET-ATTRB-COLOR-BLUE      PIC  X(3) VALUE x'2842F1'.               
         03                            PIC  X(3) VALUE  'Win'.                  
         03  SET-BUFFER-ADDRESS-2      PIC  X(3) VALUE x'11C27D'.               
         03  SET-ATTRB-COLOR-TURQ      PIC  X(3) VALUE x'2842F5'.               
         03  GRAPHIC-ESCAPE-APL-LINE   PIC  X(4) VALUE x'08A208A2'.             
                                                                                
       01  MAILWRTR-COMMAREA.                                                   
         COPY MAILWRTR.                                                         
                                                                                
      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS                     
                                                                                
      * IBM GET LDAP ATTRIBUTE INTERFACE                                        
       01  IESLDGA-COMMAREA.                                                    
         03  LDGA-AREA-LENGTH          PIC S9(09)   BINARY.                     
         03  LDGA-USER-ID              PIC  X(64).                              
         03  LDGA-SEARCH-FILTER        PIC  X(128).                             
         03  LDGA-RET-CODE             PIC S9(09)   BINARY.                     
         03  LDGA-LDAP-CODE            PIC S9(09)   BINARY.                     
         03  LDGA-ATTR-COUNT           PIC S9(04)   BINARY.                     
         03  LDGA-ATTR-ENTRY                        OCCURS 16.                  
           05  LDGA-ATTR-NAME          PIC  X(64).                              
           05  LDGA-VALUE-LENGTH       PIC S9(04)   BINARY.                     
           05  LDGA-VALUE-COUNT        PIC S9(04)   BINARY.                     
           05  LDGA-VALUE-ENTRY                     OCCURS 1.                   
             07  LDGA-ATTR-VALUE       PIC  X(128).                             
                                                                                
      * IBM GET LDAP SIGNON INTERFACE                                           
       COPY IESLDSOC.                                                           
                                                                                
       01  CICSINFO-PARMS.                                                      
         COPY CICSINFO.                                                         
                                                                                
       COPY HEXMAN.                                                             
                                                                                
       COPY LOGGING.                                                            
                                                                                
       COPY UNEXERRW.                                                           
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).                 
                                                                                
      /*****************************************************************        
      *    LINKAGE SECTION                                             *        
      ******************************************************************        
       LINKAGE SECTION.                                                         
                                                                                
      * WLSN CICS COMMUNICATION AREA                                            
       01  DFHCOMMAREA.                                                         
         COPY COMMAREA.                                                         
           05  WLSN-SAVEKEY            PIC  X(8).                               
                                                                                
      * WLSN TEMP COMMUNICATION AREA                                            
       COPY COMMTEMP.                                                           
                                                                                
      * 3270 DATA STREAM CONTROL CHARACTERS                                     
       COPY IBM3270.                                                            
                                                                                
      * VUSERID RECORD I/O AREA                                                 
       COPY VUSERID.                                                            
                                                                                
      * IESLDUM RECORD I/O AREA                                                 
       COPY IESLDUM.                                                            
                                                                                
      * BMS TERMINAL I/O AREAS                                                  
       COPY WLSNMS.                                                             
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    PROCEDURE DIVISION                                          *        
      *                                                                *        
      ******************************************************************        
       PROCEDURE DIVISION.                                                      
                                                                                
      ******************************************************************        
      *    PROGRAM INITIALIZATION                                      *        
      ******************************************************************        
       A00-WLSN-PROGRAM-ENTRY.                                                  
                                                                                
           COPY COMMENTR.                                                       
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM  ('CICSINFO')                                      
                     COMMAREA (CICSINFO-PARMS)                                  
           END-EXEC.                                                            
           MOVE CICS-FULLDATE          TO WS-WRKDATE.                           
           MOVE CICS-FULLTIME          TO WS-WRKTIME.                           
           PERFORM Q02-REFORMAT-TIME THRU Q05-EXIT.                             
                                                                                
           COPY COMM3270.                                                       
                                                                                
           SET  DAPL-LOGGING           TO TRUE.                                 
                                                                                
           IF  COMM-CURRPGM = THIS-PGM                                          
      * psuedo-conversational program continuation                              
               GO TO B50-WLSN-ROUTINE-SELECTION                                 
           END-IF.                                                              
                                                                                
       B00-WLSN-FIRST-TIME-ONLY.                                                
                                                                                
           COPY COMMINIT.                                                       
                                                                                
           MOVE SPACES                 TO LOGF-MESG.                            
           STRING CICS-USERID ' signing off (' EIBTRMID ')'                     
               DELIMITED BY SIZE     INTO LOGF-MESG.                            
           PERFORM Q100-LOGIT        THRU Q199-EXIT.                            
                                                                                
           EXEC CICS SIGNOFF                                                    
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           EXEC CICS SET TERMINAL(EIBTRMID)                                     
                     TRANIDONLY                                                 
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
       B50-WLSN-ROUTINE-SELECTION.                                              
                                                                                
           IF (COMM-CURRRTN = 'RETN' OR 'EXIT')                                 
           AND COMM-SAVE-FUNC > SPACES                                          
               MOVE COMM-CURRRTN       TO INIT-FLAG                             
               MOVE COMM-SAVE-FUNC     TO COMM-CURRRTN                          
               MOVE INIT-FLAG          TO COMM-SAVE-FUNC                        
           END-IF.                                                              
                                                                                
           EVALUATE COMM-CURRRTN                                                
           WHEN 'WLSN' GO TO C00-WLSN-ROUTINE                                   
           END-EVALUATE.                                                        
                                                                                
      /*****************************************************************        
      *    WLSN ROUTINE                                                *        
      ******************************************************************        
       C00-WLSN-ROUTINE.                                                        
                                                                                
           IF  ADDRESS OF WLSNMAPI = NULL                                       
               EXEC CICS GETMAIN                                                
                         SET      (ADDRESS OF WLSNMAPI)                         
                         LENGTH   (LENGTH OF WLSNMAPI)                          
                         INITIMG  (LOVALUE)                                     
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
           IF  COMM-CURRRTN NOT = 'WLSN'                                        
           OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
               IF  COMM-SAVE-FUNC = INIT-FLAG                                   
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
                   MOVE SPACES         TO COMM-SAVE-FUNC                        
                   MOVE HIGH-VALUES    TO INIT-FLAG                             
               END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
               PERFORM C80-WLSN-OUTPUT THRU C85-WLSN-SENDMAP                    
               GO TO C50-WLSN-DISPLAY                                           
           END-IF.                                                              
                                                                                
       C10-WLSN-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP      (WLSNMAP)                                         
                     INTO     (WLSNMAPI)                                        
                     MAPSET   (WLSNMS)                                          
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO C70-WLSN-ERRORS                                            
           END-IF.                                                              
                                                                                
       C20-WLSN-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
           WHEN AIDPFK03                                                        
               EXEC CICS SET TERMINAL(EIBTRMID)                                 
                         UCTRAN                                                 
                         NOHANDLE                                               
               END-EXEC                                                         
               MOVE SPACES             TO LOGF-MESG                             
               STRING CICS-USERID ' signon cancelled (' EIBTRMID ')'            
                   DELIMITED BY SIZE INTO LOGF-MESG                             
               PERFORM Q100-LOGIT    THRU Q199-EXIT                             
               MOVE 1                  TO UNEX-MSGL                             
               STRING 'WLSN - Signon cancelled.  '                              
                      'Press ENTER to sign on.' SCRCURSR                        
                   DELIMITED BY SIZE INTO UNEX-MSG                              
                                     WITH POINTER UNEX-MSGL                     
               SUBTRACT 1            FROM UNEX-MSGL                             
               EXEC CICS SEND                                                   
                         FROM(UNEX-MSG)                                         
                         LENGTH(UNEX-MSGL)                                      
                         CTLCHAR(WRTFKFST)                                      
                         ERASE DEFAULT                                          
               END-EXEC                                                         
               GO TO Z95-CICS-RETURN                                            
                                                                                
           WHEN AIDENTER                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE 'No current action assigned to attention key.'              
                                       TO WLSN-MESSAGE-O                        
               GO TO C80-WLSN-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE 'No current action assigned to function key.'               
                                       TO WLSN-MESSAGE-O                        
               GO TO C80-WLSN-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       C30-WLSN-PROCESS.                                                        
                                                                                
           MOVE DFHRESP(NORMAL)        TO EIBRESP.                              
                                                                                
           IF  WLSN-PASSWORD-I NOT > SPACES                                     
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-PASSWORD-L                       
               MOVE 'WinZone password is required.'                             
                                       TO WLSN-MESSAGE-O                        
           END-IF.                                                              
                                                                                
           IF  WLSN-USERID-I NOT > SPACES                                       
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE 'WinZone user id is required.'                              
                                       TO WLSN-MESSAGE-O                        
           END-IF.                                                              
                                                                                
           IF  ERRORS-FOUND                                                     
               GO TO C80-WLSN-OUTPUT                                            
           END-IF.                                                              
                                                                                
      * see if there is a mainframe id associated and then vuserid              
           PERFORM Q20-INITKEY-VUSERID.                                         
           PERFORM Q40-INITKEY-IESLDUM.                                         
           MOVE WLSN-USERID-I          TO LDUM-NETUSRID.                        
           PERFORM Q46-READEQ-IESLDUM.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               MOVE LDUM-MFUSRID       TO VUSER-KEY                             
               PERFORM Q26-READEQ-VUSERID                                       
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   MOVE SPACES         TO VUSERID-RECORD                        
               END-IF                                                           
           ELSE                                                                 
               MOVE SPACES             TO LDUM-MFUSRID                          
                                          VUSERID-RECORD                        
           END-IF.                                                              
                                                                                
      * this code translates a network user id into a vse user id               
      * note that the user's network password is required for this              
           INITIALIZE IESLDSO-COMMAREA.                                         
           MOVE WLSN-USERID-I          TO LDSO-NET-USERID.                      
           MOVE WLSN-PASSWORD-I        TO LDSO-NET-PASSWORD.                    
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM  (IESLDSOC)                                        
                     COMMAREA (IESLDSO-COMMAREA)                                
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO C70-WLSN-ERRORS                                            
           END-IF.                                                              
                                                                                
           IF  LDSO-RET-CODE NOT = ZERO                                         
             MOVE SPACES               TO WLSN-MESSAGE-O                        
             EVALUATE TRUE                                                      
             WHEN LDSO-RET-CODE = 12                                            
             AND  LDSO-LDAP-CODE = ZERO                                         
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE 'User not defined or not mapped.  Call Network Suppo        
      -             'rt.'              TO WLSN-MESSAGE-O                        
               STRING '"' LDSO-NET-USERID '"' DELIMITED BY SPACE                
                      ' user not defined or not mapped'                         
                      ' (' EIBTRMID ')'   DELIMITED BY SIZE                     
                                     INTO LOGF-MESG                             
             WHEN LDSO-RET-CODE = 13                                            
             AND  LDSO-LDAP-CODE = 49                                           
               PERFORM P10-GET-LOCKOUT-TIME THRU P15-EXIT                       
               IF  NO-ERRORS-FOUND                                              
                 MOVE LOW-VALUES       TO WLSN-PASSWORD-O                       
                 MOVE SPACES           TO WLSN-PASSWORD-O(1:32)                 
                 MOVE     -1           TO ERROR-SWITCH                          
                                          WLSN-PASSWORD-L                       
                 MOVE EXHREVRS         TO WLSN-MESSAGE-H                        
                 IF  USER-NOT-LOCKED                                            
                   EVALUATE LDGA-ATTR-VALUE(2 1)                                
                   WHEN 3                                                       
                     STRING 'Password incorrect ' DELIMITED BY SIZE             
                            LDGA-ATTR-VALUE(2 1)  DELIMITED BY SPACE            
                            ' times.  Next failure will lock you out.'          
                       DELIMITED BY SIZE INTO WLSN-MESSAGE-O                    
                   WHEN 1                                                       
                     STRING 'Password incorrect ' DELIMITED BY SIZE             
                            LDGA-ATTR-VALUE(2 1)  DELIMITED BY SPACE            
                            ' time.  Be sure caps lock is off.'                 
                       DELIMITED BY SIZE INTO WLSN-MESSAGE-O                    
                   WHEN OTHER                                                   
                     STRING 'Password incorrect ' DELIMITED BY SIZE             
                            LDGA-ATTR-VALUE(2 1)  DELIMITED BY SPACE            
                            ' times.  Be sure caps lock is off.'                
                       DELIMITED BY SIZE INTO WLSN-MESSAGE-O                    
                   END-EVALUATE                                                 
                   STRING '"' LDSO-NET-USERID '"' DELIMITED BY SPACE            
                          ' password incorrect (' EIBTRMID '):'                 
                          ' count = '             DELIMITED BY SIZE             
                          LDGA-ATTR-VALUE(2 1)    DELIMITED BY SPACE            
                          ', lockout = '          DELIMITED BY SIZE             
                          LDGA-ATTR-VALUE(1 1)    DELIMITED BY SPACE            
                                     INTO LOGF-MESG                             
                 ELSE                                                           
                   MOVE 'Too many invalid attempts.  Call Network Suppor        
      -                 't.'           TO WLSN-MESSAGE-O                        
                   STRING '"' LDSO-NET-USERID '"' DELIMITED BY SPACE            
                          ' user is locked out (' EIBTRMID '):'                 
                          ' count = '             DELIMITED BY SIZE             
                          LDGA-ATTR-VALUE(2 1)    DELIMITED BY SPACE            
                          ', lockout = '          DELIMITED BY SIZE             
                          LDGA-ATTR-VALUE(1 1)    DELIMITED BY SPACE            
                                     INTO LOGF-MESG                             
                   IF  VUSER-LOCK-FLAG NOT = 'X'                                
                       PERFORM P20-NOTIFY THRU P25-EXIT                         
                   END-IF                                                       
                 END-IF                                                         
               END-IF                                                           
             WHEN OTHER                                                         
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE SPACES             TO WLSN-MESSAGE-O                        
                                          LOGF-MESG                             
               MOVE 1                  TO IDX                                   
                                          CNT                                   
               STRING 'Unable to identify user'                                 
                   DELIMITED BY SIZE INTO WLSN-MESSAGE-O                        
                                     WITH POINTER IDX                           
               STRING LDSO-NET-USERID     DELIMITED BY SPACE                    
                      ' unable to identify (' EIBTRMID ')'                      
                   DELIMITED BY SIZE INTO LOGF-MESG                             
                                     WITH POINTER CNT                           
               MOVE LDSO-RET-CODE      TO UNEX-RESP                             
               MOVE UNEX-RESP          TO UNEX-VARIABLE                         
               PERFORM Q203-LEFT-JUSTIFY                                        
               STRING ' - rc='            DELIMITED BY SIZE                     
                      UNEX-VARIABLE       DELIMITED BY SPACE                    
                                     INTO WLSN-MESSAGE-O                        
                                     WITH POINTER IDX                           
               STRING ' - rc='            DELIMITED BY SIZE                     
                      UNEX-VARIABLE       DELIMITED BY SPACE                    
                                     INTO LOGF-MESG                             
                                     WITH POINTER CNT                           
               MOVE LDSO-LDAP-CODE     TO UNEX-RESP                             
               MOVE UNEX-RESP          TO UNEX-VARIABLE                         
               PERFORM Q203-LEFT-JUSTIFY                                        
               STRING ', rsn='            DELIMITED BY SIZE                     
                      UNEX-VARIABLE       DELIMITED BY SPACE                    
                      '.  Be sure caps lock is off.'                            
                   DELIMITED BY SIZE INTO WLSN-MESSAGE-O                        
                                     WITH POINTER IDX                           
               STRING ', rsn='            DELIMITED BY SIZE                     
                      UNEX-VARIABLE       DELIMITED BY SPACE                    
                                     INTO LOGF-MESG                             
                                     WITH POINTER CNT                           
             END-EVALUATE                                                       
             PERFORM Q100-LOGIT      THRU Q199-EXIT                             
             GO TO C80-WLSN-OUTPUT                                              
           END-IF.                                                              
                                                                                
           IF  LDUM-MFUSRID <= SPACES                                           
               SET  SIGNON-BY-VSEUSER  TO TRUE                                  
      *        MOVE     -1             TO ERROR-SWITCH                          
      *                                   WLSN-USERID-L                         
      *        MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
      *        MOVE 'The user id is not defined or not mapped.  Be sure         
      *            'caps lock is off.' TO WLSN-MESSAGE-O                        
      *        GO TO C80-WLSN-OUTPUT                                            
           END-IF.                                                              
                                                                                
           IF  LDUM-MFUSRID > SPACES                                            
               PERFORM P10-GET-LOCKOUT-TIME THRU P15-EXIT                       
               IF  ERRORS-FOUND                                                 
                   GO TO C80-WLSN-OUTPUT                                        
               END-IF                                                           
               INSPECT LDGA-ATTR-VALUE(3 1) CONVERTING LOWER-CASE               
                                                    TO UPPER-CASE               
               IF  LDSO-VSE-USERID NOT = LDGA-ATTR-VALUE(3 1)                   
                   MOVE     -1         TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
                   MOVE EXHREVRS       TO WLSN-MESSAGE-H                        
                   MOVE 'Active Directory has wrong mainframe id.  Call         
      -             'Network Support.' TO WLSN-MESSAGE-O                        
                   STRING '"' LDSO-VSE-USERID '"' DELIMITED BY SPACE            
                          ' does not match'                                     
                          ' Active Directory '    DELIMITED BY SIZE             
                          '(' LDGA-ATTR-VALUE(3 1) ')'                          
                      DELIMITED BY SPACE INTO LOGF-MESG                         
                   PERFORM Q100-LOGIT    THRU Q199-EXIT                         
                   GO TO C80-WLSN-OUTPUT                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * this code signs a vse user on to cics                                   
           EXEC CICS SIGNON                                                     
                     USERID   (LDSO-VSE-USERID)                                 
                     PASSWORD (LDSO-VSE-PASSWORD)                               
                     ESMRESP  (RESPX-RESP)                                      
                     ESMREASON(RESPX-RESP2)                                     
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
           AND EIBRESP NOT = DFHRESP(NOTAUTH)                                   
           AND EIBRESP NOT = DFHRESP(USERIDERR)                                 
               GO TO C70-WLSN-ERRORS                                            
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
           WHEN EIBRESP = DFHRESP(USERIDERR)                                    
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE 'Mapping failure on user id (see log file).  Call Ne        
      -             'twork Support.'   TO WLSN-MESSAGE-O                        
               PERFORM Q90-LOG-SIGNON-ERROR THRU Q99-EXIT                       
               GO TO C80-WLSN-OUTPUT                                            
                                                                                
           WHEN EIBRESP = DFHRESP(NOTAUTH)                                      
           AND  EIBRESP2 <= 2                                                   
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE 'Mapping failure on password (see log file).  Call N        
      -             'etwork Support.'  TO WLSN-MESSAGE-O                        
               PERFORM Q90-LOG-SIGNON-ERROR THRU Q99-EXIT                       
               GO TO C80-WLSN-OUTPUT                                            
                                                                                
           WHEN EIBRESP = DFHRESP(NOTAUTH)                                      
           AND  EIBRESP2 = 19                                                   
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE 'Associated mainframe user id is revoked.  Call Netw        
      -             'ork Support.'     TO WLSN-MESSAGE-O                        
               PERFORM Q90-LOG-SIGNON-ERROR THRU Q99-EXIT                       
               GO TO C80-WLSN-OUTPUT                                            
                                                                                
           WHEN RESPX-RESP NOT = ZERO                                           
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE 'General signon failure (see log file).  Call Networ        
      -             'k Support.'       TO WLSN-MESSAGE-O                        
               PERFORM Q90-LOG-SIGNON-ERROR THRU Q99-EXIT                       
               GO TO C80-WLSN-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
      * log the fact that the signon completed                                  
           MOVE SPACES                 TO LOGF-MESG.                            
           STRING LDSO-VSE-USERID ' signon complete (' EIBTRMID ')'             
               DELIMITED BY SIZE     INTO LOGF-MESG.                            
           PERFORM Q100-LOGIT        THRU Q199-EXIT                             
                                                                                
           EXEC CICS SET TERMINAL(EIBTRMID)                                     
                     UCTRAN                                                     
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  SIGNON-BY-NETUSER                                                
               PERFORM Q20-INITKEY-VUSERID                                      
               MOVE LDSO-VSE-USERID    TO VUSER-KEY                             
               PERFORM Q26-READUP-VUSERID                                       
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                 MOVE SPACE            TO VUSER-LOCK-FLAG                       
      * use this code to copy attributes to WIN-defined file                    
                 PERFORM P40-PARSE-OUT-UNIT-VALUE THRU P45-EXIT                 
                 MOVE LDGA-ATTR-VALUE(5 1)                                      
                                       TO VUSER-DEPT-NAME                       
                 MOVE LDGA-ATTR-VALUE(6 1)                                      
                                       TO VUSER-PRIMARY-CO                      
                 MOVE LDGA-ATTR-VALUE(7 1)                                      
                                       TO VUSER-PRIMARY-IND                     
                 INSPECT VUSER-PRIMARY-IND CONVERTING LOWER-CASE                
                                                   TO UPPER-CASE                
                 MOVE LDGA-ATTR-VALUE(8 1)                                      
                                       TO VUSER-WIN-ORG                         
                 INSPECT VUSER-WIN-ORG    CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
                 MOVE LDGA-ATTR-VALUE(9 1)                                      
                                       TO VUSER-WIN-FUNCTION                    
                 INSPECT VUSER-WIN-FUNCTION CONVERTING LOWER-CASE               
                                                    TO UPPER-CASE               
                 MOVE LDGA-ATTR-VALUE(10 1)                                     
                                       TO VUSER-WIN-DIRECTOR                    
                 INSPECT VUSER-WIN-DIRECTOR CONVERTING LOWER-CASE               
                                                    TO UPPER-CASE               
                 MOVE LDGA-ATTR-VALUE(11 1)                                     
                                       TO VUSER-WIN-POSITION                    
                 INSPECT VUSER-WIN-POSITION CONVERTING LOWER-CASE               
                                                    TO UPPER-CASE               
      * overrides for test users, only                                          
                 IF  VUSER-WIN-UNIT = 'TEST'                                    
                   MOVE VUSER-WIN-UNIT TO VUSER-DEPT-NAME                       
                   EVALUATE TRUE                                                
                   WHEN LDSO-VSE-USERID = 'WZTESTAC'                            
                     MOVE 'LINCOLWWA'  TO VUSER-WIN-UNIT                        
                     MOVE 'M01'        TO VUSER-WIN-ORG                         
                     MOVE 'AC'         TO VUSER-WIN-FUNCTION                    
                     MOVE '00783'      TO VUSER-PRIMARY-CO                      
                     MOVE 'W'          TO VUSER-PRIMARY-IND                     
                   WHEN LDSO-VSE-USERID = 'WZTESTAL'                            
                     MOVE 'AL'         TO VUSER-WIN-UNIT                        
                     MOVE 'A00'        TO VUSER-WIN-ORG                         
                     MOVE 'VP'         TO VUSER-WIN-FUNCTION                    
                     MOVE '00961'      TO VUSER-PRIMARY-CO                      
                     MOVE 'M'          TO VUSER-PRIMARY-IND                     
                   WHEN LDSO-VSE-USERID = 'WZTSTEXC'                            
                     MOVE 'EXEC'       TO VUSER-WIN-UNIT                        
                     MOVE '00924'      TO VUSER-WIN-ORG                         
                     MOVE 'WW'         TO VUSER-WIN-FUNCTION                    
                     MOVE '00924'      TO VUSER-PRIMARY-CO                      
                     MOVE 'O'          TO VUSER-PRIMARY-IND                     
                   WHEN LDSO-VSE-USERID = 'WZTSTLCP'                            
                     MOVE 'DAYTONWNE'  TO VUSER-WIN-UNIT                        
                     MOVE '00108'      TO VUSER-WIN-ORG                         
                     MOVE 'LC'         TO VUSER-WIN-FUNCTION                    
                     MOVE '00108'      TO VUSER-PRIMARY-CO                      
                     MOVE 'H'          TO VUSER-PRIMARY-IND                     
                   END-EVALUATE                                                 
                   MOVE 'Y'            TO VUSER-WIN-DIRECTOR                    
                 END-IF                                                         
      *                                                                         
                 EVALUATE TRUE                                                  
                 WHEN VUSER-WIN-FUNCTION = 'LC' OR 'AC'                         
                   SET VUSER-WISE-USER-OR-AC TO TRUE                            
                   IF  VUSER-WIN-DIRECTOR = 'Y'                                 
                       MOVE '21'       TO VUSER-CLASS                           
                   ELSE                                                         
                       MOVE '20'       TO VUSER-CLASS                           
                   END-IF                                                       
                   MOVE VUSER-PRIMARY-CO(3:3)                                   
                                       TO VUSER-IND-LIST                        
                   IF  VUSER-WIN-FUNCTION = 'AC'                                
                       MOVE '*'        TO VUSER-DFQ-SELECT(1:1)                 
                       MOVE VUSER-WIN-ORG                                       
                                       TO VUSER-DFQ-SELECT(2:)                  
                   END-IF                                                       
                                                                                
                 WHEN VUSER-WIN-FUNCTION = 'VP'                                 
                 AND  VUSER-WIN-UNIT = 'AL'                                     
                   SET VUSER-AREA-LEADER TO TRUE                                
                   MOVE '99'           TO VUSER-CLASS                           
                   MOVE '*'            TO VUSER-IND-LIST(1:1)                   
                   MOVE VUSER-WIN-ORG(1:1)                                      
                                       TO VUSER-IND-LIST(2:)                    
                   MOVE VUSER-IND-LIST TO VUSER-DFQ-SELECT                      
                                                                                
                 WHEN VUSER-WIN-FUNCTION = 'WW' OR 'WG' OR 'WS'                 
                                        OR 'VP' OR 'CO'                         
                   MOVE '9'            TO VUSER-DAPSCO                          
                   IF  VUSER-WIN-UNIT = 'ACCT' OR 'AP' OR 'COMM'                
                                     OR 'COMM'    OR 'CUSTSVC'                  
                                     OR 'DTAMGMT' OR 'EXEC'                     
                                     OR 'FIN'     OR 'HR'                       
                                     OR 'INVSTR'  OR 'IT'                       
                                     OR 'MKT'     OR 'PRICING'                  
                                     OR 'PURCHS'  OR 'ROMS'                     
                                     OR 'SPS'     OR 'TRAINING'                 
                                     OR 'WCMS'    OR 'WINSUP'                   
                       MOVE '99'       TO VUSER-CLASS                           
                   ELSE                                                         
                     IF  VUSER-WIN-DIRECTOR = 'Y'                               
                       MOVE '95'       TO VUSER-CLASS                           
                     ELSE                                                       
                       MOVE '91'       TO VUSER-CLASS                           
                     END-IF                                                     
                   END-IF                                                       
                                                                                
                 WHEN OTHER                                                     
                   MOVE 'X'            TO VUSER-DAPSCO                          
                   MOVE '00'           TO VUSER-CLASS                           
                   MOVE SPACES         TO VUSER-IND-LIST                        
                                          VUSER-DFQ-SELECT                      
                 END-EVALUATE                                                   
                                                                                
                 IF  VUSER-DO-REFRESH                                           
                   PERFORM Q27-REWRITE-VUSERID                                  
                 ELSE                                                           
                   PERFORM Q28-UNLOCK-VUSERID                                   
                 END-IF                                                         
      * use this code to exit to a menuing application                          
                 MOVE 'EXIT'             TO COMM-TRANSID                        
                 MOVE 'JOBSSELT'         TO COMM-LINKPGM                        
                 GO TO T02-LINK-CONTINUE                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * otherwise, let the user know the signon completed                       
           MOVE 1                      TO UNEX-MSGL                             
           STRING 'WLSN - Signon complete.  '                                   
                  'Press ENTER to sign on again.' SCRCURSR                      
               DELIMITED BY SIZE     INTO UNEX-MSG                              
                                     WITH POINTER UNEX-MSGL                     
           SUBTRACT 1                FROM UNEX-MSGL                             
           EXEC CICS SEND                                                       
                     FROM(UNEX-MSG)                                             
                     LENGTH(UNEX-MSGL)                                          
                     CTLCHAR(WRTFKFST)                                          
                     ERASE DEFAULT                                              
           END-EXEC                                                             
                                                                                
           GO TO Z95-CICS-RETURN.                                               
                                                                                
       C50-WLSN-DISPLAY.                                                        
                                                                                
           MOVE 'WLSN'                 TO COMM-CURRRTN.                         
                                                                                
       C60-WLSN-DETAIL.                                                         
                                                                                
           MOVE LOW-VALUES             TO WLSN-PASSWORD-O.                      
                                                                                
       C70-WLSN-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
               MOVE UNEX-MSG           TO WLSN-MESSAGE-O                        
           END-IF.                                                              
                                                                                
       C80-WLSN-OUTPUT.                                                         
                                                                                
           MOVE CICS-APPLID            TO WLSN-SYSNAME-O.                       
           MOVE WS-WRKDATE             TO WLSN-SYSDATE-O.                       
           MOVE WS-WRKTIME             TO WLSN-SYSTIME-O.                       
           MOVE SPACES                 TO WLSN-NETNAME-O.                       
           EXEC CICS ASSIGN NETNAME(WLSN-NETNAME-O) END-EXEC.                   
           IF  WLSN-NETNAME-I NOT > SPACES                                      
               STRING 'Term: ' EIBTRMID                                         
                   DELIMITED BY SIZE INTO WLSN-NETNAME-O                        
           END-IF.                                                              
                                                                                
       C85-WLSN-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               MOVE EXCRED             TO WLSN-MESSAGE-C                        
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP      (WLSNMAP)                                 
                             DATAONLY                                           
                             FROM     (WLSNMAPO)                                
                             MAPSET   (WLSNMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP      (WLSNMAP)                                 
                             DATAONLY                                           
                             FROM     (WLSNMAPO)                                
                             MAPSET   (WLSNMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR   (EIBCPOSN)                                
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               MOVE EXCYELLW           TO WLSN-MESSAGE-C                        
               MOVE 'Enter your WinZone user id and WinZone password.  (        
      -             'case sensitive)'  TO WLSN-MESSAGE-O                        
               EXEC CICS SEND                                                   
                         MAP      (WLSNMAP)                                     
                         FROM     (WLSNMAPO)                                    
                         MAPSET   (WLSNMS)                                      
                         TERMINAL                                               
                         FREEKB                                                 
                         ERASE                                                  
               END-EXEC                                                         
               EXEC CICS SEND                                                   
                         FROM(WIN-LOGO-CORRECTION)                              
               END-EXEC                                                         
               PERFORM P50-CHECK-WARNING THRU P55-EXIT                          
               IF  SEND-WARNING                                                 
                   EXEC CICS SEND                                               
                             MAP      (WARNWIN)                                 
                             MAPONLY                                            
                             MAPSET   (WLSNMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                   END-EXEC                                                     
               END-IF                                                           
           END-IF.                                                              
                                                                                
       C90-WLSN-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA     (WLSNMAPI)                                        
           END-EXEC.                                                            
                                                                                
           GO TO Z80-WLSN-RETURN.                                               
                                                                                
      /*****************************************************************        
      *    PERFORMED ROUTINES                                          *        
      ******************************************************************        
                                                                                
       P10-GET-LOCKOUT-TIME.                                                    
           INITIALIZE IESLDGA-COMMAREA.                                         
           MOVE LENGTH OF IESLDGA-COMMAREA                                      
                                       TO LDGA-AREA-LENGTH.                     
           MOVE LDSO-NET-USERID        TO LDGA-USER-ID.                         
           MOVE '&(objectClass=person)(objectClass=user)'                       
                                       TO LDGA-SEARCH-FILTER.                   
           MOVE 12                     TO LDGA-ATTR-COUNT.                      
                                                                                
           MOVE 'lockoutTime'          TO LDGA-ATTR-NAME(1).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(1).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(1).                  
                                                                                
           MOVE 'badPwdCount'          TO LDGA-ATTR-NAME(2).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(2).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(2).                  
                                                                                
           MOVE 'mfid'                 TO LDGA-ATTR-NAME(3).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(3).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(3).                  
                                                                                
           MOVE 'ou'                   TO LDGA-ATTR-NAME(4).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(4).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(4).                  
                                                                                
           MOVE 'department'           TO LDGA-ATTR-NAME(5).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(5).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(5).                  
                                                                                
           MOVE 'company'              TO LDGA-ATTR-NAME(6).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(6).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(6).                  
                                                                                
           MOVE 'industryLetter'       TO LDGA-ATTR-NAME(7).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(7).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(7).                  
                                                                                
           MOVE 'WinOrg'               TO LDGA-ATTR-NAME(8).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(8).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(8).                  
                                                                                
           MOVE 'WinFunction'          TO LDGA-ATTR-NAME(9).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(9).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(9).                  
                                                                                
           MOVE 'WinDirector'          TO LDGA-ATTR-NAME(10).                   
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(10).                
           MOVE 1                      TO LDGA-VALUE-COUNT(10).                 
                                                                                
           MOVE 'WinPosition'          TO LDGA-ATTR-NAME(11).                   
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(11).                
           MOVE 1                      TO LDGA-VALUE-COUNT(11).                 
                                                                                
           MOVE 'distinguishedName'    TO LDGA-ATTR-NAME(12).                   
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(12).                
           MOVE 1                      TO LDGA-VALUE-COUNT(12).                 
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM  (IESLDGAC)                                        
                     COMMAREA (IESLDGA-COMMAREA)                                
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE 'IESLDGAC program failure.  See DAPL log.'                  
                                       TO WLSN-MESSAGE-O                        
               PERFORM Q200-UNEX-LOG THRU Q205-EXIT                             
               GO TO P15-EXIT                                                   
           END-IF.                                                              
                                                                                
           IF  LDGA-RET-CODE = ZERO                                             
               IF  LDGA-ATTR-VALUE(1 1) NOT = '0'                               
                   SET  USER-IS-LOCKED TO TRUE                                  
               ELSE                                                             
                   SET USER-NOT-LOCKED TO TRUE                                  
               END-IF                                                           
           ELSE                                                                 
               MOVE     -1             TO ERROR-SWITCH                          
                                          WLSN-USERID-L                         
               MOVE EXHREVRS           TO WLSN-MESSAGE-H                        
               MOVE SPACES             TO WLSN-MESSAGE-O                        
                                          LOGF-MESG                             
               MOVE 1                  TO IDX                                   
                                          CNT                                   
               STRING 'Unable to retrieve attr - rc='                           
                   DELIMITED BY SIZE INTO WLSN-MESSAGE-O                        
                                     WITH POINTER IDX                           
               STRING LDGA-USER-ID        DELIMITED BY SPACE                    
                      ' unable to retrieve attr (' EIBTRMID ') - rc='           
                   DELIMITED BY SIZE     INTO LOGF-MESG                         
                                     WITH POINTER CNT                           
               MOVE LDGA-RET-CODE      TO UNEX-RESP                             
               MOVE UNEX-RESP          TO UNEX-VARIABLE                         
               PERFORM Q203-LEFT-JUSTIFY                                        
               STRING UNEX-VARIABLE       DELIMITED BY SPACE                    
                      ', rsn='                                                  
                   DELIMITED BY SIZE INTO WLSN-MESSAGE-O                        
                                     WITH POINTER IDX                           
               STRING UNEX-VARIABLE       DELIMITED BY SPACE                    
                      ', rsn='                                                  
                   DELIMITED BY SIZE     INTO LOGF-MESG                         
                                     WITH POINTER CNT                           
               MOVE LDGA-LDAP-CODE     TO UNEX-RESP                             
               MOVE UNEX-RESP          TO UNEX-VARIABLE                         
               PERFORM Q203-LEFT-JUSTIFY                                        
               STRING UNEX-VARIABLE       DELIMITED BY SPACE                    
                                     INTO WLSN-MESSAGE-O                        
                                     WITH POINTER IDX                           
               STRING UNEX-VARIABLE       DELIMITED BY SPACE                    
                                         INTO LOGF-MESG                         
                                     WITH POINTER CNT                           
               PERFORM Q100-LOGIT    THRU Q199-EXIT                             
               GO TO P15-EXIT                                                   
           END-IF.                                                              
       P15-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P20-NOTIFY.                                                              
           IF  VUSER-KEY > SPACES                                               
               PERFORM Q26-READUP-VUSERID                                       
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                   MOVE 'X'            TO VUSER-LOCK-FLAG                       
                   PERFORM Q27-REWRITE-VUSERID                                  
               END-IF                                                           
           END-IF.                                                              
                                                                                
           INITIALIZE MAILWRTR-COMMAREA.                                        
           SET  MAIL-HEAD-PARMS        TO TRUE.                                 
           MOVE 'TECHHELP_CHERWELL'    TO MAIL-HEAD-TO-GRP.                     
           MOVE '(WLSNPGM) MAINFRAME LDAP SIGNON ACTIVE DIRECTORY LOCK'         
                                       TO MAIL-HEAD-SUBJECT.                    
           SET  MAIL-DELIVERY-ON       TO TRUE.                                 
           MOVE 'SY'                   TO MAIL-HEAD-SYSTEM.                     
           PERFORM P30-CALL-MAILWRTR THRU P35-EXIT.                             
                                                                                
           SET  MAIL-DIST-PARMS        TO TRUE.                                 
           MOVE 'MAINFRAME_PROGRAMMING' TO MAIL-DIST-TO-GRP.                    
           PERFORM P30-CALL-MAILWRTR THRU P35-EXIT.                             
                                                                                
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
                     FULLDATE(DSP-DATE) DATESEP('/')                            
                     TIME(DSP-TIME) TIMESEP(':')                                
           END-EXEC.                                                            
                                                                                
           SET  MAIL-BODY-PARMS        TO TRUE.                                 
           MOVE '<pre><br>'            TO MAIL-BODY-TEXT.                       
           PERFORM P30-CALL-MAILWRTR THRU P35-EXIT.                             
                                                                                
           STRING 'Date: '                DELIMITED BY SIZE                     
                  DSP-DATE                DELIMITED BY SPACE                    
                  ' Time: '               DELIMITED BY SIZE                     
                  DSP-TIME                DELIMITED BY SPACE                    
                  '<br>'                  DELIMITED BY SIZE                     
               INTO MAIL-BODY-TEXT.                                             
           PERFORM P30-CALL-MAILWRTR THRU P35-EXIT.                             
                                                                                
           STRING LOGF-MESG '<br>'                                              
               DELIMITED BY SIZE     INTO MAIL-BODY-TEXT.                       
           PERFORM P30-CALL-MAILWRTR THRU P35-EXIT.                             
                                                                                
           MOVE '</pre>'               TO MAIL-BODY-TEXT.                       
           PERFORM P30-CALL-MAILWRTR THRU P35-EXIT.                             
                                                                                
           SET  MAIL-MESG-COMPLETE     TO TRUE.                                 
           PERFORM P30-CALL-MAILWRTR THRU P35-EXIT.                             
       P25-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P30-CALL-MAILWRTR.                                                       
           EXEC CICS LINK                                                       
                     PROGRAM (MAILWRTR)                                         
                     COMMAREA(MAILWRTR-COMMAREA)                                
           END-EXEC.                                                            
           MOVE SPACES                 TO MAIL-PARMS.                           
       P35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P40-PARSE-OUT-UNIT-VALUE.                                                
           MOVE LDGA-ATTR-VALUE(4 1)   TO VUSER-WIN-UNIT.                       
           INSPECT VUSER-WIN-UNIT         CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE.                
           IF VUSER-WIN-UNIT <= SPACES                                          
               UNSTRING LDGA-ATTR-VALUE(12 1) DELIMITED BY ','                  
                   INTO WORD1 WORD2 WORD3 WORD4 WORD5                           
               IF  'OU=' = WORD2(1:3) AND WORD3(1:3)                            
                   MOVE WORD2(4:)      TO VUSER-WIN-UNIT                        
                   INSPECT VUSER-WIN-UNIT CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               END-IF                                                           
           END-IF.                                                              
       P45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P50-CHECK-WARNING.                                                       
           SET  WARNING-SENT           TO TRUE.                                 
                                                                                
           IF  CICS-CCYYMMDD < 20200125                                         
               SET  SEND-WARNING       TO TRUE                                  
           END-IF.                                                              
       P55-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    MORE PERFORMED ROUTINES                                     *        
      ******************************************************************        
                                                                                
       Q00-FORMAT-DATE-AND-TIME.                                                
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
                     FULLDATE (WS-WRKDATE)          DATESEP                     
                     TIME     (WS-WRKTIME)          TIMESEP                     
           END-EXEC.                                                            
       Q02-REFORMAT-TIME.                                                       
           IF  WS-HRSTIME < 12                                                  
               MOVE ' AM'              TO WS-XXXTIME                            
               IF  WS-HRSTIME = 00                                              
                   MOVE 12             TO WS-HRSTIME                            
               END-IF                                                           
           ELSE                                                                 
               MOVE ' PM'              TO WS-XXXTIME                            
               IF  WS-HRSTIME > 12                                              
                   SUBTRACT 12       FROM WS-HRSTIME                            
               END-IF                                                           
           END-IF.                                                              
       Q05-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q20-INITKEY-VUSERID.                                                     
           IF  ADDRESS OF VUSERID-RECORD = NULL                                 
               EXEC CICS GETMAIN                                                
                         SET      (ADDRESS OF VUSERID-RECORD)                   
                         LENGTH   (LENGTH OF VUSERID-RECORD)                    
               END-EXEC                                                         
           END-IF.                                                              
           MOVE SPACES                 TO VUSER-KEY.                            
       Q26-READEQ-VUSERID.                                                      
           EXEC CICS READ                                                       
                     DATASET  (VUSERID)                                         
                     INTO     (VUSERID-RECORD)                                  
                     RIDFLD   (VUSER-KEY)                                       
                     EQUAL                                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q26-READUP-VUSERID.                                                      
           EXEC CICS READ                                                       
                     DATASET  (VUSERID)                                         
                     INTO     (VUSERID-RECORD)                                  
                     RIDFLD   (VUSER-KEY)                                       
                     EQUAL                                                      
                     UPDATE                                                     
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q27-REWRITE-VUSERID.                                                     
           IF  VUSERID-RECORD(144:) = LOW-VALUES                                
           OR  VUSERID-RECORD(165:) = LOW-VALUES                                
               MOVE SPACES             TO VUSERID-RECORD(144:)                  
           END-IF                                                               
           EXEC CICS REWRITE                                                    
                     DATASET  (VUSERID)                                         
                     FROM     (VUSERID-RECORD)                                  
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q28-UNLOCK-VUSERID.                                                      
           EXEC CICS UNLOCK                                                     
                     DATASET  (VUSERID)                                         
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q29-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q40-INITKEY-IESLDUM.                                                     
           IF  ADDRESS OF IESLDUM-RECORD = NULL                                 
               EXEC CICS GETMAIN                                                
                         SET      (ADDRESS OF IESLDUM-RECORD)                   
                         LENGTH   (LENGTH OF IESLDUM-RECORD)                    
               END-EXEC                                                         
           END-IF.                                                              
           MOVE LOW-VALUES             TO LDUM-KEY.                             
           SET  LDUM-USRMAP-RECORD     TO TRUE.                                 
       Q46-READEQ-IESLDUM.                                                      
           EXEC CICS READ                                                       
                     DATASET  (IESLDUM)                                         
                     INTO     (IESLDUM-RECORD)                                  
                     RIDFLD   (LDUM-KEY)                                        
                     EQUAL                                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q49-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q90-LOG-SIGNON-ERROR.                                                    
           MOVE SPACES                 TO LOGF-MESG.                            
           MOVE 1                      TO CNT.                                  
           STRING LDSO-VSE-USERID                                               
                  ' signon failed (' EIBTRMID ') '                              
               DELIMITED BY SIZE     INTO LOGF-MESG                             
                                     WITH POINTER CNT.                          
           MOVE '-'                    TO UNEX-PAD.                             
           MOVE 'rc'                   TO UNEX-RCODE.                           
           MOVE RESPX-RESP             TO UNEX-RESP.                            
           PERFORM Q95-LOG-APPEND-RESP.                                         
           MOVE ','                    TO UNEX-PAD.                             
           MOVE 'rsn'                  TO UNEX-RCODE.                           
           MOVE RESPX-RESP2            TO UNEX-RESP.                            
           PERFORM Q95-LOG-APPEND-RESP.                                         
           MOVE 'resp'                 TO UNEX-RCODE.                           
           MOVE EIBRESP                TO UNEX-RESP.                            
           PERFORM Q95-LOG-APPEND-RESP.                                         
           MOVE 'resp2'                TO UNEX-RCODE.                           
           MOVE EIBRESP2               TO UNEX-RESP.                            
           PERFORM Q95-LOG-APPEND-RESP.                                         
           PERFORM Q100-LOGIT        THRU Q199-EXIT.                            
           MOVE SPACE                  TO UNEX-PAD.                             
           GO TO Q99-EXIT.                                                      
       Q95-LOG-APPEND-RESP.                                                     
           MOVE UNEX-RESP              TO UNEX-VARIABLE.                        
           PERFORM Q203-LEFT-JUSTIFY.                                           
           STRING UNEX-PAD ' '            DELIMITED BY SIZE                     
                  UNEX-RCODE '='          DELIMITED BY SPACE                    
                  UNEX-VARIABLE           DELIMITED BY SPACE                    
                                     INTO LOGF-MESG                             
                                     WITH POINTER CNT.                          
       Q99-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    UNEXPECTED ERRORS AND DEBUG LOGGING                         *        
      ******************************************************************        
       COPY LOGGINGP.                                                           
                                                                                
      /*****************************************************************        
      *    PROGRAM EXIT TO 2ND-LEVEL PGM OR RETURN TO 1ST-LEVEL PGM    *        
      ******************************************************************        
       COPY COMMLINK.                                                           
      * the above copybook ends with a return to cics                           
                                                                                
      /*****************************************************************        
      *    PROGRAM ERRORS (UNEXPECTED)                                 *        
      ******************************************************************        
       COPY UNEXERRP.                                                           
      * the above copybook ends with a return to cics                           
                                                                                
      /*****************************************************************        
      *    PROGRAM EXIT ROUTINES (NORMAL)                              *        
      ******************************************************************        
                                                                                
       Z80-WLSN-RETURN.                                                         
                                                                                
           COPY COMMRETN.                                                       
      * the above copybook is a psuedo-conversational return to cics            
                                                                                
       Z90-WLSN-TERMINATION.                                                    
                                                                                
           EXEC CICS SET TERMINAL(EIBTRMID)                                     
                     UCTRAN                                                     
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
      * use the following if you don't want to pass back any data               
           COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.                  
                                                                                
      * the above copybook ends with a return to cics and GOBACK                
                                                                                
