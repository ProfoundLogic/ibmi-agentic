     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE SA008R - USER'S SUBCRIBER/GROUP/EMPLOYER AUTHORITY
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL:                             *
     H*                 1. IN MASTER FILE 'WORK WITH' SCREENS, PRE-   *
     H*                    DETERMINE WHAT MASTER INFORMATION THE USER *
     H*                    IS ALLOWED ACCESS TO.                      *
     H*                 2. IN INTERACTIVE PROGRAMS, INFORM THE CALLING*
     H*                    PROGRAM THAT THE USER DOES NOT HAVE ACCESS *
     H*                    TO AN ENTERED VALUE.                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 01/29/99  PROGRAMMER: C:33  PROJECT: 48WOM/48MCB    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/02/15 C:33  34843                                          *
     H*               -When checking specific criteria, if no records *
     H*                exist in SMAZP, user is authorized.            *
     H*                                                               *
     H* 01/03/14 C:62  Support: WC114 84581                           *
     H*                No need to do further checks if no group       *
     H*                authority setup                                *
     H*                                                               *
     H* 04/12/00 C:33  PROJECT: 48WUE  WC048/22520                    *
     H*                CORRECT 'FROM' RESTRICTIONS                    *
     H*                                                               *
     H* 02/17/00 C:61  PROJECT 48WUE                                  *
     H*                Enable user to be restricted FROM              *
     H*                specified groups / employers.                  *
     H*****************************************************************
     FSMAZL     IF   E           K DISK    USROPN
     F                                     RENAME(PAZ01:PAZ01W)
     FSMAZL1    IF   E           K DISK    USROPN
     F                                     RENAME(PAZ01:PAZ01M)

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

      //***  Begin Add  ***  06/02/15  ********************************
     DIsRecordExists   pr             1n
      //***  End   Add  ***  06/02/15  ********************************

     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  DATA AUTHORIZATION FILE
     C*
     C     AZKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@USR
     C*
     C     AZKY1W        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@USR
     C                   KFLD                    @@FND
     C*
     C     AZKY2W        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@USR
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C*
     C     AZKY1M        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@USR
     C                   KFLD                    @@SUB#
     C*
     C     AZKY2M        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@USR
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C*
     C     AZKY3M        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@USR
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    *IN60         @@RTCD
     C     *LIKE         DEFINE    AZCO#         @@CO#
     C     *LIKE         DEFINE    AZUSR         @@USR
     C     *LIKE         DEFINE    AZAPPL        @@APPL
     C     *LIKE         DEFINE    AZAUTH        @@AUTH
     C     *LIKE         DEFINE    AZSUB#        @@SUB#
     C     *LIKE         DEFINE    AZFND         @@FND
     C     *LIKE         DEFINE    AZFEMP        @@EMP#
     C     *LIKE         DEFINE    AZSUB#        XXSUB#
     C     *LIKE         DEFINE    AZFND         XXFND
     C     *LIKE         DEFINE    AZFEMP        XXEMP#
     C     *LIKE         DEFINE    AZTEMP        XXTEMP
     C     *LIKE         DEFINE    AZFND         XZFND
     C     *LIKE         DEFINE    AZAUTH        XXAUTH
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@USR
     C                   PARM                    @@APPL
     C                   PARM                    @@RTCD
     C                   PARM                    @@AUTH
     C                   PARM                    @@FND
     C                   PARM                    @@EMP#
     C                   PARM                    @@SUB#
     C*
     C* NOTE: -@@CO#/@@USR/@@APPL/@@RTCD MUST BE PASSED IN.
     C*       -IF CALLED FROM A MANAGED CARE APPLICATION, @@APPL NEEDS
     C*        TO BE 'M'.
     C*       -IF CALLED FROM A WORKER'S COMP APPLICATION, @@APPL NEEDS
     C*        TO BE 'W'.
     C*       -IF @@SUB#, @@FND, AND @@EMP# ARE NOT PASSED IN OR ARE
     C*        ZEROS, @@RTCD WILL BE RETURNED *OFF IF THERE ARE NO HITS
     C*        ON SMAZP, OTHERWISE *ON
     C*   -MANAGED CAREAPPLICATIONS
     C*       -IF ONLY @@SUB# IS PASSED IN, @@RTCD WILL BE RETURNED
     C*        *OFF IF THE USER HAS AUTHORIZATION TO THE SUBCRIBER
     C*        PASSED IN, OTHERWISE *ON
     C*       -IF @@SUB# AND @@FND ARE PASSED IN, @@RTCD WILL BE
     C*        RETURNED *OFF IF THE USER HAS AUTHORIZATION TO THE
     C*        SUBSCRIBER AND GROUP PASSED IN, OTHERWISE *ON.  (IF
     C*        PASSING IN A GROUP, THE SUBSCRIBER MUST ALSO BE PASSED
     C*        IN.)
     C*       -IF @@SUB#, @@FND AND @@EMP# ARE PASSED IN, @@RTCD WILL
     C*        BE RETURNED *OFF IF THE USER HAS AUTHORIZATION TO THE
     C*        SUBSCRIBER, GROUP AND EMPLOYER PASSED IN, OTHERWISE *ON.
     C*        (IF PASSING IN A EMPLOYER, THE GROUP AND SUBSCRIBER MUST
     C*        ALSO BE PASSED IN.)
     C*   -WORKER'S COMP APPLICATIONS
     C*       -IF ONLY @@FND IS PASSED IN, @@RTCD WILL BE RETURNED *OFF
     C*        IF THE USER HAS AUTHORIZATION TO THE GROUP PASSED IN,
     C*        OTHERWISE *ON
     C*       -IF @@FND AND @@EMP# ARE PASSED IN, @@RTCD WILL BE
     C*        RETURNED *OFF IF THE USER HAS AUTHORIZATION TO THE GROUP
     C*        AND EMPLOYER PASSED IN, OTHERWISE *ON (IF PASSING IN AN
     C*        EMPLOYER THE GROUP MUST ALSO BE PASSED IN.)
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   OPEN      SMAZL
     C     @@APPL        WHENEQ    'M'
     C                   OPEN      SMAZL1
     C                   ENDSL
     C*
     C     Q1PRMS        IFGE      5
     C                   CLEAR                   @@AUTH
     C                   ENDIF                                                  Q1PRMS>=5
     C*
     C                   CLEAR                   XXFND
     C                   CLEAR                   XXEMP#
     C                   CLEAR                   XXSUB#
     C*
     C     Q1PRMS        IFGE      6
     C                   Z-ADD     @@FND         XXFND
     C                   ENDIF                                                  Q1PRMS>=6
     C*
     C     Q1PRMS        IFGE      7
     C                   Z-ADD     @@EMP#        XXEMP#
     C                   ENDIF                                                  Q1PRMS>=7
     C*
     C     Q1PRMS        IFGE      8
     C                   Z-ADD     @@SUB#        XXSUB#
     C                   ENDIF                                                  Q1PRMS>=8
     C*
      ***** BEG ADD ******* 02/17/00 **********************************
      *
      * Check to see if restricted FROM sub/grp/emp instead of TO
      *
     C                   SETOFF                                       41
      *
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKY          CHAIN     PAZ01W                             60
     C     @@APPL        WHENEQ    'M'
     C     AZKY          CHAIN     PAZ01M                             60
     C                   ENDSL
      *
     C     *IN60         IFEQ      *OFF
     C*** Begin Chg *** 01/03/14 ****************************************
     C*****AZAUTH        ANDEQ     'N'
     C     AZAUTH        IFEQ      'N'
     C*** End   Chg *** 01/03/14 ****************************************
     C                   SETON                                        41
     C                   ENDIF
      *
      ***** END ADD ******* 02/17/00 **********************************
     C                   MOVE      *OFF          @@RTCD
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     XXFND         ANDEQ     *ZERO
     C     @@APPL        OREQ      'M'
     C     XXSUB#        ANDEQ     *ZERO
     C                   EXSR      CHCKAZ
      ***** BEG ADD ******* 02/17/00 **********************************
     C                   SETOFF                                       41
      ***** END ADD ******* 02/17/00 **********************************
      //***  Begin Add  ***  06/02/15  ********************************
      /Free
       When not IsRecordExists;
          @@rtCd = *off;
          AZauth = 'A';
      /End-free
      //***  End   Add  ***  06/02/15  ********************************
     C     @@APPL        WHENEQ    'M'
     C     XXFND         ANDEQ     *ZERO
     C                   EXSR      CHCKSB
     C     XXEMP#        WHENEQ    *ZERO
     C                   EXSR      CHCKFN
     C     XXEMP#        WHENNE    *ZERO
     C                   EXSR      CHCKEM
     C                   ENDSL
     C*
     C     @@RTCD        IFEQ      *OFF
     C     Q1PRMS        ANDGE     5
     C                   MOVE      AZAUTH        @@AUTH
     C                   ENDIF                                                  @@RTCD=*OFF
     C*
      ***** BEG ADD ******* 02/17/00 **********************************
      *
      *  Reverse return values if user restricted FROM sub/grp/emp
      *
     C     *IN41         IFEQ      *ON
      *
     C     @@RTCD        IFEQ      *ON
     C                   MOVE      *OFF          @@RTCD
     C*
     C     Q1PRMS        IFGE      5
     C                   MOVE      'A'           @@AUTH
     C                   ENDIF                                                  Q1PRMS>=5
      *
     C                   ELSE
      *
     C                   MOVE      *ON           @@RTCD
      *
     C     Q1PRMS        IFGE      5
     C                   MOVE      *BLANKS       @@AUTH
     C                   ENDIF                                                  Q1PRMS>=5
      *
     C                   ENDIF                                                  RTCD
      *
     C                   ENDIF                                                  *IN41
     C*** Begin Add *** 01/03/14 ****************************************
     C                   ENDIF                                                  *IN60
     C*** End   Add *** 01/03/14 ****************************************
      *
      ***** END ADD ******* 02/17/00 **********************************
     C*
     C                   MOVE      *ON           *INLR
     C*_______________________________________________________________
     C*
     C*================================================================
     C*  CHCKAZ - Check SMAZP for any active user restriction(s)
     C*================================================================
     C*
     C     CHCKAZ        BEGSR
      //***  Begin Add  ***  06/02/15  ********************************
      // This code wil be the only code for this routine, the rest of the
      //  routine was moved to the IsRecordExists procedure
      /Free
       @@rtCd = *off;
       If IsRecordExists;
          @@rtCd = *on;
       Endif;
       Leavesr;
      /End-free
      //***  End   Add  ***  06/02/15  ********************************
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKY          CHAIN     PAZ01W                             60
     C     @@APPL        WHENEQ    'M'
     C     AZKY          CHAIN     PAZ01M                             60
     C                   ENDSL
     C*
     C     *IN60         DOWEQ     *OFF
     C     AZSTAT        ANDEQ     'I'
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKY          READE     PAZ01W                                 60
     C     @@APPL        WHENEQ    'M'
     C     AZKY          READE     PAZ01M                                 60
     C                   ENDSL
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           @@RTCD
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHCKAZ
     C*
     C*================================================================
     C*  CHCKEM - Check SMAZP to determine user's authority to @@EMP#
     C*================================================================
     C*
     C     CHCKEM        BEGSR
     C*
     C                   EXSR      CHCKFN
     C*
     C*
     C     @@RTCD        IFEQ      *OFF
     C     XXTEMP        ANDNE     *ZERO
      ***** BEG ADD ******* 02/17/00 **********************************
     C     *IN41         OREQ      *ON
      ***** END ADD ******* 02/17/00 **********************************
     C***  BEGIN ADD  ***  04/12/00  *********************************
     C     @@RTCD        ANDEQ     *ON
     C***  BEGIN ADD  ***  04/12/00  *********************************
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKY2W        SETGT     PAZ01W
     C     AZKY1W        READPE    PAZ01W                                 60
     C     @@APPL        WHENEQ    'M'
     C     AZKY3M        SETGT     PAZ01M
     C     AZKY2M        READPE    PAZ01M                                 60
     C                   ENDSL
     C*
     C     *IN60         DOWEQ     *OFF
     C     AZSTAT        ANDEQ     'I'
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKY1W        READPE    PAZ01W                                 60
     C     @@APPL        WHENEQ    'M'
     C     AZKY2M        READPE    PAZ01M                                 60
     C                   ENDSL
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     @@EMP#        IFLT      AZFEMP
     C     @@EMP#        ORGT      AZTEMP
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  AZFEMP<@@EMP#<AZTEMP
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      *IN60         @@RTCD
     C*
     C                   ENDIF                                                  @@RTCD=*OFF
     C*
     C                   ENDSR                                                  CHCKEM
     C*
     C*================================================================
     C*  CHCKFN - Check SMAZP to determine user's authority to @@FND
     C*================================================================
     C*
     C     CHCKFN        BEGSR
     C*
     C                   MOVE      *OFF          @@RTCD
     C                   MOVE      *HIVAL        XZFND
     C*
     C                   SELECT
     C*
     C     @@APPL        WHENEQ    'W'
     C     AZKY1W        CHAIN     PAZ01W                             60
     C     @@APPL        WHENEQ    'M'
     C                   EXSR      CHCKSB
     C     AZKY2M        CHAIN     PAZ01M                             60
     C                   ENDSL
     C*
     C     @@RTCD        IFEQ      *OFF
     C     XZFND         ANDNE     *ZERO
      ***** BEG ADD ******* 02/17/00 **********************************
     C     *IN41         OREQ      *ON
      ***** END ADD ******* 02/17/00 **********************************
     C***  BEGIN ADD  ***  04/12/00  *********************************
     C     @@RTCD        ANDEQ     *ON
     C***  BEGIN ADD  ***  04/12/00  *********************************
     C*
     C                   CLEAR                   XXAUTH
     C                   MOVE      *ON           @@RTCD
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AZSTAT        IFNE      'I'
     C*
      ***** BEG ADD ******* 02/17/00 **********************************
     C     *IN41         IFEQ      *OFF
     C     AZTEMP        OREQ      *ZERO
      ***** END ADD ******* 02/17/00 **********************************
     C                   MOVE      *OFF          @@RTCD
      ***** BEG ADD ******* 02/17/00 **********************************
     C                   ENDIF                                                  *IN41
      ***** END ADD ******* 02/17/00 **********************************
     C                   Z-ADD     AZTEMP        XXTEMP
     C*
     C     AZAUTH        IFEQ      'I'
     C                   MOVE      'I'           XXAUTH
     C                   ENDIF                                                  AZAUTH=I
     C*
     C                   ENDIF                                                  AZSTAT<>I
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKY1W        READE     PAZ01W                                 60
     C     @@APPL        WHENEQ    'M'
     C     AZKY2M        READE     PAZ01M                                 60
     C                   ENDSL
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     XXAUTH        IFEQ      'I'
     C                   MOVE      'I'           AZAUTH
     C                   ELSE
     C                   MOVE      'A'           AZAUTH
     C                   ENDIF                                                  XXAUTH=I
     C*
     C                   ENDIF                                                  @@RTCD=*OFF
     C*
     C                   ENDSR                                                  CHCKFN
     C*
     C*================================================================
     C*  CHCKSB - Check SMAZP to determine user's authority to @@SUB#
     C*================================================================
     C*
     C     CHCKSB        BEGSR
     C*
     C     AZKY1M        CHAIN     PAZ01M                             60
     C*
     C                   CLEAR                   XXAUTH
     C                   MOVE      *ON           @@RTCD
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AZSTAT        IFNE      'I'
     C*
      ***** BEG ADD ******* 02/17/00 **********************************
     C     *IN41         IFEQ      *OFF
     C     AZFND         OREQ      *ZERO
      ***** END ADD ******* 02/17/00 **********************************
     C                   MOVE      *OFF          @@RTCD
      ***** BEG ADD ******* 02/17/00 **********************************
     C                   ENDIF                                                  *IN41
      ***** END ADD ******* 02/17/00 **********************************
     C                   Z-ADD     AZFND         XZFND
     C*
     C     AZAUTH        IFEQ      'I'
     C                   MOVE      'I'           XXAUTH
     C                   ENDIF                                                  AZAUTH=I
     C*
     C                   ENDIF                                                  AZSTAT<>I
     C*
     C     AZKY1M        READE     PAZ01M                                 60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     XXAUTH        IFEQ      'I'
     C                   MOVE      'I'           AZAUTH
     C                   ELSE
     C                   MOVE      'A'           AZAUTH
     C                   ENDIF                                                  XXAUTH=I
     C*
     C                   ENDSR                                                  CHCKSB

      //***  Begin Add  ***  06/02/15  ********************************
      //===============================================================
     PIsRecordExists   b
     DIsRecordExists   pi             1n

      /Free
       Select;
       When @@appl = 'W';
          Setll azky paz01w;
       When @@appl = 'M';
          Setll azky paz01m;
       Endsl;

       Dou %eof;
          Select;
          When @@appl = 'W';
             Reade azky paz01w;
          When @@appl = 'M';
             Reade azky paz01m;
          Endsl;
          If AZstat <> 'I';
             Leave;
          Endif;
       Enddo;

       If %eof;
          Return *off;
       Endif;
       Return *on;
      /End-free
     PIsRecordExists   e
      //***  End   Add  ***  06/02/15  ********************************
