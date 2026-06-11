     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE W8370R - POLICY DATA EXPORT PROMPT
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:  W8370C - SUBMIT                                    *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/30/18 C:136 Project: 150WCW                                *
     H*                Add Accidental Fund processing for Sunz        *
     H*                                                               *
     H* 07/26/17 C:291 Project: 150WAT                                *
     H*                Add Export File Path Name on the screen        *
     H*                                                               *
     H* 06/26/09 C:198 PROJECT: WC1694                                *
     H*                CHANGED POLICY EXPORT PROMPT                   *
     H*****************************************************************
     FW8370F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMVPP     IF   E             DISK
     FWMVPL     IF   E           K DISK
     F                                     RENAME(PVP01:PVP02)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

      *** beg add ***** 04/30/18 ***************************
     d isAFMode        s               n
     d afPthNameDs     ds                  Dtaara('AFPOLEXPTH')
     d  strAfPathName               256
     d pgmLDA          ds                  Dtaara(*lda)
     d  afMode                       15
      *** end add ***** 04/30/18 ***************************
      *** beg add ***** 07/26/17 ******************************************
     d pathNameDta     ds                  dtaara('POLEXPPATH')
     d  lastPathName           1    256

     d statds          DS                  BASED(p_statds)
     d  st_mode                      10U 0
     d  st_ino                       10U 0
     d  st_nlink                      5U 0
     d  st_pad                        2A
     d  st_uid                       10U 0
     d  st_gid                       10U 0
     d  st_size                      10I 0
     d  st_atime                     10I 0
     d  st_mtime                     10I 0
     d  st_ctime                     10I 0
     d  st_dev                       10U 0
     d  st_blksize                   10U 0
     d  st_allocsize                 10U 0
     d  st_objtype                   10A
     d  st_codepage                   5U 0
     d  st_reserved1                 62A
     d  st_ino_gen_id                10U 0


     d p_statds        S               *
     d myStat          S                   like(statds)

     d**********************************************************************
     d* Prototypes
     d**********************************************************************
     D* Check for Document Authorities
     D access          pr            10I 0 ExtProc('access')
     D   path                          *   Value Options(*string)
     D   aMode                       10I 0 Value

     d* Retrieve Document Stats
     d stat            PR            10I 0 ExtProc('stat')
     d   path                          *   value options(*string)
     d   buf                           *   value

     d/copy SPrCompany
     d/copy wPrUsrPrms
     d/copy sPrGenPop

     d************************************************************
     d* Work Fields
     d************************************************************
     d PathNameErr     s              1n
     d PathNmLen       s              3  0
     d BlankPos        s              3  0
     d fullPathName    s            258
     d xxDirectory     s            258
     d lastSlash       s              3  0
     d X               s              3  0

     d docExists       c                   0                                    Exists
     d docAccess       c                   4                                    Access
      *** end add ***** 07/26/17 ******************************************
     D*
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  VENDOR FILE
     C     VPKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCVEND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  GET THE USER PARAMETER FILE
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCFRDT
     C                   MOVEL     SRDTO         SCTODT
     C*
     C*  GET THE COMPANY RECORD
     C     STRT01        TAG
     C                   MOVE      UPCO#         XXCO#             3 0
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      QQPRDS        SCPRDS
      *
      * Center Screen Prompt in display field
      *
     C                   MOVEL(P)  SCPRDS        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
      *** beg chg ***** 04/30/18 **************************
     C*** BEG ADD *** 06/26/09 *********************************
     C***  QQCALL        IFEQ      'G'
     C***                SETON                                        24
     C***                ENDIF                                                  QQCALL = 'G'
     C*** BEG ADD *** 06/26/09 *********************************

      /Free
       scDateHdg  = 'Export activity date range';
       posContTop = 250;
       If qqCall = 'V';
          qqCall = 'F';
          in pgmLDA;
          afMode = 'Validate';
          out pgmLDA;
       Endif;
       Select;
       When QQcall  = 'F';
          posContTop = 225;
          scDateHdg = 'Policy date range';
          isAfMode = *On;
          *In24 = *On;
       When QQcall  = 'G';
          posContTop = 225;
          *In24 = *On;
       Endsl;
      /End-Free
      *** end chg ***** 04/30/18 **************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*   CONVERT THE FROM DATE
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C*   CONVERT THE TO DATE
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT

      *** beg add ***** 07/26/17 ******************************************
      /Free
       //*** beg chg ***** 04/30/18 **************************
       //// If scPathName = *blanks;
       //// In(e) pathNameDta;
       //// If %error;
       //// // Do nothing
       //// Endif;
       //// Clear scPathName;
       //// scPathName = %trim(lastPathName);
       //// Endif;
        Select;
        When isAfMode;
           In(e) afPthNameDs;
           If %Error;
            // Do nothing
           Else;
              Clear scPathName;
              scPathName = %trim(strAfPathName);
           Endif;
           posPathLbl = 60;
           posPathNam = 75;
        Other;
           In(e) pathNameDta;
           If %Error;
            // Do nothing
           Else;
             Clear scPathName;
             scPathName = %trim(lastPathName);
           Endif;
           posPathLbl = 35;
           posPathNam = 50;
        EndSl;
       //*** end chg ***** 04/30/18 **************************
      /End-Free

      *** end add ***** 07/26/17 ******************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC
     C*
     C*   F10 = SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCFRDT
     C                   ENDIF                                                  SRERR IFEQ
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCTODT
     C                   ENDIF                                                  SRERR IFEQ
     C*
     C     HFLD          IFEQ      'SCVEND'
     C*
     C                   CALL      'W0003R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C     *INKL         ANDEQ     *OFF
     C*
     C     ##RRN         CHAIN     PVP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      VPVDR#        SCVEND
     C                   SETON                                        31
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ELSE
     C                   SETOFF                                       31
     C                   ENDIF                                                  ##RRN<>0
     C*
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ IFEQ
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      Q2RECF        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN10 IFEQ
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF                                                  *IN60 IFEQ
     C                   MOVE      UPCO#         XXCO#
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM IFEQ
     C*
     C                   ENDIF                                                  *IN11 IFEQ
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C* VALIDATE VENDOR NUMBER IF ENTERED.
     C*
     C     SCVEND        IFNE      *ZERO
     C*
     C     VPKY          CHAIN     PVP02                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SCVEND IFNE
     C*
     C*  VALIDATE FROM DATE
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE TO DATE
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE FROM DATE LESS THAN TO DATE
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0


     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF

      *** beg add ***** 04/30/18 **************************
     c                   If        isAfMode
     c                   Z-Add     ScValDt       srdti
     c                   Call      'SRCVTDAT'    datChk
     c                   Movel     srdto         scValDt
     c                   If        srErr = *On or scValDt = 0
     c                   Eval      erValDate = *On
     c                   Goto      strt
     c                   Else
     c                   Eval      erValDate = *Off
     c                   Endif
     c                   Endif
      *** end add ***** 04/30/18 **************************

      *** beg add ***** 07/26/17 ******************************************
      /Free
       Clear fullPathName;
       fullPathName = %Xlate(lo:up:scPathName);
       PathNmLen = %scan('.TXT':fullPathName);
       PathNmLen = PathNmLen + 4;                    // Add 4 pos to find blank position after .txt
       BlankPos = 258 - PathNmLen;                   // Calc No of positions to be repl w/ blnk
       fullPathName = %replace(' ':fullPathName:PathNmLen+1:BlankPos);
        //--------------------------
        // Path Name validations
        //--------------------------
        PathNameErr = *Off;
        errPathNam = *Off;
        msgPathNam = ' ';
        // Path is Required
        If scPathName = *Blanks;
           errPathNam = *On;
           msgPathNam = 'This field can not be blank.';
           PathNameErr = *On;
        Else;
           // Must start with a '/'
           If %subst(scPathName:1:1) <> '/';
              errPathNam = *On;
              msgPathNam = 'The export file path name must start with a ''/''.';
              PathNameErr = *On;
           Else;
           // Must be a .TXT file
              If %scan('.TXT':fullPathName) = 0;
                 errPathNam = *On;
                 msgPathNam = 'The file extension must be .TXT.';
                 PathNameErr = *On;
              Else;
                 //Directory must exist.
                 Clear X;
                 dou X = 0;
                     X = %scan('/':fullPathName:x+1);
                     If X <> 0;
                        lastSlash = X;
                     Endif;
                 Enddo;
                 xxDirectory = %subst(fullPathName:1:lastSlash-1);
                 If Stat(%Trim(xxDirectory):%Addr(myStat)) < 0;
                    errPathNam = *On;
                    msgPathNam = 'Invalid Document Path.';
                    PathNameErr = *On;
                 Else;
                    If Access(%Trim(xxDirectory):docAccess) < 0;
                      errPathNam = *On;
                      msgPathNam='Not Authorized to Access this document.';
                      PathNameErr = *On;
                    EndIf;
                 Endif;
              Endif;
           Endif;
        Endif;
       // Load position 57 and 58 with '&&' and send it to W8371C to
       // differenciate the call from the Menu verses job Scheduler.
       %Subst(fullPathName:257:2) = '&&';
      /End-Free
     C                   If        errPathNam = *On
     C                   Goto      STRT
     C                   Endif
      *** end add ***** 07/26/17 ******************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      SCVEND        QQVEND            5
      *** beg add ***** 04/30/18 **************************
     C                   MOVE      SCVALDT       QQVALDT           8
      *** end add ***** 04/30/18 **************************

     C*
     C                   CALL      'W8370C'
     C                   PARM                    QQCO#
     C                   PARM                    QQVEND
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C*** BEG ADD *** 06/26/09 ************************************
     C                   PARM                    QQCALL
     C*** END ADD *** 06/26/09 ************************************
     C*** Beg Add *** 07/26/17 ************************************
     C                   Parm                    fullPathName
     C*** End Add *** 07/26/17 ************************************
     C*** beg add ***** 04/30/18 **********************************
     C                   Parm                    qqValDt
     C*** end add ***** 04/30/18 **********************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
