     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*TITLE W6502R - RETURN PREMIUM CHECK PRINT PROMPT               *
     H*                                                               *
     H* CREATED BY:  C:183  12/26/07   PROJECT# 81WZB                 *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     A*  02/15/18  C:062 Support: 64410(129)                          *
     A*                  Submit if spool file.                        *
     H*                                                               *
     A*  08/03/17  C:062 Project: SY395                               *
     A*                  Adding scriptura prcoessing.                 *
     H*                                                               *
     A*  03/25/09  C:220 SUP#: 66853                                  *
     A*                  Independent Cash and Return deposit should   *
     A*                  display only if client has purchased this    *
     A*                  module.                                      *
     A*                                                               *
     H*  06/26/08 C:136 Project: 81WZB                                *
     H*                 Allow secondary sort with CU as primary sort. *
     H*                                                               *
     H*  03/27/08 C:136 PROJECT: 81WZB                                *
     H*                 CALL WA023R1 WITH THE CORRECT PARMS           *
     H*****************************************************************
      *
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW6502F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
     FWAR4L12   UF   E           K DISK
     F
     FSMCOL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FSWCPF     IF   E           K DISK
      *
     D MG              S             60    DIM(3) CTDATA PERRCD(1)
     D APPL            S              2    INZ('RP')
     D CALL            S              1    INZ('C')
     D @PREMCHKDT      S              8  0
     D @@PREMCHKDT     S              8
     D SVFND           S                   LIKE(R4FND)
     D SVFYR           S                   LIKE(R4FYR)
     D SVBTYP          S                   LIKE(R4BTYP)
     D QQPRDS          S             40
     D SYS_DATE        S               D   DATFMT(*USA)
     D*** Begin Add *** 08/03/17 ***************************************
     D isUseScript     S              1n   inz
     d processType     c                   const('Return Premium Checks')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1

     D svLda           ds          1024    DtaAra(*lda)
     D xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)

     d* Output Control
     d OutputControl   pr

     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrCompany
     d/copy wPrUsrPrms
     d/copy sPrLetter
     D/copy sPrXML
     D*** End   Add *** 08/03/17 ***************************************
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DLYA                   1     80
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
      *
     C                   MOVE      *OFF          *INLR
      /EJECT
      *
     C     *LIKE         DEFINE    CODT          XXDSDT
     C     *LIKE         DEFINE    CODT          XXPRDT
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *** Begin Del *** 08/03/17 ****************************
     C*****DTCYMD        PLIST
     C**********         PARM                    SRDTI
     C**********         PARM      *BLANKS       SRFFMT
     C**********         PARM      '*CYMD'       SRTFMT
     C**********         PARM                    SRDTO
     C**********         PARM                    SRERR
      *
     C*****DTMDYY        PLIST
     C**********         PARM                    SRDTI
     C**********         PARM      '*CYMD'       SRFFMT
     C**********         PARM      *BLANKS       SRTFMT
     C**********         PARM                    SRDTO
     C**********         PARM                    SRERR
      *** End   Del *** 08/03/17 ****************************
      *
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  KEY LIST FOR USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
      *
     C     *DTAARA       DEFINE                  PREMCHK          80
      *
     C                   MOVEL     MG(3)         QQPRDS
      *
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      *BLANKS       SCMSG1
     C                   MOVE      *BLANKS       SCMSG2
      *
     c     *USA          MOVE      *DATE         SYS_DATE
      *
      *** Begin Chg *** 08/03/17 ****************************
     C**********         Z-ADD     DSDATE        SRDTI
     C**********         CALL      'SRCVTDAT'    DTCYMD
     C**********         MOVEL     SRDTO         XXDSDT
     C                   Eval      xxDsDt = dtCYMD(dsDate)
      *** End   Chg *** 08/03/17 ****************************
     C*** Begin Add *** 08/03/17 *******************************************
      /Free
       // Check to see if Scriptura is on
       isUseScript = UseScriptura(processType);
      /end-free
     C*** Begin Add *** 08/03/17 *******************************************
      *
      *  GET THE USER PARAMETER FILE
      *
      /EJECT
     C     TOP           TAG
      *
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END                                                    *IN60 = '1'
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
     C                   IF        CLTID = '081'
     C                   EVAL      *IN31 = *ON
     C                   ENDIF                                                   CLTID = '081'
      *** BEG ADD ***** 03/25/09 ***********************************************
      *
      * Independent Cash and Return deposit should display only if client has
      * purchased this module.
      *
      *
     C                   MOVE      *OFF          *IN55
     C                   MOVE      *OFF          *IN56
     C                   READ      SWCPF
     C                   IF        NOT %EOF AND WCFL36 = 'Y'
     C
     C                   MOVE      *ON           *IN55
     C
     C                   ENDIF
      *
      *** END ADD ***** 03/25/09 ***********************************************
      *
      *
      *  GET THE COMPANY RECORD
      *
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      COCO#         SCCO#
      *
     C     *LOCK         IN        PREMCHK
     C                   EVAL      @@PREMCHKDT = %SUBST(PREMCHK:1:8)
     C                   MOVE      @@PREMCHKDT   @PREMCHKDT
     C                   OUT       PREMCHK
      *
     C                   IF        @PREMCHKDT <> *ZEROS
      *
     C                   EXFMT     INFOWDW
      *
     C                   EVAL      SCPRDT = @PREMCHKDT
     C                   EVAL      *IN36 = *ON
      *
     C                   IF        *INKL = *ON
     C     *LOCK         IN        PREMCHK
     C                   EVAL      PREMCHK = *ZEROS
     C                   EVAL      @PREMCHKDT = *ZEROS
     C                   OUT       PREMCHK
     C                   EVAL      *INKL = *OFF
     C                   Z-ADD     DSDATE        SCPRDT
     C                   ENDIF                                                   *IN12 = *ON
      *
     C                   ELSE
     C                   Z-ADD     DSDATE        SCPRDT
     C                   ENDIF                                                  @PREMCHKDT <> *ZEROS
      *
      /EJECT
      *   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
      *   DISPLAY ALL FORMATS
      *
     C     STRT          TAG
      *
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END                                                     Q2RECF = 'SCR01'
      *
     C     *INKC         IFEQ      '1'
     C     *INKL         OREQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                     *INKC = '1'
      *
      *   GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      /EJECT
      *
      *   PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *   CHECK FOR A VALID DATE
      *
     C                   SELECT
     C                   WHEN      @PREMCHKDT = 0 AND SCPRDT <> %DEC(DSDATE)
     C                   MOVE      '1'           *IN38
     C                   GOTO      STRT
     C                   WHEN      SCPRDT <> %DEC(DSDATE) AND
     C                             (@PREMCHKDT <> 0 AND SCPRDT <> @PREMCHKDT)
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   ENDSL
      *
      * Check for the proper batch type
      *
     C                   SELECT
      *
     C                   WHEN      CLTID = '081'
     C                   IF        SCBTYP <> 'A' AND SCBTYP <> 'D'
     C                             AND SCBTYP <> 'I' AND SCBTYP <> 'P'
     C                             AND SCBTYP <> 'S' AND SCBTYP <> *BLANK
     C                   EVAL      *IN33 = *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  SCBTYP <> 'A'
      *
     C                   OTHER
     C                   IF        SCBTYP <> 'A' AND SCBTYP <> 'D'
     C                             AND SCBTYP <> 'I' AND SCBTYP <> 'P'
     C                             AND SCBTYP <> *BLANK
     C                   EVAL      *IN37 = *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  SCBTYP <> *BLANK
     C                   ENDSL
      *** BEG ADD ***** 03/25/09 ***********************************************
     C                   IF        WCFL36 <> 'Y' AND SCBTYP = 'I'
     C                   EVAL      *IN56 = *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  SCBTYP <> 'A'
      *** END ADD ***** 03/25/09 ***********************************************
      *
      * Check for the proper Primary Sort Value
      *
     C                   IF        SCPSOR <> 'AN' AND SCPSOR <> 'CU'
     C                             AND SCPSOR <> 'EN'
     C                   EVAL      *IN34 = *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPSOR <> 'AN'
      *
      * Check for the proper Secondary Sort value
      *
     C                   IF        (SCPSOR = 'AN' AND SCSSOR <> 'CU'
     C                             AND SCSSOR <> 'EN')
     C                   EVAL      *IN35 = *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  SCPSOR = 'AN'
      *
      *** beg del ***** 06/23/08 ***********************************************
     C***********        IF        SCPSOR <> 'AN' AND SCSSOR <> *BLANK
     C***********        EVAL      *IN38 = *ON
     C***********        GOTO      STRT
     C***********        ENDIF                                                  SCSSOR <> *BLANK
      *** end del ***** 06/23/08 ***********************************************
      *** beg add ***** 06/26/08 ***********************************************
     c                   if        scpsor = 'EN' and scssor <> *blank
     c                   eval      *in38 = *on
     c                   goto      strt
     c                   endif

     c                   if        scpsor = 'CU' and scssor =  *blank
     c                   eval      *in39 = *on
     c                   goto      strt
     c                   endif

     c                   if        scssor <> 'CU' and
     c                             scssor <> 'EN' and
     c                             scssor <> *blank
     c                   eval      *in40 = *on
     c                   goto      strt
     c                   endif
      *** end add ***** 06/26/08 ***********************************************
      *
      * Start Reading WAR4L12 in CO#/FND/FYR/Batch type order
      *
     C                   READ      PR401
      *
     C                   DOW       NOT %EOF(WAR4L12)
      *
     C                   IF        R4SELC = 'Y'
      *
     C                   IF        SVFND <> R4FND OR SVFYR <> R4FYR
     C                             OR SVBTYP <> R4BTYP
      *
     C                   IF        R4BTYP = 'A' OR R4BTYP = 'D'
     C                             OR R4BTYP = 'P'
     C                   EVAL      #1APPL = 'RP'
     C                   ELSEIF    R4BTYP = 'I'
     C                   EVAL      #1APPL = 'IC'
     C                   ENDIF                                                   R4BTYP = 'A'
      *
     C                   EVAL      #1CO# = %EDITC(R4CO#:'X')
     C                   EVAL      #1FND = %EDITC(R4FND:'X')
     C                   EVAL      #1FYR = %EDITC(R4FYR:'X')
      *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           #1MODE            3
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1APPL            2
      *** BEG CHG ***** 03/27/08 ***********************************************
     C*****************  PARM                    #1DATA            1
     C                   PARM                    #1DATA           75
      *** END CHG ***** 03/27/08 ***********************************************
     C                   PARM                    #1CKTP           10
     C                   PARM                    #1RTCD            1
      *
     C                   IF        #1RTCD <> '1'
      *
     C                   MOVE      SCPRDT        @PRDT
      *
     C                   CALL      'WA023R'
     C                   PARM      #1CO#         ##CO#             3
     C                   PARM      #1FND         ##FND             3
     C                   PARM      #1APPL        ##APPL            2
     C                   PARM      @PRDT         ##PRDT            8
     C                   PARM                    ##ACCT           20
     C                   PARM      'C'           ##CALL            1
     C                   PARM                    ##HDCY            1
     C                   PARM                    ##CHKQ           10
     C                   PARM                    ##FTYP           10
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##EFDT            7
     C                   PARM                    ##CPYQ           10
     C                   PARM      #1CKTP        ##CKTP           10
      *
     C                   ENDIF                                                  #1RTCD <> *BLANK
      *
     C                   EVAL      SCMSG1 = *BLANKS
     C                   EVAL      SCMSG2 = *BLANKS
      *
     C                   IF        #1RTCD <> *BLANK OR ##RTCD <> *BLANK
     C                   EVAL      SCMSG1 = 'Return fund bank information is +
     C                             not set up for group ' + ##FND
      *
     C                   IF        ##CKTP <> *BLANK
     C                   EVAL      SCMSG2 = 'with a check type of ' + ##CKTP
     C                   ENDIF                                                   ##CKTP <> *BLANK
      *
     C                   EXFMT     MSGWDW
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  #1RTCD <> *BLANK
      *
     C                   ENDIF                                                  SVFND <> R4FND
      *
     C                   SELECT
      *
     C                   WHEN      SCPSOR = 'EN'
     C                   EVAL      R4SORT = %EDITC(R4FND:'X') +
     C                             %EDITC(R4EMP#:'X')
     C                             + %EDITC(R4DIV:'X') + %EDITC(R4FYR:'X')
      *
      *** beg chg ***** 06/26/08 *******************************************************
     C***********        WHEN      SCPSOR = 'CU'
     C***********        EVAL      R4SORT = R4CUST
     c                   when      scpsor = 'CU' and scssor = 'EN'
     c                   eval      R4SORT = %trim(R4CUST) +
     c                                      %editc(R4FND:'X') +
     c                                      %editc(R4EMP#:'X') +
     c                                      %editc(R4DIV:'X') +
     c                                      %editc(R4FYR:'X')
     c                   when      scpsor = 'CU' and scssor = 'AN'
     c                   eval      R4SORT = %trim(R4CUST) +
     c                                      %editc(R4AGT#:'X')
      *** end chg ***** 06/26/08 *******************************************************
      *
     C                   WHEN      SCPSOR = 'AN' AND SCSSOR = 'EN'
     C                   EVAL      R4SORT = %EDITC(R4AGT#:'X') +
     C                             %EDITC(R4EMP#:'X')
     C                             + %EDITC(R4DIV:'X') + %EDITC(R4FYR:'X')
      *
     C                   WHEN      SCPSOR = 'AN' AND SCSSOR = 'CU'
     C                   EVAL      R4SORT = %EDITC(R4AGT#:'X') +
     C                             R4CUST
      *
     C                   ENDSL
      *
     C     *CYMD         MOVE      SYS_DATE      R4DATE
     C                   TIME                    R4TIME
     C                   EVAL      R4PGM  = Q1PGM
     C                   EVAL      R4USER = Q1USER
     C                   UPDATE    PR401
      *
     C                   ENDIF                                                  R4SELC = 'Y'
      *
     C                   EVAL      SVFND = R4FND
     C                   EVAL      SVFYR = R4FYR
     C                   EVAL      SVBTYP = R4BTYP
      *
     C                   READ      PR401
      *
     C                   ENDDO                                                  NOT %EOF(WAR411)
      *
      *  SUBMIT THE JOB
      *
     c*** Begin Add ** 08/03/17 ********************************************
     c*
     c* Call output control window
     c                   CallP     OutputControl()
     c                   If        xxSubmit = *On
     c*** End   Add ** 08/03/17 ********************************************
     C                   MOVE      SCCO#         @CO#              3
     C                   MOVE      SCFFND        @FFND             3
     C                   MOVE      SCTFND        @TFND             3
     C                   MOVE      SCPRDT        @PRDT
     C                   MOVE      SCBTYP        @BTYP
      *
     C                   CALL      'W6502C'
     C                   PARM                    @CO#
     C                   PARM                    @FFND
     C                   PARM                    @TFND
     C                   PARM                    @PRDT             8
     C                   PARM                    @BTYP             1
     c*** Begin Add ** 08/03/17 ********************************************
     c                   EndIf
     c*** End   Add ** 08/03/17 ********************************************
      *
     C                   EVAL      *INKC = *ON
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
       //*** Begin Add *** 08/03/17 ****************************************
       //===================================================================
       // OutputControl - Output Control Processing
       //===================================================================
     p OutputControl   b
     d OutputControl   pi

      /Free

       If isUseScript;

          xxProcType = processType;
          xxMass     = 'Y';

          In svLda;

          Clear svLDA;

          xxDart = 'N';
          xxRMCD = ' ';

          Out svLda;

          xxSubmit = OutputWindow(xxProcType:xxMass);

       //*** Begin Add *** 02/15/18 *********************************
       Else;
          xxSubmit = *On;
       //*** End   Add *** 02/15/18 *********************************
       Endif;

      /End-Free

     p OutputControl   e
       //*** End   Add *** 08/03/17 ****************************************
**  TABLE MG
      Date entered is not equal to current system date.
    Change date if necessary and press ENTER to continue.
       RETURN FUND CHECK PRINT
