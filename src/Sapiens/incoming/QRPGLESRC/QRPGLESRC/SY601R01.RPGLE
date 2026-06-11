     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H* 05/31/19  C:079  Support: 99/79854
     H*                  Change Use R&C table to a checkbox
     H*
     H* 05/11/17  C:180  Project: GA038
     H*                  - Change scFSMRA to a checkbox.
     H*                  - Add dropdown box scMsg (FSMRA State Msg Id)
     H*                    and related processing.
     H*
     H* 04/04/16  C:212  Project: OK022
     H*                  add fields scRC/SCFSMRA in display file and
     H*                  related processing
     H*
     H* 02/15/16  C:212  Project: 114MXB
     H*                  add field SCFSTY in display file and related
     H*                  processing
     H*
     H* 12/14/15  C:309  Support: 38813
     H*                  Added a new check box "Extra Territorial"
     H*
     H* 05/18/15  C:212  Project: NC039
     H*                  change Medicare based FS field from checkbox
     H*                  to dropdown in DSPF and related program changes
     H*
     H* 08/01/14  C:212  Project: 114MNV
     H*                  add field STPAUF in display file and related
     H*                  processing
     H*
     H* 07/17/14  C:079  PROJECT:MC376
     H*                  Added new field EDI Release
     H*
     H* 09/18/12  C:284  PROJECT:TX079
     H*                  Added new field Medicare zip + 4.
     H*
     H* 03/25/11  C:235  Project:WC1983
     H*                  Added FROI flag field.
     H*
     H* 10/01/10  C:198  Project:WV009
     H*                  Added field for Medicare based FS flag.
     H*
     H* 08/12/10  C:251  PROJECT:114MWS
     H*                  ADDED FIELD FOR MEDICARE BASED PPO PROCESSING
     H*
     H* 04/02/09  C:227  PROJECT:MC331
     H*                  ADDED FIELD FOR NCCI MEDICAL DATA CALL
     H*
     H* 04/17/08  C:180  PROJECT:MC288
     H*                  NO UNITS BREAKDOWN/INDIVIDUAL LINES
     H*
     H*****************************************************************
     FSY601F01  CF   E             WORKSTN
     F                                     Handler('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FSTSTP     UF A E             DISK
     F                                     RENAME(PST01:DBRCDF)
     FSTABL     IF   E           K DISK
     F***** Beg Add *** 05/18/15 **************************************
     FFTGTP     IF   E           K DISK
     F***** End Add *** 05/18/15 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('State Maintenance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    STKEY         KKST
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     STSTAT        IFEQ      'I'
     C                   MOVE      'A'           STSTAT
     C                   ELSE
     C                   MOVE      'I'           STSTAT
     C                   ENDIF                                                  STSTAT
     C*
     C                   MOVEL     Q1USER        STUSER
     C                   MOVEL     Q1PGM         STPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STDT
     C*
     C                   MOVEL     DSTIME        STTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C***** Beg Add *** 08/01/14 **************************************
     C/free
       // initialize error fields and flags
        erAudFlag = *off;
        msAudFlag = *blanks;
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C*
     C***** Beg Add *** 02/15/16 **************************************
     C/free
        erFSTyp = *off;
        msFSTyp = *blanks;
      /end-free
     C***** End Add *** 02/15/16 **************************************
     C***** Beg Add *** 04/04/16 **************************************
     C/free
        erRCAnd = *off;
        msRCAnd = *blanks;
        erFSMRA = *off;
        msFSMRA = *blanks;
      /end-free
     C***** End Add *** 04/04/16 **************************************
     C***** Beg Add *** 12/14/15 **************************************
      /Free
        erExTerr = *Off;
        msExTerr = *Blanks;
        //*** Begin Add *** 05/11/17 **********************************
        erMsg = *off;
        msMsg = *blanks;
        //*** End   Add *** 05/11/17 **********************************
      /End-free
     C***** End Add *** 12/14/15 **************************************
     C* STATE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCKEY         IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE STATE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCKEY         KKST
     C*
     C   40
     CANN90STKY          CHAIN     PST01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* STATE NAME CANNOT BE BLANK
     C*
     C  N90SCNAME        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCNAME
     C*
     C* MANAGED CARE FLAG MUST BE Y OR N
     C*
     C  N90SC1A3         IFNE      'Y'
     C     SC1A3         ANDNE     'N'
     C                   SETON                                        74  90
     C                   ENDIF                                                  SC1A3
     C***** BEG ADD *****  03/25/11 **********************************
     C*
     C* FROI Install Flag must be 'Y' or Blank
     C*
     C  N90SC1A6         IFNE      'Y'
     C     SC1A6         ANDNE     ' '
     C                   SETON                                        85  90
     C                   ELSE
     C                   SETOFF                                       85
     C                   ENDIF                                                  SC1A6
     C***** END ADD *****  03/25/11 **********************************
     C*
     C* EDI FLAG MUST BE Y OR N
     C*
     C  N90SC1A4         IFNE      'Y'
     C     SC1A4         ANDNE     'N'
     C                   SETON                                        75  90
     C                   ENDIF                                                  SC1A4
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90STDT          IFGT      SAVEDT
     C     STDT          OREQ      SAVEDT
     C     STTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
      ***** BEGIN 04/17/08 ********************************************
      * SPLIT INTO MULTIPLE UNITS
      *
     C     *IN90         IFEQ      *OFF
     C     SCMLSP        IFEQ      *BLANK
     C     SCMLSP        OREQ      'N'
     C                   ELSE
     C                   MOVE      *ON           *IN79
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
      *
      * SPLIT EFF DATE
      *
     C     *IN90         IFEQ      *OFF
     C     SCMLDT        ANDNE     *ZEROS
     C                   Z-ADD     SCMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMLDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN92
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  *IN90
      *
      * BILL TYPES MUST BE Y OR BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCPHYS        ANDNE     'Y'
     C     SCPHYS        ANDNE     'N'
     C     SCPHYS        ANDNE     ' '
     C                   SETON                                        7690
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCHOSP        ANDNE     'Y'
     C     SCHOSP        ANDNE     'N'
     C     SCHOSP        ANDNE     ' '
     C                   SETON                                        7790
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCDENT        ANDNE     'Y'
     C     SCDENT        ANDNE     'N'
     C     SCDENT        ANDNE     ' '
     C                   SETON                                        8090
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCPHAR        ANDNE     'Y'
     C     SCPHAR        ANDNE     'N'
     C     SCPHAR        ANDNE     ' '
     C                   SETON                                        8190
     C                   ENDIF
      *
     C***** BEG ADD **** 04/02/09 *************************************
     C     *IN90         IFEQ      *OFF
     C     SCNCDC        ANDNE     'Y'
     C     SCNCDC        ANDNE     ' '
     C                   SETON                                        8290
     C                   ENDIF
     C***** END ADD **** 04/02/09 *************************************
      *
     C***** BEG ADD **** 08/12/10 *************************************
     C     *IN90         IFEQ      *OFF
     C     SCMPPO        ANDNE     'Y'
     C     SCMPPO        ANDNE     ' '
     C                   SETON                                        8390
     C                   ENDIF
     C***** END ADD **** 08/12/10 *************************************
     C*** Beg Add **** 10/01/10 *************************************
     C***** Beg Dlt *** 05/18/15 **************************************
     C**** *IN90         IFEQ      *OFF
     C**** SCMFS         ANDNE     'Y'
     C**** SCMFS         ANDNE     ' '
     C****               SETON                                        8490
     C****               ENDIF
     C***** End Dlt *** 05/18/15 **************************************
     C*** End Add **** 10/01/10 *************************************
      ***** END N 04/17/08 ********************************************
     C*** Beg Add **** 09/18/12 ************************************************
     C     *IN90         IFEQ      *OFF
     C     SCZIP4        ANDNE     'Y'
     C     SCZIP4        ANDNE     ' '
     C                   SETON                                        8590
     C                   ENDIF
     C*** End Add **** 09/18/12 ************************************************
     C***** Beg Add *** 08/01/14 **************************************
     C/free
       // Procura Audit Flag must be Y or blank
        if *IN90 = *OFF
           and dspAudFlag = *on
           and SCPAUF <> 'Y'
           and SCPAUF <> *blanks;
          erAudFlag = *on;
          msAudFlag = 'MCR0501';
          *in90 = *on;
        endif;
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C***** Beg Add *** 12/14/15 **************************************
     C/Free
        //Extra Territorial Flag must be Y or blank
        If *In90 = *Off and ScExTerr <> 'Y' and ScExTerr <> *blanks;
          ErExTerr = *On;
          msExTerr = 'MCR0571';
          *In90 = *On;
        Endif;
      /End-free
     C***** End Add *** 12/14/15 **************************************
     C***** Beg Add *** 02/15/16 **************************************
     C/free
       // Fee Schedule Type must be valid
       // This is being enforced in screen by only allowing valid values
       // to be chosen via dropdown
      /end-free
     C***** End Add *** 02/15/16 **************************************
      *
      /Free
       //*** Begin Add *** 05/11/17 ***********************************
       If scMsg = *blanks and scFSMRA = 'Y';
          erMsg = *on;
          msMsg = 'MCR2500';
          *In90 = *On;
       Endif;

       If scFSMRA  = *blanks and scMsg > *Blanks;
          erFSMRA = *on;
          msFSMRA = 'MCR2501';
          *In90 = *On;
       Endif;
       //*** End   Add *** 05/11/17 ***********************************
      /End-Free
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     STKEY         SCKEY
     C                   MOVEL     STNAME        SCNAME
     C                   MOVEL     ST1A3         SC1A3
     C***** BEG ADD *****  03/25/11 **********************************
     C                   MOVEL     ST1A6         SC1A6
     C***** END ADD *****  03/25/11 **********************************
     C                   MOVEL     ST1A4         SC1A4
     C                   MOVEL     STSTAT        SCSTAT
      ***** BEGIN ADD ****  04/17/08 **********************************
     C                   MOVE      STMLSP        SCMLSP
     C                   Z-ADD     STMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMLDT
     C                   MOVE      STPHYS        SCPHYS
     C                   MOVE      STHOSP        SCHOSP
     C                   MOVE      STDENT        SCDENT
     C                   MOVE      STPHAR        SCPHAR
     C***** BEG ADD **** 04/02/09 *************************************
     C                   MOVEL     STNCDC        SCNCDC
     C***** END ADD **** 04/02/09 *************************************
     C***** BEG ADD **** 08/12/10 *************************************
     C                   MOVEL     STMPPO        SCMPPO
     C***** END ADD **** 08/12/10 *************************************
     C*** Beg Add **** 10/01/10 *************************************
     C                   MOVEL     STMFS         SCMFS
     C*** End Add **** 10/01/10 *************************************
      ****** END ADD *******  04/17/08 ********************************
     C*** BEG ADD *** 09/18/12 *************************************************
     C                   MOVEL     STZIP4        SCZIP4
     C*** END ADD *** 09/18/12 *************************************************
     C*** Beg Add *****  07/17/14 **********************************
     C                   Eval      scErel = STediRel
     C*** End Add *****  07/17/14 **********************************
     C***** Beg Add *** 08/01/14 **************************************
     C/free
        If dspAudFlag = *on;
            scPAUF = STpauf;
        Endif;
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C*** Beg Add *****  12/14/15 **********************************
     C                   Eval      ScExTerr = STINST
     C*** End Add *****  12/14/15 **********************************
     C***** Beg Add *** 02/15/16 **************************************
     C/free
            scFsTy = STfsty;
      /end-free
     C***** End Add *** 02/15/16 **************************************
     C***** Beg Add *** 04/04/16 **************************************
     C/free
       //*** Beg Chg *** 05/31/19 ***************************************
       //***scRC = STbs02;
            scRC = %Subst(STbs02:1:1);
       //*** End Chg *** 05/31/19 ***************************************
            scFSMRA = STbs03;
       //*** Begin Add *** 05/11/17 *************************************
            scMsg = ST7002;
       //*** End   Add *** 05/11/17 *************************************

      /end-free
     C***** End Add *** 04/04/16 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCKEY
     C                   MOVEL     *BLANKS       SCNAME
     C                   MOVEL     *BLANKS       SC1A3
     C***** BEG ADD *****  03/25/11 **********************************
     C                   MOVEL     *BLANKS       SC1A6
     C***** END ADD *****  03/25/11 **********************************
     C                   MOVEL     *BLANKS       SC1A4
      ****** BEGIN ADD  ****  04/17/08 ********************************
     C                   MOVE      *BLANKS       SCMLSP
     C                   MOVE      *ZEROS        SCMLDT
     C                   MOVE      *BLANKS       SCPHYS
     C                   MOVE      *BLANKS       SCHOSP
     C                   MOVE      *BLANKS       SCDENT
     C                   MOVE      *BLANKS       SCPHAR
     C***** BEG ADD **** 08/12/10 *************************************
     C                   MOVE      *BLANKS       SCMPPO
     C***** END ADD **** 08/12/10 *************************************
     C*** Beg Add **** 10/01/10 *************************************
     C                   MOVEL     *BLANKS       SCMFS
     C*** End Add **** 10/01/10 *************************************
     C*** BEG ADD *** 09/18/12 *************************************************
     C                   MOVEL     *BLANKS       SCZIP4
     C*** END ADD *** 09/18/12 *************************************************
     C***** Beg Add *** 08/01/14 **************************************
     C                   eval      scPAUF = *blanks
     C***** End Add *** 08/01/14 **************************************
     C***** Beg Add *** 02/15/16 **************************************
     C                   eval      scFSTY = *blanks
     C***** End Add *** 02/15/16 **************************************
     C***** Beg Add *** 04/04/16 **************************************
     C                   eval      scRC    = *blanks
     C                   eval      scFSMRA = *blanks
     C***** End Add *** 04/04/16 **************************************
     C                   MOVEL     'A'           SCSTAT
      ****** END ADD *******  04/17/08 ********************************
     C*** Beg Add *****  07/17/14 **********************************
     C                   Eval      scErel = *blanks
     C*** End Add *****  07/17/14 **********************************
     C***** Beg Add *** 12/14/15 **************************************
     C                   eval      ScExTerr = *Blanks
     C***** End Add *** 12/14/15 **************************************
     C***** Beg Add *** 05/11/17 **************************************
     C                   eval      ScMsg = *BLANKS
     C***** End Add *** 05/11/17 **************************************
     C*
     C                   ENDIF
     C*
     C***** Beg Add *** 05/11/17 **************************************
      /Free
       SelFSMRA = 'MIMSEQ = 0 ' + 'AND MIST = ''' + STkey + ''' +
                   and MISTAT = ''A''';
      /End-Free
     C***** End Add *** 05/11/17 **************************************
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCKEY         STKEY
     C                   MOVEL     SCNAME        STNAME
     C                   MOVEL     SC1A3         ST1A3
     C***** BEG ADD *****  03/25/11 **********************************
     C                   MOVEL     SC1A6         ST1A6
     C***** END ADD *****  03/25/11 **********************************
     C                   MOVEL     SC1A4         ST1A4
      ***** BEGIN ADD ****  04/17/08 **********************************
     C                   MOVE      SCMLSP        STMLSP
     C                   Z-ADD     SCMLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STMLDT
     C                   MOVE      SCPHYS        STPHYS
     C                   MOVE      SCHOSP        STHOSP
     C                   MOVE      SCDENT        STDENT
     C                   MOVE      SCPHAR        STPHAR
      ****** END ADD *******  04/17/08 ********************************
     C                   MOVEL     SCSTAT        STSTAT
     C***** BEG ADD **** 04/02/09 *************************************
     C                   MOVEL     SCNCDC        STNCDC
     C***** END ADD **** 04/02/09 *************************************
     C*
     C***** BEG ADD **** 08/12/10 *************************************
     C                   MOVEL     SCMPPO        STMPPO
     C***** END ADD **** 08/12/10 *************************************
     C*** Beg Add **** 10/01/10 *************************************
     C                   MOVEL     SCMFS         STMFS
     C*** End Add **** 10/01/10 *************************************
     C*** BEG ADD *** 09/18/12 *************************************************
     C                   MOVEL     SCZIP4        STZIP4
     C*** END ADD *** 09/18/12 *************************************************
     C*** Beg Add *****  07/17/14 **********************************
     C                   Eval      STediRel = scErel
     C*** End Add *****  07/17/14 **********************************
     C***** Beg Add *** 08/01/14 **************************************
     C/free
        If dspAudFlag = *on;
          STpauf = scPAUF;
        Endif;
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C***** Beg Add *** 02/15/16 **************************************
     C/free
        STfsty = scFsTy;
      /end-free
     C***** End Add *** 02/15/16 **************************************
     C***** Beg Add *** 04/04/16 **************************************
     C/free
       //*** Beg Chg *** 05/31/19 *************************************
       //*** STbs02 = scRC;
        %Subst(STbs02:1:1) = scRC;
       //*** End Chg *** 05/31/19 *************************************
        STbs03 = scFSMRA;
      /end-free
     C***** End Add *** 04/04/16 **************************************
     C*** Beg Add *****  12/14/15 **********************************
     C                   Eval      STINST = ScExTerr
     C*** End Add *****  12/14/15 **********************************
     C*
     C*** Beg Add *****  05/11/17 **********************************
     C                   Eval      ST7002 = ScMsg
     C*** End Add *****  05/11/17 **********************************
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C***** Beg Add *** 08/01/14 **************************************
     C*  initialize error fields and flags
     C*
     C/free
       // display and process procura audit flag only for client 114
        dspAudFlag = *off;
        if cltid = '114';
          dspAudFlag = *on;
        endif;
      /end-free
     C***** End Add *** 08/01/14 **************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        STUSER
     C                   MOVEL     Q1PGM         STPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STDT
     C*
     C                   MOVEL     DSTIME        STTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        STUSER
     C                   MOVEL     Q1PGM         STPGM
     C                   MOVEL     Q1USER        STIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         STDT
     C*
     C                   MOVEL     SRDTO         STIDT
     C*
     C                   MOVEL     DSTIME        STTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      STSTP
     C*
     C                   ENDSR
