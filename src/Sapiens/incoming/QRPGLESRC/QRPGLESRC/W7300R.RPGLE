     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7300R - PROMPT FOR AGENT LOSS RATIO & PERFORMANCE REPORTS
     H*****************************************************************
     H* REVISIONS                                                     *
     H*                                                               *
     H* 09/26/18  C:72   Support: 140/73282
     H*                  Remove activation group QILE                 *
     H*                  Edit Agent against group select file         *
     H*                                                               *
     H* 09/01/10  C:61   Project: SY190  PRISS 148
     H*                  Pass 'D'emand mode when called from menu.
     F*                                                               *
     H* 05/14/10  C:246  Project: 139WNI PRISS 19
     H*                  Fix the issue with the Report Title.
     F*                                                               *
     H* 01/08/10  C:238  Project: 139WNI
     H*                  Agent Loss Ratio Report for all Groups
     F*                                                               *
     F* 10/20/09 C:189 PROJECT WC1726                                 *
     F*                CSB-AGT LOSS RATIO/PPR RPT SELECTION CHG       *
     H*                                                               *
     H*  07/30/08  C:183 Project: 46WGA                               *
     H*                  Added Report type PARM and its screen        *
     H*                  validations                                  *
     H*                                                               *
     H*  08/24/05  C:33  Project: WC1314                              *
     H*                  Add agt# and producer selections.            *
     H*                                                               *
     H*  06/03/04 C:132 PROJECT: 81WWH Add Client 081 MODs to Base    *
     H*               - ADD an additional sort option by Agent; in    *
     H*                 addition to current sort options by Chapter,  *
     H*                 Area and County                               *
     H*                                                               *
     H*  05/12/04  C:62  PROJECT: WC1215                              *
     H*                  Remove fyr prompt and add effective date     *
     H*                  range prompt.                                *
     H*                                                               *
     H*  04/16/02  C:90  PROJECT: SEQ#31829                           *
     H*                  Prompt user for output type.                 *
     H*                                                               *
     H*  05/16/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
     H*                                                               *
     H* 12/16/99  C:90  PROJECT: WC577                                *
     H*                 Y2K FIX - RIGHT ADJUST POLICY YEAR, MOVED     *
     H*                 PERFORMANCE REPORT SORT SELECTION.            *
     H*                                                               *
     H* 04/28/99  C:118  PROJECT:  WC577                              *
     H*                  MADE V3 CHANGES TO DISPLAY FILE FOR SORT AND *
     H*                  FILE NAME.                                   *
     H*                                                               *
     H* 06/27/91 TAA  CHANGED DEFAULT DATE TO SYSTEM DATE.            *
     H* PROJ:BASE CHANGE                                              *
     H*****************************************************************
     FW7300F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F***  Begin Add  ***  08/24/05  **************************************
     F                                     INFDS(FNFDS)
     F***  End   Add  ***  08/24/05  **************************************
     F*** BEGIN DEL *** 05/12/04 **************************************
     F**********WDF1L   IF  E           K        DISK
     F*** END   DEL *** 05/12/04 **************************************
     F* * * Fund Detail File
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F***  Begin Add  ***  08/24/05  **************************************
     FWMAAL1    IF   E           K DISK
     F                                     INFDS(AAFDS1)
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA0P)
     FWMAGL     IF   E           K DISK
     F                                     RENAME(PAG01:PAG00)
     F                                     INFDS(AGFDS)
     FWMAPL     IF   E           K DISK
     F                                     RENAME(PAP01:PAP00)
     FWMAPP     IF   E             DISK
     F                                     RENAME(PAP01:PAP0P)
     f*** Beg Add *** 09/26/18 ******************************
     fwxgsl     if   e           k disk
     f*** End Add *** 09/26/18 ******************************

     D FNFDS           DS
     D  FNRRN                397    400B 0
     D AAFDS1          DS
     D  AARRN1               397    400B 0
     D AGFDS           DS
     D  AGRRN                397    400B 0
     D*
     D OPTNM           C                   CONST('Agent Loss Ratio -
     D                                     (PPR Report)')
     D***  End   Add  ***  08/24/05  **************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C***  Begin Add  ***  08/24/05  **************************************
     C     AAKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    SCAGT#
     C*
     C     AGKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C     APKY          KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    SCPROD
     C***  End   Add  ***  08/24/05  **************************************
     C*** BEGIN DEL *** 05/12/04 **************************************
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C********** F1KEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C*** END   DEL *** 05/12/04 **************************************
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C***  BEG Chg  ***  01/08/10  **********************************  ****
     C                   MOVE      UPFND         SCFND             3 0
     C***  END Chg  ***  01/08/10  **********************************  ****
     C*** BEGIN DEL *** 05/12/04 **************************************
     C**********           MOVE UPFYR     SCFYR
     C*** END   DEL *** 05/12/04 **************************************
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C***  BEG Chg  ***  01/08/10  **********************************  ****
     C                   MOVEL     FNNAME        SCNAME           60
     C***  END Chg  ***  01/08/10  **********************************  ****
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDTE
     C*
     C***  Begin Chg  ***  08/24/05  **************************************
     C**********           MOVE QQPRDS    SCPRDS
     C     Q1PRMS        IFGE      1
     C     QQPRDS        ANDNE     *BLANKS
     C                   MOVEL     QQPRDS        ##DATA
     C                   ELSE
     C                   MOVEL     OPTNM         ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C***  End   Chg  ***  08/24/05  **************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEG ADD ******** 05/16/01 ********************************************
     C*
     C*   DISPLAY LOCATION OF OUTPUT FILE
     C*
     C                   CALL      'SA030R'
     C                   PARM                    QQSUFX            3
      **** BEGIN CHG **** 04/16/02 ************************************
     C********** 'PDSFIL'  CAT  QQSUFX    XXRPTL 10 P
     C     'PDSRPT'      CAT(P)    QQSUFX        XXRPTL           10
      **** END   CHG **** 04/16/02 ************************************
     C                   CAT(P)    '.':0         XXRPTL
     C                   MOVE      XXRPTL        SCRPTL
     C*
     C*** END ADD ******** 05/16/01 ********************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C***  Begin Add  ***  08/24/05  **************************************
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRDTO         IFNE      *ZEROS
     C                   MOVEL     SRDTO         SCDTE
     C                   ENDIF
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRDTO         IFNE      *ZEROS
     C                   MOVEL     SRDTO         SCFRDT
     C                   ENDIF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRDTO         IFNE      *ZEROS
     C                   MOVEL     SRDTO         SCTODT
     C                   ENDIF
     C*
     C*  F10=Search
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCAGT#'
     C                   EXSR      SRCHAG
     C     HFLD          WHENEQ    'SCPROD'
     C                   EXSR      SRCHAP
     C                   ENDSL
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C***  End   Add  ***  08/24/05  **************************************
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C*** BEGIN DEL *** 05/12/04 **************************************
     C**********           MOVE UPFYR     SCFYR
     C*** END   DEL *** 05/12/04 **************************************
     C*  GET THE FUND RECORD, LOAD FUND NAME FOR SCREEN
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   Z-ADD     UPPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE
     C*** BEGIN ADD *** 05/12/04 **************************************
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*** END   ADD *** 05/12/04 **************************************
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*** BEGIN DEL *** 05/12/04 **************************************
     C*
     C*  VALIDATE THE FUND YEAR FOR THE FUND
     C*
     C**********           Z-ADDSCFYR     XXFYR   30
     C**         F1KEY ----------------- CO#, FND, FYR
     C********** F1KEY     CHAINPF101                25
     C********** *IN25     CABEQ'1'       STRT
     C*** END   DEL *** 05/12/04 **************************************
     C*** BEGIN ADD *** 05/12/04 **************************************
     C*
     C*  FROM DATE MUST BE VALID
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        26
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     SRDTO         SCFRDT
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
     C                   ENDIF
     C*
     C*  TO DATE MUST BE VALID
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        27
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVEL     SRDTO         SCTODT
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
     C                   ENDIF
     C*
     C*  DATES MUST BE IN ASCENDING ORDER
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   SETON                                        30
     C                   GOTO      STRT
     C                   ENDIF
     C*** END   ADD *** 05/12/04 **************************************
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE SELECTION FLAGS
     C*
     C     SCLR          IFNE      'Y'
     C     SCLR          ANDNE     'N'
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   END
     C*** BEG  ADD *** 10/20/09 ************************************************
     C*
     C     SCAGPR        IFNE      'Y'
     C     SCAGPR        ANDNE     'N'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C     SCLR          IFEQ      'N'
     C     SCAGPR        ANDNE     'Y'
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*** END  ADD *** 10/20/09 ************************************************
     C*** BEG ADD ******** 07/30/08 ********************************************
     C*
     C****BEG  ADD *** 10/20/09 ************************************************
     C     SCLR          IFEQ      'Y'
     C****END  ADD *** 10/20/09 ************************************************
     C     SCRTYP        IFNE      'D'
     C     SCRTYP        ANDNE     'S'
     C     SCRTYP        ANDNE     'B'
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END
     C****BEG  ADD *** 10/20/09 ************************************************
     C                   END
     C****END  ADD *** 10/20/09 ************************************************
     C*** END ADD ******** 07/30/08 ********************************************
     C*
     C****BEG  DEL *** 10/20/09 ************************************************
     C*********  SCAGPR    IFNE 'Y'
     C*********  SCAGPR    ANDNE'N'
     C*********            MOVE '1'       *IN29
     C*********            GOTO STRT
     C*********            END
     C****END  DEL *** 10/20/09 ************************************************
     C*
     C***  Begin Add  ***  08/24/05  **************************************
     C****BEG  DEL *** 10/20/09 ************************************************
     C***********           SCAGPR    IFEQ 'Y'
     C****END  DEL *** 10/20/09 ************************************************
     C*
     C*  Validate Agent #, if entered
     C*
       //*** Beg Del *** 09/26/18 *****************************************
     C****
     C**** SCAGT#        IFNE      *ZEROS
     C**** AAKY          CHAIN     PAA01                              60
     C**** *IN60         IFEQ      *ON
     C**** AASTAT        OREQ      'I'
     C****               MOVE      *ON           *IN71
     C****               GOTO      STRT
     C****               ENDIF                                                  *IN60=*ON
     C****               ENDIF
       //*** End Del *** 09/26/18 *****************************************
       //*** Beg Add *** 09/26/18 *****************************************
      /Free
                         If scAgt# <> *zeros;
                            *In71 = *on;
                            Setll *loval pgs01;
                            Read pgs01;
                            Dow Not %Eof(wxgsl);
                               If GSieFl = 'I';
                                  Chain (upco#:GSfnd:scAgt#) paa01;
                                  If %Found(wmaal1) and AAstat <> 'I';
                                     *In71 = *off;
                                     Leave;
                                  Endif;
                               Endif;
                               Read pgs01;
                            Enddo;
                         Endif;
      /End-Free
     C     *in71         Ifeq      *on
     C                   Goto      strt
     C                   Endif
       //*** End Add *** 09/26/18 *****************************************
     C*
     C*  Validate Producer, if entered
     C*
     C     SCPROD        IFNE      *ZEROS
     C     APKY          CHAIN     PAP00                              60
     C     *IN60         IFEQ      *ON
     C     APSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF
     C*
     C****BEG  DEL *** 10/20/09 ************************************************
     C************         ENDIF
     C****END  DEL *** 10/20/09 ************************************************
     C***  End   Add  ***  08/24/05  **************************************
     C*
     C*  VALIDATE SORT FLAG FOR PRODUCER PERFORMANCE SELECT
     C*
     C     SCAGPR        IFEQ      'Y'
     C     SCSORT        ANDNE     '1'
     C     SCSORT        ANDNE     '2'
     C     SCSORT        ANDNE     '3'
      ***** BEG ADD ***** 06/03/04 ***********************************
     C     SCSORT        ANDNE     '4'
      ***** END ADD ***** 06/03/04 ***********************************
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
      **** BEGIN ADD **** 04/16/02 ************************************
      *
      * OUTPUT TYPE MUST BE P,F,B
      *
     C     SCOUTP        IFNE      'P'
     C     SCOUTP        ANDNE     'F'
     C     SCOUTP        ANDNE     'B'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      **** END   ADD **** 04/16/02 ************************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C***  Begin Chg  ***  01/08/10  **************************************
     C***                  MOVE SCFND     QQFND   3
     C                   MOVE      *ZEROS        QQFND             3
     C***  END Chg  ***  01/08/10  **********************************  ****
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           MOVE XXFYR     QQFYR   3
     C                   MOVE      XXFRDT        QQFRDT            7
     C                   MOVE      XXTODT        QQTODT            7
     C*** END   CHG *** 05/12/04 **************************************
     C                   MOVE      SCDTE         QQDTE             8
     C***  Begin Add  ***  08/24/05  **************************************
     C                   MOVE      SCAGT#        QQAGT#
     C                   MOVE      SCPROD        QQPROD
     C***  End   Add  ***  08/24/05  **************************************
     C*** BEG ADD ******** 07/30/08 ********************************************
     C                   MOVE      SCRTYP        QQRTYP
     C*** END ADD ******** 07/30/08 ********************************************
     C*
     C                   CALL      'W7300C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C*** BEGIN CHG *** 05/12/04 **************************************
     C**********           PARM           QQFYR
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C*** END   CHG *** 05/14/10 **************************************
     C                   PARM                    QQDTE
     C                   PARM                    UPOUTQ
     C*** BEGIN CHG *** 05/14/10 **************************************
     C*********            PARM 'D'       MODE    1
      ***** BEG CHG ******* 09/01/10 **********************************
     C*********            PARM           QQRTYP
     C                   PARM      'D'           MODE              1
      ***** END CHG ******* 09/01/10 **********************************
     C*** END   CHG *** 05/12/04 **************************************
     C                   PARM                    SCLR
     C                   PARM                    SCAGPR
     C                   PARM                    SCSORT
      **** BEGIN ADD **** 04/16/02 ************************************
     C                   PARM                    SCOUTP
      **** END   ADD **** 04/16/02 ************************************
     C***  Begin Add  ***  08/24/05  **************************************
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQPROD            5
     C***  End   Add  ***  08/24/05  **************************************
     C*** BEG ADD ******** 07/30/08 ********************************************
     C                   PARM                    QQRTYP            1
     C*** END ADD ******** 07/30/08 ********************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C***  Begin Add  ***  08/24/05  **************************************
     C*====================================================================
     C* Search Agents
     C*====================================================================
     C     SRCHAG        BEGSR
     C*
     C                   CALL      'W0040R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      FNRRN         ##RRN             9 0
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     WMAAP                              60
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   MOVE      *ON           *IN82
     C                   ELSE
     C                   MOVE      *ON           *IN81
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Search Producers
     C*====================================================================
     C     SRCHAP        BEGSR
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     AGKY          CHAIN     PAG00                              60
     C*
     C                   CALL      'W0042R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM      AGRRN         ##RRN
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     WMAPP                              60
     C                   Z-ADD     APPROD        SCPROD
     C*** BEG  ADD *** 10/20/09 ************************************************
     C***************      MOVE *ON       *IN83
     C                   MOVE      *ON           *IN85
     C*** END  ADD *** 10/20/09 ************************************************
     C                   ELSE
     C                   MOVE      *ON           *IN82
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN71
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add  ***  08/24/05  **************************************
