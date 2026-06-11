     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0090R1 - PROMPT SCREEN FOR SINGLE DIVISION REVISED BILL
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: PROMPT UP EMPLOYER POLICY INFORMATION    *
     H*                      FOR A SINGLE DIVISION REVISED BILL.      *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED: 02/27/06               PROJECT: WC1327               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/24/25  C:65  PROJECT:  SUPP#120120  (Amwins)              *
     H*                  Use new revised bill pgm instead of W0090R.  *
     H*                                                               *
     H*  02/11/08  C:65  PROJECT:  SUPP#62164                         *
     H*                  ALLOW REVISED BILL WITHOUT PRIOR EMPLR       *
     H*                  HISTORY ON NON-SUMMARIZED DIVISIONS.         *
     H*                                                               *
     H*****************************************************************
      *
     FW0090F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *
     FWMEMP     IF   E             DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PEM01:PEM02)
      *
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
      *
     FSMCOL     IF   E           K DISK
      *
     FWMUPL     IF   E           K DISK
      *
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
      *
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL03)
     FWMRPL4    IF   E           K DISK
     FWMGCL     IF   E           K DISK
     FWDEHL     IF   E           K DISK
     F*
      *
      *
     C*** begin add ***  11/24/25  ****************************************
     d/copy sprLock
     d strCommand      s            255
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const
     C*** end add *****  11/24/25  ****************************************

     D*
     D OPTNM           C                   CONST('Single Division Revi-
     D                                     sed Bill')
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5) ASCEND                        CANCAL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  GROUP MASTER FILE
      *

     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCYR
     C*
     C*  GROUP CARRIER INFORMATION FILE
     C*
     C     GCKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL5        PLIST
     C                   PARM      'S'           ##MODE            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      DBRRN2        ##RRN             9 0
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
      *
     C     TOP           TAG
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
      *
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      *
      *  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
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
     C                   END
      *
      *  PROCESS ALL COMMAND KEYS
      *
     C     *IN11         IFEQ      *ON
      *
      *  F3=EXIT
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
      *
      *  F10 - SEARCH
      *
     C     *INKJ         IFEQ      *ON
      *
     C     Q2RECF        IFEQ      'SCR01'
      *
     C                   MOVE      *OFF          *IN38                          SCYR POSITION
     C                   MOVE      *OFF          *IN39                          SCEMP# POSITION
      *
      *  F10 - TABLE FILE SEARCH - POLICY YEAR
      *
     C     HFLD          IFEQ      'SCYR'
      *
     C                   MOVE      *ON           *IN38                          SCYR POSITION
      *
      *  EMPLOYER NUMBER EQUAL TO ZERO
      *
     C     SCEMP#        IFEQ      *ZEROS
      *
     C                   MOVE      *ZEROS        @@RRN
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     @@RRN         IFNE      *ZEROS
      *
     C     @@RRN         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL(P)  FNNAME        SCNAME
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  *INKL=OFF,@@RRN
      *
     C                   ENDIF                                                  SCEMP#=0
      *
      *  EMPLOYER NUMBER NOT EQUAL TO ZERO
      *
     C     SCEMP#        IFNE      *ZEROS
      *
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
      *
     C     EMKEY         CHAIN     PEM02                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     XXFND         @1RRN
     C                   Z-ADD     DBRRN3        @1RRN2
      *
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     @1RRN         IFNE      *ZEROS
      *
     C     @1RRN         CHAIN     PEL03                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCYR
      *
     C                   MOVE      *OFF          *IN38                          SCYR POSITION
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  @1RRN<>0
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  SCEMP#<>0
      *
     C                   ENDIF                                                  HFLD=SCYR
      *
      *  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
      *
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
      *
     C                   MOVE      *ON           *IN39                          SCEMP# POSITION
      *
     C                   MOVE      *ZEROS        @@RRN
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     @@RRN         IFNE      *ZEROS
      *
     C     @@RRN         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
      *
     C                   MOVE      *OFF          *IN39                          SCEMP# POSITION
      *
     C                   ENDIF                                                  *IN60=OFF
      *
     C                   ENDIF                                                  *INKL=OFF,@@RRN
      *
     C                   ENDIF                                                  HFLD=SCEMP#
      *
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  Q2RECF=SCR01
      *
     C                   ENDIF                                                  *INKJ=ON
      *
      *  PROCESS CHANGE OF USER PARAMETERS
      *
     C     *INKM         IFEQ      '1'
      *
     C                   CALL      'W0006R'
      *
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
      *
      *   PROCESS HELP REQUESTS
      *
     C     *IN10         IFEQ      '1'
      *
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
      *
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C                   END
      *
      *  GOTO THE APPROPRIATE SCREEN
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
      *
      *  PROCESS REQUEST SCREEN
      *
     C     SCR01T        TAG
      *
      *  YEAR CANNOT BE LESS THAN OR EQUAL TO ZERO
      *
     C     SCYR          CABLE     0             STRT                   2020
      *
      *  EMPLOYER AND DIVISION NUMBERS MUST BE VALID
      *
     C     EMKY          CHAIN     WMEML                              21
     C     *IN21         CABEQ     '1'           STRT
      *
     C     ELKY          CHAIN     WDELL                              23
     C     *IN23         CABEQ     '1'           STRT
     C*
     C* DO NOT ALLOW SINGLE DIVISION REVISED BILL IF NO BILLING HAS
     C* ALREADY BEEN RUN.
     C*
     C*** BEGIN ADD ***  02/11/08  *******************************************
     C     ELARSM        IFEQ      'Y'
     C*** END ADD *****  02/11/08  *******************************************
     C     ELKY          CHAIN     PEH01                              33
     C     *IN33         CABEQ     '1'           STRT
     C*** BEGIN ADD ***  02/11/08  *******************************************
     C                   ENDIF
     C*** END ADD *****  02/11/08  *******************************************
     C*
     C* NO BILL IF AUDITED.
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* NO BILL IF CANCELLED.
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C                   Z-ADD     0             SCCX              7 0
     C*
     C                   Z-ADD     1             I1                2 0
     C*
     C     *ZERO         LOOKUP    ED(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C                   Z-ADD     CX(I1)        SCCX
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C     SCCX          IFNE      *ZERO
     C     SCCX          ANDLE     XXDT
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* ONLY ALLOW BILL TO BE RUN IF A CARRIER POLICY # IS ENTERED
     C* ON THE DIVISION.
     C*
     C     GCKY          CHAIN     PGC01                              60
     C     *IN60         IFEQ      *OFF
     C     GCCRFL        ANDEQ     'Y'
      *
     C                   MOVE      *BLANKS       RPCAP#
     C     RPKY          CHAIN     PRP01                              61
      *
     C     RPCAP#        IFEQ      *BLANKS
     C     *IN61         OREQ      *ON
     C                   SETON                                        73
     C                   GOTO      STRT
     C                   ENDIF                                                  RPCAP# = BLANKS
      *
     C                   ENDIF                                                  *IN60, GCCRFL
     C*
     C* CALL THE REVISED BILLING PROGRAM.
     C*
     C*** begin chg ***  11/24/25  ****************************************
     C******             CALL      'W0090R'      ACDPL5
     C                   Move      DBRRN2        #aRRN
     C                   Move      'S'           ##MODE
     C                   Move      *blanks       ##NEXT
     C                   Move      *blanks       ##TAB
     C                   Move      *blank        ##RTCD
     C                   CALL      'WTPOLRBILL'
     C                   PARM                    #aRRN             9
     C                   PARM                    ##MODE
     C                   PARM                    ##NEXT           10
     C                   PARM                    ##TAB             5
     C                   PARM                    ##RTCD            1
     C                   Move      *off          *inkc
     C                   Move      *on           *inkl
      /Free

         //* Remove policy lock since session is not ended.
             If ##NEXT = 'CANCEL' or ##NEXT = 'EXIT';
                ClearPolicyLock(ELco#:ELfnd:ELemp#);
             Endif;

         //* Remove temporary files to allow next revised bill to recreate them.
             Clear strCommand;
             strCommand = 'DLTF FILE(QTEMP/WXEVL)';
             Callp QCMDEXC(strCommand:%len(strCommand));

             Clear strCommand;
             strCommand = 'DLTF FILE(QTEMP/WXEVP)';
             Callp QCMDEXC(strCommand:%len(strCommand));

      /End-Free
     C*** end chg *****  11/24/25  ****************************************
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
