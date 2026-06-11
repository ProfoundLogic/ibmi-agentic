     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0070R7 - PERMANENT IMPAIRMENT BODY PART MAINT/INQ
     H* ***************************************************************
     H* DESCRIPTION:  THIS PROGRAM ALLOWS THE USER TO ENTER ADDL      *
     H*               BODY PART CODES AND PERCENTAGES ON A SPECIFIC   *
     H*               CLAIM.                                          *
     H*                                                               *
     H* CALLED FROM:  W0070R10                                        *
     H* CALLS TO:     NONE                                            *
     H* CREATED BY:   15                                              *
     H* PROJECT:      25WDE                01/10/96                   *
     H* ***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20 C:318 Project: SY420
     H*                 Mask SSN
     H*
     H* 10/19/07 C:191 Project: WC1584                                *
     H*                 EDI Release 3                                 *
     H*                                                               *
     H* 05/22/00 C:15  Project: WC468                                 *
     H*                Chgd edits to allow a zero percent impairment. *
     H*                                                               *
     H* 07/10/97  47  60ANC                                           *
     H*           UPDATED TO VERSION 3 SPECS.                         *
     H* ***************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*  *IN09  -  INDICATES MODE (MAINT/INQ)                         *
     H*  *IN20  -  INVALID ROLL REQUEST                               *
     H*  *IN21  -  INVALID TABLE VALUE FOUND                          *
     H*  *IN22  -  STATUS MUST BE 'A' OR 'I'                          *
     H*  *IN23  -  AMOUNT CANNOT BE LESS THAN ZERO                    *
     H*  *IN28  -  POSITION CURSOR ON BODY PART CODE                  *
     H*  *IN29  -  POSITION CURSOR ON PERCENTAGE                      *
     H*  *IN31  -  INDICATES LINE 1                                   *
     H*  *IN32  -  INDICATES LINE 2                                   *
     H*  *IN33  -  INDICATES LINE 3                                   *
     H*  *IN34  -  INDICATES LINE 4                                   *
     H*  *IN35  -  INDICATES LINE 5                                   *
     H*  *IN36  -  INDICATES LINE 6                                   *
     H*  *IN41  -  PROTECTS BODY PART CD FROM BEING CHANGED (LINE 1)  *
     H*  *IN42  -  PROTECTS BODY PART CD FROM BEING CHANGED (LINE 2)  *
     H*  *IN43  -  PROTECTS BODY PART CD FROM BEING CHANGED (LINE 3)  *
     H*  *IN44  -  PROTECTS BODY PART CD FROM BEING CHANGED (LINE 4)  *
     H*  *IN45  -  PROTECTS BODY PART CD FROM BEING CHANGED (LINE 5)  *
     H*  *IN46  -  PROTECTS BODY PART CD FROM BEING CHANGED (LINE 6)  *
     H*  *IN40  -  INDICATES WHETHER TO WRITE OR UPDATE RECORD        *
     H*  *IN60  -  GENERAL USAGE (CHAIN, READ, ETC..)                 *
     H*  *IN69  -  EXCESS REINSURANCE
     H*  *IN71-6   POSITION CURSOR
     H* ***************************************************************
     FW0070F7   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMBYL     UF A E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)

     D SQ              S              5  0 DIM(6)
     D OCD             S              2    DIM(6)
     D OPC             S              5  2 DIM(6)
     D OST             S              1    DIM(6)
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYS            S              5  0 DIM(2)
     D PRDS            S             40    DIM(2) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
      * CODE
     D                 DS
     D  CD                     1     12
     D                                     DIM(6)
     D  SCCD01                 1      2
     D  SCCD02                 3      4
     D  SCCD03                 5      6
     D  SCCD04                 7      8
     D  SCCD05                 9     10
     D  SCCD06                11     12
      * DESCRIPTION
     D                 DS
     D  DS                     1    180
     D                                     DIM(6)
     D  SCDS01                 1     30
     D  SCDS02                31     60
     D  SCDS03                61     90
     D  SCDS04                91    120
     D  SCDS05               121    150
     D  SCDS06               151    180
      * PERCENT IMPAIRMENT
     D                 DS
     D  PC                     1     30  2
     D                                     DIM(6)
     D  SCPC01                 1      5  2
     D  SCPC02                 6     10  2
     D  SCPC03                11     15  2
     D  SCPC04                16     20  2
     D  SCPC05                21     25  2
     D  SCPC06                26     30  2
      * STATUS
     D                 DS
     D  ST                     1      6
     D                                     DIM(6)
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D XCEXRE          C                   CONST('EXCS REIN ')
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display- ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0

     C*
     C*  KEY LIST FOR THE ADDITIONAL BODY PARTS FILE
     C*
     C     BYKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C     BYKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE TABLE FILES
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    BYKEY         XXKEY
     C     *LIKE         DEFINE    BYSEQ#        XXSEQ#
     C*
     C* ******************************************************** ********
     C*  *ENTRY PARAMETERS FOR PROGRAM
     C* ******************************************************** ********
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
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
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  MODEC         SCMODE
     C     QQCALL        COMP      '1'                                    09
     C  N09              MOVE      PRDS(1)       SCPRDS
     C   09              MOVE      PRDS(2)       SCPRDS
     C   09              MOVEL(P)  MODED         SCMODE
     C*
     C*  RETRIEVE THE USER PARAMETER, COMPANY AND FUND RECORDS
     C*
     C                   Z-ADD     0             XXCO#
     C***********          Z-ADD0         XXFND
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     UPCO#         XXCO#
     C***********          Z-ADDUPFND     XXFND
     C                   END
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     XXFND         SCFND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C*  RETRIEVE THE CLAIMANT INFORMATION
     C*
      *  FYR-CASE
     C                   MOVE(P)   XXFYR         XAFYR             3
     C                   MOVE(P)   XXCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCL
     C     SCYRCL        CAT       XACASE:0      SCYRCL
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     DBRRN         XXRRN             9 0
     C*
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   END
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C     CMEXCS        IFNE      'Y'
     C     XXRRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    XXRRN                          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   END
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL     CMACL#        SCSCL#
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
     C*
     C                   MOVEL     CMPART        SCPART
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C  N60              MOVEL     PADESC        SCBPDS
     C*
     C*  RETRIEVE THE CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              60
     C                   MOVE      *BLANKS       SCCNAM
     C                   MOVEL     CDLSN         SCCNAM
     C     CDLSN         IFNE      *BLANKS
     C     SCCNAM        CAT       ',':0         SCCNAM
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ELSE
     C                   MOVEL     CDFSN         SCCNAM
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C                   ENDIF
     C*
     C                   END                                                    *IN60
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        KEYS
     C                   MOVE      '1'           XXDOWN            1
     C     BYKY1         CHAIN(N)  PBY01                              60
     C*
     C                   EXSR      LOAD
     C*
     C*  POSITION CURSOR ON FIRST BLANK CODE SPACE
     C*
     C                   DO        6             Y                 1 0
     C     CD(Y)         IFEQ      *BLANK
     C     70            ADD       Y             Y1                3 0
     C                   MOVE      '1'           *IN(Y1)
     C                   LEAVE
     C                   END
     C                   ENDDO
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C                   MOVEL     RR(2)         SCDOWN
     C                   END
     C*
     C                   EXFMT     SCR01
     C                   MOVEA     '000000'      *IN(71)
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*   F12 = CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C*
     C*   F10 = TABLE FILE SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*   - BODY PART SEARCH
     C*
     C     HFLD          IFEQ      'SCCD01'
     C     HFLD          OREQ      'SCCD02'
     C     HFLD          OREQ      'SCCD03'
     C     HFLD          OREQ      'SCCD04'
     C     HFLD          OREQ      'SCCD05'
     C     HFLD          OREQ      'SCCD06'
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   CALL      'W2126R'
     C                   PARM      '2'           QQCALL
     C     *INKC         PARM      '0'           QQKC              1
     C     *INKL         PARM      '0'           QQKL              1
     C                   PARM                    XXKEY
     C                   PARM      2             QQVIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      STRT
     C                   END
     C*
     C     *INKL         IFEQ      *ON
     C     XXKEY         OREQ      *BLANK
     C     RTRN          TAG
     C                   MOVE      '1'           *IN28
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCD01'
     C                   MOVE      *ON           *IN31
     C                   MOVEL     XXKEY         SCCD01
     C                   Z-ADD     1             X
     C*
     C     HFLD          WHENEQ    'SCCD02'
     C                   MOVE      *ON           *IN32
     C                   MOVEL     XXKEY         SCCD02
     C                   Z-ADD     2             X
     C*
     C     HFLD          WHENEQ    'SCCD03'
     C                   MOVE      *ON           *IN33
     C                   MOVEL     XXKEY         SCCD03
     C                   Z-ADD     3             X
     C*
     C     HFLD          WHENEQ    'SCCD04'
     C                   MOVE      *ON           *IN34
     C                   MOVEL     XXKEY         SCCD04
     C                   Z-ADD     4             X
     C*
     C     HFLD          WHENEQ    'SCCD05'
     C                   MOVE      *ON           *IN35
     C                   MOVEL     XXKEY         SCCD05
     C                   Z-ADD     5             X
     C*
     C     HFLD          WHENEQ    'SCCD06'
     C                   MOVE      *ON           *IN36
     C                   MOVEL     XXKEY         SCCD06
     C                   Z-ADD     6             X
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C     XXKEY         IFNE      *BLANK
     C     TBKY          CHAIN     PPA01                              60
     C     PASTAT        CABEQ     'I'           RTRN
     C                   MOVEL     PADESC        DS(X)
     C                   MOVE      PASTAT        ST(X)
     C*** BEG ADD *** 10/19/07 ******************************
     C                   MOVE      *ZEROS        PC(X)
     C*** END ADD *** 10/19/07 ******************************
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    HFLD = SCCD01-6
     C                   GOTO      STRT
     C                   END                                                    *INKJ
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
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
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END OF CMD KEYS
     C* ************************************************************************
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   EXSR      RITE
     C     XXUP          IFNE      *BLANK
     C     CD(6)         ANDEQ     *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     *IN19         IFEQ      '1'
     C                   EXSR      RITE
     C     XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYS(2)       XXSEQ#
     C     BYKY          CHAIN(N)  PBY01                              60
     C     BYKY1         READE(N)  PBY01                                  60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYS(1)       XXSEQ#
     C     XXSEQ#        IFEQ      0
     C     BYKY1         SETGT     PBY01
     C                   ELSE
     C     BYKY          CHAIN(N)  PBY01                              60
     C                   END
     C     BYKY1         READPE(N) PBY01                                  60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  PROCESS ANY CHANGES.
     C*
     C                   EXSR      RITE
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     C*  CLEAR THE SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       DS
     C                   MOVE      *ZEROS        PC
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *ZEROS        SQ
     C                   MOVE      *BLANKS       OCD
     C                   MOVE      *BLANKS       OPC
     C                   MOVE      *BLANKS       OST
     C                   MOVEA     '000000'      *IN(41)
     C                   Z-ADD     *ZERO         I1                3 0          ARRAY INDEX
     C                   ENDSR
     C*
     C**************************************************************************
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVEL     BYKEY         CD(I1)
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     BYKEY         XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C  N60              MOVEL     PADESC        DS(I1)
     C                   Z-ADD     BYPCT         PC(I1)
     C                   MOVE      BYSTAT        ST(I1)
     C                   Z-ADD     BYSEQ#        SQ(I1)
     C                   MOVE      CD(I1)        OCD(I1)
     C                   MOVE      PC(I1)        OPC(I1)
     C                   MOVE      ST(I1)        OST(I1)
     C     I1            ADD       40            X1
     C                   MOVE      '1'           *IN(X1)
     C                   ENDSR
     C*
     C**************************************************************************
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVER          TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     BYKY1         READE(N)  PBY01                                  60
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C     I1            COMP      6                                    61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     SQ(1)         KEYS(1)
     C                   Z-ADD     SQ(6)         KEYS(2)
     C                   ENDSR
     C*
     C**************************************************************************
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     7             I1
     C*
     C     OVERP         TAG
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C     BYKY1         READPE(N) PBY01                                  60
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     SQ(1)         KEYS(1)
     C                   Z-ADD     SQ(6)         KEYS(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C**************************************************************************
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN
     C                   MOVE      *BLANK        XXUP
     C     BYKY1         CHAIN(N)  PBY01                              60
     C   60              MOVE      '1'           XXUP
     C   60              GOTO      DONEB
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     BYKY1         READE(N)  PBY01                                  60
     C   60              MOVE      '1'           XXUP
     C   60              GOTO      DONEB
     C     I1            COMP      6                                    61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C**************************************************************
     C*     RITE SUBROUTINE - UPDATE RECORDS                       *
     C**************************************************************
     C     RITE          BEGSR
     C*
     C*   TURN OFF POSITION CURSOR INDICATORS IF ROLLING UP OR DOWN
     C*
     C                   MOVEA     '000000'      *IN(31)
     C*
     C                   DO        6             X                 3 0
     C     X             ADD       30            X1                2 0          ERROR
     C*
     C*   COMPARE HOLD FIELDS FROM ORIGINAL SCREEN TO THE CURRENT
     C*   FIELDS. IF THE FIELDS CHANGED, EDIT & UPDATE OR WRITE.
     C*
     C     CD(X)         IFNE      OCD(X)
     C     PC(X)         ORNE      OPC(X)
     C     ST(X)         ORNE      OST(X)
     C*
     C*  VALIDATE ANY NEW CODES WHICH WERE ENTERED.
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CD(X)         XXKEY
     C     TBKY          CHAIN     PPA01                              21
     C     *IN21         IFEQ      '1'
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE ANY NEW CODES WHICH WERE ENTERED.
     C*
     C*** BEGIN CHG *** 05/22/00 **************************************
     C********** PC,X      IFLE 0
     C     PC(X)         IFLT      0
     C*** END CHG ***** 05/22/00 **************************************
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE STATUS CODE IF IT WAS CHANGED.
     C*
     C     ST(X)         IFNE      *BLANK
     C     ST(X)         ANDNE     'A'
     C     ST(X)         ANDNE     'I'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN(X1)
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE SEQ# IS 0, RETRIEVE THE NEXT AVAILABLE SEQ#.
     C*
     C     SQ(X)         IFEQ      0
     C     BYKY1         SETGT     PBY01
     C     BYKY1         READPE(N) PBY01                                  60
     C  N60BYSEQ#        ADD       1             SQ(X)
     C   60              Z-ADD     1             SQ(X)
     C                   END
     C*
     C*  WRITE A NEW RECORD OR UPDATE AN EXISTING RECORD.
     C*
     C                   MOVEL     SQ(X)         XXSEQ#
     C     BYKY          CHAIN     PBY01                              40
     C*
     C                   Z-ADD     XXCO#         BYCO#
     C                   Z-ADD     XXFND         BYFND
     C                   Z-ADD     CMFYR         BYFYR
     C                   Z-ADD     CMCASE        BYCASE
     C                   MOVEL     CD(X)         BYKEY
     C                   Z-ADD     PC(X)         BYPCT
     C                   Z-ADD     XXSEQ#        BYSEQ#
     C                   MOVE      ST(X)         BYSTAT
     C     ST(X)         IFEQ      *BLANK
     C                   MOVE      'A'           BYSTAT
     C                   END
     C*
     C                   MOVE      Q1USER        BYUSER
     C                   MOVE      Q1PGM         BYPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BYDT
     C*
     C                   Z-ADD     DSTIME        BYTIME
     C*
     C     *IN40         IFEQ      '0'
     C     BYSTAT        IFEQ      'I'
     C                   DELETE    PBY01
     C                   ELSE
     C                   UPDATE    PBY01
     C                   ENDIF
     C                   ELSE
     C                   WRITE     PBY01
     C                   END
     C*
     C*  MOVE NEW FIELDS TO OLD FIELDS
     C*
     C                   MOVE      CD(X)         OCD(X)
     C                   Z-ADD     PC(X)         OPC(X)
     C                   MOVE      ST(X)         OST(X)
     C*
     C                   END
     C                   END
     C*
     C                   Z-ADD     SQ(1)         KEYS(1)
     C                   Z-ADD     SQ(6)         KEYS(2)
     C*
     C                   ENDSR
     C**************************************************************************
**
ROLL UP
ROLL DOWN
** PGM DESC
    IMPAIRMENT BODY PART MAINTENANCE
      IMPAIRMENT BODY PART INQUIRY
