     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H* PROGRAM: F0348R01
     H* DESCRIPTION: WORK WITH EOMBS
     H* PROJECT: FL021   C:117
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 06/09/20  C:212  Project : SY420
     H*                  Mask SSN
     H*
     H* 05/05/00  C:079  PROJ: FS211
     H*                  1. EOMB CODE IS REQUIRED ONLY IF PAID IS LT
     H*                     CHARGED.
     H*
     H* 12/16/99  C:117  PROJ: Y2K
     H*                  HAD TO CONVERT THE DATE BEFORE SAVING IT
     H*                  TO THE DATABASE.
     H*
     H*****************************************************************
     FF0348F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDCAP     UF A E             DISK
     F                                     RENAME(PCA01:DBRCDF)
     FFMPRLFL   IF   E           K DISK
     FFMRCL1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFTEBLFL   IF   E           K DISK
     F                                     RENAME(PEB01:PEB02)
     FFAEOP     IF   E             DISK
     FFAEOL32   IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FFTABL     IF   E           K DISK
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPS01)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************
      /COPY SCOPYMT01D
     D QCA             S             55    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Detail - Data Subm C-
     D                                     hgs')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  38     39

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFDAT
     C*
     C     RCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EOKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCNTL
     C                   KFLD                    KKEOMB
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA
     C*
     C     EBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    CASS#         KKSS#
     C     *LIKE         DEFINE    EOCNTL        KKCNTL
     C     *LIKE         DEFINE    EOEOB#        KKEOMB
     C     *LIKE         DEFINE    EODIA1        XXDIA
     C     *LIKE         DEFINE    EOCNTL        QQCNTL
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C     *LIKE         DEFINE    CACPT4        HLCPT4
     C*
     C/COPY SCOPYMT01C
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
     C*  Make sure that Diag code xref isn't blank and that it is valid
     C*
     C     SCDXR         IFEQ      *ZEROS
     C                   SETON                                        7190
     C                   ELSE
     C                   SETOFF                                       71
     C     SCDXR         IFNE      1
     C     SCDXR         ANDNE     2
     C     SCDXR         ANDNE     3
     C     SCDXR         ANDNE     4
     C                   SETON                                        7290
     C                   ELSE
     C                   SETOFF                                       72
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN71         IFEQ      *OFF
     C     *IN72         ANDEQ     *OFF
     C     SCDXR         ANDEQ     2
     C     SCDIA2        ANDEQ     *BLANKS
     C                   SETON                                        7690
     C                   ENDIF
     C*
     C     *IN71         IFEQ      *OFF
     C     *IN72         ANDEQ     *OFF
     C     SCDXR         ANDEQ     3
     C     SCDIA3        ANDEQ     *BLANKS
     C                   SETON                                        7690
     C                   ENDIF
     C*
     C     *IN71         IFEQ      *OFF
     C     *IN72         ANDEQ     *OFF
     C     SCDXR         ANDEQ     4
     C     SCDIA4        ANDEQ     *BLANKS
     C                   SETON                                        7690
     C                   ENDIF
     C*
     C*  Make sure EOB code isn't blank if paid is less than charged
     C*  and make sure it is a valid code.
     C     SCEOB         IFEQ      *BLANKS
     C     *IN55         ANDEQ     *OFF
     C                   SETON                                        7390
     C                   ELSE
     C*** BEG ADD *** 05/05/00 ****************************************
     C     SCEOB         IFNE      *BLANKS
     C*** END ADD *** 05/05/00 ****************************************
     C                   SETOFF                                       73
     C                   MOVEL     SCEOB         KKKEY
     C     EBKY          CHAIN     PEB02                              60
     C  N60              MOVEL     EBDESC        SCDSC3
     C   60              SETON                                        7490
     C  N60              SETOFF                                       74
     C*** BEG ADD *** 05/05/00 ****************************************
     C                   ENDIF                                                  SCEOB IFNE ' '
     C*** END ADD *** 05/05/00 ****************************************
     C                   ENDIF
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C*
     C     HFLD          WHENEQ    'SCBPRC'
     C*
     C                   MOVE      *BLANKS       QQCPT4
     C                   Z-ADD     *ZEROS        QQFDAT
     C                   CALL      'F0520R'
     C                   PARM                    QQPRDS           40
     C                   PARM      ' '           QQCALL            1
     C                   PARM                    QQCPT4            5
     C                   PARM                    QQFDAT            8 0
     C                   PARM      'FL'          SCAST             2
     C                   PARM                    QQRTCD            1
     C*
     C                   SETON                                        90
     C     QQCPT4        IFNE      *BLANKS
     C                   MOVEL     QQCPT4        SCBPRC
     C                   MOVEL     SCBPRC        KKCPT4
     C     CARCFL        IFEQ      'Y'
     C     RCKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDSC1        SCDSC2
     C                   ELSE
     C     PRKY          CHAIN     PPR01                              60
     C  N60              MOVEL     PRDSC1        SCDSC2
     C                   ENDIF
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCEOB'
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     'FL'          @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C***        STOPN     IFNE '1'                          NOT CURRENTLY OPEN
     C***                  OPEN FTABL
     C***                  MOVE '1'       STOPN   1
     C***                  END
     C*
     C                   MOVE      *BLANKS       QQXKEY           10
     C                   MOVEL     'FL'          QQST              2
     C                   CALL      'F2131R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQXKEY
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQST              2
     C*
     C                   SETON                                        90
     C     QQXKEY        IFNE      *BLANKS
     C                   MOVEL     QQXKEY        SCEOB
     C                   MOVEL     SCEOB         KKKEY
     C     EBKY          CHAIN     PEB02                              60
     C  N60              MOVEL     EBDESC        SCDSC3
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDXR'
     C                   SETON                                        9075
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     @@RRN2        CHAIN     PEO01                              90
     C*
     C                   MOVEL     EOCO#         KKCO#
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   MOVEL     EOFYR         KKFYR
     C                   MOVEL     EOCASE        KKCASE
     C*
     C                   MOVEL(P)  EOSUB#        XXSUB#            5
     C                   MOVEL(P)  EOFND         XXFND             3
     C                   MOVEL(P)  EOFYR         XXFYR             3
     C                   MOVEL(P)  EOCASE        XXCASE            7
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   MOVEL     CASS#         KKSS#
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*** Get Claimant name
     C                   MOVE      *BLANKS       SCCNAM
     C*
     C                   MOVEL(P)  CDLSN         ##LAST           56
     C                   MOVEL(P)  CDFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCCNAM
     C*
     C                   ENDIF                                                  *IN99
     C*
     C* CONCAT FOR CASE NUMBER
     C*
     C     XXSUB#        CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXFND:0       SCCASE
     C                   CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXFYR:0       SCCASE
     C                   CAT(P)    '-':0         SCCASE
     C                   CAT(P)    XXCASE:0      SCCASE
     C*
     C                   MOVE      EOEOB#        KKEOMB
     C                   MOVE      EOCNTL        KKCNTL
     C     EOKY1         CHAIN     PEO02                              60
     C                   MOVE      EOFEIN        SCFEIN
     C                   MOVE      EOMOD         SCMOD
     C                   MOVE      EOPNAM        SCPNAM
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C                   MOVE      EODIA1        SCDIA1
     C                   MOVE      EODIA2        SCDIA2
     C                   MOVE      EODIA3        SCDIA3
     C                   MOVE      EODIA4        SCDIA4
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      CASS#         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'CASS#':CASS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     CAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C                   MOVEL     CACPT4        SCCPT4
     C*
     C                   MOVEL     SCCPT4        KKCPT4
     C     CARCFL        IFEQ      'Y'
     C     RCKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDSC1        SCDSC1
     C                   ELSE
     C     PRKY          CHAIN     PPR01                              60
     C  N60              MOVEL     PRDSC1        SCDSC1
     C                   ENDIF
     C*
     C     CABPRC        IFNE      *BLANKS
     C                   MOVEL     CABPRC        SCBPRC
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   MOVE      *BLANKS       SCBPRC
     C                   ENDIF
     C*
     C                   MOVEL     SCBPRC        KKCPT4
     C     CARCFL        IFEQ      'Y'
     C     RCKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDSC1        SCDSC2
     C                   ELSE
     C     PRKY          CHAIN     PPR01                              60
     C  N60              MOVEL     PRDSC1        SCDSC2
     C                   ENDIF
     C*
     C                   MOVEL     CAPMOD        SCPMOD
     C                   MOVEL     CADXR         SCDXR
     C                   Z-ADD     CATOTC        SCTOTC
     C                   Z-ADD     CATOTP        SCTOTP
     C                   Z-ADD     CATOTD        SCTOTD
     C                   Z-ADD     CAOTHR        SCOTHR
     C                   Z-ADD     CANDIS        SCNTWK
     C*
     C     CAEOMB        IFNE      *BLANKS
     C                   MOVEL     CAEOMB        SCEOB
     C                   ELSE
     C                   MOVE      *BLANKS       SCEOB
     C                   MOVE      *BLANKS       SCDSC3
     C                   ENDIF
     C*
     C                   MOVEL     CAEOMB        KKKEY
     C     EBKY          CHAIN     PEB02                              60
     C  N60              MOVEL     EBDESC        SCDSC3
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCBPRC        CABPRC
     C                   MOVEL     SCEOB         CAEOMB
     C                   MOVEL     SCDXR         CADXR
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   MOVE      DSTIME        CATIME
     C*** BEG CHG *** 12/16/99 ****************************************
     C***********          MOVE DSDATE    CADT
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C*** END CHG *** 12/16/99 ****************************************
     C*
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
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*** CLOSE FILE IF OPENED
     C***        STOPN     IFEQ '1'
     C***                  CLOSEFTABL
     C***                  MOVE ' '       STOPN   1
     C***                  MOVE ' '       STOVR
     C***                  END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     EBKY          CHAIN     PEB01                              60
     C*
     C     CATOTC        SUB       CATOTP        XXDIFF            5 0
     C*** BEG CHG *** 05/05/00 ****************************************
     C********** XXDIFF    IFEQ 0
     C     XXDIFF        IFLE      0
     C*** END CHG *** 05/05/00 ****************************************
     C                   SETON                                        55
     C*** BEG ADD *** 05/05/00 ****************************************
     C                   ELSE
     C                   SETOFF                                       55
     C*** END ADD *** 05/05/00 ****************************************
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FDCAP
     C                   ENDSR
**  QCMDEXC TABLE
DLTOVR FILE(FTABL)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
