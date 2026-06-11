     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0483R - Dental Detail Information FOR EDI CHG
     H*****************************************************************
     H*                                                               *
     H* F0483R - Dental Detail Information FOR EDI CHG
     H*                                                               *
     H*****************************************************************
     H* Created on 02/21/2003 by c:62 for Project FL027               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  10/12/15  C:212  Supp: 37776                                 *
     H*                   Increase Diag codes from 5A to 7A and       *
     H*                   related ICD10 changes                       *
     H*                                                               *
     H*  05/14/14  C:296 Project: NC034                               *
     H*                  Remove dental code check for EDI flag = Y.   *
     H*                                                               *
     H*  06/24/08  C:198  PROJ: SY156                                 *
     H*                   Corrected help on SCBPRC to be taken from   *
     H*                   FMPRPTX.                                    *
     H*                                                               *
     H*  04/30/08  C:191  PROJ: TX045                                 *
     H*                   TX EDI Correction required fields           *
     H*                                                               *
     H*****************************************************************
     FF0483F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDDDP     UF A E             DISK
     F                                     RENAME(PDD01:DBRCDF)
     FFDDDL     IF   E           K DISK
     F*  Procedure Code File
     FFMPRL     IF   E           K DISK
     F                                     INFDS(DBFPR)
     FFMRCL1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFAEOP     IF   E             DISK
     FFDCAP     IF   E             DISK
     FFAEDL     IF   E           K DISK
     FFMCXL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     F***FMVCLTX IF  E           K        DISK
     F*** BEG ADD ****** 04/30/08 ************************************
     FFMPRLTX   IF   E           K DISK
     F                                     RENAME(PPR01:PTX01)
     F*** END ADD ****** 04/30/08 ************************************
     F*** Beg Add *** 10/12/15 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 10/12/15 ***************************************
     F*
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Dental Detail Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D*** BEG ADD ****** 04/30/08 ************************************
     D NT0             C                   CONST('Field was changed fo-
     D                                     r EDI correction:')
     D NT1             C                   CONST('Original Value:')
     D FL1const        C                   CONST('Paid procedure code')
     D                 DS
     D  XXNOT1                 1     70
     D  XXNT1                  1     38
     D  XXNT2                 39     70
     D                 DS
     D  XXNOT2                 1     70
     D  XXNT21                 1     16
     D  XXNT22                17     70

     I*** END ADD ****** 04/30/08 ************************************

     D*** Beg Add *** 10/12/15 ***************************************
     D kkicd10         s                   like(iccode)
     D*** End Add *** 10/12/15 ***************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FDDHP
     C                   PARM                    @@RRN2            9 0          FAEOP
     C                   PARM                    @@RRN4            9 0          FDCAP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C*
     C     APIPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EOSUB#        #2SUB#            5 0
     C                   PARM      EOFND         #2FND             3 0
     C                   PARM      EOFYR         #2FYR             3 0
     C                   PARM      EOCASE        #2CASE            7 0
     C                   PARM      EOEOB#        #2EOB#            9 0
     C                   PARM      EORCN         #2RCN             3 0
     C                   PARM      DDDSEQ        #2DSEQ            5 0
     C                   PARM      'D'           #2CALL            1
     C                   PARM                    #2OVAL           70
     C                   PARM                    #2NVAL           70
     C*** END ADD ****** 04/30/08 ************************************
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
     C     DIKY          KLIST
     C                   KFLD                    XXDIA
     C*
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*  Key List for Header File
     C*
     C     EDKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*  Key List for Detail File
     C*
     C     DDKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C*  KEY LIST FOR VALID STATE CODE FILE
     C*
     C     VCKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKFDAT
     C*
     C     VCKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DDCO#         KKCO#
     C     *LIKE         DEFINE    DDSUB#        KKSUB#
     C     *LIKE         DEFINE    DDFND         KKFND
     C     *LIKE         DEFINE    DDFYR         KKFYR
     C     *LIKE         DEFINE    DDCASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    DDSS#         KKSS#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EODIA1        XXDIA
     C     *LIKE         DEFINE    EOCNTL        QQCNTL
     C     *LIKE         DEFINE    CACPT4        HLCPT4
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C*** BEG ADD ****** 04/30/08 *************************************
     C     *LIKE         DEFINE    DDPRCD        XOCPT4
     C*** END ADD ****** 04/30/08 *************************************
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
     C     EOAST         IFNE      'TX'
     C*
     C*  Make sure that the CPT code is in the table file
     C*
     C                   MOVE      SCBPRC        KKCPT4
     C*
     C     PRKY          SETLL     PPR01
     C     NXTCDE        TAG
     C     PRKY          READE     PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     PRSTAT        ANDEQ     'I'
     C*
     C                   GOTO      NXTCDE
     C                   ENDIF
     C*
     C* Not found or user defined
     C*
     C     *IN99         IFEQ      *ON
     C     PR1AG         OREQ      *ON
     C*
     C                   SETON                                        7090
     C                   GOTO      CK1END
     C*
     C                   ENDIF
     C                   ELSE
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C*
     C* VALIDATE CPT4 FOR TEXAS
     C                   MOVE      SCCPT4        KKCPT4
     C*
     C     PRKY          SETLL     PTX01
     C     PRKY          READE     PTX01                                  99
     C     *IN99         DOWEQ     *OFF
     C     PRSTAT        IFNE      'I'
     C                   MOVEL(P)  PRDSC1        SCDSC1
     C                   LEAVE
     C                   ENDIF
     C     PRKY          READE     PTX01                                  99
     C                   ENDDO
     C* NOT FOUND
     C     *IN99         IFEQ      *ON
     C     PR1AG         OREQ      *ON
     C                   SETON                                        9057
     C                   GOTO      CK1END
     C                   ENDIF
     C*** END ADD ****** 04/30/08 ************************************
     C                   MOVEL(P)  SCBPRC        KKCPT4
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFDAT
     C*
     C     PRKY          SETLL     PTX01
     C     PRKY          READE     PTX01                                  99
     C     *IN99         DOWEQ     *OFF
     C     PRSTAT        IFNE      'I'
     C                   MOVEL(P)  PRDSC1        SCDSC2
     C                   LEAVE
     C                   ENDIF
     C     PRKY          READE     PTX01                                  99
     C                   ENDDO
     C* NOT FOUND
     C     *IN99         IFEQ      *ON
     C     PR1AG         OREQ      *ON
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C********** VCKY1     SETGTPVC01
     C********** VCKY2     REDPEPVC01                    99
     C*
     C********** *IN99     IFEQ *ON
     C********** VCSTAT    OREQ 'I'
     C**********           SETON                     9089
     C**********           ENDIF
     C*
     C                   ENDIF
     C*
     C***** Beg Del ***** 05/14/14 ******************************
     C***** IF EDI THEN MUST BE DENTAL CODE
     C*****
     C*****EDEDI         IFEQ      'Y'
     C*****SCBPRC        IFLT      'D0001'
     C*****SCBPRC        ORGT      'D9999'
     C*****              SETON                                        8890
     C*****              GOTO      CK1END
     C*****              ENDIF
     C*****              ENDIF
     C***** End Del ***** 05/14/14 ******************************
     C*
     C*  Make sure that Diag code xref isn't blank and that it is valid
     C*
     C     *IN28         IFEQ      *ON
     C     *IN29         OREQ      *ON
     C     *IN30         OREQ      *ON
     C     *IN31         OREQ      *ON
     C     SCDXR1        IFEQ      *BLANKS
     C     SCDXR2        ANDEQ     *BLANKS
     C     SCDXR3        ANDEQ     *BLANKS
     C     SCDXR4        ANDEQ     *BLANKS
     C                   SETON                                        7190
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       71
     C     *IN28         IFEQ      *ON
     C     SCDXR1        IFNE      'X'
     C     SCDXR1        ANDNE     ' '
     C                   SETON                                        7290
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       72
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN29         IFEQ      *ON
     C     SCDXR2        IFNE      'X'
     C     SCDXR2        ANDNE     ' '
     C                   SETON                                        7390
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       73
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN30         IFEQ      *ON
     C     SCDXR3        IFNE      'X'
     C     SCDXR3        ANDNE     ' '
     C                   SETON                                        7490
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       74
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN31         IFEQ      *ON
     C     SCDXR4        IFNE      'X'
     C     SCDXR4        ANDNE     ' '
     C                   SETON                                        7590
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       75
     C                   ENDIF
     C                   ENDIF
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
     C     HFLD          WHENEQ    'SCBPRC'
     C*
     C*** BEG DEL ******** 06/24/08 ************************************
     C******     EOAST     IFNE 'TX'
     C*** END DEL ******** 06/24/08 ************************************
     C                   MOVE      SCCPT4        HLCPT4
     C                   MOVEL(P)  SCDSC1        HLDESC           40
     C                   MOVE      *BLANKS       ##PROC            5
     C                   MOVE      *BLANKS       ##PROD           50
     C                   Z-ADD     0             ##RRN1            9 0
     C                   MOVE      EOAST         ##AST             2
     C                   CALL      'F2013R'
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN1
     C                   PARM                    ##PROC
     C                   PARM                    ##PROD
     C                   PARM      ' '           ##RTCD
     C                   PARM                    ##AST
     C*
     C     ##RTCD        IFEQ      '1'
     C     ##PROD        OREQ      *BLANKS
     C                   MOVEL     HLCPT4        SCBPRC
     C                   MOVEL(P)  HLDESC        SCDSC2
     C                   SETON                                        9054      pos cursor
     C                   ELSE
     C                   MOVEL     ##PROC        SCBPRC
     C                   MOVEL(P)  ##PROD        SCDSC2
     C                   SETON                                        9034
     C                   ENDIF                                                  ##RTCD = 1
     C*** BEG DEL ******** 06/24/08 ************************************
     C********             ELSE
     C********             CALL 'F2532R'
     C********             PARM '2'       ##CALL  1
     C********             PARM '0'       ##KC
     C********             PARM '0'       ##KL
     C********             PARM EOAST     ##AST
     C********             PARM *BLANKS   ##CPT4 15
     C********             PARM *BLANKS   ##DESC 40
     C*
     C********   ##KC      IFEQ '1'
     C********   ##KL      OREQ '1'
     C********   ##CPT4    OREQ *BLANKS
     C********             SETON                     5490  pos cursor
     C********             ELSE
     C********             MOVEL##CPT4    SCBPRC    P
     C********             MOVEL##DESC    SCDSC2    P
     C********             SETON                     3490  pos cursor
     C********             ENDIF
     C********             ENDIF
     C*** END DEL ******** 06/24/08 ************************************
     C*
     C     HFLD          WHENEQ    'SCDXR1'
     C                   SETON                                        9076
     C     HFLD          WHENEQ    'SCDXR2'
     C                   SETON                                        9077
     C     HFLD          WHENEQ    'SCDXR3'
     C                   SETON                                        9078
     C     HFLD          WHENEQ    'SCDXR4'
     C                   SETON                                        9079
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C     HFLD          WHENEQ    'SCCPT4'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE(P)   SCCPT4        XXCPT4            5
     C                   MOVEL(P)  SCDSC1        XXDSC1           40
     C                   MOVE      *BLANKS       ##PROC            5
     C                   MOVE      *BLANKS       ##PROD           50
     C                   Z-ADD     0             #FRRN1            9 0
     C                   MOVE      EOAST         #FAST             2
     C                   CALL      'F2013R'
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    #FRRN1
     C                   PARM                    ##PROC
     C                   PARM                    ##PROD
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #FAST
     C*
     C     ##RTCD        IFEQ      '1'
     C     ##PROD        OREQ      *BLANKS
     C                   MOVEL(P)  XXCPT4        SCCPT4
     C                   MOVE(P)   XXDSC1        SCDSC1
     C                   SETON                                        9058      pos cursor
     C                   ELSE
     C                   MOVEL(P)  ##PROC        SCCPT4
     C                   MOVEL(P)  ##PROD        SCDSC1
     C                   SETON                                        9054      pos cursor
     C                   ENDIF                                                  ##RTCD=' '
     C*** END ADD ****** 04/30/08 ************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCDXR1
     C                   MOVE      *BLANKS       SCDIA1
     C                   MOVE      *BLANKS       SCDS1
     C                   MOVE      *BLANKS       SCDXR2
     C                   MOVE      *BLANKS       SCDIA2
     C                   MOVE      *BLANKS       SCDS2
     C                   MOVE      *BLANKS       SCDXR3
     C                   MOVE      *BLANKS       SCDIA3
     C                   MOVE      *BLANKS       SCDS3
     C                   MOVE      *BLANKS       SCDXR4
     C                   MOVE      *BLANKS       SCDIA4
     C                   MOVE      *BLANKS       SCDS4
     C*
     C     @@RRN2        CHAIN     PEO01                              90
     C*
     C                   MOVEL     EOCO#         KKCO#
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   MOVEL     EOFYR         KKFYR
     C                   MOVEL     EOCASE        KKCASE
     C                   MOVEL     EOEOB#        KKEOB#
     C*
     C** Get 'Amount & type code' headings
     C*
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMR1
     C                   MOVEL     CXHNET        SCHNTW
     C                   MOVEL     CXHOT1        SCHOT1
     C                   MOVEL     CXHOT2        SCHOT2
     C*
     C                   MOVEL(P)  EOSUB#        XXSUB#            5
     C                   MOVEL(P)  EOFND         XXFND             3
     C                   MOVEL(P)  EOFYR         XXFYR             3
     C                   MOVEL(P)  EOCASE        XXCASE            7
     C                   MOVEL     EOEOB#        SCEOMB
     C                   MOVEL     EOCNTL        SCCNTL
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     EDKY          CHAIN     PED01                              60
     C     @@RRN4        CHAIN     PCA01                              60
     C                   MOVEL     CADSEQ        KKDSEQ
     C*
     C*** BEG ADD ****** 10/12/15 ************************************
     C                   If        EDICDIND <> 'Y'
     C*** END ADD ****** 10/12/15 ************************************
     C     EDDIA1        IFNE      *BLANKS
     C                   SETON                                        28
     C                   ELSE
     C                   SETOFF                                       28
     C                   ENDIF                                                  EDDIA1 NE *BLANKS
     C*
     C     EDDIA2        IFNE      *BLANKS
     C                   SETON                                        29
     C                   ELSE
     C                   SETOFF                                       29
     C                   ENDIF                                                  EDDIA2 NE *BLANKS
     C*
     C     EDDIA3        IFNE      *BLANKS
     C                   SETON                                        30
     C                   ELSE
     C                   SETOFF                                       30
     C                   ENDIF                                                  EDDIA3 NE *BLANKS
     C*
     C     EDDIA4        IFNE      *BLANKS
     C                   SETON                                        31
     C                   ELSE
     C                   SETOFF                                       31
     C                   ENDIF                                                  EDDIA4 NE *BLANKS
     C*
     C*** BEG ADD ****** 10/12/15 ************************************
     C                   Else

     C     EDICD1001     IFNE      *BLANKS
     C                   SETON                                        28
     C                   ELSE
     C                   SETOFF                                       28
     C                   ENDIF                                                  EDDIA1 NE *BLANKS
     C*
     C     EDICD1002     IFNE      *BLANKS
     C                   SETON                                        29
     C                   ELSE
     C                   SETOFF                                       29
     C                   ENDIF                                                  EDDIA2 NE *BLANKS
     C*
     C     EDICD1003     IFNE      *BLANKS
     C                   SETON                                        30
     C                   ELSE
     C                   SETOFF                                       30
     C                   ENDIF                                                  EDDIA3 NE *BLANKS
     C*
     C     EDICD1004     IFNE      *BLANKS
     C                   SETON                                        31
     C                   ELSE
     C                   SETOFF                                       31
     C                   ENDIF                                                  EDDIA4 NE *BLANKS

     C                   EndIf
     C*** END ADD ****** 10/12/15 ************************************
     C*
     C     DDKY          CHAIN(N)  PDD01                              60
     C*
     C*** BEG ADD ****** 10/12/15 ************************************
     C                   If        EDICDIND <> 'Y'
     C*** END ADD ****** 10/12/15 ************************************
     C                   MOVEL     EDDIA1        SCDIA1
     C                   MOVEL     EDDIA1        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDS1
     C*
     C                   MOVEL     EDDIA2        SCDIA2
     C                   MOVEL     EDDIA2        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDS2
     C*
     C                   MOVEL     EDDIA3        SCDIA3
     C                   MOVEL     EDDIA3        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDS3
     C*
     C                   MOVEL     EDDIA4        SCDIA4
     C                   MOVEL     EDDIA4        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDS4
     C*
     C*** BEG ADD ****** 10/12/15 ************************************
     C                   Else
     C/free
        If EDICD1001 <> *blanks;
           SCDIA1 = EDICD1001;
           kkICD10 = SCDIA1;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDS1 = ICDSC1;
           EndIf;
        EndIf;

        If EDICD1002 <> *blanks;
           SCDIA2 = EDICD1002;
           kkICD10 = SCDIA2;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDS2 = ICDSC1;
           EndIf;
        EndIf;

        If EDICD1003 <> *blanks;
           SCDIA3 = EDICD1003;
           kkICD10 = SCDIA3;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDS3 = ICDSC1;
           EndIf;
        EndIf;

        If EDICD1004 <> *blanks;
           SCDIA4 = EDICD1004;
           kkICD10 = SCDIA4;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDS4 = ICDSC1;
           EndIf;
        EndIf;

      /end-free
     C                   EndIf
     C*** END ADD ****** 10/12/15 ************************************
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C                   MOVEL     DDPRCD        SCCPT4
     C                   MOVEL(P)  DDPRDS        SCDSC1
     C*** BEG ADD ****** 04/30/08 ************************************
     C                   MOVEL     DDPRCD        XOCPT4
     C                   MOVE      SCCPT4        KKCPT4
     C*** END ADD ****** 04/30/08 ************************************
     C*
     C     DDBPRC        IFNE      *BLANKS
     C                   MOVEL     DDBPRC        SCBPRC
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   ENDIF
     C*
     C     EOAST         IFNE      'TX'
     C                   MOVEL     SCBPRC        KKCPT4
     C     DDRCFL        IFEQ      'Y'
     C     RCKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDSC1        SCDSC2
     C                   ELSE
     C     PRKY          CHAIN     PPR01                              60
     C  N60              MOVEL     PRDSC1        SCDSC2
     C                   ENDIF
     C                   ELSE
     C     PRKY          CHAIN     PPR01                              60
     C  N60              MOVEL     PRDSC1        SCDSC2
     C********** VCKY2     CHAINPVC01                60
     C**N60*****           MOVELVCDSC1    SCDSC2
     C                   ENDIF
     C*
     C                   MOVEL     DDDXR1        SCDXR1
     C                   MOVEL     DDDXR2        SCDXR2
     C                   MOVEL     DDDXR3        SCDXR3
     C                   MOVEL     DDDXR4        SCDXR4
     C                   Z-ADD     CACHRG        SCTOTC
     C                   Z-ADD     CAPAID        SCTOTP
     C                   Z-ADD     CAMRA         SCMRA
     C                   Z-ADD     CANDIS        SCNTWK
     C                   Z-ADD     CAOTHR        SCOTH1
     C                   Z-ADD     CAOTH2        SCOTH2
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCBPRC        DDBPRC
     C                   MOVEL     SCDXR1        DDDXR1
     C                   MOVEL     SCDXR2        DDDXR2
     C                   MOVEL     SCDXR3        DDDXR3
     C                   MOVEL     SCDXR4        DDDXR4
     C*** BEG ADD ****** 04/30/08 ************************************
     C                   MOVEL     SCCPT4        DDPRCD
     C*** END ADD ****** 04/30/08 ************************************
     C*
     C                   MOVE      Q1USER        DDUSER
     C                   MOVE      Q1PGM         DDPGM
     C                   MOVE      DSTIME        DDTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
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
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*** BEG ADD ****** 04/30/08 ************************************
     C     EOAST         IFEQ      'TX'
     C                   SETON                                        59
     C                   ELSE
     C                   SETOFF                                       59
     C                   ENDIF
     C*** END ADD ****** 04/30/08 ************************************
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
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
     C*** BEG ADD ****** 04/30/08 ************************************
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FDDDP
     C*** BEG ADD ****** 04/30/08 ************************************
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
     C                   ENDSR
     C*** BEG ADD ****** 04/30/08 ************************************
     C*================================================================
     C* SRWREP - Write header note record
     C*================================================================
     C     SRWREP        BEGSR
     C* IF FIELDS CHANGED THEN CALL API
     C*
     C     DDPRCD        IFNE      XOCPT4
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL1const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOCPT4        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DDPRCD
     C*
     C                   ENDSR
     C*** END ADD ****** 04/30/08 ************************************
