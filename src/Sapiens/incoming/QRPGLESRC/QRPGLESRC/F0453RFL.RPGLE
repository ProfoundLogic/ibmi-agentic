     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0453R - Work With EOMBs Detail Information              *
     H*****************************************************************
     H* F0453R - Work With EOMBs Detail Information                   *
     H* Project: FL021  C:117                                         *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  09/01/16  C:212 FL091 Phase 2                                *
     H*                  Renamed Dia1/2/3/4 to A/B/C/D                *
     H*                  Added Dia E-L and related changes            *
     H*                                                               *
     H*  10/12/15  C:212  Supp: 37776                                 *
     H*                   Increase Diag codes from 5A to 7A and       *
     H*                   related ICD10 changes                       *
     H*                                                               *
     H*  01/19/07  C:062  PROJ: MC010 56556                           *
     H*                   REMOVE NDC CODE EDIT.                       *
     H*                                                               *
     H*  01/27/06  C:079  PROJ: MCMNT                                 *
     H*                   1. OPEN UP PAID CODE FIELD                  *
     H*                   2. UNDELETE PLACE OF SERVICE CODE THAT WAS  *
     H*                      HARD DELETED ON 05/10/00
     H*                                                               *
     H*  12/23/05  C:079  PROJ: FL056                                 *
     H*                   ADD NDC CODE                                *
     H*                                                               *
     H*  10/09/03  C:062  PROJ: FL045                                 *
     H*                   ALLOW THE MODIFIERS TO BE CHANGED           *
     H*                                                               *
     H*  08/27/01  C:062  PROJ: TX015                                 *
     H*                   ENHANCED THE BILLED CODE EDIT               *
     H*                                                               *
     H*  08/13/01  C:079  PROJ: FLMNT                                 *
     H*                   ADDED EDIT ON BILLED CODE                   *
     H*                                                               *
     H*  TEMPORARILY DELETING CODE DUE TO UNRESOLVED ISSUE            *
     H*  05/10/00  C:117  PROJ: MCMNT                                 *
     H*                   ADDED POS TO DETAIL SCREEN                  *
     H*                                                               *
     H*****************************************************************
     FF0453FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDDHP     UF A E             DISK
     F                                     RENAME(PDH01:DBRCDF)
     FFDDHL     IF   E           K DISK
     F*  Procedure Code File
     FFMPRL     IF   E           K DISK
     F                                     INFDS(DBFPR)
     FFMRCL1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFAEOP     IF   E             DISK
     FFDCAP     IF   E             DISK
     FFAEHL     IF   E           K DISK
     F***FTABL   IF  E           K        DISK
     F***         PRC01                             KIGNORE
     F***         PPS01                             KIGNORE
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     FFMDIL     IF   E           K DISK
     F*** BEG ADD *** 05/10/00 ****************************************
     F*  Place of Service Table File
     FFTPSL     IF   E           K DISK
     F*** END ADD *** 05/10/00 ****************************************
     F*** BEGIN ADD *** 08/27/01 **************************************
     FFMVCLTX   IF   E           K DISK
     F*** END   ADD *** 08/27/01 **************************************
     F*** BEGIN ADD *** 10/09/03 **************************************
     FFTABL     IF   E           K DISK    USROPN
     F                                     RENAME(PTS01:PTS02)
     F                                     IGNORE(PAC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PFT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PPS01)
     F*** END   ADD *** 10/09/03 **************************************
     F*
     F*** BEG ADD *** 12/23/05 ****************************************
     F*
     F*  WCMC NDC Drug Code File
     FFMRXP     IF   E             DISK
     F                                     RENAME(PRX01:PRX02)
     FFMRXL     IF   E           K DISK
     F*** END ADD *** 12/23/05 ****************************************
     F*** BEG ADD *** 01/27/06 ****************************************
     FFDDEL     IF   E           K DISK
     FFDDEP     O    E             DISK
     F                                     RENAME(PDE01:PDE02)
     F*** END ADD *** 01/27/06 ****************************************
     F*** Beg Add *** 10/12/15 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 10/12/15 ***************************************
      /COPY SCOPYMT01D
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
     D*** BEG ADD *** 01/27/06 ****************************************
     D NT1             C                   CONST('Paid code ')
     D NT3             C                   CONST(' was changed for EDI-
     D                                      correction')
     D*** END ADD *** 01/27/06 ****************************************
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D*** BEG ADD *** 01/27/06 ****************************************
     D                 DS
     D  XXNOT1                 1     46
     D  XXNT1                  1     10
     D  XXNT2                 11     15
     D  XXNT3                 16     46

     I*** END ADD *** 01/27/06 ****************************************

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
     C*** BEGIN ADD *** 10/09/03 **************************************
     C*
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*** END   ADD *** 10/09/03 **************************************
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
     C     EHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*  Key List for Detail File
     C*
     C     DHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEG ADD *** 05/10/00 ****************************************
     C                   KFLD                    KKDSEQ
     C*
     C*  KEY LIST FOR PLACE OF SERVICE FILE
     C*
     C     PSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPOS
     C*** END ADD *** 05/10/00 ****************************************
     C*
     C*** BEGIN ADD *** 08/27/01 **************************************
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
     C*** END   ADD *** 08/27/01 **************************************
     C*** BEG ADD *** 12/23/05 ****************************************
     C*  KEY LIST FOR NDC CODE FILE
     C*
     C     RXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKNDC
     C*** END ADD *** 12/23/05 ****************************************
     C*** BEG ADD *** 01/27/06 ****************************************
     C*
     C     DEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKSEQ#
     C*** END ADD *** 01/27/06 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DHCO#         KKCO#
     C     *LIKE         DEFINE    DHSUB#        KKSUB#
     C     *LIKE         DEFINE    DHFND         KKFND
     C     *LIKE         DEFINE    DHFYR         KKFYR
     C     *LIKE         DEFINE    DHCASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    DHSS#         KKSS#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EODIA1        XXDIA
     C     *LIKE         DEFINE    EOCNTL        QQCNTL
     C     *LIKE         DEFINE    CACPT4        HLCPT4
     C*** BEG ADD *** 05/10/00 ****************************************
     C     *LIKE         DEFINE    PSPOS         KKPOS
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C*** END ADD *** 05/10/00 ****************************************
     C*** BEGIN ADD *** 10/09/03 **************************************
     C     *LIKE         DEFINE    PMKEY         KKKEY
     C*** END   ADD *** 10/09/03 **************************************
     C*** BEGIN ADD *** 12/23/05 **************************************
     C     *LIKE         DEFINE    DHNDC         KKNDC
     C*** END   ADD *** 12/23/05 **************************************
     C*** BEG ADD *** 01/27/06 ****************************************
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        XXSEQ#
     C*** END ADD *** 01/27/06 ****************************************
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
     C***** Beg Add *** 09/01/16 *********************************************
     C/free

       // intiailize error indicators

       erDiaE = *off;
       MsDiaE = *blanks;
       erDiaF = *off;
       MsDiaF = *blanks;
       erDiaG = *off;
       MsDiaG = *blanks;
       erDiaH = *off;
       MsDiaH = *blanks;
       erDiaI = *off;
       MsDiaI = *blanks;
       erDiaJ = *off;
       MsDiaJ = *blanks;
       erDiaK = *off;
       MsDiaK = *blanks;
       erDiaL = *off;
       MsDiaL = *blanks;
       erNDC2 = *off;
       MsNDC2 = *blanks;

      /end-free
     C***** End Add *** 09/01/16 *********************************************
     C*
     C*** BEG DEL *** 08/13/01 ****************************************
     C*******************
     C***Make*sure*that*there are no blanks in the Billed Procedure
     C********Code*field*
     C***********SCBPRC**  IFNE *BLANKS
     C***********'*'*****  SCAN SCBPRC                   70
     C***70**************  SETON                         90
     C***70**************  GOTO CK1END
     C*******************  ENDIF
     C*** END DEL *** 08/13/01 ****************************************
     C*** BEGIN ADD *** 08/27/01 **************************************
     C     EOAST         IFNE      'TX'
     C*** END   ADD *** 08/27/01 **************************************
     C*** BEG ADD *** 01/27/06 ****************************************
     C*** END ADD *** 01/27/06 ****************************************
     C     SCCPT4        IFEQ      *BLANKS
     C                   SETON                                        8490
     C                   GOTO      CK1END
     C                   ENDIF
     C*** BEG ADD *** 08/13/01 ****************************************
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
     C*** END ADD *** 08/13/01 ****************************************
     C*** BEGIN ADD *** 08/27/01 **************************************
     C                   ELSE
     C*
     C                   MOVEL(P)  SCBPRC        KKCPT4
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFDAT
     C*
     C     VCKY1         SETGT     PVC01
     C     VCKY2         READPE    PVC01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C     VCSTAT        OREQ      'I'
     C                   SETON                                        9089
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 08/27/01 **************************************
     C*** BEGIN ADD *** 10/31/05 **************************************
     C*
     C* VALIDATE NATIONAL DRUG CODE (NDC)
     C*
     C     SCNDC         IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCNDCD
     C                   ENDIF
     C***** Beg Add *** 09/01/16 *********************************************
     C*
     C     SCNDC2        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCNDCD2
     C                   ENDIF
     C***** End Add *** 09/01/16 *********************************************
     C*
     C*** BEGIN DEL *** 01/19/07 **************************************
     C********** SCBPRC    IFEQ '96370'
     C********** SCNDC     ANDEQ*BLANKS
     C**********           SETON                     9078
     C**********           GOTO CK1END
     C**********           ENDIF                           SCBPRC IFEQ 96370
     C*** END   DEL *** 01/19/07 **************************************
     C*
     C     SCNDC         IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  SCNDC         KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C     RXSTAT        IFEQ      'I'
     C                   SETON                                        9079
     C                   GOTO      CK1END
     C                   ENDIF                                                  RXSTAT = 'I'
     C                   ENDIF                                                  SCNDC IFNE BLANKS
     C*
     C*** END   ADD *** 10/31/05 **************************************
     C***** Beg Add *** 09/01/16 *********************************************
     C*
     C     SCNDC2        IFNE      *BLANKS
     C     *IN46         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  SCNDC2        KKNDC
     C     RXKY          CHAIN     PRX01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        90
     C                   Eval      erNDC2 = *on
     C                   Eval      MsNDC2 = 'MCR2434'
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C     RXSTAT        IFEQ      'I'
     C                   SETON                                        90
     C                   Eval      erNDC2 = *on
     C                   Eval      MsNDC2 = 'MCR2434'
     C                   GOTO      CK1END
     C                   ENDIF                                                  RXSTAT = 'I'
     C                   ENDIF                                                  SCNDC IFNE BLANKS
     C***** End Add *** 09/01/16 *********************************************
     C*** BEGIN ADD *** 10/10/03 **************************************
     C*
     C* VALIDATE THE PROCEDURE CODE MODIFIERS IF ENTETED (1-4)
     C*
     C     SCMOD1        IFNE      *BLANKS
     C                   MOVEL(P)  SCMOD1        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD1 IFNE *BLANKS
     C*
     C     SCMOD2        IFNE      *BLANKS
     C                   MOVEL(P)  SCMOD2        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD2 IFNE *BLANKS
     C*
     C     SCMOD3        IFNE      *BLANKS
     C                   MOVEL(P)  SCMOD3        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD3 IFNE *BLANKS
     C*
     C     SCMOD4        IFNE      *BLANKS
     C                   MOVEL(P)  SCMOD4        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD4 IFNE *BLANKS
     C*** END   ADD *** 10/10/03 **************************************
     C*
     C*  Make sure that Diag code xref isn't blank and that it is valid
     C*
     C     SCDXR1        IFEQ      *BLANKS
     C     SCDXR2        ANDEQ     *BLANKS
     C     SCDXR3        ANDEQ     *BLANKS
     C     SCDXR4        ANDEQ     *BLANKS
     C***** Beg Add *** 09/01/16 *********************************************
     C     SCDXRE        ANDEQ     *BLANKS
     C     SCDXRF        ANDEQ     *BLANKS
     C     SCDXRG        ANDEQ     *BLANKS
     C     SCDXRH        ANDEQ     *BLANKS
     C     SCDXRI        ANDEQ     *BLANKS
     C     SCDXRJ        ANDEQ     *BLANKS
     C     SCDXRK        ANDEQ     *BLANKS
     C     SCDXRL        ANDEQ     *BLANKS
     C                   Eval      erDiaE = *on
     C                   Eval      MsDiaE = 'MCR0503'
     C                   Eval      erDiaF = *on
     C                   Eval      MsDiaF = 'MCR0503'
     C                   Eval      erDiaG = *on
     C                   Eval      MsDiaG = 'MCR0503'
     C                   Eval      erDiaH = *on
     C                   Eval      MsDiaH = 'MCR0503'
     C                   Eval      erDiaI = *on
     C                   Eval      MsDiaI = 'MCR0503'
     C                   Eval      erDiaJ = *on
     C                   Eval      MsDiaJ = 'MCR0503'
     C                   Eval      erDiaK = *on
     C                   Eval      MsDiaK = 'MCR0503'
     C                   Eval      erDiaL = *on
     C                   Eval      MsDiaL = 'MCR0503'
     C***** End Add *** 09/01/16 *********************************************
     C                   SETON                                        7190
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       71
     C     SCDXR1        IFNE      'X'
     C     SCDXR1        ANDNE     ' '
     C                   SETON                                        7290
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       72
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDXR2        IFNE      'X'
     C     SCDXR2        ANDNE     ' '
     C                   SETON                                        7390
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       73
     C                   ENDIF
     C*
     C     SCDXR3        IFNE      'X'
     C     SCDXR3        ANDNE     ' '
     C                   SETON                                        7490
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       74
     C                   ENDIF
     C*
     C     SCDXR4        IFNE      'X'
     C     SCDXR4        ANDNE     ' '
     C                   SETON                                        7590
     C                   GOTO      CK1END
     C                   ELSE
     C                   SETOFF                                       75
     C                   ENDIF
     C***** Beg Add *** 09/01/16 *********************************************
     C*
     C     SCDXRE        IFNE      'X'
     C     SCDXRE        ANDNE     ' '
     C                   Eval      erDiaE = *on
     C                   Eval      MsDiaE = 'MC40221'
     C                   SETON                                          90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXRF        IFNE      'X'
     C     SCDXRF        ANDNE     ' '
     C                   Eval      erDiaF = *on
     C                   Eval      MsDiaF = 'MC40221'
     C                   SETON                                          90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXRG        IFNE      'X'
     C     SCDXRG        ANDNE     ' '
     C                   Eval      erDiaG = *on
     C                   Eval      MsDiaG = 'MC40221'
     C                   SETON                                          90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXRH        IFNE      'X'
     C     SCDXRH        ANDNE     ' '
     C                   Eval      erDiaH = *on
     C                   Eval      MsDiaH = 'MC40221'
     C                   SETON                                          90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXRI        IFNE      'X'
     C     SCDXRI        ANDNE     ' '
     C                   Eval      erDiaI = *on
     C                   Eval      MsDiaI = 'MC40221'
     C                   SETON                                          90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXRJ        IFNE      'X'
     C     SCDXRJ        ANDNE     ' '
     C                   Eval      erDiaJ = *on
     C                   Eval      MsDiaJ = 'MC40221'
     C                   SETON                                          90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXRK        IFNE      'X'
     C     SCDXRK        ANDNE     ' '
     C                   Eval      erDiaK = *on
     C                   Eval      MsDiaK = 'MC40221'
     C                   SETON                                          90
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C     SCDXRL        IFNE      'X'
     C     SCDXRL        ANDNE     ' '
     C                   Eval      erDiaL = *on
     C                   Eval      MsDiaL = 'MC40221'
     C                   SETON                                          90
     C                   GOTO      CK1END
     C                   ENDIF
     C***** End Add *** 09/01/16 *********************************************
     C*** BEG ADD *** 05/10/00 ****************************************
     C* VALIDATE PLACE OF SERVICE
     C*
     C     SCPOS         IFEQ      *BLANKS
     C                   SETON                                        9068
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPOS IFEQ BLANKS
     C*
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9069
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C*** END ADD *** 05/10/00 ****************************************
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
     C* KJDO   F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C***** Beg Add *** 09/01/16 *********************************************
     C* initialize pos cursor indicators
     C                   Eval      indFoNDC2 = *off
     C***** End Add *** 09/01/16 *********************************************
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCBPRC'
     C*
     C*** BEGIN ADD *** 08/27/01 **************************************
     C     EOAST         IFNE      'TX'
     C*** END   ADD *** 08/27/01 **************************************
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
     C*** BEG CHG *** 12/23/05 ****************************************
     C******************** SETON                     9034
     C                   SETON                                        9055
     C*** END CHG *** 12/23/05 ****************************************
     C                   ENDIF                                                  ##RTCD = 1
     C*** BEGIN ADD *** 08/27/01 **************************************
     C                   ELSE
     C                   CALL      'F2532R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      EOAST         ##AST
     C                   PARM      *BLANKS       ##CPT4           15
     C                   PARM      *BLANKS       ##DESC           40
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   SETON                                        5490      pos cursor
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCBPRC
     C                   MOVEL(P)  ##DESC        SCDSC2
     C*** BEG CHG *** 12/23/05 ****************************************
     C******************** SETON                     3490  pos cursor
     C                   SETON                                        5590      pos cursor
     C*** END CHG *** 12/23/05 ****************************************
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 08/27/01 **************************************
     C*** BEG ADD *** 05/10/00 ****************************************
     C* PLACE OF SERVICE SEARCH
     C*
     C     HFLD          WHENEQ    'SCPOS'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##POS
     C                   MOVEL     SCPOS         HLPOS            15
     C                   CALL      'F2370R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##AST             2
     C                   PARM                    ##POS            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##POS         OREQ      *BLANKS
     C                   MOVEL(P)  HLPOS         SCPOS
     C                   SETON                                        9031      pos cursor
     C                   ELSE
 CCC C                   MOVEL     ##POS         TPPOS             3
 AAA C                   MOVE      TPPOS         SCPOS
 CCC C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C                   SETON                                        9056      pos cursor
     C                   ENDIF                                                  ##KC IFEQ 1
     C*** END ADD *** 05/10/00 ****************************************
     C*
     C*** BEG ADD *** 01/27/06 ****************************************
     C*
     C* PAID CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCCPT4'
     C*
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
     C                   MOVEL     HLCPT4        SCCPT4
     C                   MOVEL(P)  HLDESC        SCDSC1
     C                   SETON                                        9056      pos cursor
     C                   ELSE
     C                   MOVEL     ##PROC        SCCPT4
     C                   MOVEL(P)  ##PROD        SCDSC1
     C                   SETON                                        9054
     C                   ENDIF                                                  ##RTCD = 1
     C*** END ADD *** 01/27/06 ****************************************
     C*** BEGIN ADD *** 10/09/03 **************************************
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #1)
     C*
     C     HFLD          WHENEQ    'SCMOD1'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   SETON                                        9034      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD1
     C                   SETON                                        9035      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #2)
     C*
     C     HFLD          WHENEQ    'SCMOD2'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   SETON                                        9035      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD2
     C                   SETON                                        9036      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #3)
     C*
     C     HFLD          WHENEQ    'SCMOD3'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   SETON                                        9036      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD3
     C                   SETON                                        9037      pos cursor
     C                   ENDIF
     C*
     C* PROCEDURE MODIFIER SEARCH (for modifier #4)
     C*
     C     HFLD          WHENEQ    'SCMOD4'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##KEY
     C                   CALL      'F2315R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##AST
     C                   PARM                    ##KEY            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##KEY         OREQ      *BLANKS
     C                   SETON                                        9037      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY         SCMOD4
     C                   SETON                                        9038      pos cursor
     C                   ENDIF
     C*** END   ADD *** 10/09/03 **************************************
     C*** BEG ADD *** 12/23/05 ****************************************
     C*
     C*  NDC CODE SEARCH
     C     HFLD          WHENEQ    'SCNDC'
     C                   MOVEL     SCNDC         HLNDC            11
     C                   CALL      'F2350R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLNDC         SCNDC
     C                   SETON                                        9055      pos cursor
     C                   ELSE
     C     ##RRN         CHAIN     PRX02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         SCNDC
     C                   MOVEL(P)  RXNAME        SCNDCD
     C***** Beg Chg *** 09/01/16 *********************************************
     C****               SETON                                        9034      pos cursor
     C                   SETON                                        90
     C                   Eval      indFoNDC2 = *on
     C***** End Chg *** 09/01/16 *********************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C***** Beg Add *** 09/01/16 *********************************************
     C*  NDC CODE 2 SEARCH
     C     HFLD          WHENEQ    'SCNDC2'
     C                   MOVEL     SCNDC2        HLNDC            11
     C                   CALL      'F2350R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLNDC         SCNDC2
     C                   SETON                                        90
     C                   Eval      indFoNDC2 = *on
     C                   ELSE
     C     ##RRN         CHAIN     PRX02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RXNDC         SCNDC2
     C                   MOVEL(P)  RXNAME        SCNDCD2
     C                   SETON                                        9034
     C                   ENDIF
     C                   ENDIF
     C***** End Add *** 09/01/16 *********************************************
     C*** END ADD *** 12/23/05 ****************************************
     C*
     C     HFLD          WHENEQ    'SCDXR1'
     C                   SETON                                        9076
     C*
     C     HFLD          WHENEQ    'SCDXR2'
     C                   SETON                                        9077
     C*
     C     HFLD          WHENEQ    'SCDXR3'
     C                   SETON                                        9078
     C*
     C     HFLD          WHENEQ    'SCDXR4'
     C                   SETON                                        9079
     C***** Beg Add *** 09/01/16 *********************************************
     C*
     C     HFLD          WHENEQ    'SCDXRE'
     C                   SETON                                        90
     C                   Eval      erDiaE = *on
     C                   Eval      MsDiaE = 'MC40215'
     C*
     C     HFLD          WHENEQ    'SCDXRF'
     C                   SETON                                        90
     C                   Eval      erDiaF = *on
     C                   Eval      MsDiaF = 'MC40215'
     C*
     C     HFLD          WHENEQ    'SCDXRG'
     C                   SETON                                        90
     C                   Eval      erDiaG = *on
     C                   Eval      MsDiaG = 'MC40215'
     C*
     C     HFLD          WHENEQ    'SCDXRH'
     C                   SETON                                        90
     C                   Eval      erDiaH = *on
     C                   Eval      MsDiaH = 'MC40215'
     C*
     C     HFLD          WHENEQ    'SCDXRI'
     C                   SETON                                        90
     C                   Eval      erDiaI = *on
     C                   Eval      MsDiaI = 'MC40215'
     C*
     C     HFLD          WHENEQ    'SCDXRJ'
     C                   SETON                                        90
     C                   Eval      erDiaJ = *on
     C                   Eval      MsDiaJ = 'MC40215'
     C*
     C     HFLD          WHENEQ    'SCDXRK'
     C                   SETON                                        90
     C                   Eval      erDiaK = *on
     C                   Eval      MsDiaK = 'MC40215'
     C*
     C     HFLD          WHENEQ    'SCDXRL'
     C                   SETON                                        90
     C                   Eval      erDiaL = *on
     C                   Eval      MsDiaL = 'MC40215'
     C***** End Add *** 09/01/16 *********************************************
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
     C***** Beg Add *** 09/01/16 **************************************
     C                   MOVE      *BLANKS       SCDXRE
     C                   MOVE      *BLANKS       SCDIAE
     C                   MOVE      *BLANKS       SCDSE
     C                   MOVE      *BLANKS       SCDXRF
     C                   MOVE      *BLANKS       SCDIAF
     C                   MOVE      *BLANKS       SCDSF
     C                   MOVE      *BLANKS       SCDXRG
     C                   MOVE      *BLANKS       SCDIAG
     C                   MOVE      *BLANKS       SCDSG
     C                   MOVE      *BLANKS       SCDXRH
     C                   MOVE      *BLANKS       SCDIAH
     C                   MOVE      *BLANKS       SCDSH
     C                   MOVE      *BLANKS       SCDXRI
     C                   MOVE      *BLANKS       SCDIAI
     C                   MOVE      *BLANKS       SCDSI
     C                   MOVE      *BLANKS       SCDXRJ
     C                   MOVE      *BLANKS       SCDIAJ
     C                   MOVE      *BLANKS       SCDSJ
     C                   MOVE      *BLANKS       SCDXRK
     C                   MOVE      *BLANKS       SCDIAK
     C                   MOVE      *BLANKS       SCDSK
     C                   MOVE      *BLANKS       SCDXRL
     C                   MOVE      *BLANKS       SCDIAL
     C                   MOVE      *BLANKS       SCDSL
     C***** End Add *** 09/01/16 **************************************
     C*** BEG ADD *** 05/10/00 ****************************************
     C                   MOVE      *BLANKS       SCPOS
     C*** END ADD *** 05/10/00 ****************************************
     C*** BEG ADD *** 12/23/05 ****************************************
     C                   MOVE      *BLANKS       SCNDC
     C                   MOVE      *BLANKS       SCNDCD
     C*** END ADD *** 12/23/05 ****************************************
     C***** Beg Add *** 09/01/16 *********************************************
     C                   MOVE      *BLANKS       SCNDC2
     C                   MOVE      *BLANKS       SCNDCD2
     C***** End Add *** 09/01/16 *********************************************
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
     C     EHKY          CHAIN     PEH01                              60
     C     @@RRN4        CHAIN     PCA01                              60
     C                   MOVEL     CADSEQ        KKDSEQ
     C*
     C     DHKY          CHAIN(N)  PDH01                              60
     C*
     C*** BEG ADD ****** 10/12/15 ************************************
     C                   If        EHICDIND <> 'Y'
     C*** END ADD ****** 10/12/15 ************************************
     C                   MOVEL     EHDIA1        SCDIA1
     C                   MOVEL     EHDIA1        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDS1
     C*
     C                   MOVEL     EHDIA2        SCDIA2
     C                   MOVEL     EHDIA2        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDS2
     C*
     C                   MOVEL     EHDIA3        SCDIA3
     C                   MOVEL     EHDIA3        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDS3
     C*
     C                   MOVEL     EHDIA4        SCDIA4
     C                   MOVEL     EHDIA4        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDS4
     C***** Beg Add *** 09/01/16 **************************************
     C*
     C                   MOVEL     EHICDE        SCDIAE
     C                   MOVEL     EHICDE        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSE
     C*
     C                   MOVEL     EHICDF        SCDIAF
     C                   MOVEL     EHICDF        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSF
     C*
     C                   MOVEL     EHICDG        SCDIAG
     C                   MOVEL     EHICDG        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSG
     C*
     C                   MOVEL     EHICDH        SCDIAH
     C                   MOVEL     EHICDH        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSH
     C*
     C                   MOVEL     EHICDI        SCDIAI
     C                   MOVEL     EHICDI        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSI
     C*
     C                   MOVEL     EHICDJ        SCDIAJ
     C                   MOVEL     EHICDJ        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSJ
     C*
     C                   MOVEL     EHICDK        SCDIAK
     C                   MOVEL     EHICDK        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSK
     C*
     C                   MOVEL     EHICDL        SCDIAL
     C                   MOVEL     EHICDL        XXDIA
     C     DIKY          CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSL
     C***** End Add *** 09/01/16 **************************************
     C*
     C*** BEG ADD ****** 10/12/15 ************************************
     C                   Else
     C/free
        If EHICD1001 <> *blanks;
           SCDIA1 = EHICD1001;
           kkICD10 = SCDIA1;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDS1 = ICDSC1;
           EndIf;
        EndIf;

        If EHICD1002 <> *blanks;
           SCDIA2 = EHICD1002;
           kkICD10 = SCDIA2;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDS2 = ICDSC1;
           EndIf;
        EndIf;

        If EHICD1003 <> *blanks;
           SCDIA3 = EHICD1003;
           kkICD10 = SCDIA3;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDS3 = ICDSC1;
           EndIf;
        EndIf;

        If EHICD1004 <> *blanks;
           SCDIA4 = EHICD1004;
           kkICD10 = SCDIA4;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDS4 = ICDSC1;
           EndIf;
        EndIf;

        //***** Beg Add *** 09/01/16 **************************************
        If EHICDE <> *blanks;
           SCDIAE = EHICDE;
           kkICD10 = SCDIAE;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSE = ICDSC1;
           EndIf;
        EndIf;

        If EHICDF <> *blanks;
           SCDIAF = EHICDF;
           kkICD10 = SCDIAF;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSF = ICDSC1;
           EndIf;
        EndIf;

        If EHICDG <> *blanks;
           SCDIAG = EHICDG;
           kkICD10 = SCDIAG;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSG = ICDSC1;
           EndIf;
        EndIf;

        If EHICDH <> *blanks;
           SCDIAH = EHICDH;
           kkICD10 = SCDIAH;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSH = ICDSC1;
           EndIf;
        EndIf;

        If EHICDI <> *blanks;
           SCDIAI = EHICDI;
           kkICD10 = SCDIAI;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSI = ICDSC1;
           EndIf;
        EndIf;

        If EHICDJ <> *blanks;
           SCDIAJ = EHICDJ;
           kkICD10 = SCDIAJ;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSJ = ICDSC1;
           EndIf;
        EndIf;

        If EHICDK <> *blanks;
           SCDIAK = EHICDK;
           kkICD10 = SCDIAK;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSK = ICDSC1;
           EndIf;
        EndIf;

        If EHICDL <> *blanks;
           SCDIAL = EHICDL;
           kkICD10 = SCDIAL;

           Chain (kkICD10:'D') FMICD10L1;
           If %Found(FMICD10L1);
             SCDSL = ICDSC1;
           EndIf;
        EndIf;

        //***** End Add *** 09/01/16 **************************************
      /end-free
     C                   EndIf
     C*** END ADD ****** 10/12/15 ************************************
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C                   MOVEL     DHPRCD        SCCPT4
     C                   MOVEL(P)  DHPRDS        SCDSC1
     C*
     C     DHBPRC        IFNE      *BLANKS
     C                   MOVEL     DHBPRC        SCBPRC
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   ENDIF
     C*** BEG ADD *** 12/23/05 ****************************************
     C                   MOVEL(P)  DHNDC         SCNDC
     C                   MOVEL(P)  DHNDCD        SCNDCD
     C*** END ADD *** 12/23/05 ****************************************
     C***** Beg Add *** 09/01/16 *********************************************
     C                   MOVEL(P)  DHNDC2        SCNDC2
     C                   MOVEL(P)  DHDESC2       SCNDCD2
     C***** End Add *** 09/01/16 *********************************************
     C*
     C*** BEGIN ADD *** 08/27/01 **************************************
     C     EOAST         IFNE      'TX'
     C*** END   ADD *** 08/27/01 **************************************
     C                   MOVEL     SCBPRC        KKCPT4
     C     DHRCFL        IFEQ      'Y'
     C     RCKY          CHAIN     PRC01                              60
     C  N60              MOVEL     RCDSC1        SCDSC2
     C                   ELSE
     C     PRKY          CHAIN     PPR01                              60
     C  N60              MOVEL     PRDSC1        SCDSC2
     C                   ENDIF
     C*** BEGIN ADD *** 08/27/01 **************************************
     C                   ELSE
     C     VCKY2         CHAIN     PVC01                              60
     C  N60              MOVEL     VCDSC1        SCDSC2
     C                   ENDIF
     C*** END   ADD *** 08/27/01 **************************************
     C*
     C*** BEG ADD *** 05/10/00 ****************************************
     C                   MOVE(P)   DHPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*** END ADD *** 05/10/00 ****************************************
     C                   MOVEL(P)  DHMOD1        SCMOD1
     C                   MOVEL(P)  DHMOD2        SCMOD2
     C                   MOVEL(P)  DHMOD3        SCMOD3
     C                   MOVEL(P)  DHMOD4        SCMOD4
     C                   MOVEL     DHDXR1        SCDXR1
     C                   MOVEL     DHDXR2        SCDXR2
     C                   MOVEL     DHDXR3        SCDXR3
     C                   MOVEL     DHDXR4        SCDXR4
     C***** Beg Add *** 09/01/16 *********************************************
     C                   MOVEL     DHDXRE        SCDXRE
     C                   MOVEL     DHDXRF        SCDXRF
     C                   MOVEL     DHDXRG        SCDXRG
     C                   MOVEL     DHDXRH        SCDXRH
     C                   MOVEL     DHDXRI        SCDXRI
     C                   MOVEL     DHDXRJ        SCDXRJ
     C                   MOVEL     DHDXRK        SCDXRK
     C                   MOVEL     DHDXRL        SCDXRL
     C***** End Add *** 09/01/16 *********************************************
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
     C                   MOVEL     SCBPRC        DHBPRC
     C                   MOVEL     SCDXR1        DHDXR1
     C                   MOVEL     SCDXR2        DHDXR2
     C                   MOVEL     SCDXR3        DHDXR3
     C                   MOVEL     SCDXR4        DHDXR4
     C***** Beg Add *** 09/01/16 *********************************************
     C                   MOVEL     SCDXRE        DHDXRE
     C                   MOVEL     SCDXRF        DHDXRF
     C                   MOVEL     SCDXRG        DHDXRG
     C                   MOVEL     SCDXRH        DHDXRH
     C                   MOVEL     SCDXRI        DHDXRI
     C                   MOVEL     SCDXRJ        DHDXRJ
     C                   MOVEL     SCDXRK        DHDXRK
     C                   MOVEL     SCDXRL        DHDXRL
     C***** End Add *** 09/01/16 *********************************************
     C*** BEG ADD *** 05/10/00 ****************************************
     C                   MOVE(P)   SCPOS         DHPOS
     C*** END ADD *** 05/10/00 ****************************************
     C*** BEGIN ADD *** 10/09/03 **************************************
     C                   MOVEL(P)  SCMOD1        DHMOD1
     C                   MOVEL(P)  SCMOD2        DHMOD2
     C                   MOVEL(P)  SCMOD3        DHMOD3
     C                   MOVEL(P)  SCMOD4        DHMOD4
     C*** END   ADD *** 10/09/03 **************************************
     C*** BEG ADD *** 12/23/05 ****************************************
     C                   MOVEL(P)  SCNDC         DHNDC
     C                   MOVEL(P)  SCNDCD        DHNDCD
     C*** END ADD *** 12/23/05 ****************************************
     C***** Beg Add *** 09/01/16 *********************************************
     C                   MOVEL(P)  SCNDC2        DHNDC2
     C                   MOVEL(P)  SCNDCD2       DHDESC2
     C***** End Add *** 09/01/16 *********************************************
     C*** BEG ADD *** 01/27/06 ****************************************
     C*
     C* IF THE PAID CODE IS CHANGED THEN WRITE AN EOB NOTE
     C*
     C     SCCPT4        IFNE      DHPRCD
     C*
     C                   EXSR      WRTDEP
     C                   MOVEL(P)  SCCPT4        DHPRCD
     C*
     C                   MOVEL(P)  SCCPT4        KKCPT4
     C*
     C     PRKY          SETLL     PPR01
     C     NXTCD1        TAG
     C     PRKY          READE     PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     PRSTAT        ANDEQ     'I'
     C*
     C                   GOTO      NXTCD1
     C*
     C                   ELSE
     C  N99              MOVEL(P)  PRDSC1        DHPRDS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 01/27/06 ****************************************
     C*
     C                   MOVE      Q1USER        DHUSER
     C                   MOVE      Q1PGM         DHPGM
     C                   MOVE      DSTIME        DHTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
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
     C*** BEGIN ADD *** 10/10/03 **************************************
     C                   CLOSE     FTABL
     C*** END   ADD *** 10/10/03 **************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** BEGIN ADD *** 10/10/03 **************************************
     C                   OPEN      FTABL
     C*** END   ADD *** 10/10/03 **************************************
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
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
     C*******    @@RRN     CHAINDBRCDF               99
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
     C                   FEOD      FDDHP
     C                   ENDSR
     C*** BEG ADD *** 01/27/06 ****************************************
     C*================================================================
     C* WRTDEP - Write note indicating change on paid code
     C*================================================================
     C     WRTDEP        BEGSR
     C*
     C                   MOVE      *HIVAL        KKSEQ#
     C*
     C     DEKY2         SETGT     PDE01
     C     DEKY1         READPE    PDE01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     DESEQ#        ADD       1             XXSEQ#
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             XXSEQ#
     C*
     C                   ENDIF
     C*
     C                   CLEAR                   PDE01
     C*
     C                   Z-ADD     EOCO#         DECO#
     C                   Z-ADD     EOSUB#        DESUB#
     C                   Z-ADD     EOFND         DEFND
     C                   Z-ADD     EOFYR         DEFYR
     C                   Z-ADD     EOCASE        DECASE
     C                   Z-ADD     EOEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     XXSEQ#        DESEQ#
     C                   MOVEL     *BLANKS       DEEOB
     C                   MOVEL(P)  NT1           XXNT1
     C                   MOVEL(P)  DHPRCD        XXNT2
     C                   MOVEL(P)  NT3           XXNT3
     C                   MOVEL(P)  XXNOT1        DENOT1
     C                   MOVEL(P)  *BLANKS       DENOT2
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  'N'           DEPRNT
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  EOSS#         DESS#
     C                   Z-ADD     EOACDT        DEACDT
     C                   MOVE      'U'           DEESRC                         user entered
     C                   MOVEL     Q1USER        DEUSER
     C                   MOVEL     Q1USER        DEIUSR
     C                   MOVEL     Q1PGM         DEPGM
     C                   MOVEL     Q1PGM         DEIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DEDT
     C*
     C                   MOVEL     SRDTO         DEIDT
     C*
     C                   MOVEL     DSTIME        DETIME
     C                   MOVEL     DSTIME        DEITIM
     C*
     C                   WRITE     PDE02
     C*
     C                   ENDSR
     C*** END ADD *** 01/27/06 ****************************************
