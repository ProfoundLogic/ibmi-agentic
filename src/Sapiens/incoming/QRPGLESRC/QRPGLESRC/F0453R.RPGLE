     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr**** dftactgrp(*no) actgrp('QILE')
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
     H*  10/12/15  C:212  Supp: 37776                                 *
     H*                   Increase Diag codes from 5A to 7A and       *
     H*                   related ICD10 changes                       *
     H*                                                               *
     H*  11/11/13  C:280  SUPP: 84028                                 *
     H*                   add npir to screen to allow for update      *
     H*                                                               *
     H*  05/09/09  C:227  SUPP: 68270                                 *
     H*                   EDI Changes                                 *
     H*                                                               *
     H*  04/30/08  C:191  PROJ: TX045                                 *
     H*                   TX EDI Correction required fields           *
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
     FF0453F    CF   E             WORKSTN
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
     F***FTPSL   IF  E           K        DISK
     F*** END ADD *** 05/10/00 ****************************************
     F*** BEGIN ADD *** 08/27/01 **************************************
     F*** BEG DEL ****** 05/09/09 ************************************
     F***FMVCLTX IF  E           K        DISK
     F*** END DEL ****** 05/09/09 ************************************
     F*** END   ADD *** 08/27/01 **************************************
     F*** BEG ADD ****** 04/30/08 ************************************
     F*** BEG DEL ****** 05/09/09 ************************************
     F***FMPRLTX IF  E           K        DISK
     F***         PPR01                             KRENAMEPTX01
     F*** END DEL ****** 05/09/09 ************************************
     F*** BEG CHG ****** 05/09/09 ************************************
     F***FTABLTX IF  E           K        DISK
     FFTABL     IF   E           K DISK
     F*** END CHG ****** 05/09/09 ************************************
     F                                     IGNORE(PAC01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PEB01)
     F                                     IGNORE(PFT01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PPS01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PTS01)
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
     D OPTNM           C                   CONST('Detail - Data Subm C-
     D                                     hgs')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D*** BEG ADD ****** 04/30/08 ************************************
     D NT0             C                   CONST('Field was changed fo-
     D                                     r EDI correction:')
     D NT1             C                   CONST('Original Value:')
     D FL1const        C                   CONST('Paid code')
     D FL2const        C                   CONST('Proc code modifier 1')
     D FL3const        C                   CONST('Proc code modifier 2')
     D FL4const        C                   CONST('Proc code modifier 3')
     D FL5const        C                   CONST('Proc code modifier 4')
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
     C                   PARM      DHDSEQ        #2DSEQ            5 0
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
     C***        PSKY      KLIST
     C***                  KFLD           KKCO#
     C***                  KFLD           KKPOS
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
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY            10
     C*** END ADD ****** 04/30/08 ************************************
     C*
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
     C****       *LIKE     DEFN PSPOS     KKPOS
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C*** END ADD *** 05/10/00 ****************************************
     C*** BEG ADD ****** 04/30/08 *************************************
     C     *LIKE         DEFINE    DHPRCD        XOCPT4
     C     *LIKE         DEFINE    DHMOD1        XOMOD1
     C     *LIKE         DEFINE    DHMOD2        XOMOD2
     C     *LIKE         DEFINE    DHMOD3        XOMOD3
     C     *LIKE         DEFINE    DHMOD4        XOMOD4
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
     C*** BEG ADD ****** 04/30/08 ************************************
     C* VALIDATE CPT4 FOR TEXAS
     C     SCCPT4        IFNE      *BLANKS
     C                   MOVE      SCCPT4        KKCPT4
     C*
     C*** BEG CHG ****** 05/09/09 ************************************
     C***        PRKY      SETLLPTX01
     C***        PRKY      READEPTX01                    99
     C     PRKY          SETLL     PPR01
     C     PRKY          READE     PPR01                                  99
     C*** END CHG ****** 05/09/09 ************************************
     C     *IN99         DOWEQ     *OFF
     C     PRSTAT        IFNE      'I'
     C                   MOVE(P)   PRDSC1        SCDSC1
     C                   LEAVE
     C                   ENDIF
     C*** BEG CHG ****** 05/09/09 ************************************
     C***        PRKY      READEPTX01                    99
     C     PRKY          READE     PPR01                                  99
     F*** END CHG ****** 05/09/09 ************************************
     C                   ENDDO
     C* NOT FOUND
     C     *IN99         IFEQ      *ON
     C     PR1AG         OREQ      *ON
     C                   SETON                                        9058
     C                   SETOFF                                       555657
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  SCCPT4<>*BLANKS
     C*** END ADD ****** 04/30/08 ************************************
     C*
     C                   MOVEL(P)  SCBPRC        KKCPT4
     C*
     C                   Z-ADD     SCFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFDAT
     C*
     C*** BEG CHG ****** 05/09/09 ************************************
     C***        VCKY1     SETGTPVC01
     C***        VCKY2     REDPEPVC01                    99
     C     VCKY1         SETGT     PPR01
     C     VCKY2         READPE    PPR01                                  99
     C*** END CHG ****** 05/09/09 ************************************
     C*
     C     *IN99         IFEQ      *ON
     C*** BEG CHG ****** 05/09/09 ************************************
     C***        VCSTAT    OREQ 'I'
     C     PRSTAT        OREQ      'I'
     C*** END CHG ****** 05/09/09 ************************************
     C                   SETON                                        9089
     C                   ENDIF
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C     SCMOD1        IFNE      *BLANKS
     C                   MOVEL(P)  SCMOD1        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9056
     C                   SETOFF                                       555758
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD1 IFNE *BLANKS
     C*
     C     SCMOD2        IFNE      *BLANKS
     C                   MOVEL(P)  SCMOD2        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        905558
     C                   SETOFF                                       5657
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD2 IFNE *BLANKS
     C*
     C     SCMOD3        IFNE      *BLANKS
     C                   MOVEL(P)  SCMOD3        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        905556
     C                   SETOFF                                       5758
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD3 IFNE *BLANKS
     C*
     C     SCMOD4        IFNE      *BLANKS
     C                   MOVEL(P)  SCMOD4        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        905556
     C                   SETON                                        57
     C                   SETOFF                                       58
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCMOD4 IFNE *BLANKS
     C*** END ADD ****** 04/30/08 ************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 08/27/01 **************************************
     C*
     C*  Make sure that Diag code xref isn't blank and that it is valid
     C*
     C     SCDXR1        IFEQ      *BLANKS
     C     SCDXR2        ANDEQ     *BLANKS
     C     SCDXR3        ANDEQ     *BLANKS
     C     SCDXR4        ANDEQ     *BLANKS
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
     C*** BEG ADD *** 05/10/00 ****************************************
     C* VALIDATE PLACE OF SERVICE
     C*
     C****       SCPOS     IFEQ *BLANKS
     C****                 SETON                     9068
     C****                 GOTO CK1END
     C****                 ENDIF                           SCPOS IFEQ BLANKS
     C*
     C****                 MOVE SCPOS     KKPOS     P
     C****       PSKY      CHAINPPS01                99
     C****       *IN99     IFEQ *ON
     C****                 SETON                     9069
     C****                 GOTO CK1END
     C****                 ENDIF                           *IN99 = *ON
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
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
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
     C                   SETON                                        9057      pos cursor
     C                   SETOFF                                       555658
     C                   ELSE
     C                   MOVEL(P)  ##PROC        SCCPT4
     C                   MOVEL(P)  ##PROD        SCDSC1
     C                   SETON                                        9054      pos cursor
     C                   ENDIF                                                  #FCODE<>' '
     C*
     C     HFLD          WHENEQ    'SCMOD1'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE      EOAST         #FAST             2
     C                   MOVE(P)   *BLANKS       #FKEY            15
     C                   MOVE(P)   SCMOD1        XXMOD             2
     C                   CALL      'F2315R'
     C                   PARM      '2'           #FCALL            1
     C                   PARM      ' '           #FKC              1
     C                   PARM      ' '           #FKL              1
     C                   PARM                    #FAST
     C                   PARM                    #FKEY            15
     C*
     C     #FKC          IFEQ      '1'
     C     #FKL          OREQ      '1'
     C     #FKEY         OREQ      *BLANKS
     C                   MOVE      XXMOD         SCMOD1
     C                   SETON                                        9055
     C                   SETOFF                                       565758
     C                   ELSE
     C                   MOVEL     #FKEY         SCMOD1
     C                   SETON                                        905557
     C                   SETOFF                                       5658
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCMOD2'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE      EOAST         #FAST
     C                   MOVE      *BLANKS       #FKEY
     C                   MOVE(P)   SCMOD2        XXMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           #FCALL
     C                   PARM      ' '           #FKC
     C                   PARM      ' '           #FKL
     C                   PARM                    #FAST
     C                   PARM                    #FKEY            15
     C*
     C     #FKC          IFEQ      '1'
     C     #FKL          OREQ      '1'
     C     #FKEY         OREQ      *BLANKS
     C                   MOVE      XXMOD         SCMOD2
     C                   SETON                                        905557
     C                   SETOFF                                       5658
     C                   ELSE
     C                   MOVEL     #FKEY         SCMOD2
     C                   SETON                                        905556
     C                   SETON                                        58
     C                   SETOFF                                       57
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCMOD3'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE      EOAST         #FAST
     C                   MOVE      *BLANKS       #FKEY
     C                   MOVE(P)   SCMOD3        XXMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           #FCALL
     C                   PARM      ' '           #FKC
     C                   PARM      ' '           #FKL
     C                   PARM                    #FAST
     C                   PARM                    #FKEY            15
     C*
     C     #FKC          IFEQ      '1'
     C     #FKL          OREQ      '1'
     C     #FKEY         OREQ      *BLANKS
     C                   MOVE      XXMOD         SCMOD3
     C                   SETON                                        905556
     C                   SETON                                        58
     C                   SETOFF                                       57
     C                   ELSE
     C                   MOVEL     #FKEY         SCMOD3
     C                   SETON                                        905556
     C                   SETON                                        5758
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCMOD4'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE      EOAST         #FAST
     C                   MOVE      *BLANKS       #FKEY
     C                   MOVE(P)   SCMOD4        XXMOD
     C                   CALL      'F2315R'
     C                   PARM      '2'           #FCALL
     C                   PARM      ' '           #FKC
     C                   PARM      ' '           #FKL
     C                   PARM                    #FAST
     C                   PARM                    #FKEY            15
     C*
     C     #FKC          IFEQ      '1'
     C     #FKL          OREQ      '1'
     C     #FKEY         OREQ      *BLANKS
     C                   MOVE      XXMOD         SCMOD4
     C                   SETON                                        905556
     C                   SETON                                        5758
     C                   ELSE
     C                   MOVEL     #FKEY         SCMOD4
     C                   SETON                                        9034
     C                   ENDIF
     C*
     C*** END ADD ****** 04/30/08 ************************************
     C     HFLD          WHENEQ    'SCBPRC'
     C*
     C*** BEGIN ADD *** 08/27/01 **************************************
     C*** BEG DEL *** 05/09/09 ****************************************
     C***        EOAST     IFNE 'TX'
     C*** END DEL *** 05/09/09 ****************************************
     C*** END   ADD *** 08/27/01 **************************************
     C*** BEG CHG *** 05/09/09 ****************************************
     C***                  MOVE SCCPT4    HLCPT4
     C***                  MOVELSCDSC1    HLDESC 40 P
     C                   MOVE(P)   SCBPRC        XXCPT4
     C                   MOVEL(P)  SCDSC2        XXDSC1
     C*** END CHG *** 05/09/09 ****************************************
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
     C*** BEG CHG *** 05/09/09 ****************************************
     C***                  MOVELHLCPT4    SCBPRC
     C***                  MOVELHLDESC    SCDSC2    P
     C                   MOVEL     XXCPT4        SCBPRC
     C                   MOVEL(P)  XXDSC1        SCDSC2
     C*** END CHG *** 05/09/09 ****************************************
     C                   SETON                                        9054      pos cursor
     C                   ELSE
     C                   MOVEL     ##PROC        SCBPRC
     C                   MOVEL(P)  ##PROD        SCDSC2
     C*** BEG ADD *** 05/09/09 ****************************************
     C     EOAST         IFNE      'TX'
     C*** END ADD *** 05/09/09 ****************************************
     C                   SETON                                        9034
     C*** BEG ADD *** 05/09/09 ****************************************
     C                   ELSE
     C                   SETON                                        9055
     C                   SETOFF                                       565758
     C                   ENDIF                                                  ##RTCD = 1
     C*** END ADD *** 05/09/09 ****************************************
     C                   ENDIF                                                  ##RTCD = 1
     C*** BEGIN ADD *** 08/27/01 **************************************
     C*** BEG DEL *** 05/09/09 ****************************************
     C***                  ELSE
     C***                  CALL 'F2532R'
     C***                  PARM '2'       ##CALL  1
     C***                  PARM '0'       ##KC
     C***                  PARM '0'       ##KL
     C***                  PARM EOAST     ##AST
     C***                  PARM *BLANKS   ##CPT4 15
     C***                  PARM *BLANKS   ##DESC 40
     C*
     C***        ##KC      IFEQ '1'
     C***        ##KL      OREQ '1'
     C***        ##CPT4    OREQ *BLANKS
     C***                  SETON                     5490  pos cursor
     C***                  ELSE
     C***                  MOVEL##CPT4    SCBPRC    P
     C***                  MOVEL##DESC    SCDSC2    P
     C*** BEG CHG ****** 04/30/08 ************************************
     C*****************    SETON                     3490  pos cursor
     C***                  SETON                     9055
     C***                  SETOF                     565758
     C*** END CHG ****** 04/30/08 ************************************
     C***                  ENDIF
     C***                  ENDIF
     C*** END DEL *** 05/09/09 ***************************************
     C*** END   ADD *** 08/27/01 **************************************
     C*** BEG ADD *** 05/10/00 ****************************************
     C* PLACE OF SERVICE SEARCH
     C*
     C****       HFLD      WHEQ 'SCPOS'
     C****                 MOVE EOAST     ##AST
     C****                 MOVE *BLANKS   ##POS
     C****                 MOVELSCPOS     HLPOS  15
     C****                 CALL 'F2370R'
     C****                 PARM '2'       ##CALL  1
     C****                 PARM ' '       ##KC    1
     C****                 PARM ' '       ##KL    1
     C****                 PARM           ##AST   2
     C****                 PARM           ##POS  15
     C*
     C****       ##KC      IFEQ '1'
     C****       ##KL      OREQ '1'
     C****       ##POS     OREQ *BLANKS
     C****                 MOVELHLPOS     SCPOS     P
     C****                 SETON                     9031  pos cursor
     C****                 ELSE
 CCC C****                 MOVEL##POS     TPPOS   3
 AAA C****                 MOVE TPPOS     SCPOS
 CCC C****                 MOVE SCPOS     KKPOS     P
     C****       PSKY      CHAINPPS01                99
     C**N99                MOVELPSDESC    SCPODS    P
     C****                 SETON                     9032  pos cursor
     C****                 ENDIF                           ##KC IFEQ 1
     C*** END ADD *** 05/10/00 ****************************************
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
     C*** BEG ADD *** 05/10/00 ****************************************
     C****                 MOVE *BLANKS   SCPOS
     C*** END ADD *** 05/10/00 ****************************************
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

      /end-free
     C                   EndIf
     C*** END ADD ****** 10/12/15 ************************************
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
     C*** BEG CHG ****** 05/09/09 ************************************
     C***        VCKY2     CHAINPVC01                60
     C**N60                MOVELVCDSC1    SCDSC2
     C     VCKY2         CHAIN     PPR01                              60
     C  N60              MOVEL     PRDSC1        SCDSC2
     C*** END CHG ****** 05/09/09 ************************************
     C                   ENDIF
     C*** END   ADD *** 08/27/01 **************************************
     C*
     C*** BEG ADD *** 05/10/00 ****************************************
     C****                 MOVE DHPOS     SCPOS     P
     C****                 MOVE SCPOS     KKPOS     P
     C****       PSKY      CHAINPPS01                99
     C**N99                MOVELPSDESC    SCPODS    P
     C*** END ADD *** 05/10/00 ****************************************
     C                   MOVEL(P)  DHMOD1        SCMOD1
     C                   MOVEL(P)  DHMOD2        SCMOD2
     C                   MOVEL(P)  DHMOD3        SCMOD3
     C                   MOVEL(P)  DHMOD4        SCMOD4
     C                   MOVEL     DHDXR1        SCDXR1
     C                   MOVEL     DHDXR2        SCDXR2
     C                   MOVEL     DHDXR3        SCDXR3
     C                   MOVEL     DHDXR4        SCDXR4
     C                   Z-ADD     CACHRG        SCTOTC
     C                   Z-ADD     CAPAID        SCTOTP
     C                   Z-ADD     CAMRA         SCMRA
     C                   Z-ADD     CANDIS        SCNTWK
     C                   Z-ADD     CAOTHR        SCOTH1
     C                   Z-ADD     CAOTH2        SCOTH2
     C*** BEG ADD ****** 11/11/13 ************************************
     C                   MOVEL     DHNPIR        SCNPIR
     C*** END ADD ****** 11/11/13 ************************************
     C*** BEG ADD ****** 04/30/08 ************************************
     C* STORE ORIGINAL VALUES
     C                   MOVEL     DHPRCD        XOCPT4
     C                   MOVEL(P)  DHMOD1        XOMOD1
     C                   MOVEL(P)  DHMOD2        XOMOD2
     C                   MOVEL(P)  DHMOD3        XOMOD3
     C                   MOVEL(P)  DHMOD4        XOMOD4
     C*** END ADD ****** 04/30/08 ************************************
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
     C*** BEG ADD *** 05/10/00 ****************************************
     C****                 MOVE SCPOS     DHPOS     P
     C*** END ADD *** 05/10/00 ****************************************
     C*
     C*** BEG ADD ****** 11/11/13 ************************************
     C                   MOVEL     SCNPIR        DHNPIR
     C*** END ADD ****** 11/11/13 ************************************
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C                   MOVEL(P)  SCCPT4        DHPRCD
     C                   MOVEL(P)  SCMOD1        DHMOD1
     C                   MOVEL(P)  SCMOD2        DHMOD2
     C                   MOVEL(P)  SCMOD3        DHMOD3
     C                   MOVEL(P)  SCMOD4        DHMOD4
     C*** END ADD ****** 04/30/08 ************************************
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
     C*** BEG ADD ****** 04/30/08 ************************************
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FDDHP
     C*** BEG ADD ****** 04/30/08 ************************************
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
     C                   ENDSR
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C*================================================================
     C* SRWREP - Write header note record
     C*================================================================
     C     SRWREP        BEGSR
     C* IF FIELDS CHANGED THEN CALL API
     C*
     C     DHPRCD        IFNE      XOCPT4
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
     C                   ENDIF                                                  DHCPT4
     C*
     C     DHMOD1        IFNE      XOMOD1
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL2const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOMOD1        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DHMOD1
     C*
     C     DHMOD2        IFNE      XOMOD2
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL3const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOMOD2        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DHMOD2
     C*
     C     DHMOD3        IFNE      XOMOD3
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL4const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOMOD3        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DHMOD3
     C*
     C     DHMOD4        IFNE      XOMOD4
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL5const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOMOD4        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DHMOD4
     C*
     C                   ENDSR
     C*** END ADD ****** 04/30/08 ************************************
