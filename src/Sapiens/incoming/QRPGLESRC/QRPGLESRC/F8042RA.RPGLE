     H*****************************************************************
     H copyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*                                                               *
     H*  F8042RA- Upload Bills - Transfer Program 2                   *
     H*****************************************************************
      *                                                               *
      *  Called from: F8041RA - Upload Bills - Transfer Pgm 1         *
      *  Calls to   : FA700R, WA052R, RTVCLTID, W7009C AND WA023R1    *
      *                                                               *
      *****************************************************************
      *  Created on 12/28/2005 by C:163 for project 136MBR            *
      *          Copied from F8042R.                                  *
      *                                                               *
      *  Created on 04/20/2004 by c:62 for project 51MFN              *
      *****************************************************************
      *  Revisions:                                                   *
      *                                                               *
      * 03/26/18 C:079 Support: 136 64925                             *
      *                Load the reserve code properly                 *
      *                                                               *
      * 10/26/16 C:079 Support: 40643/136                             *
      *                Remove revision 05/10/16 for U1PROV            *
      *                                                               *
      * 07/14/16 C:309 Project: 136MGV                                *
      *                Modify the Medical Bill Import process for     *
      *                physician and dental bills to lookup procedure *
      *                code in the generic table file using ID = CASV *
      *                                                               *
      *  06/15/16 C:079 Support: 42033(136)                           *
      *                 Z-add units while loading metric qty          *
      *                                                               *
      *  05/10/16 C:062 SUPPORT: 40643(136)                           *
      *                 If U1prov passed, then use the payee name and *
      *                 address from the provider file                *
      *                                                               *
      *  11/02/15 C:309 SUPPORT: 37884                                *
      *                 EOR missing information                       *
      *                                                               *
      *  06/11/14 C:212 PROJECT: 136MGI                               *
      *                 Priss: Fix breaking of Upload Part 1          *
      *                                                               *
      *  05/19/14 C:212 PROJECT: 136MGH                               *
      *                 ICD10 - MC/Prvider Bill Interface Process     *
      *                                                               *
      *  03/05/10 C:220 PROJECT: 136MEX                               *
      *                 Med Bill Upload Use Prov FEIN/Mod             *
      *                                                               *
      *  11/02/09 C:232 PROJECT: 139WMR                               *
      *                 OFAC Reporting-Add Pending Status Indicator.  *
      *                                                               *
      *  06/08/09 C:198 PROJECT 136MEI                                *
      *                 Changed to have the ability to display        *
      *                 'RECORD ONLY' files.                          *
     *                                                               *
     *  10/01/08 C:79  Support: 65351                                *
     *                 Set check requests to RPSTS=PD                *
     *                                                               *
     *  09/12/08 C:79  Project: 136MBR
     *                 Set check requests to RPSTS=PY                *
     *                                                               *
     *  05/27/08 C:47  Project: 136MBR
     *                 WARPP:RPFND wasn't getting loaded if bill was
     *                 a recon.
     *                                                               *
     *  11/26/07 C:79  Project: 136MBR                               *
     *                 Only load U2BPRC if not blank                 *
     *                                                               *
     *  10/03/07 C:47  Project: 136MBR
     *                 FAEOP:EOTAPE MUST BE 'Y' WHEN WRITING OR UPDATING
     *                                                               *
     *  06/13/07 C:079 Project: 136MBR                               *
     *                 Load HCPC Code when Rev Code = blanks         *
     *                                                               *
     *  10/25/05 C:163 Project: 136MBR                               *
     *                 Managed Care/Prov Bill Interface Process      *
     *                                                               *
     *  08/15/05 C:62 PROJECT 132MDN                                 *
     *                ADD CLT132 SPECIFIC CHANGES                    *
     *                                                               *
     *****************************************************************
     *  Indicators used:                                             *
     *                                                               *
     *****************************************************************
     *
     * Transfer files
     *
     FFAU1P     UF   E             DISK
     FFAU1L1    UF   E           K DISK
     F                                     RENAME(PU101:PU102)
     FFAU1L2    UF   E           K DISK
     F                                     RENAME(PU101:PU103)
     FFAU2L1    UF   E           K DISK
     FFAU2L2    UF   E           K DISK
     F                                     RENAME(PU201:PU202)
     FFAU9P     UF   E             DISK
     *
     * Header files
     *
     FFAEOL4    UF A E           K DISK
     FFAEAL1    IF A E           K DISK
     FFAEAL37   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FFAESL2    IF A E           K DISK
     FFAEEL     IF A E           K DISK
     FFAEDL     IF A E           K DISK
     FFAEHL     IF A E           K DISK
     FFAEPL     IF A E           K DISK
     FFAEUL     IF A E           K DISK
     *
     * Detail files
     *
     FFDCAP     O    E             DISK
     FFDDAL2    IF A E           K DISK
     FFDDDL     IF A E           K DISK
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDHL     IF A E           K DISK
     FFDDPL     IF A E           K DISK
     FFDDUL     IF A E           K DISK
     *
     * Check Request file
     *
     FWARPLN    IF A E           K DISK
     *
     * Miscellaneous files
     *
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFDCAL19   UF   E           K DISK
     F                                     RENAME(PCA01:PCA03)
     FFMC1L     UF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCMLB    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMF1L     IF   E           K DISK
     FFMGZL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FSTSTL     IF   E           K DISK
     FWARPL     IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FWMCIL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
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
     FWTPSL     IF   E           K DISK
      *
     F***** Beg Add **** 07/14/16 **************************
     FFTGTL     IF   E           K DISK
     F***** End Add **** 07/14/16 **************************
     *
     D CLM             S              1    DIM(20)
     *****************************************************************
     * Program constants.
     *****************************************************************
     D COMAPI          C                   CONST('FA007R')
     D REAS1           C                   CONST('Completed by -
     D                                     bill upload')
     D REAS2           C                   CONST('Reconsidered by -
     D                                     bill upload')
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
     D CODEDS          C                   CONST('MEDICINE DISPENSED -
     D                                     BY PHYSICIA')
     D TRANS           C                   CONST('Transaction ID:')
     D DIGITS          C                   CONST('0123456789')
     D USERID          C                   CONST('PDSPGMR   ')
     *================================================================
     *  User defined data structures
     *================================================================
     D                 DS
     D DSDTTM                        14  0
     D  DSTIME                        6  0 overlay(DSDTTM)
     D  DSDATE                        8  0 overlay(DSDTTM:7)
     D                 DS
     D CLAIM#                        20
     D  GROUP                         3    overlay(CLAIM#)
     D  YEAR                          3    overlay(CLAIM#:4)
     D  CASE                          7    overlay(CLAIM#:7)
     D                 DS
     D KKCMSV                        11
     D  KKRVCD                        3    INZ('000')
     D                                     overlay(KKCMSV)
     D  KKHCPC                        5    overlay(KKCMSV:4)
     D  KKUNTS                        3    INZ('000')
     D                                     overlay(KKCMSV:9)
     *
     D                 DS
     D XXDXR                          4  0
     D  DX1                           1  0 overlay(XXDXR)
     D  DX2                           1  0 overlay(XXDXR:2)
     D  DX3                           1  0 overlay(XXDXR:3)
     D  DX4                           1  0 overlay(XXDXR:4)
     D                 DS
     D*** Beg Chg *** 07/14/16 ****************************************
     D*** DATA                          50
     D***  QXTRDT                        7  0 overlay(DATA)
     D***  QXFRDT                        7  0 overlay(DATA:8)
     D***  QXTODT                        7  0 overlay(DATA:15)
     D***  QXTRCD                        2    overlay(DATA:22)
     D***  QXRECD                        2    overlay(DATA:24)
     D***  QXPV1                         2    overlay(DATA:26)
     D***  QXPV2                         2    overlay(DATA:28)
     D DATA                          75
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D  QXSEQ#                30     34  0
     D  QXEOB#                35     43
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
     D*** End Chg *** 07/14/16 ****************************************
     *
     D                 DS
     D VALUE                         11
     D  SIGN                          1    overlay(VALUE)
     D  AMOUNT                       10  2 overlay(VALUE:2)
     D                 DS
     D CHRG1                         11
     D  CHRG2                        10  2 overlay(CHRG1:2)
     D                 DS
     D MRA1                          11
     D  MRA2                         10  2 overlay(MRA1:2)
     D                 DS
     D NDIS1                         11
     D  NDIS2                        10  2 overlay(NDIS1:2)
     D                 DS
     D PAID1                         11
     D  PAID2                        10  2 overlay(PAID1:2)
     D                 DS
     D******* BEG ADD  ****** 03/05/10 ************************************
     D U1PROV                        13
     D  U1FEIN1                1      9  0
     D  U1MOD1                10     12  0
     D******* END ADD  ****** 03/05/10 ************************************
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D $$APPL          s              2
     D $$CASE          s              7
     D $$CKTP          s             10
     D $$CO#           s              3
     D*** Beg Chg ***** 07/14/16 ***************************
     D*** $$DATA          s             50
     D $$DATA          s             75
     D*** Beg Chg ***** 07/14/16 ***************************
     D $$DATE          s              7
     D $$FND           s              3
     D $$FYR           s              3
     D $$GRP           s              3
     D $$MODE          s              1
     D $$PCDE          s              2
     D $$RCDE          s              2
     D $$RTCD          s              1
     D $#AMT           s              9  2
     D $#CASE          s              7  0
     D $#CO#           s              3  0
     D $#FEIN          s              9  0
     D $#FND           s              3  0
     D $#FROM          s              7  0
     D $#FYR           s              3  0
     D $#RECD          s              2
     D $#RETP          s              1
     D $#RTCD          s              2
     D $#TO            s              7  0
     D ##CLT#          s              3
     D ##CO#           s              3  0
     D ##CONM          s             40
     D ##FNAM          s             40
     D ##FND           s              3  0
     D ##FYR           s              3  0
     D ##RTCD          s              1
     D ##SUB#          s              5  0
     D #1CO#           s              3
     D #1FND           s              3
     D #1SUB#          s              5
     D #1WCF#          s              3
     D @@NAME          s             10
     D @@PRVC          s              1
     D @@RRN           s              9  0
     D @@RRN2          s              9  0
     D COUNT           s              3  0
     D C132            s              1
     D E               s              1  0
     D HDSEQ#          s                   like(DESEQ#)
     D KKACDT          s                   like(CMACDT)
     D KKCASE          s                   like(CMCASE)
     D KKCO#           s                   like(CMCO#)
     D KKDSEQ          s                   like(CADSEQ)
     D KKEOB#          s                   like(EOEOB#)
     D KKEOBS          s                   like(RPEOBS)
     D KKFEIN          s                   like(PDFEIN)
     D KKFND           s                   like(CMFND)
     D KKFYR           s                   like(CMFYR)
     D KKKEY           s                   like(PCKEY)
     D KKLIN#          s                   like(CALIN#)
     D KKMOD           s                   like(PDMOD)
     D KKPTYP          s                   like(CIPTYP)
     D KKRCN           s                   like(EORCN)
     D KKSEQ#          s                   like(DESEQ#)
     D KKSINV          s                   like(EASINV)
     D KKSS#           s                   like(CMSS#)
     D KKST            s             10
     D KKSUB#          s                   like(CMSUB#)
     D KKSYS           s                   like(CISYS)
     D KKWCF#          s                   like(CMFND)
     D KKZIPA          s                   like(GZZIPA)
     D N               s              3  0
     D RELEAS          s              1
     D SRDTI           s              8  0
     D SRDTO           s             10
     D SRERR           s              1
     D SRFFMT          s              8
     D SRTFMT          s              8
     D USER            s             10
     D X               s              3  0
     D XXAST           s                   like(EOAST)
     D XXCCNT          s              9  0
     D XXCLM#          s             20
     D XXDLRE          s             69
     D XXDSEQ          s                   like(CADSEQ)
     D XXEOBE          s                   like(U2EOB1)
     D XXEODE          s                   like(U2EOD1)
     D XXFIV           s              5  0
     D XXFOU           s              4  0
     D XXFRDT          s              7  0
     D XXONE           s              1  0
     D*** Beg Chg *** 06/15/16 ********************************************
     D****** XXQTY           s              9  0
     D XXQTY           s             10a
     D*** End Chg *** 06/15/16 ********************************************
     D XXRCNT          s              9  0
     D XXSAGE          s                   like(U2SAG1)
     D XXSEQ#          s                   like(RPSEQ#)
     D XXSEV           s              7  0
     D XXSIX           s              6  0
     D XXTHR           s              3  0
     D XXTODT          s              7  0
     D XXTWO           s              2  0
     D XXUCNT          s              9  0
     D***** Beg Add *** 07/14/16 ********************************************
     D isFndWMCIP      s               n   Inz(*off)
     D isMatch         s               n   Inz(*off)
     D xxPRCD          s                   like(DHPRCD)
     D xxPRCDLowr      s                   like(DHPRCD)
     D xxPRCDHigh      s                   like(DHPRCD)
     D***** End Add *** 07/14/16 ********************************************
     *================================================================
     *  /COPY required constants
     *================================================================
     D/COPY SCOPYWW01I
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     IPCM03
     I              CMOSCL                      CWOSCL
     *
     *================================================================
     *  PLIST definitions
     *================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@NAME
     C                   PARM                    @@RRN                          FAU9P RRN
     C                   PARM                    @@RRN2                         FAU1P RRN
     C                   PARM                    @@PRVC                         PROV CHANGE
     *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     *
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     *
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM
     C                   PARM                    ##FNAM
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     *================================================================
     *  KLIST definitions
     *================================================================
     *
     C     C1KY          KLIST
     C                   KFLD                    ##CO#
     *
     C     CAKY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKDSEQ
     *
     C     CIKY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKPTYP
     C                   KFLD                    KKSYS
     *
     C     CAKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     *
     C     CAKY3         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     C                   KFLD                    KKLIN#
     *
     C     CAPKY1        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     *
     C     CAPKY2        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKRCN
     *
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     *
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     *
     C     CMKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     *
     C     CMKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     *
     C     CMKY4         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     *
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     *
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     *
     C     EAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSINV
     *
     C     EOBKY1        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     *
     C     EOBKY2        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    U1EOB#
     C                   KFLD                    U1RCN
     *
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     *
     C     F1KY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     *
     C     GZKY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKZIPA
     *
     C     PDKY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     *
     C     PDKY2         KLIST
     C                   KFLD                    U1PFEI
     C                   KFLD                    U1MOD
     *
     C     P1KY          KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
     *
     C     RPKY1         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     *
     C     RPKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     *
     C     STKY          KLIST
     C                   KFLD                    KKST
     *
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     *
     C     U1KY1         KLIST
     C                   KFLD                    @@NAME
     *
     C     U1KY2         KLIST
     C                   KFLD                    U1EOB#
     C                   KFLD                    U1RCN
     *
     C     U2KY1         KLIST
     C                   KFLD                    U1TRID
     *
     C     U2KY2         KLIST
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     *
     C     EEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     *
     C     EEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKSEQ#
     *
     *================================================================
     *  *LIKE DEFN statements
     *================================================================
     *
     *
     *****************************************************************
     *
     * EXECUTE MAIN SUBROUTINE
     *
     C                   EXSR      MAINSR
     *
     *================================================================
     * ERRCK - ERROR CHECK SUBROUTINE
     *================================================================
     C     ERRCK         BEGSR
     *
     C                   eval      *in90 = *off
     C                   eval      *in91 = *off
     C                   eval      *in92 = *off
     *** BEGIN ADD *** 08/15/05 **************************************
     C                   eval      *in93 = *off
     *** END   ADD *** 08/15/05 **************************************
     C                   eval      KKRCN = *ZEROS
     C**** Beg Add *** 07/14/16 ****************************************
     C                   eval      isFndWMCIP= *off
     C**** End Add *** 07/14/16 ****************************************
     *
     * LETS SEE IF THIS IS A RECONSIDERATION
     *
     *
B001 C                   if        U1RCNF <> 'Y'
 001 *
 001 * Check to see if recon flag was set correctly
 001 *
 001 C                   MOVEL(P)  U1TRID        KKSINV
 001 C     EAKY          CHAIN     PEA02                              99
B002 C                   if        *IN99 = *ON
 002 *
 002 * Check WCMC Claim Master to see if SS#/ACDT exists.
 002 *
 002 C                   EXSR      GETCLM
 002 C     CMKY3         CHAIN     PCM01                              99
B003 C                   if        *IN99 = *ON
 003 *
 003 C                   eval      COUNT = *ZEROS
 003 C                   MOVEL     U1SS#         KKSS#
 003 C                   eval      SRDTI = U1DOI
 003 C                   CALL      'SRCVTDAT'    DTCYM2
 003 C                   MOVEL     SRDTO         KKACDT
 003 *
 003 C     CMKY1         CHAIN     PCM02                              99
B004 C                   dow       *IN99 = *OFF
 004 C                   eval      COUNT = COUNT + 1
 004 C     CMKY1         READE     PCM02                                  99
E004 C                   ENDDO
X003 C                   ELSE
 003 C                   eval      COUNT = 1
E003 C                   ENDIF
 002 *
B003 C                   SELECT
 003 *
 003 * SS#/ACDT doesn't exist.
 003 *
 003 C                   when      COUNT = 0
 003 C                   eval      *in90 = *on
 003 *
 003 * Let's get the WCMS group number
 003 *
 003 C                   when      COUNT = 1
 003 C                   MOVE      ##CO#         #1CO#
 003 C                   MOVE      CMSUB#        #1SUB#
 003 C                   MOVE      CMFND         #1FND
 003 *
 003 C                   CALL      'FA700R'
 003 C                   PARM      *BLANKS       ##RTCD
 003 C                   PARM                    #1CO#
 003 C                   PARM      *BLANKS       #1WCF#
 003 C                   PARM                    #1SUB#
 003 C                   PARM                    #1FND
 003 *
B004 C                   if        ##RTCD = *BLANKS
 004 C                   MOVE      #1WCF#        KKWCF#
X004 C                   ELSE
 004 C                   eval      KKWCF# = *ZEROS
E004 C                   ENDIF
     C*** BEG ADD *** 06/08/09 ****************************************
     C     CMLTFL        IFEQ      'R'
     C                   eval      *IN90 = *on
     C                   ENDIF
     C*** END ADD *** 06/08/09 ****************************************
 003 *
 003 * SS#/ACDT occurs more than once.
 003 *
 003 C                   when      COUNT > 1
 003 C                   eval      *in90 = *on
 003 *
E003 C                   ENDSL
 002 *
 002 * FEIN/MOD does not exist.
 002 *
B003 C                   if        *IN90 = *OFF
     C*** BEG CHG *** 03/05/10 ****************************************
B003 C                   IF        U1PROV <> *BLANKS
 003 C***********        MOVE      U1FEIN        KKFEIN
 003 C                   MOVE      U1FEIN1       KKFEIN
 003 C***********        eval      KKMOD = *ZEROS
 003 C                   eval      KKMOD = U1MOD1
     C*** Begin Add *** 05/10/16 **************************************
     C*** Begin Del *** 10/26/16 **************************************
     C***********        Else
 003 C***********        Eval      kkFein = U1fein
 003 C***********        Eval      kkMod  = *Zeros
     C*** End   Del *** 10/26/16 **************************************
     C*** Else  Add *** 05/10/16 **************************************
B003 C                   ENDIF
     C*** END CHG *** 03/05/10 ****************************************
 003 *
 003 C     PDKY1         CHAIN     PPD01                              99
B004 C                   if        *IN99 = *ON
 004 C                             or *IN99 = *OFF
     C*** BEG CHG *** 11/02/09 **************************************
 004 C****                         and PDSTAT = 'I'
     C                             and PDSTAT <> 'A'
     C                             and PDSTAT <> *BLANKS
     C*** END CHG *** 11/02/09 **************************************
 004 C                   eval      *in90 = *on
E004 C                   ENDIF
E003 C                   ENDIF
 002 *** BEGIN ADD *** 08/15/05 **************************************
 002 *
 002 * IF NOT PROVIMA CHECK THEN CHECK TO SEE IF DUPLICATE
 002 *
B003 C                   if        C132 = *ON
B004 C                   if        U1RCNF = ' '
 004 C                             and *IN90 = *OFF
 004 C                             or U1RCNF = 'N'
 004 C                             and *IN90 = *OFF
 004 *
 004 C                   MOVE      U1PAID        VALUE
B005 C                   if        SIGN = '-'
 005 C                   eval      $#AMT = 0 - AMOUNT
X005 C                   ELSE
 005 C                   eval      $#AMT = AMOUNT
E005 C                   ENDIF
 004 *
B005 C                   if        $#AMT <> *ZEROS
 005 *
 005 *
 005 C                   eval      SRDTI = U1FRDT
 005 C                   CALL      'SRCVTDAT'    DTCYM2
 005 C                   MOVEL     SRDTO         XXFRDT
 005 *
 005 C                   eval      SRDTI = U1TODT
 005 C                   CALL      'SRCVTDAT'    DTCYM2
 005 C                   MOVEL     SRDTO         XXTODT
 005 *
 005 * Check for duplicate request/payment
 005 *
 005 C                   CALL      'WA052R'
 005 C                   PARM      CMCO#         $#CO#                          Company #
 005 C                   PARM      KKWCF#        $#FND                          Fund
 005 C                   PARM      CMFYR         $#FYR                          Fund Year
 005 C                   PARM      CMCASE        $#CASE                         Case
 005 C                   PARM      XXFRDT        $#FROM                         From Date
 005 C                   PARM      XXTODT        $#TO                           To Date
 005 C                   PARM                    $#AMT                          Amount
 005 C                   PARM      'ME'          $#RECD                         Reserve Code
 005 C                   PARM      PDFEIN        $#FEIN                         FEIN #
 005 C                   PARM      'M'           $#RETP                         Reserve Type
 005 C                   PARM      *BLANKS       $#RTCD
 005 *
B006 C                   if        $#RTCD <> *BLANKS
B007 C                   if        $#RTCD = 'DR'
 007 C                             or $#RTCD = 'DP'
 007 C                   eval      *in93 = *on
 007 C                   eval      *in90 = *on
E007 C                   ENDIF
E006 C                   ENDIF
 005 *
E005 C                   ENDIF
E004 C                   ENDIF
E003 C                   ENDIF
 002 *** END   ADD *** 08/15/05 **************************************
 002 *
X002 C                   ELSE
 002 C                   eval      *in90 = *on
 002 C                   eval      *in91 = *on
E002 C                   ENDIF
 001 *
X001 C                   ELSE
 001 *
 001 * LET'S FIND THE ORIGINAL BILL
 001 *
 001 C                   MOVEL(P)  U1TRID        KKSINV
 001 C     EAKY          CHAIN     PEA02                              99
B002 C                   if        *IN99 = *ON
 002 C                   eval      *in90 = *on
 002 C                   eval      *in91 = *on
X002 C                   ELSE
 002 *
B003 C                   if        U1EOB# = *ZEROS
 003 C     EOKY          CHAIN(N)  PEO01                              99
 003 C                   eval      KKRCN = EORCN + 1
X003 C                   ELSE
 003 C                   eval      KKRCN = U1RCN
E003 C                   ENDIF
 002 *
 002 C                   eval      *in92 = *on
E002 C                   ENDIF
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * GETAST - GET THE ADJUDICATION STATE
     *================================================================
     C     GETAST        BEGSR
     *
     C                   MOVEL(P)  'UC'          XXAST
     C                   MOVEL(P)  CMAST         KKST
     *
     C     STKY          CHAIN     PST01                              99
B001 C                   if        *IN99 = *OFF
 001 C                             and STINST = 'Y'
 001 C                   MOVE(P)   CMAST         XXAST
X001 C                   ELSE
 001 C     P1KY          CHAIN     PP101                              99
B002 C                   if        *IN99 = *OFF
B003 C                   if        P1ST <> *BLANKS
 003 C                   MOVEL(P)  P1ST          XXAST
E003 C                   ENDIF
E002 C                   ENDIF
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * GETCLM - GET THE CLAIM NUMBER
     *================================================================
     C     GETCLM        BEGSR
     *
     C                   eval      XXCLM# = *BLANKS
     C                   eval      XXONE = *ZEROS
     C                   eval      XXTWO = *ZEROS
     C                   eval      XXTHR = *ZEROS
     C                   eval      XXFOU = *ZEROS
     C                   eval      XXFIV = *ZEROS
     C                   eval      XXSIX = *ZEROS
     C                   eval      XXSEV = *ZEROS
     C                   eval      KKWCF# = *ZEROS
     C                   eval      KKSUB# = *ZEROS
     C                   eval      KKFND = *ZEROS
     C                   eval      KKFYR = *ZEROS
     C                   eval      KKCASE = *ZEROS
     *
     * LET'S EXTRACT THE GROUP
     *
     C                   CLEAR                   CLM
     C                   MOVEA     U1CLM#        CLM
     C                   eval      X = 1
     C     '-'           LOOKUP    CLM(X)                                 99
B001 C                   if        *IN99 = *ON
B002 C                   SELECT
 002 C                   when      X = 2
 002 C                   MOVEL     U1CLM#        XXONE
 002 C                   eval      KKWCF# = XXONE
 002 C                   when      X = 3
 002 C                   MOVEL     U1CLM#        XXTWO
 002 C                   eval      KKWCF# = XXTWO
 002 C                   when      X = 4
 002 C                   MOVEL     U1CLM#        XXTHR
 002 C                   eval      KKWCF# = XXTHR
E002 C                   ENDSL
 001 *
 001 C                   eval      X = X + 1
 001 C                   MOVEA(P)  CLM(X)        XXCLM#
 001 *
 001 * LET'S GET THE YEAR
 001 *
 001 C                   CLEAR                   CLM
 001 C                   MOVEA     XXCLM#        CLM
 001 C                   eval      X = 1
 001 C     '-'           LOOKUP    CLM(X)                                 99
B002 C                   if        *IN99 = *ON
B003 C                   SELECT
 003 C                   when      X = 2
 003 C                   MOVEL     XXCLM#        XXONE
 003 C                   eval      KKFYR = XXONE
 003 C                   when      X = 3
 003 C                   MOVEL     XXCLM#        XXTWO
 003 C                   eval      KKFYR = XXTWO
 003 C                   when      X = 4
 003 C                   MOVEL     XXCLM#        XXTHR
 003 C                   eval      KKFYR = XXTHR
E003 C                   ENDSL
 002 C                   eval      X = X + 1
 002 C                   MOVEA(P)  CLM(X)        XXCLM#
E002 C                   ENDIF
 001 *
 001 * LET'S GET THE YEAR
 001 *
 001 C                   CLEAR                   CLM
 001 C                   MOVEA     XXCLM#        CLM
 001 C                   eval      X = 1
 001 C     ' '           LOOKUP    CLM(X)                                 99
B002 C                   if        *IN99 = *ON
B003 C                   SELECT
 003 C                   when      X = 2
 003 C                   MOVEL     XXCLM#        XXONE
 003 C                   eval      KKCASE = XXONE
 003 C                   when      X = 3
 003 C                   MOVEL     XXCLM#        XXTWO
 003 C                   eval      KKCASE = XXTWO
 003 C                   when      X = 4
 003 C                   MOVEL     XXCLM#        XXTHR
 003 C                   eval      KKCASE = XXTHR
 003 C                   when      X = 5
 003 C                   MOVEL     XXCLM#        XXFOU
 003 C                   eval      KKCASE = XXFOU
 003 C                   when      X = 6
 003 C                   MOVEL     XXCLM#        XXFIV
 003 C                   eval      KKCASE = XXFIV
 003 C                   when      X = 7
 003 C                   MOVEL     XXCLM#        XXSIX
 003 C                   eval      KKCASE = XXSIX
 003 C                   when      X = 8
 003 C                   MOVEL     XXCLM#        XXSEV
 003 C                   eval      KKCASE = XXSEV
E003 C                   ENDSL
E002 C                   ENDIF
X001 C                   ELSE
 001 C                   MOVEL     U1CLM#        CLAIM#
 001 C                   MOVE      GROUP         KKWCF#
 001 C                   MOVE      YEAR          KKFYR
 001 C                   MOVE      CASE          KKCASE
E001 C                   ENDIF
     *
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      KKWCF#        #1WCF#
     *
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    #1WCF#
     C                   PARM      *BLANKS       #1SUB#
     C                   PARM      *BLANKS       #1FND
     *
B001 C                   if        ##RTCD = *BLANKS
 001 C                   MOVE      #1SUB#        KKSUB#
 001 C                   MOVE      #1FND         KKFND
X001 C                   ELSE
 001 C                   eval      KKSUB# = *ZEROS
 001 C                   eval      KKFND = *ZEROS
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * MAINSR - MAIN SUBROUTINE
     *================================================================
     C     MAINSR        BEGSR
     *
     *** BEGIN ADD *** 08/15/05 **************************************
     C                   MOVE      *OFF          RELEAS
     *
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#
     *
     C                   MOVE      *OFF          C132
B001 C                   if        ##CLT# = '132'
 001 C                   MOVE      *ON           C132
E001 C                   ENDIF
     *** END   ADD *** 08/15/05 **************************************
     C                   CALL      COMAPI        COMPL                  99
     C                   eval      *in16 = *off
     C                   eval      XXCCNT = *ZEROS
     C                   eval      XXRCNT = *ZEROS
     C                   eval      XXUCNT = *ZEROS
     *
B001 C                   if        Q1PRMS > 2
 001 *
 001 C     @@RRN2        CHAIN(N)  PU101                              98
B002 C                   if        *in98 = *off
 002 C     CMKY2         CHAIN     PCM01                              98
E002 C                   endif
B002 C                   if        *in98 = *off
 002 C     PDKY2         CHAIN     PPD01                              98
E002 C                   endif
B002 C                   if        *IN98 = *OFF
 002 *
 002 C                   MOVE      ##CO#         #1CO#
 002 C                   MOVE      CMSUB#        #1SUB#
 002 C                   MOVE      CMFND         #1FND
 002 *
 002 C                   CALL      'FA700R'
 002 C                   PARM      *BLANKS       ##RTCD
 002 C                   PARM                    #1CO#
 002 C                   PARM      *BLANKS       #1WCF#
 002 C                   PARM                    #1SUB#
 002 C                   PARM                    #1FND
 002 *
B003 C                   if        ##RTCD = *BLANKS
 003 C                   MOVE      #1WCF#        KKWCF#
X003 C                   ELSE
 003 C                   eval      KKWCF# = *ZEROS
E003 C                   ENDIF
 002 *
B003 C                   if        @@PRVC = 'Y'
 003 C                   eval      *in16 = *on
E003 C                   ENDIF
 002 C                   EXSR      WRITER
 002 C                   EXSR      UPDAT1
E002 C                   ENDIF
 001 *
X001 C                   ELSE
 001 *
 001 C     U1KY1         CHAIN     PU102                              98
B002 C                   if        *IN98 = *OFF
B003 C                   dow       *IN98 = *OFF
 003 *
 003 C                   EXSR      ERRCK
 003 C                   EXSR      UPDT1A
B004 C                   if        *IN90 = *OFF
 004 C                   EXSR      WRITER
 004 C                   EXSR      UPDT1B
E004 C                   ENDIF
 003 *
 003 C     U1KY1         READE     PU102                                  98
B004 C                   if        *in98 = *on
 004 C                   EXSR      UPDAT9
E004 C                   endif
E003 C                   ENDDO
X002 C                   ELSE
 002 C                   EXSR      UPDAT9
E002 C                   ENDIF
E001 C                   ENDIF
     *
     C                   EXSR      PGMEND
     *
     C                   ENDSR
     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     PGMEND        BEGSR
     *
     *** BEGIN ADD *** 08/15/05 **************************************
B001 C                   if        C132 = *ON
 001 C                             and RELEAS = *ON
 001 *
 001 C                   MOVE      ##CO#         $$CO#
 001 *
 001 C                   CALL      'W7009C'
 001 C                   PARM                    $$CO#
 001 C                   PARM      '0000000'     $$DATE
 001 C                   PARM      'PD'          $$PCDE
 001 C                   PARM      'PY'          $$RCDE
 001 C                   PARM      '000'         $$GRP
 001 *
E001 C                   ENDIF
     *** END   ADD *** 08/15/05 **************************************
     *
     C                   eval      *inLR = *on
     *
     C                   ENDSR
     *================================================================
     * UPDT1A - Update Provima Bill Header Record (FAU1P) - 1ST UPDATE
     *================================================================
     C     UPDT1A        BEGSR
     *
B001 C                   if        *IN90 = *ON
B002 C                   if        *IN91 = *OFF
 002 C                   MOVEL(P)  'E'           U1PRFL
 002 C                   eval      XXCCNT = XXCCNT + 1
X002 C                   ELSE
 002 C                   MOVEL(P)  'R'           U1PRFL
 002 C                   eval      XXRCNT = XXRCNT + 1
E002 C                   ENDIF
 001 *** BEGIN ADD *** 08/15/05 **************************************
B002 C                   if        *IN93 = *ON
 002 C                   MOVE      'D'           U1DLRE
E002 C                   ENDIF
 001 *** END   ADD *** 08/15/05 **************************************
     C*** BEG ADD *** 06/08/09 ****************************************
     C     CMLTFL        IFEQ      'R'
     C                   MOVE      'R'           U1DLRE
     C                   ENDIF
     C*** END ADD *** 06/08/09 ****************************************
X001 C                   ELSE
 001 C                   eval      XXUCNT = XXUCNT + 1
 001 *
B002 C                   if        *IN92 = *OFF
B003 C                   if        U1EOB# = *ZEROS
 003 C     C1KY          CHAIN     PC101                              99
B004 C                   if        *IN99 = *OFF
 004 C                   eval      C1EOB# = C1EOB# + 1
 004 C                   UPDATE    PC101
E004 C                   ENDIF
 003 C                   eval      U1EOB# = C1EOB#
E003 C                   ENDIF
 002 *
 002 C                   eval      U1CO# = CMCO#
 002 C                   eval      U1SUB# = CMSUB#
 002 C                   eval      U1FND = CMFND
 002 C                   eval      U1FYR = CMFYR
 002 C                   eval      U1CASE = CMCASE
 002 C                   eval      U1PFEI = PDFEIN
 002 C                   eval      U1MOD = PDMOD
 002 C                   MOVEL(P)  CMSS#         U1PSS#
 002 C                   eval      U1ACDT = CMACDT
 002 C                   eval      U1RCN = *ZEROS
X002 C                   ELSE
B003 C                   if        U1EOB# = *ZEROS
 003 C                   eval      U1CO# = EOCO#
 003 C                   eval      U1SUB# = EOSUB#
 003 C                   eval      U1FND = EOFND
 003 C                   eval      U1FYR = EOFYR
 003 C                   eval      U1CASE = EOCASE
 003 C                   eval      U1EOB# = EOEOB#
 003 C                   eval      U1RCN = KKRCN
 003 C                   eval      U1PFEI = EOFEIN
 003 C                   eval      U1MOD = EOMOD
 003 C                   MOVEL(P)  EOSS#         U1PSS#
 003 C                   eval      U1ACDT = EOACDT
E003 C                   ENDIF
E002 C                   ENDIF
 001 *
E001 C                   ENDIF
     *
     C                   UPDATE    PU102
     *
     C                   ENDSR
     *================================================================
     * UPDT1B - Update Provima Bill Header Record (FAU1P) - 2ND UPDATE
     *================================================================
     C     UPDT1B        BEGSR
     *
     C     U1KY2         CHAIN     PU103                              99
B001 C                   if        *IN99 = *OFF
 001 *
 001 C                   TIME                    DSDTTM
 001 C                   eval      SRDTI = DSDATE
 001 C                   CALL      'SRCVTDAT'    DTCYMD
 001 C                   MOVEL     SRDTO         U1PRDT
 001 *
 001 C                   eval      U1DLUS = *BLANKS
 001 C                   eval      U1DLPG = *BLANKS
 001 C                   eval      U1DLTM = *ZEROS
 001 C                   eval      U1DLDT = *ZEROS
 001 *** BEGIN CHG *** 08/15/05 **************************************
 001 **********           MOVEL*BLANKS   U1DLRE
 001 C                   eval      XXDLRE = *BLANKS
 001 C                   MOVEL     XXDLRE        U1DLRE
 001 *** END   CHG *** 08/15/05 **************************************
 001 C                   MOVEL(P)  'Y'           U1PRFL
 001 *
 001 C                   UPDATE    PU103
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * UPDAT1 - Update Provima Bill Header Record (FAU1P) - SINGLE MODE
     *================================================================
     C     UPDAT1        BEGSR
     *
     C     @@RRN2        CHAIN     PU101                              98
B001 C                   if        *IN98 = *OFF
 001 *
 001 C                   MOVEL(P)  'Y'           U1PRFL
 001 C                   TIME                    DSDTTM
 001 C                   eval      SRDTI = DSDATE
 001 C                   CALL      'SRCVTDAT'    DTCYMD
 001 C                   MOVEL     SRDTO         U1PRDT
 001 *
 001 C                   UPDATE    PU101
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * UPDT2A - Update Provima Bill Detail Record (FAU2P) - 1ST UPDATE
     *================================================================
     C     UPDT2A        BEGSR
     *
     C                   eval      U2EOB# = CAEOB#
     C                   eval      U2DSEQ = CADSEQ
     *
     C                   UPDATE    PU201
     *
     C                   ENDSR
     *================================================================
     * UPDT2B - Update Provima Bill Detail Record (FAU2P) - 2ND UPDATE
     *================================================================
     C     UPDT2B        BEGSR
     *
     C     U2KY2         CHAIN     PU202                              99
B001 C                   if        *IN99 = *OFF
 001 C                   MOVEL(P)  'Y'           U2PRFL
 001 C                   UPDATE    PU202
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * UPDAT9 - Update Provima Bill Transmission File (FAU9P).
     *================================================================
     C     UPDAT9        BEGSR
     *
     C     U1KY1         CHAIN(N)  PU102                              99
B001 C                   if        *IN99 = *ON
 001 C     @@RRN         CHAIN     PU901                              99
B002 C                   if        *IN99 = *OFF
 002 *
B003 C                   if        U9CCNT = *ZEROS
 003 C                             and U9RCNT = *ZEROS
 003 C                             and U9UCNT = *ZEROS
 003 C                   eval      U9CCNT = XXCCNT
 003 C                   eval      U9RCNT = XXRCNT
 003 C                   eval      U9UCNT = XXUCNT
E003 C                   ENDIF
 002 *
 002 C                   MOVEL(P)  Q1USER        U9USER
 002 C                   MOVEL(P)  Q1PGM         U9PGM
 002 C                   TIME                    DSDTTM
 002 C                   eval      SRDTI = DSDATE
 002 C                   CALL      'SRCVTDAT'    DTCYMD
 002 C                   MOVEL     SRDTO         U9PRDT
 002 C                   MOVEL     SRDTO         U9DT
 002 C                   eval      U9PRTM = DSTIME
 002 *
 002 C                   UPDATE    PU901
E002 C                   ENDIF
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * UPDCAP - Update FDCAP record
     *================================================================
     C     UPDCAP        BEGSR
     *
     C                   eval      KKLIN# = U2LIN#
     *
     C     CAKY3         CHAIN     PCA03                              99
B001 C                   if        *IN99 = *OFF
 001 *
 001 C                   MOVEL(P)  EOUSER        CAUSER
 001 C                   MOVEL(P)  EOPGM         CAPGM
 001 C                   eval      CADT = EODT
 001 C                   eval      CATIME = EOTIME
 001 *
 001 C                   MOVE      U2CHRG        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      CACHRG = CACHRG - AMOUNT
X002 C                   ELSE
 002 C                   eval      CACHRG = CACHRG + AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2MRA         VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      CAMRA = CAMRA - AMOUNT
X002 C                   ELSE
 002 C                   eval      CAMRA = CAMRA + AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2NDIS        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      CANDIS = CANDIS - AMOUNT
X002 C                   ELSE
 002 C                   eval      CANDIS = CANDIS + AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2PAID        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      CAPAID = CAPAID - AMOUNT
X002 C                   ELSE
 002 C                   eval      CAPAID = CAPAID + AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   UPDATE    PCA03
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     WRITER        BEGSR
     *
     * Get the adjudication state
     *
     C                   EXSR      GETAST
     *
     * Write the header records
     *
     C                   EXSR      WRTEOP                                       FAEOP
     C                   EXSR      WRTEAP                                       FAEAP
     C                   EXSR      WRTESP                                       FAESP
     C                   EXSR      WRTEEP                                       FAEEP
     *
B001 C                   SELECT
 001 C                   when      EOBTYP = 'D'
 001 C                   EXSR      WRTEDP                                       FAEDP
 001 C                   when      EOBTYP = 'H'
 001 C                   EXSR      WRTEUP                                       FAEUP
 001 C                   when      EOBTYP = 'N'
 001 C                   EXSR      WRTEHP                                       FAEHP
 001 C                   when      EOBTYP = 'X'
 001 C                   EXSR      WRTEPP                                       FAEPP
E001 C                   ENDSL
     *
     * Write the detail records
     *
     C                   EXSR      WRTDTL
     *
     * Write the check request
     *
B001 C                   if        EAPLNK = '3'
 001 C                   EXSR      WRTRPP                                       WARPP
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTEOP - Write FAEOP record
     *================================================================
     C     WRTEOP        BEGSR
     *
     * WRITE FIRST EOMB
     *
     C     EOBKY1        CHAIN     PEO01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PEO01
 001 C                   eval      EOCO# = U1CO#
 001 C                   eval      EOFND = U1FND
 001 C                   eval      EOFYR = U1FYR
 001 C                   eval      EOCASE = U1CASE
 001 *
 001 C                   eval      SRDTI = U1TRDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EOTRDT
 001 *
 001 C                   MOVE      U1PFEI        EOFEIN
 001 C                   eval      EOMOD = U1MOD
 001 *
 001 C                   eval      SRDTI = U1FRDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EOFRDT
 001 *
 001 C                   eval      SRDTI = U1TODT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EOTODT
 001 *
B002 C                   if        *IN16 = *ON
B003 C                   if        PDDBA1 <> 'Y'
B004 C                   if        PDFSN = *BLANKS
 004 C                   MOVE      PDNAME        EOPNAM
X004 C                   ELSE
 004 C     PDFSN         CAT(P)    PDNAME:1      EOPNAM
E004 C                   ENDIF
 003 C                   MOVE      PDADR1        EOADR1
 003 C                   MOVE      PDADR2        EOADR2
X003 C                   ELSE
 003 C                   MOVE      PDADR1        EOPNAM
 003 C                   MOVE      PDADR2        EOADR1
 003 C                   eval      EOADR2 = *BLANKS
E003 C                   ENDIF
 002 C                   MOVE      PDCTY         EOCTY
 002 C                   MOVE      PDST          EOST
 002 C                   MOVE      PDZIP         EOZIP
X002 C                   ELSE
B003 C                   if        U1PFSN = *BLANKS
 003 C                   MOVEL(P)  U1PNAM        EOPNAM
X003 C                   ELSE
 003 C     U1PFSN        CAT(P)    U1PNAM:1      EOPNAM
E003 C                   ENDIF
 002 C                   MOVEL(P)  U1ADR1        EOADR1
 002 C                   MOVEL(P)  U1ADR2        EOADR2
 002 C                   MOVEL(P)  U1CTY         EOCTY
 002 C                   MOVEL(P)  U1ST          EOST
 002 C                   MOVE      U1ZIP         EOZIP
     C*** Begin Add *** 05/10/16 *********************************
     C*** Begin Del *** 10/26/16 *********************************
     C***********
     C* If Provider Fein and Mod passed in U1PROV then get the name and address
     C* from the provider file.
     C***********
     C***********        If        U1prov <> *Blanks
     C***********
B003 C***********        If        PDdba1 <> 'Y'
     C***********
B004 C***********        If        PDfsn = *blanks
 004 C***********        Eval      EOpNam = PDname
X004 C***********        Else
     C***********        Eval      EOpNam = %Trim(PDfsn) + ' ' + %Trim(PDname)
E004 C***********        EndIf
     C***********
 003 C***********        Eval      EOadr1 = PDadr1
 003 C***********        Eval      EOadr2 = PDadr2
     C***********
X003 C***********        Else
     C***********
     C***********        Eval      EOPnam = PDadr1
     C***********        Eval      EOadr1 = PDadr2
 003 C***********        Eval      EOadr2 = *Blanks
E003 C***********        EndIf
     C***********
 002 C***********        Eval      EOcty = PDcty
 002 C***********        Eval      EOst  = PDst
 002 C***********        Eval      EOzip = PDzip
     C***********
     C***********        EndIf
     C*** End   Del *** 10/26/16 *********************************
     C*** End   Add *** 05/10/16 *********************************
E002 C                   ENDIF
 001 *
 001 C                   MOVEL     PDSPCD        EOEOB1
 001 *
 001 C                   eval      EOCHK# = U1EOB#
 001 C                   eval      EOEOB# = U1EOB#
 001 C                   eval      EORCN = KKRCN
 001 *
B002 C                   if        U1BTYP = 'H'
 002 C                   MOVEL(P)  'H'           EOSYS
X002 C                   ELSE
 002 C                   MOVEL(P)  'P'           EOSYS
E002 C                   ENDIF
 001 *** BEGIN ADD *** 08/15/05 **************************************
 001 C                   MOVEL(P)  USERID        USER
 001 *
B002 C                   SELECT
 002 C                   when      C132 = *ON
 002 *
B003 C                   SELECT
 003 C                   when      U1NTWK = 'COMP'
 003 C                   MOVEL(P)  'COMPIQ'      USER
 003 C                   when      U1NTWK = 'BC'
 003 C                   MOVEL(P)  'BC'          USER
E003 C                   ENDSL
 002 *
E002 C                   ENDSL
 001 *** END   ADD *** 08/15/05 **************************************
 001 *
 001 C                   MOVEL(P)  USER          EOUSER
 001 C                   MOVEL(P)  Q1PGM         EOPGM
 001 C                   TIME                    DSDTTM
 001 C                   eval      EOTIME = DSTIME
 001 C                   eval      SRDTI = DSDATE
 001 C                   CALL      'SRCVTDAT'    DTCYMD
 001 C                   MOVEL     SRDTO         EODT
 001 C                   MOVEL(P)  USER          EOIUSR
 001 C                   MOVEL     SRDTO         EOIDT
 001 *
 001 C                   MOVEL(P)  U1ICD1        EODIA1
 001 C                   MOVEL(P)  U1ICD2        EODIA2
 001 C                   MOVEL(P)  U1ICD3        EODIA3
 001 C                   MOVEL(P)  U1ICD4        EODIA4
 001 *
B002 C                   if        U1LIC# = *BLANKS
 002 C                   MOVEL(P)  'N'           EOTAPE
X002 C                   ELSE
 002 C                   MOVEL(P)  'Y'           EOTAPE
E002 C                   ENDIF
 001 *
 001 *** BEG ADD *** 10/03/07 **************************************
B002 C                   if        ##CLT# = '136'
 002 C                   MOVEL(P)  'Y'           EOTAPE
E002 C                   ENDIF
 001 *** END ADD *** 10/03/07 **************************************
 001 *
 001 C                   MOVEL     U1ZIPA        EOFLIC
 001 C                   eval      EO#BLL = 1
 001 C                   MOVEL     XXAST         EOAST
 001 C                   eval      EOSUB# = U1SUB#
 001 *
 001 C                   MOVEL(P)  U1ZIPA        KKZIPA
 001 C     GZKY          SETGT     PGZ01
 001 C                   READP     PGZ01                                  99
B002 C                   if        *IN99 = *OFF
 002 C                             and KKZIPA <= GZTOZP
 002 C                   MOVE      GZZIPA        EOZIPA
X002 C                   ELSE
 002 C                   eval      EOZIPA = *BLANKS
E002 C                   ENDIF
 001 *
 001 C                   MOVEL(P)  U1PSS#        EOSS#
 001 C                   eval      EOACDT = U1ACDT
 001 *
 001 C                   MOVEL(P)  Q1PGM         EOIPGM
 001 C                   eval      EOITIM = DSTIME
 001 *
B002 C                   SELECT
 002 C                   when      U1BTYP = 'M'
 002 C                   MOVEL(P)  'N'           EOBTYP
 002 C                   when      U1BTYP = 'H'
 002 C                   MOVEL(P)  'H'           EOBTYP
 002 C                   when      U1BTYP = 'P'
 002 C                   MOVEL(P)  'X'           EOBTYP
 002 C                   when      U1BTYP = 'D'
 002 C                   MOVEL(P)  'D'           EOBTYP
E002 C                   ENDSL
 001 *
 001 C                   MOVEL(P)  'C'           EOBLST
 001 C                   MOVEL     '2'           EOCSCD
 001 *
 001 C                   eval      SRDTI = U1CSDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EOCSDT
 001 *
 001 C                   eval      SRDTI = U1CRDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EOCRDT
 001 *
 001 C                   MOVEL     U1TRID        EOSINV
 001 C                   MOVEL(P)  U1CLM#        EOCID#
 001 *
 001 C                   MOVE      U1CHRG        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      EOCHRG = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      EOCHRG = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U1MRA         VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      EOMRA = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      EOMRA = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U1NDIS        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      EONDIS = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      EONDIS = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U1PAID        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      EOPAID = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      EOPAID = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVEL(P)  U1NTWK        EONTWK
 001 *
B002 C                   if        U1SPCD = *BLANKS
 002 C                   MOVEL     PDSPCD        EOSPCD
X002 C                   ELSE
 002 C                   MOVEL     U1SPCD        EOSPCD
E002 C                   ENDIF
 001 *
 001 C                   MOVEL(P)  U1PSS#        KKSS#
 001 C     CDKY          CHAIN     PCD01                              99
B002 C                   if        *IN99 = *OFF
 002 C                   MOVEL(P)  CDLSN         EOLSN
 002 C                   MOVEL(P)  CDFSN         EOFSN
 002 C                   MOVEL(P)  CDMID         EOMID
X002 C                   ELSE
 002 C                   eval      EOLSN = *BLANKS
 002 C                   eval      EOFSN = *BLANKS
 002 C                   eval      EOMID = *BLANKS
E002 C                   ENDIF
 001 *
 001 C                   MOVEL(P)  U1IDOC        EOIDOC
 001 C                   MOVEL(P)  U1CONT        EOCONT
 001 C                   eval      EOFFEI = U1FFEI
 001 C                   MOVEL(P)  U1FNAM        EOFNAM
 001 C                   eval      EOFZIP = U1FZIP
 001 C                   MOVEL(P)  U1BLIC        EOBLIC
 001 C                   MOVEL(P)  U1BTYC        EOBTYC
 001 C                   MOVEL(P)  U1RNAM        EORNAM
 001 C                   eval      EOSFEI = U1RFEI
 001 C                   MOVEL(P)  U1BLID        EOBLID
 001 *
 001 C                   WRITE     PEO01
X001 C                   ELSE
 001 *
 001 * CORVEL HAS RECONSIDERED THIS BILL
 001 *
B002 C                   if        *IN92 = *ON
 002 *
 002 C                   MOVE      U1CHRG        VALUE
B003 C                   if        SIGN = '-'
 003 C                   eval      EOCHRG = EOCHRG - AMOUNT
X003 C                   ELSE
 003 C                   eval      EOCHRG = EOCHRG + AMOUNT
E003 C                   ENDIF
 002 *
 002 C                   MOVE      U1MRA         VALUE
B003 C                   if        SIGN = '-'
 003 C                   eval      EOMRA = EOMRA - AMOUNT
X003 C                   ELSE
 003 C                   eval      EOMRA = EOMRA + AMOUNT
E003 C                   ENDIF
 002 *
 002 C                   MOVE      U1NDIS        VALUE
B003 C                   if        SIGN = '-'
 003 C                   eval      EONDIS = EONDIS - AMOUNT
X003 C                   ELSE
 003 C                   eval      EONDIS = EONDIS + AMOUNT
E003 C                   ENDIF
 002 *
 002 C                   MOVE      U1PAID        VALUE
B003 C                   if        SIGN = '-'
 003 C                   eval      EOPAID = EOPAID - AMOUNT
X003 C                   ELSE
 003 C                   eval      EOPAID = EOPAID + AMOUNT
E003 C                   ENDIF
 002 *
 002 C                   eval      EORCN = KKRCN
 002 C                   MOVEL(P)  U1TRID        EOSINV
E002 C                   ENDIF
 001 *
 001 C                   UPDATE    PEO01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTEAP - Write FAEAP record
     *================================================================
     C     WRTEAP        BEGSR
     *
     C     EOBKY2        CHAIN     PEA01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PEA01
 001 C                   eval      EACO# = EOCO#
 001 C                   eval      EASUB# = EOSUB#
 001 C                   eval      EAFND = EOFND
 001 C                   eval      EAFYR = EOFYR
 001 C                   eval      EACASE = EOCASE
 001 C                   MOVEL     EOSS#         EASS#
 001 C                   eval      EAACDT = EOACDT
 001 C                   eval      EAEOB# = EOEOB#
 001 C                   eval      EARCN = EORCN
 001 C                   eval      EATRDT = EOTRDT
 001 *
 001 C                   MOVE      U1CHRG        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      EACHRG = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      EACHRG = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U1MRA         VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      EAMRA = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      EAMRA = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U1NDIS        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      EANDIS = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      EANDIS = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U1PAID        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      EAPAID = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      EAPAID = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVEL(P)  'A'           EASTAT
 001 C                   MOVEL(P)  EOUSER        EAUSER
 001 C                   MOVEL(P)  EOPGM         EAPGM
 001 C                   eval      EATIME = EOTIME
 001 C                   eval      EADT = EODT
 001 C                   MOVEL(P)  EOUSER        EAIUSR
 001 C                   MOVEL(P)  EOPGM         EAIPGM
 001 C                   eval      EAITIM = EOITIM
 001 C                   eval      EAIDT = EOIDT
 001 *
 001 C                   eval      EANAMT = U1COST
 001 C                   MOVEL(P)  'H'           EABHRF
 001 C                   MOVEL(P)  'H'           EANHRF
 001 C                   MOVEL(P)  EOSYS         EASYS
 001 C                   eval      EANCAM = U1COST
 001 *
 001 C     F1KY          CHAIN     PF101                              99
B002 C                   if        *IN99 = *OFF
B003 C                   if        F1OUTA = 'Y'
 003 C                   MOVE      '1'           EAPLNK
X003 C                   ELSE
B004 C                   if        EOPAID <= 0
 004 C                   MOVE      '2'           EAPLNK
X004 C                   ELSE
 004 C                   MOVE      '3'           EAPLNK
E004 C                   ENDIF                                                  EOPAID LE 0
E003 C                   ENDIF
E002 C                   ENDIF
 001 *
 001 C                   eval      EAFEIN = EOFEIN
 001 C                   eval      EAMOD = EOMOD
 001 C                   MOVEL(P)  U1TRID        EASINV
 001 *
B002 C                   if        *IN16 = *ON
B003 C                   if        PDDBA1 <> 'Y'
B004 C                   if        PDFSN = *BLANKS
 004 C                   MOVE      PDNAME        EAPNAM
X004 C                   ELSE
 004 C     PDFSN         CAT(P)    PDNAME:1      EAPNAM
E004 C                   ENDIF
 003 C                   MOVE      PDADR1        EAADR1
 003 C                   MOVE      PDADR2        EAADR2
X003 C                   ELSE
 003 C                   MOVE      PDADR1        EAPNAM
 003 C                   MOVE      PDADR2        EAADR1
 003 C                   eval      EAADR2 = *BLANKS
E003 C                   ENDIF
 002 C                   MOVE      PDCTY         EACTY
 002 C                   MOVE      PDST          EAST
 002 C                   MOVE      PDZIP         EAZIP
X002 C                   ELSE
B003 C                   if        U1PFSN = *BLANKS
 003 C                   MOVEL(P)  U1PNAM        EAPNAM
X003 C                   ELSE
 003 C     U1PFSN        CAT(P)    U1PNAM:1      EAPNAM
E003 C                   ENDIF
 002 C                   MOVEL(P)  U1ADR1        EAADR1
 002 C                   MOVEL(P)  U1ADR2        EAADR2
 002 C                   MOVEL(P)  U1CTY         EACTY
 002 C                   MOVEL(P)  U1ST          EAST
 002 C                   MOVE      U1ZIP         EAZIP
     C*** Begin Add *** 05/10/16 *********************************
     C*** Begin Del *** 10/26/16 *********************************
     C************
     C* If Provider Fein and Mod passed in U1PROV then get the name and address
     C* from the provider file.
     C************
     C************       If        U1prov <> *Blanks
     C************
B003 C************       If        PDdba1 <> 'Y'
     C************
B004 C************       If        PDfsn = *blanks
 004 C************       Eval      EApNam = PDname
X004 C************       Else
     C************       Eval      EApNam = %Trim(PDfsn) + ' ' + %Trim(PDname)
E004 C************       EndIf
     C************
 003 C************       Eval      EAadr1 = PDadr1
 003 C************       Eval      EAadr2 = PDadr2
     C************
X003 C************       Else
     C************
     C************       Eval      EAPnam = PDadr1
     C************       Eval      EAadr1 = PDadr2
 003 C************       Eval      EAadr2 = *Blanks
E003 C************       EndIf
     C************
 002 C************       Eval      EActy = PDcty
 002 C************       Eval      EAst  = PDst
 002 C************       Eval      EAzip = PDzip
     C************
     C************       EndIf
     C*** End   Del *** 10/26/16 *********************************
     C*** End   Add *** 05/10/16 *********************************
E002 C                   ENDIF
 001 *
 001 C                   WRITE     PEA01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTEDP - Write FAEDP record
     *================================================================
     C     WRTEDP        BEGSR
     *
     C                   eval      KKEOB# = EOEOB#
     C     EOBKY1        CHAIN     PED01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PED01
 001 C                   eval      EDCO# = EOCO#
 001 C                   eval      EDSUB# = EOSUB#
 001 C                   eval      EDFND = EOFND
 001 C                   eval      EDFYR = EOFYR
 001 C                   eval      EDCASE = EOCASE
 001 C                   eval      EDEOB# = EOEOB#
 001 C                   MOVEL     EOSS#         EDSS#
 001 C                   eval      EDACDT = EOACDT
 001 C                   MOVEL     U1LIC#        EDLIC#
 001 *
B002 C                   if        EDLIC# = *BLANKS
 002 C                   MOVEL(P)  'N'           EDEDI
X002 C                   ELSE
 002 C                   MOVEL(P)  'Y'           EDEDI
E002 C                   ENDIF
 001 *
 001 C                   MOVEL     'A'           EDSTAT
 001 C                   MOVEL     U1PCON        EDACCT
 001 *
 001 C                   MOVEL(P)  EOUSER        EDUSER
 001 C                   MOVEL(P)  EOPGM         EDPGM
 001 C                   eval      EDTIME = EOTIME
 001 C                   eval      EDDT = EODT
 001 C                   MOVEL(P)  EOUSER        EDIUSR
 001 C                   MOVEL(P)  EOPGM         EDIPGM
 001 C                   eval      EDITIM = EOITIM
 001 C                   eval      EDIDT = EOIDT
 001 *
     C***** beg add *** 05/19/14 *******************************************
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL(P)  EODIA1        EDDIA1
 001 C                   MOVEL(P)  EODIA2        EDDIA2
 001 C                   MOVEL(P)  EODIA3        EDDIA3
 001 C                   MOVEL(P)  EODIA4        EDDIA4
     C***** beg add *** 05/19/14 *******************************************
     C                   EVAL      EDICDIND = 'N'
     C                   EndIf
     C***** end add *** 05/19/14 *******************************************
     C***** beg add *** 05/19/14 *******************************************
      /free
        If U1ICD10Ind = 'Y';
           EDICD1001 = U1ICD1001;
           EDICD1002 = U1ICD1002;
           EDICD1003 = U1ICD1003;
           EDICD1004 = U1ICD1004;
           EDICDIND = 'Y';
        EndIf;
      /end-free
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL(P)  U1POTX        EDPOTX
 001 *
 001 C                   MOVEL(P)  U1PPLN        EDPPLN
 001 C                   MOVEL(P)  U1ACCT        EDACCT
 001 C                   MOVEL(P)  U1RLIC        EDFLIC
 001 *
 001 C                   WRITE     PED01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTEHP - Write FAEHP record
     *================================================================
     C     WRTEHP        BEGSR
     *
     C                   eval      KKEOB# = EOEOB#
     C     EOBKY1        CHAIN     PEH01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PEH01
 001 C                   eval      EHCO# = EOCO#
 001 C                   eval      EHSUB# = EOSUB#
 001 C                   eval      EHFND = EOFND
 001 C                   eval      EHFYR = EOFYR
 001 C                   eval      EHCASE = EOCASE
 001 C                   eval      EHEOB# = EOEOB#
 001 C                   MOVEL(P)  EOSS#         EHSS#
 001 C                   eval      EHACDT = EOACDT
 001 C                   MOVEL     U1PCON        EHACCT
 001 *
     C***** beg add *** 05/19/14 *******************************************
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL(P)  EODIA1        EHDIA1
 001 C                   MOVEL(P)  EODIA2        EHDIA2
 001 C                   MOVEL(P)  EODIA3        EHDIA3
 001 C                   MOVEL(P)  EODIA4        EHDIA4
     C***** beg add *** 05/19/14 *******************************************
     C                   EVAL      EHICDIND = 'N'
     C                   EndIf
     C***** end add *** 05/19/14 *******************************************
     C***** beg add *** 05/19/14 *******************************************
      /free
        If U1ICD10Ind = 'Y';
           EHICD1001 = U1ICD1001;
           EHICD1002 = U1ICD1002;
           EHICD1003 = U1ICD1003;
           EHICD1004 = U1ICD1004;
           EHICDIND = 'Y';
        EndIf;
      /end-free
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL(P)  U1LIC#        EHLIC#
 001 *
B002 C                   if        EHLIC# = *BLANKS
 002 C                   MOVEL(P)  'N'           EHEDI
X002 C                   ELSE
 002 C                   MOVEL(P)  'Y'           EHEDI
E002 C                   ENDIF
 001 *
 001 C                   MOVEL(P)  'Y'           EHEMPL
 001 C                   MOVEL(P)  'A'           EHSTAT
 001 *
 001 C                   MOVEL(P)  EOUSER        EHUSER
 001 C                   MOVEL(P)  EOPGM         EHPGM
 001 C                   eval      EHTIME = EOTIME
 001 C                   eval      EHDT = EODT
 001 C                   MOVEL(P)  EOUSER        EHIUSR
 001 C                   MOVEL(P)  EOPGM         EHIPGM
 001 C                   eval      EHITIM = EOITIM
 001 C                   eval      EHIDT = EOIDT
 001 *
 001 C                   MOVEL     U1PPLN        EHPPLN
 001 C                   MOVEL     U1ACCT        EHACCT
 001 C                   MOVEL     U1RLIC        EHFLIC
 001 *
 001 C                   WRITE     PEH01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTEPP - Write FAEPP record
     *================================================================
     C     WRTEPP        BEGSR
     *
     C                   eval      KKEOB# = EOEOB#
     C     EOBKY1        CHAIN     PEP01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PEP01
 001 C                   eval      EPCO# = EOCO#
 001 C                   eval      EPSUB# = EOSUB#
 001 C                   eval      EPFND = EOFND
 001 C                   eval      EPFYR = EOFYR
 001 C                   eval      EPCASE = EOCASE
 001 C                   eval      EPEOB# = EOEOB#
 001 C                   MOVEL(P)  EOSS#         EPSS#
 001 C                   eval      EPACDT = EOACDT
 001 C                   MOVEL     'N'           EPEDI
 001 C                   MOVEL     U1PCON        EPACCT
 001 C                   MOVEL     U1PHAR        EPPHAR
 001 *
 001 C                   MOVEL     U1ACCT        EPACCT
 001 C                   MOVEL     U1RLIC        EPFLIC
 001 *
 001 C                   MOVEL(P)  EOUSER        EPUSER
 001 C                   MOVEL(P)  EOPGM         EPPGM
 001 C                   eval      EPTIME = EOTIME
 001 C                   eval      EPDT = EODT
 001 C                   MOVEL(P)  EOUSER        EPIUSR
 001 C                   MOVEL(P)  EOPGM         EPIPGM
 001 C                   eval      EPITIM = EOITIM
 001 C                   eval      EPIDT = EOIDT
 001 *
 001 C                   WRITE     PEP01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTESP - Write FAESP record
     *================================================================
     C     WRTESP        BEGSR
     *
     C                   eval      KKEOB# = EOEOB#
     C     EOBKY2        CHAIN     PES01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 * WRITE RECON RECORD IF THIS IS A RECONSIDERATION
 001 *
B002 C                   if        *IN92 = *ON
 002 C                   CLEAR                   PES01
 002 C                   eval      ESCO# = EOCO#
 002 C                   eval      ESSUB# = EOSUB#
 002 C                   eval      ESFND = EOFND
 002 C                   eval      ESFYR = EOFYR
 002 C                   eval      ESCASE = EOCASE
 002 C                   eval      ESEOB# = EOEOB#
 002 C                   eval      ESRCN = EORCN
 002 C                   MOVEL     EOSS#         ESSS#
 002 C                   eval      ESACDT = EOACDT
 002 C                   MOVEL(P)  'R'           ESBLST
 002 C                   MOVEL(P)  REAS2         ESREAS
 002 *
 002 C                   eval      SRDTI = U1CRDT
 002 C                   CALL      'SRCVTDAT'    DTCYM2
 002 C                   MOVEL     SRDTO         ESRRDT
 002 *
 002 C                   eval      EOTIME = EOTIME + 1
 002 C                   MOVEL     'A'           ESSTAT
 002 C                   MOVEL(P)  EOUSER        ESUSER
 002 C                   MOVEL(P)  EOPGM         ESPGM
 002 C                   eval      ESTIME = EOTIME
 002 C                   eval      ESDT = EODT
 002 C                   MOVEL(P)  EOUSER        ESIUSR
 002 C                   MOVEL(P)  EOPGM         ESIPGM
 002 C                   eval      ESITIM = EOTIME
 002 C                   eval      ESIDT = EODT
 002 C                   eval      EOTIME = EOTIME + 1
 002 C                   WRITE     PES01
E002 C                   ENDIF
 001 *
 001 C                   CLEAR                   PES01
 001 C                   eval      ESCO# = EOCO#
 001 C                   eval      ESSUB# = EOSUB#
 001 C                   eval      ESFND = EOFND
 001 C                   eval      ESFYR = EOFYR
 001 C                   eval      ESCASE = EOCASE
 001 C                   eval      ESEOB# = EOEOB#
 001 C                   eval      ESRCN = EORCN
 001 C                   MOVEL     EOSS#         ESSS#
 001 C                   eval      ESACDT = EOACDT
 001 C                   MOVEL(P)  'C'           ESBLST
 001 C                   MOVEL(P)  REAS1         ESREAS
 001 *
 001 C                   MOVEL     'A'           ESSTAT
 001 C                   MOVEL(P)  EOUSER        ESUSER
 001 C                   MOVEL(P)  EOPGM         ESPGM
 001 C                   eval      ESTIME = EOTIME
 001 C                   eval      ESDT = EODT
 001 C                   MOVEL(P)  EOUSER        ESIUSR
 001 C                   MOVEL(P)  EOPGM         ESIPGM
 001 C                   eval      ESIDT = EOIDT
 001 C                   eval      ESITIM = EOTIME
 001 *
 001 C                   WRITE     PES01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTEEP - Write FAEEP record
     *================================================================
     C     WRTEEP        BEGSR
     *
     C                   eval      KKCO# = EOCO#
     C                   eval      KKSUB# = EOSUB#
     C                   eval      KKFND = EOFND
     C                   eval      KKFYR = EOFYR
     C                   eval      KKCASE = EOCASE
     C                   eval      KKEOB# = EOEOB#
     C                   MOVE      *HIVAL        KKSEQ#
     *
     C     EEKY2         SETGT     PEE01
     C     EEKY1         READPE    PEE01                                  99
     *
B001 C                   if        *IN99 = *OFF
 001 *
 001 C                   eval      XXSEQ# = EESEQ# + 1
 001 *
X001 C                   ELSE
 001 *
 001 C                   eval      XXSEQ# = 1
 001 *
E001 C                   ENDIF
     *
     C                   CLEAR                   PES01
     *
     C                   eval      EECO# = EOCO#                                COMPANY#
     C                   eval      EESUB# = EOSUB#                              SUBSCRIBER
     C                   eval      EEFND = EOFND                                GROUP
     C                   eval      EEFYR = EOFYR                                YEAR
     C                   eval      EECASE = EOCASE                              CASE#
     C                   MOVEL     EOSS#         EESS#                          SOC SEC#
     C                   eval      EEACDT = EOACDT                              ACC DATE
     C                   MOVEL     EOEOB#        EEEOB#                         EOMB#
     C                   eval      EESEQ# = XXSEQ#                              SEQ#
     C                   MOVEL     U1BADJ        EEEOB                          EOB CODE
     C                   MOVEL     U1BAGP        EEBADG                         BILL ADJ GROUP CODE
     C                   MOVEL     U1BACD        EENOT1                         NOTE 1
     C                   MOVE      U1BACD        EENOT2                         NOTE 2
     C                   MOVEL     'A'           EESTAT                         STATUS
     C                   MOVEL     EOUSER        EEUSER                         MAINT USER
     C                   MOVEL     EOPGM         EEPGM                          MAINT PGM
     C                   eval      EETIME = EOTIME                              MAINT TIME
     C                   eval      EEDT = EODT                                  MAINT DATE
     C                   MOVEL     EOUSER        EEIUSR                         INIT USER
     C                   MOVEL     EOPGM         EEIPGM                         INIT PGM
     C                   eval      EEITIM = EOTIME                              INIT TIME
     C                   eval      EEIDT = EODT                                 INIT DATE
     C                   eval      EERCN = EORCN                                NUMBER OF RECONS
B001 C                   if        U1BACD = ' '
 001 C                   MOVEL     'N'           EEPRNT                         PRINT FLAG
X001 C                   ELSE
 001 C                   MOVEL     'Y'           EEPRNT                         PRINT FLAG
E001 C                   ENDIF                                                  U1BACD=' '
     *
     C                   WRITE     PEE01
     *
     C                   ENDSR
     *================================================================
     * WRTEUP - Write FAEUP record
     *================================================================
     C     WRTEUP        BEGSR
     *
     C                   eval      KKEOB# = EOEOB#
     C     EOBKY1        CHAIN     PEU01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PEU01
 001 C                   eval      EUCO# = EOCO#
 001 C                   eval      EUSUB# = EOSUB#
 001 C                   eval      EUFND = EOFND
 001 C                   eval      EUFYR = EOFYR
 001 C                   eval      EUCASE = EOCASE
 001 C                   MOVEL     EOSS#         EUSS#
 001 C                   eval      EUACDT = EOACDT
 001 C                   eval      EUEOB# = EOEOB#
 001 C                   MOVE      '0'           EUPCM
 001 C                   MOVEL(P)  U1MCAR        EUMCAR
 001 *
 001 C                   eval      SRDTI = U1CFDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EUSCPF
 001 *
 001 C                   eval      SRDTI = U1CTDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EUSCPT
 001 *
 001 C                   MOVEL(P)  U1PCON        EUACCT
 001 *
 001 C                   MOVEL(P)  U1BLTY        EUBLTY
 001 *
 001 C                   eval      SRDTI = U1ADDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EUADDT
 001 *
     C***** beg add *** 05/19/14 *******************************************
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL(P)  EODIA1        EUDIA1
 001 C                   MOVEL(P)  EODIA2        EUDIA2
 001 C                   MOVEL(P)  EODIA3        EUDIA3
 001 C                   MOVEL(P)  EODIA4        EUDIA4
     C***** beg add *** 05/19/14 *******************************************
     C                   EVAL      EUICDIND = 'N'
     C                   EndIf
     C***** end add *** 05/19/14 *******************************************
     C***** beg add *** 05/19/14 *******************************************
      /free
        If U1ICD10Ind = 'Y';
           EUICD1001 = U1ICD1001;
           EUICD1002 = U1ICD1002;
           EUICD1003 = U1ICD1003;
           EUICD1004 = U1ICD1004;
           EUICD1005 = U1ICD1005;
           EUICD1006 = U1ICD1006;
           EUICD1007 = U1ICD1007;
           EUICD1008 = U1ICD1008;
           EUICDIND = 'Y';
        EndIf;
      /end-free
     C***** end add *** 05/19/14 *******************************************
     C***** beg add *** 05/19/14 *******************************************
      /free
        If U1ICD10Ind = 'Y';
           EUCAU1001 = U1CAU1001;
           EUCAU1002 = U1CAU1002;
           EUCAU1003 = U1CAU1003;
        EndIf;
      /end-free
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL     U1LIC#        EULIC#
 001 *
B002 C                   if        EULIC# = *BLANKS
 002 C                   MOVEL(P)  'N'           EUEDI
X002 C                   ELSE
 002 C                   MOVEL(P)  'Y'           EUEDI
E002 C                   ENDIF
 001 *
 001 C                   MOVEL     'A'           EUSTAT
 001 C                   MOVEL(P)  EOUSER        EUUSER
 001 C                   MOVEL(P)  EOPGM         EUPGM
 001 C                   eval      EUTIME = EOTIME
 001 C                   eval      EUDT = EODT
 001 C                   MOVEL(P)  EOUSER        EUIUSR
 001 C                   MOVEL(P)  EOPGM         EUIPGM
 001 C                   eval      EUITIM = EOITIM
 001 C                   eval      EUIDT = EOIDT
 001 *
 001 C                   MOVEL     U1SVTY        EUICN
 001 C                   eval      EUADHR = U1ADHR
 001 *
 001 C                   eval      SRDTI = U1DCDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EUDCDT
 001 *
 001 C                   eval      EUDCHR = U1DCHR
 001 C                   MOVEL     U1ADTY        EUADTY
 001 C                   MOVEL     U1TSS         EUDCN
 001 C                   MOVEL     U1PCM         EUPCM
     C***** beg add *** 05/19/14 *******************************************
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL     U1PRC1        EUPRC1
 001 C                   MOVEL     U1PRC2        EUPRC2
     C***** beg add *** 05/19/14 *******************************************
     C                   EndIf
     C***** end add *** 05/19/14 *******************************************
     C***** beg add *** 05/19/14 *******************************************
      /free
        If U1ICD10Ind = 'Y';
           EUPRC1001 = U1PRC1001;
           EUPRC1002 = U1PRC1002;
           EUPRC1003 = U1PRC1003;
           EUPRC1004 = U1PRC1004;
           EUPRC1005 = U1PRC1005;
           EUPRC1006 = U1PRC1006;
        EndIf;
      /end-free
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL     U1PPLN        EUPPLN
 001 C                   MOVEL     U1ACCT        EUACCT
 001 C                   MOVE      U1DRG#        EUDRG#
 001 *
 001 C                   eval      SRDTI = U1PPDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EUPRD1
 001 *
     C***** beg add *** 05/19/14 *******************************************
     C                   If        U1ICD10Ind = 'N' or
     C                             U1ICD10Ind = *blanks
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL     U1ADDG        EUADDI                         CHECK W/RICHIE
     C***** beg add *** 05/19/14 *******************************************
     C                   EndIf
     C***** end add *** 05/19/14 *******************************************
     C***** beg add *** 05/19/14 *******************************************
      /free
        If U1ICD10Ind = 'Y';
           EUADIA1001 = U1ADIA1001;
        EndIf;
      /end-free
     C***** end add *** 05/19/14 *******************************************
 001 C                   MOVEL     U1RLIC        EUFLIC
 001 *
 001 C                   eval      SRDTI = U1PCDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         EUPRD2
 001 *
 001 C                   WRITE     PEU01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTCAP - Write FDCAP record
     *================================================================
     C     WRTCAP        BEGSR
     *
B001 C                   if        U2EOB# = *ZEROS
 001 C                             and U2DSEQ = *ZEROS
 001 *
 001 C                   eval      KKDSEQ = *HIVAL
 001 C     CAKY          SETGT     PCA02
 001 C     CAKY2         READPE    PCA02                                  99
B002 C                   if        *IN99 = *OFF
 002 C                   eval      XXDSEQ = CADSEQ + 1
X002 C                   ELSE
 002 C                   eval      XXDSEQ = 1
E002 C                   ENDIF
 001 *
 001 C                   CLEAR                   PCA01
 001 C                   eval      CACO# = EOCO#
 001 C                   eval      CAFND = EOFND
 001 C                   eval      CAFYR = EOFYR
 001 C                   eval      CACASE = EOCASE
 001 C                   MOVEL     '2'           CACSCD
 001 *
 001 C                   eval      SRDTI = U2SVDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         CAFDAT
 001 *
 001 C                   eval      CATDAT = CAFDAT
 001 C                   eval      CARDAT = EOTRDT
 001 C                   eval      CAFEIN = EOFEIN
 001 C                   eval      CAMOD = EOMOD
 001 *
B002 C                   SELECT
 002 C                   when      EOBTYP = 'N'
 002 C                   MOVEL     '1'           CAPRTY
 002 C                   when      EOBTYP = 'H'
 002 C                   MOVEL     '4'           CAPRTY
 002 C                   when      EOBTYP = 'D'
 002 C                   MOVEL     'D'           CAPRTY
 002 C                   when      EOBTYP = 'X'
 002 C                   MOVEL     'X'           CAPRTY
E002 C                   ENDSL
 001 *
B002 C                   if        U2UNTS <> *ZEROS
     C***** Beg Chg *** 06/11/14 ******************************************
 002 C                   z-add     U2UNTS        CAUNTS
 002 C***                eval      CAUNTS = U2UNTS
     C***** End Chg *** 06/11/14 ******************************************
X002 C                   ELSE
 002 C                   eval      CAUNTS = 1
E002 C                   ENDIF
 001 *
 001 C                   eval      CADSEQ = XXDSEQ
 001 C                   eval      CACHK# = EOCHK#
 001 C                   eval      CAEOB# = EOEOB#
 001 C                   eval      CARCN = EORCN
 001 C                   MOVEL     EOSS#         CASS#
 001 C                   eval      CAACDT = EOACDT
 001 C                   MOVEL     'A'           CASTAT
 001 *
B002 C                   SELECT
 002 C                   when      EOBTYP = 'D'
 002 C                   MOVEL(P)  'D'           CARCFL
 002 C                   when      EOBTYP = 'H'
 002 C                   MOVEL(P)  'S'           CARCFL
 002 C                   when      EOBTYP = 'N'
 002 C                   MOVEL(P)  'C'           CARCFL
 002 C                   when      EOBTYP = 'X'
 002 C                   MOVEL(P)  'X'           CARCFL
E002 C                   ENDSL
 001 *
 001 C                   MOVEL(P)  EOUSER        CAUSER
 001 C                   MOVEL(P)  EOPGM         CAPGM
 001 C                   eval      CADT = EODT
 001 C                   eval      CATIME = EOTIME
 001 C                   MOVEL(P)  EOUSER        CAIUSR
 001 C                   MOVEL(P)  EOPGM         CAIPGM
 001 C                   eval      CAIDT = EOIDT
 001 C                   eval      CAITIM = EOITIM
 001 *
 001 C                   eval      CASUB# = EOSUB#
 001 C                   MOVE      EOZIPA        CAZIPA
 001 C                   MOVEL(P)  EOAST         CAAST
 001 *
 001 C                   MOVE      U2CHRG        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      CACHRG = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      CACHRG = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2MRA         VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      CAMRA = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      CAMRA = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2NDIS        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      CANDIS = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      CANDIS = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2PAID        VALUE
B002 C                   if        SIGN = '-'
 002 C                   eval      CAPAID = 0 - AMOUNT
X002 C                   ELSE
 002 C                   eval      CAPAID = AMOUNT
E002 C                   ENDIF
 001 *
 001 C                   eval      CALIN# = U2LIN#
 001 *
B002 C                   SELECT
 002 C                   when      EOBTYP = 'N'
B003 C                   if        U2CODE = *BLANKS
     C***** Beg Add *** 11/02/15 **************************************
     C                   If        U2NDC  = *BLANKS
     C***** End Add *** 11/02/15 **************************************
     C                   MOVEL(P)  '96370'       CACODE
     C                   MOVEL(P)  '96370'       CACPT4
     C                   MOVEL(P)  '96370'       CABPRC
     C***** Beg Add *** 11/02/15 **************************************
     C                   Else
     C                   Eval      CACODE = %Subst(U2NDC:1:11)
     C                   EndIf
     C***** End Add *** 11/02/15 **************************************
     C***** Beg Dlt *** 11/02/15 **************************************
 003 C*****              MOVEL(P)  '96370'       CACPT4
 003 C*****              MOVEL(P)  '96370'       CABPRC
     C***** End Dlt *** 11/02/15 **************************************
X003 C                   ELSE
 003 C                   MOVEL(P)  U2CODE        CACODE
 003 C                   MOVEL(P)  U2CODE        CACPT4
 003 C                   MOVEL(P)  U2CODE        CABPRC
E003 C                   ENDIF
 002 C                   when      EOBTYP = 'D'
 002 C                   MOVEL(P)  U2CODE        CACODE
 002 C                   MOVEL(P)  U2CODE        CACPT4
 002 C                   MOVEL(P)  U2CODE        CABPRC
 002 C                   when      EOBTYP = 'X'
 002 C                   MOVEL(P)  U2NDC         CACODE
 002 C                   when      EOBTYP = 'H'
 002 C                   RESET                   KKCMSV
 002 C     DIGITS        CHECK     U2RVCD        N                        99
B003 C                   if        *IN99 = *ON
     C*** BEG ADD *** 06/13/07 *********************************************
B003 C                             and U2RVCD <> *blanks
     C*** END ADD *** 06/13/07 *********************************************
B004 C                   if        U2CODE = *BLANKS
 004 C                   MOVEL     U2RVCD        KKHCPC
E004 C                   ENDIF
X003 C                   ELSE
 003 C                   MOVE      U2RVCD        KKRVCD
 003 C                   MOVEL(P)  U2CODE        KKHCPC
E003 C                   ENDIF
 002 C                   MOVE      KKRVCD        CASVCD
 002 C                   MOVEL     KKCMSV        CACODE
E002 C                   ENDSL
 001 *
 001 C                   WRITE     PCA01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTDAP - Write FDDAP record
     *================================================================
     C     WRTDAP        BEGSR
     *
     C                   MOVE      U1CHRG        CHRG1
     C                   MOVE      U1MRA         MRA1
     C                   MOVE      U1NDIS        NDIS1
     C                   MOVE      U1PAID        PAID1
     C                   eval      KKEOB# = CAEOB#
     C                   eval      KKDSEQ = CADSEQ
     C     CAPKY2        CHAIN     PDA01                              99
B001 C                   if        *IN99 = *ON
B002 C                   if        *IN92 = *OFF
 002 C                             or *IN92 = *ON
 002 C                             and U2RLIN <> *ZEROS
 002 C                             and CHRG2 <> *ZEROS
 002 C                             or *IN92 = *ON
 002 C                             and U2RLIN <> *ZEROS
 002 C                             and MRA2 <> *ZEROS
 002 C                             or *IN92 = *ON
 002 C                             and U2RLIN <> *ZEROS
 002 C                             and NDIS2 <> *ZEROS
 002 C                             or *IN92 = *ON
 002 C                             and U2RLIN <> *ZEROS
 002 C                             and PAID2 <> *ZEROS
 002 *
 002 C                   CLEAR                   PDA01
 002 C                   eval      DACO# = CACO#
 002 C                   eval      DASUB# = CASUB#
 002 C                   eval      DAFND = CAFND
 002 C                   eval      DAFYR = CAFYR
 002 C                   eval      DACASE = CACASE
 002 C                   MOVEL     CASS#         DASS#
 002 C                   eval      DAACDT = CAACDT
 002 C                   eval      DAEOB# = CAEOB#
 002 C                   eval      DARCN# = EORCN
 002 C                   eval      DADSEQ = CADSEQ
 002 C                   eval      DATRDT = CARDAT
 002 *
 002 C                   MOVE      U2CHRG        VALUE
B003 C                   if        SIGN = '-'
 003 C                   eval      DACHRG = 0 - AMOUNT
X003 C                   ELSE
 003 C                   eval      DACHRG = AMOUNT
E003 C                   ENDIF
 002 *
 002 C                   MOVE      U2MRA         VALUE
B003 C                   if        SIGN = '-'
 003 C                   eval      DAMRA = 0 - AMOUNT
X003 C                   ELSE
 003 C                   eval      DAMRA = AMOUNT
E003 C                   ENDIF
 002 *
 002 C                   MOVE      U2NDIS        VALUE
B003 C                   if        SIGN = '-'
 003 C                   eval      DANDIS = 0 - AMOUNT
X003 C                   ELSE
 003 C                   eval      DANDIS = AMOUNT
E003 C                   ENDIF
 002 *
 002 C                   MOVE      U2PAID        VALUE
B003 C                   if        SIGN = '-'
 003 C                   eval      DAPAID = 0 - AMOUNT
X003 C                   ELSE
 003 C                   eval      DAPAID = AMOUNT
E003 C                   ENDIF
 002 *
 002 C                   MOVEL     'A'           DASTAT
 002 C                   MOVEL(P)  CAUSER        DAUSER
 002 C                   MOVEL(P)  CAPGM         DAPGM
 002 C                   eval      DATIME = CATIME
 002 C                   eval      DADT = CADT
 002 C                   MOVEL(P)  CAUSER        DAIUSR
 002 C                   MOVEL(P)  CAPGM         DAIPGM
 002 C                   eval      DAITIM = CAITIM
 002 C                   eval      DAIDT = CAIDT
 002 *
 002 C                   WRITE     PDA01
E002 C                   ENDIF
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTDDP - Write FDDDP record
     *================================================================
     C     WRTDDP        BEGSR
     *
     C                   eval      KKEOB# = CAEOB#
     C                   eval      KKDSEQ = CADSEQ
     C     CAPKY1        CHAIN     PDD01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PDD01
 001 C                   eval      DDCO# = CACO#
 001 C                   eval      DDSUB# = CASUB#
 001 C                   eval      DDFND = CAFND
 001 C                   eval      DDFYR = CAFYR
 001 C                   eval      DDCASE = CACASE
 001 C                   MOVEL     CASS#         DDSS#
 001 C                   eval      DDACDT = CAACDT
 001 C                   eval      DDEOB# = CAEOB#
 001 C                   eval      DDDSEQ = CADSEQ
 001 *
B002 C                   if        U2UNTS = *ZEROS
 002 C                   eval      DDUNTS = 1
X002 C                   ELSE
     C***** Beg Chg *** 06/11/14 ******************************************
 002 C                   z-add     U2UNTS        DDUNTS
 002 C***                eval      DDUNTS = U2UNTS
     C***** End Chg *** 06/11/14 ******************************************
E002 C                   ENDIF
 001 *
 001 C                   MOVEL     U2CODE        DDPRCD
 001 C                   MOVEL     U2CDSC        DDPRDS
 001 C                   MOVEL     U2CODE        DDBPRC
 001 C                   MOVEL     U2CDSC        DDBPRD
 001 C                   MOVEL     U2POS         DDPOS
 001 *
B002 C                   if        DDPOS = *BLANKS
 002 C                             or DDPOS = '00'
 002 C                   MOVEL     '11 '         DDPOS
E002 C                   ENDIF
 001 *
 001 C                   MOVE      'C'           DDRCFL
 001 *
 001 C                   MOVE      U2DXR         XXDXR
 001 *
B002 C                   if        DX1 = 1
 002 C                             or DX2 = 1
 002 C                             or DX3 = 1
 002 C                             or DX4 = 1
 002 C                   MOVEL     'X'           DDDXR1
E002 C                   ENDIF
 001 *
B002 C                   if        DX1 = 2
 002 C                             or DX2 = 2
 002 C                             or DX3 = 2
 002 C                             or DX4 = 2
 002 C                   MOVEL     'X'           DDDXR2
E002 C                   ENDIF
 001 *
B002 C                   if        DX1 = 3
 002 C                             or DX2 = 3
 002 C                             or DX3 = 3
 002 C                             or DX4 = 3
 002 C                   MOVEL     'X'           DDDXR3
E002 C                   ENDIF
 001 *
B002 C                   if        DX1 = 4
 002 C                             or DX2 = 4
 002 C                             or DX3 = 4
 002 C                             or DX4 = 4
 002 C                   MOVEL     'X'           DDDXR4
E002 C                   ENDIF
 001 *
B002 C                   SELECT
 002 C                   when      DDDXR1 = *BLANKS
 002 C                             and DDDXR2 = *BLANKS
 002 C                             and DDDXR3 = *BLANKS
 002 C                             and DDDXR3 = *BLANKS
 002 C                             and EDDIA1 <> *BLANKS
 002 C                   MOVEL     'X'           DDDXR1
 002 C                   when      DDDXR1 = *BLANKS
 002 C                             and DDDXR2 = *BLANKS
 002 C                             and DDDXR3 = *BLANKS
 002 C                             and DDDXR3 = *BLANKS
 002 C                             and EDDIA2 <> *BLANKS
 002 C                   MOVEL     'X'           DDDXR2
 002 C                   when      DDDXR1 = *BLANKS
 002 C                             and DDDXR2 = *BLANKS
 002 C                             and DDDXR3 = *BLANKS
 002 C                             and DDDXR3 = *BLANKS
 002 C                             and EDDIA3 <> *BLANKS
 002 C                   MOVEL     'X'           DDDXR3
 002 C                   when      DDDXR1 = *BLANKS
 002 C                             and DDDXR2 = *BLANKS
 002 C                             and DDDXR3 = *BLANKS
 002 C                             and DDDXR3 = *BLANKS
 002 C                             and EDDIA4 <> *BLANKS
 002 C                   MOVEL     'X'           DDDXR4
E002 C                   ENDSL
 001 *
 001 C                   MOVEL     'A'           DDSTAT
 001 C                   MOVEL(P)  CAUSER        DDUSER
 001 C                   MOVEL(P)  CAPGM         DDPGM
 001 C                   eval      DDTIME = CATIME
 001 C                   eval      DDDT = CADT
 001 C                   MOVEL(P)  CAUSER        DDIUSR
 001 C                   MOVEL(P)  CAPGM         DDIPGM
 001 C                   eval      DDITIM = CAITIM
 001 C                   eval      DDIDT = CAIDT
 001 *
 001 C                   MOVEL(P)  U2TOO#        DDPART
     C*** BEG ADD *** 11/26/07 *********************************************
     C     U2BPRC        IFNE      *BLANKS
     C*** END ADD *** 11/26/07 *********************************************
 001 C                   MOVEL(P)  U2BPRC        DDBPRC
     C*** BEG ADD *** 11/26/07 *********************************************
     C                   ENDIF
     C*** END ADD *** 11/26/07 *********************************************
 001 C                   MOVEL(P)  U2BMD1        DDMOD1
 001 *
 001 C                   WRITE     PDD01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTDEP - Write FDDEP record
     *================================================================
     C     WRTDEP        BEGSR
     *
     C                   eval      KKSEQ# = *HIVAL
     C     DEKY2         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  99
B001 C                   if        *IN99 = *OFF
 001 C                   eval      HDSEQ# = DESEQ# + 1
X001 C                   ELSE
 001 C                   eval      HDSEQ# = 1
E001 C                   ENDIF
     *
B001 C                   DO        4             E
B002 C                   SELECT
 002 C                   when      E = 1
 002 C                   MOVE      U2EOB1        XXEOBE
 002 C                   MOVE      U2SAG1        XXSAGE
 002 C                   MOVE      U2EOD1        XXEODE
 002 C                   when      E = 2
 002 C                   MOVE      U2EOB2        XXEOBE
 002 C                   MOVE      U2SAG2        XXSAGE
 002 C                   MOVE      U2EOD2        XXEODE
 002 C                   when      E = 3
 002 C                   MOVE      U2EOB3        XXEOBE
 002 C                   MOVE      U2SAG3        XXSAGE
 002 C                   MOVE      U2EOD3        XXEODE
 002 C                   when      E = 4
 002 C                   MOVE      U2EOB4        XXEOBE
 002 C                   MOVE      U2SAG4        XXSAGE
 002 C                   MOVE      U2EOD4        XXEODE
E002 C                   ENDSL
 001 *
B002 C                   if        XXEODE <> *BLANKS
 002 C                   CLEAR                   PDE01
 002 C                   eval      DECO# = CACO#
 002 C                   eval      DESUB# = CASUB#
 002 C                   eval      DEFND = CAFND
 002 C                   eval      DEFYR = CAFYR
 002 C                   eval      DECASE = CACASE
 002 C                   MOVEL     CASS#         DESS#
 002 C                   eval      DEACDT = CAACDT
 002 C                   eval      DEEOB# = CAEOB#
 002 C                   eval      DEDSEQ = CADSEQ
 002 C                   eval      DESEQ# = HDSEQ#
 002 C                   eval      HDSEQ# = HDSEQ# + 1
 002 C                   MOVEL(P)  XXEOBE        DEEOB
 002 C                   MOVEL(P)  XXEODE        DENOT1
 002 C                   MOVE(P)   XXEODE        DENOT2
 002 C                   MOVEL(P)  XXSAGE        DESAGP
 002 C                   MOVEL     'A'           DESTAT
 002 C                   MOVEL(P)  CAUSER        DEUSER
 002 C                   MOVEL(P)  CAPGM         DEPGM
 002 C                   eval      DETIME = CATIME
 002 C                   eval      DEDT = CADT
 002 C                   MOVEL(P)  CAUSER        DEIUSR
 002 C                   MOVEL(P)  CAPGM         DEIPGM
 002 C                   eval      DEITIM = CAITIM
 002 C                   eval      DEIDT = CAIDT
 002 C                   MOVEL(P)  'U'           DEESRC
 002 C                   eval      DERCN = EORCN
B003 C                   if        XXEODE = ' '
 003 C                   MOVEL(P)  'N'           DEPRNT
X003 C                   ELSE
 003 C                   MOVEL(P)  'Y'           DEPRNT
E003 C                   ENDIF                                                  XXEODE = ' '
 002 C                   WRITE     PDE01
E002 C                   ENDIF
 001 *
E001 C                   ENDDO                                                  DO 4 E
     *
B001 C                   if        U2REAS <> *BLANKS
 001 C                   CLEAR                   PDE01
 001 C                   eval      DECO# = CACO#
 001 C                   eval      DESUB# = CASUB#
 001 C                   eval      DEFND = CAFND
 001 C                   eval      DEFYR = CAFYR
 001 C                   eval      DECASE = CACASE
 001 C                   MOVEL     CASS#         DESS#
 001 C                   eval      DEACDT = CAACDT
 001 C                   eval      DEEOB# = CAEOB#
 001 C                   eval      DEDSEQ = CADSEQ
 001 C                   eval      DESEQ# = HDSEQ#
 001 C                   MOVEL(P)  U2REAS        DENOT1
 001 *
 001 C                   MOVEL     'A'           DESTAT
 001 C                   MOVEL(P)  CAUSER        DEUSER
 001 C                   MOVEL(P)  CAPGM         DEPGM
 001 C                   eval      DETIME = CATIME
 001 C                   eval      DEDT = CADT
 001 C                   MOVEL(P)  CAUSER        DEIUSR
 001 C                   MOVEL(P)  CAPGM         DEIPGM
 001 C                   eval      DEITIM = CAITIM
 001 C                   eval      DEIDT = CAIDT
 001 C                   MOVEL(P)  'U'           DEESRC
 001 C                   eval      DERCN = EORCN
 001 C                   MOVEL(P)  'Y'           DEPRNT
 001 C                   WRITE     PDE01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTDHP - Write FDDHP record
     *================================================================
     C     WRTDHP        BEGSR
     *
     C                   eval      KKEOB# = CAEOB#
     C                   eval      KKDSEQ = CADSEQ
     C     CAPKY1        CHAIN     PDH01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PDH01
 001 C                   eval      DHCO# = CACO#
 001 C                   eval      DHSUB# = CASUB#
 001 C                   eval      DHFND = CAFND
 001 C                   eval      DHFYR = CAFYR
 001 C                   eval      DHCASE = CACASE
 001 C                   MOVEL     CASS#         DHSS#
 001 C                   eval      DHACDT = CAACDT
 001 C                   eval      DHEOB# = CAEOB#
 001 C                   eval      DHDSEQ = CADSEQ
 001 *
B002 C                   if        U2CODE = *BLANKS
     C***** Beg Dlt *** 11/02/15 **************************************
 002 C*****              MOVEL     '96370'       DHPRCD
 002 C*****              MOVEL     '96370'       DHBPRC
 002 C*****              MOVEL(P)  CODEDS        DHBPRD
 002 C*****              Movel(P)  CODEDS        DHPRDS
     C***** End Dlt *** 11/02/15 **************************************
      *
     C***** Beg Add *** 11/02/15 **************************************
     C                   If        U2NDC  = *BLANKS
     C***** End Add *** 11/02/15 **************************************
     C                   MOVEL(P)  CODEDS        DHBPRD
     C                   Movel(P)  CODEDS        DHPRDS
     C***** Beg Add *** 11/02/15 **************************************
     C                   MOVEL     '96370'       DHPRCD
     C                   MOVEL     '96370'       DHBPRC
     C                   Else
     C                   Eval      DHBPRD = *Blanks
     C                   Eval      DHBPRC = *Blanks
     C                   Eval      DHPRDS = %Subst(U2CDSC:1:25)
     C                   EndIf
     C***** End Add *** 11/02/15 **************************************
X002 C                   ELSE
 002 C                   MOVEL     U2CODE        DHPRCD
 002 C                   MOVEL     U2CDSC        DHPRDS
 002 C                   MOVEL     U2CODE        DHBPRC
 002 C                   MOVEL     U2CDSC        DHBPRD
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2POS         DHPOS
 001 *
B002 C                   if        DHPOS = *BLANKS
 002 C                             or DHPOS = '00'
 002 C                   MOVEL     ' 11'         DHPOS
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2MOD         DHMOD1
 001 C                   MOVE      'C'           DHRCFL
 001 *
B002 C                   if        U2UNTS = *ZEROS
 002 C                   eval      DHUNTS = 1
X002 C                   ELSE
     C***** Beg Chg *** 06/11/14 ******************************************
 002 C                   z-add     U2UNTS        DHUNTS
 002 C***                eval      DHUNTS = U2UNTS
     C***** End Chg *** 06/11/14 ******************************************
E002 C                   ENDIF
 001 *
 001 C                   MOVE      U2DXR         XXDXR
 001 *
B002 C                   if        DX1 = 1
 002 C                             or DX2 = 1
 002 C                             or DX3 = 1
 002 C                             or DX4 = 1
 002 C                   MOVEL     'X'           DHDXR1
E002 C                   ENDIF
 001 *
B002 C                   if        DX1 = 2
 002 C                             or DX2 = 2
 002 C                             or DX3 = 2
 002 C                             or DX4 = 2
 002 C                   MOVEL     'X'           DHDXR2
E002 C                   ENDIF
 001 *
B002 C                   if        DX1 = 3
 002 C                             or DX2 = 3
 002 C                             or DX3 = 3
 002 C                             or DX4 = 3
 002 C                   MOVEL     'X'           DHDXR3
E002 C                   ENDIF
 001 *
B002 C                   if        DX1 = 4
 002 C                             or DX2 = 4
 002 C                             or DX3 = 4
 002 C                             or DX4 = 4
 002 C                   MOVEL     'X'           DHDXR4
E002 C                   ENDIF
 001 *
B002 C                   SELECT
 002 C                   when      DHDXR1 = *BLANKS
 002 C                             and DHDXR2 = *BLANKS
 002 C                             and DHDXR3 = *BLANKS
 002 C                             and DHDXR3 = *BLANKS
 002 C                             and EHDIA1 <> *BLANKS
 002 C                   MOVEL     'X'           DHDXR1
 002 C                   when      DHDXR1 = *BLANKS
 002 C                             and DHDXR2 = *BLANKS
 002 C                             and DHDXR3 = *BLANKS
 002 C                             and DHDXR3 = *BLANKS
 002 C                             and EHDIA2 <> *BLANKS
 002 C                   MOVEL     'X'           DHDXR2
 002 C                   when      DHDXR1 = *BLANKS
 002 C                             and DHDXR2 = *BLANKS
 002 C                             and DHDXR3 = *BLANKS
 002 C                             and DHDXR3 = *BLANKS
 002 C                             and EHDIA3 <> *BLANKS
 002 C                   MOVEL     'X'           DHDXR3
 002 C                   when      DHDXR1 = *BLANKS
 002 C                             and DHDXR2 = *BLANKS
 002 C                             and DHDXR3 = *BLANKS
 002 C                             and DHDXR3 = *BLANKS
 002 C                             and EHDIA4 <> *BLANKS
 002 C                   MOVEL     'X'           DHDXR4
E002 C                   ENDSL
 001 *
 001 C                   MOVEL     'A'           DHSTAT
 001 C                   MOVEL(P)  CAUSER        DHUSER
 001 C                   MOVEL(P)  CAPGM         DHPGM
 001 C                   eval      DHTIME = CATIME
 001 C                   eval      DHDT = CADT
 001 C                   MOVEL(P)  CAUSER        DHIUSR
 001 C                   MOVEL(P)  CAPGM         DHIPGM
 001 C                   eval      DHITIM = CAITIM
 001 C                   eval      DHIDT = CAIDT
 001 *
 001 C                   MOVEL(P)  U2MOD2        DHMOD2
 001 C                   MOVEL(P)  U2MOD3        DHMOD3
 001 C                   MOVEL(P)  U2MOD4        DHMOD4
 001 C                   MOVEL(P)  U2BLFC        DHBLFC
 001 C                   MOVEL(P)  U2DUCD        DHDUCD
     C*** BEG ADD *** 11/26/07 *********************************************
     C     U2BPRC        IFNE      *BLANKS
     C*** END ADD *** 11/26/07 *********************************************
 SS  C                   If        U2NDC  = *BLANKS
 001 C                   MOVEL(P)  U2BPRC        DHBPRC
 SS  C                   ELSE
 SS  C                   MOVEL(P)  *BLANKS       DHBPRC
 SS  C                   ENDIF
     C*** BEG ADD *** 11/26/07 *********************************************
     C                   ENDIF
     C*** END ADD *** 11/26/07 *********************************************
     C***** Beg Add *** 11/02/15 **************************************
      /Free
         DHNDC  = %Trim(%Subst(U2NDC:1:11))  ;
         DHNDCD = %Trim(%Subst(U2NDCD:1:25)) ;
      /End-Free
     C***** Beg Add *** 11/02/15 **************************************
 001 C                   MOVEL(P)  U2BMD1        DHMOD1
 001 *
 001 C                   WRITE     PDH01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTDPP - Write FDDPP record
     *================================================================
     C     WRTDPP        BEGSR
     *
     C                   eval      KKEOB# = CAEOB#
     C                   eval      KKDSEQ = CADSEQ
     C     CAPKY1        CHAIN     PDP01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PDP01
 001 C                   eval      DPCO# = CACO#
 001 C                   eval      DPSUB# = CASUB#
 001 C                   eval      DPFND = CAFND
 001 C                   eval      DPFYR = CAFYR
 001 C                   eval      DPCASE = CACASE
 001 C                   MOVEL     CASS#         DPSS#
 001 C                   eval      DPACDT = CAACDT
 001 C                   eval      DPEOB# = CAEOB#
 001 C                   eval      DPDSEQ = CADSEQ
 001 C                   MOVEL(P)  U2NDC         DPNDC
 001 C                   MOVEL(P)  U2RX#         DPRX#
 001 C                   MOVEL(P)  DPNDC         DPBNDC
     C*** Beg Chg *** 06/15/16 *****************************************
 001 C*******************eval      XXQTY = U2UNTS
 001 C****************** eval      DPRXMQ = XXQTY
 001 C                   eval      xxQty = %subst(u2rxMq:1:5) + '.' +
 001 C                                     %subst(u2rxMq:6:4)
     C                   monitor
 001 C                   eval      DPrxMq = %dec(xxQty:9:4)
     C                   on-error
     C                   endmon
     C*** End Chg *** 06/15/16 *****************************************
 001 C                   MOVEL(P)  U2CDSC        DPDESC
 001 *
 001 C                   eval      SRDTI = U2SVDT
 001 C                   CALL      'SRCVTDAT'    DTCYM2
 001 C                   MOVEL     SRDTO         DPRXFD
 001 *
B002 C                   if        U2UNTS = *ZEROS
 002 C                   eval      DPUNTS = 1
X002 C                   ELSE
     C***** Beg Chg *** 06/11/14 ******************************************
 002 C                   z-add     U2UNTS        DPUNTS
 002 C***                eval      DPUNTS = U2UNTS
     C***** End Chg *** 06/11/14 ******************************************
E002 C                   ENDIF
 001 *
 001 C                   MOVEL     'A'           DPSTAT
 001 C                   MOVEL(P)  CAUSER        DPUSER
 001 C                   MOVEL(P)  CAPGM         DPPGM
 001 C                   eval      DPTIME = CATIME
 001 C                   eval      DPDT = CADT
 001 C                   MOVEL(P)  CAUSER        DPIUSR
 001 C                   MOVEL(P)  CAPGM         DPIPGM
 001 C                   eval      DPITIM = CAITIM
 001 C                   eval      DPIDT = CAIDT
 001 *
 001 C                   MOVEL(P)  U2NWRF        DPNWRF
 001 C                   MOVEL(P)  U2CERT        DPCERT
 001 C                   MOVEL(P)  U2LIC#        DPLIC#
 001 C                   MOVEL(P)  U2NDCD        DPDESC
 001 C                   eval      DPRXDY = U2RXDY
 001 C                   MOVEL(P)  U2DFEE        DPDFEE
 001 C                   MOVEL(P)  U2RXDI        DPRXDI
 001 C                   MOVEL(P)  U2BCDT        DPBCDT
 001 C                   MOVEL(P)  U2BNDC        DPBNDC
 001 *
 001 C                   WRITE     PDP01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTDTL - Write Detail Records
     *================================================================
     C     WRTDTL        BEGSR
     *
B001 C                   if        *IN92 = *OFF
 001 C     U2KY1         CHAIN     PU201                              99
B002 C                   dow       *IN99 = *OFF
 002 *
 002 C                   EXSR      WRTCAP                                       FDCAP
 002 C                   EXSR      UPDT2A
 002 C                   EXSR      WRTDAP                                       FDDAP
 002 *
B003 C                   SELECT
 003 C                   when      EOBTYP = 'D'
 003 C                   EXSR      WRTDDP                                       FADDP
 003 C                   when      EOBTYP = 'H'
 003 C                   EXSR      WRTDUP                                       FADUP
 003 C                   when      EOBTYP = 'N'
 003 C                   EXSR      WRTDHP                                       FADHP
 003 C                   when      EOBTYP = 'X'
 003 C                   EXSR      WRTDPP                                       FADHP
E003 C                   ENDSL
 002 C                   EXSR      WRTDEP                                       FADEP
 002 *
 002 C                   EXSR      UPDT2B
 002 *
 002 C     U2KY1         READE     PU201                                  99
E002 C                   ENDDO
 001 *
X001 C                   ELSE
 001 *
 001 C     U2KY1         CHAIN     PU201                              99
B002 C                   dow       *IN99 = *OFF
 002 *
B003 C                   if        U2RLIN <> *ZEROS
 003 C                   EXSR      UPDCAP                                       FDCAP
E003 C                   ENDIF
 002 C                   EXSR      UPDT2A
B003 C                   if        U2RLIN <> *ZEROS
 003 C                   EXSR      WRTDAP                                       FDDAP
 003 C                   EXSR      WRTDEP                                       FDDEP
E003 C                   ENDIF
 002 C                   EXSR      UPDT2B
 002 *
 002 C     U2KY1         READE     PU201                                  99
E002 C                   ENDDO
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTDUP - Write FDDUP record
     *================================================================
     C     WRTDUP        BEGSR
     *
     C                   eval      KKEOB# = CAEOB#
     C                   eval      KKDSEQ = CADSEQ
     C     CAPKY1        CHAIN     PDU01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C                   CLEAR                   PDU01
 001 C                   eval      DUCO# = CACO#
 001 C                   eval      DUSUB# = CASUB#
 001 C                   eval      DUFND = CAFND
 001 C                   eval      DUFYR = CAFYR
 001 C                   eval      DUCASE = CACASE
 001 C                   MOVEL     CASS#         DUSS#
 001 C                   eval      DUACDT = CAACDT
 001 C                   eval      DUEOB# = CAEOB#
 001 C                   eval      DUDSEQ = CADSEQ
 001 *
 001 C                   MOVEL     KKRVCD        DURVCD
 001 C                   MOVEL     KKHCPC        DURATE
 001 *
 001 C                   MOVE      U2POS         DUPOS
 001 *
B002 C                   if        DUPOS = *BLANKS
 002 C                             or DUPOS = '00'
 002 C                   MOVEL     ' 21'         DUPOS
E002 C                   ENDIF
 001 *
     C***** Beg Chg *** 06/11/14 ******************************************
 002 C                   z-add     U2UNTS        DUUNTS
 001 C***                eval      DUUNTS = U2UNTS
     C***** End Chg *** 06/11/14 ******************************************
 001 C                   MOVE      DURVCD        DUBPRC
 001 C                   MOVE      DURATE        DUBRAT
 001 C                   MOVEL(P)  U2CDSC        DUDESC
 001 C                   MOVEL(P)  DUDESC        DUBPRD
 001 *
 001 C                   MOVEL     'A'           DUSTAT
 001 C                   MOVEL(P)  CAUSER        DUUSER
 001 C                   MOVEL(P)  CAPGM         DUPGM
 001 C                   eval      DUTIME = CATIME
 001 C                   eval      DUDT = CADT
 001 C                   MOVEL(P)  CAUSER        DUIUSR
 001 C                   MOVEL(P)  CAPGM         DUIPGM
 001 C                   eval      DUITIM = CAITIM
 001 C                   eval      DUIDT = CAIDT
 001 *
 001 C                   MOVEL(P)  U2DUCD        DUDUCD
     C*** BEG ADD *** 11/26/07 *********************************************
     C     U2BPRC        IFNE      *BLANKS
     C*** END ADD *** 11/26/07 *********************************************
 001 C                   MOVEL(P)  U2BPRC        DUBRAT
     C*** BEG ADD *** 11/26/07 *********************************************
     C                   ENDIF
     C*** END ADD *** 11/26/07 *********************************************
 001 C                   MOVEL(P)  U2BMD1        DUPMOD
 001 *
 001 C                   WRITE     PDU01
E001 C                   ENDIF
     *
     C                   ENDSR
     *================================================================
     * WRTRPP - Write WARPP record
     *================================================================
     C     WRTRPP        BEGSR
     *
     C                   eval      KKEOB# = EOEOB#
     C                   eval      KKEOBS = EORCN
     C     RPKY1         CHAIN     PRP01                              99
B001 C                   if        *IN99 = *ON
 001 *
 001 C     RPKY2         SETGT     PRP02
 001 C     RPKY2         READPE    PRP02                                  99
B002 C                   if        *IN99 = *OFF
 002 C                   eval      XXSEQ# = RPSEQ# + 1
X002 C                   ELSE
 002 C                   eval      XXSEQ# = 1
E002 C                   ENDIF
 001 *
 001 C                   CLEAR                   PRP01
 001 C                   eval      RPCO# = EOCO#
 001 C                   eval      RPFND = KKWCF#
 001 *
     *** BEG ADD *** 05/27/08 **************************************
B002 C                   if        u1rcnF = 'Y'
 001 C                   eval      RPFND  = eofnd
E002 C                   end
     *** END ADD *** 05/27/08 **************************************
 001 *
 001 C                   eval      RPFYR = EOFYR
 001 C                   eval      RPCASE = EOCASE
 001 C                   eval      RPSEQ# = XXSEQ#
 001 C                   eval      RPTRDT = EOTRDT
 001 C                   eval      RPFEIN = EOFEIN
 001 C                   eval      RPMOD = EOMOD
 001 C                   eval      RPAMT = EAPAID
 001 *
 001 C     CMKY4         CHAIN     PCM03                              99
 001 C                   MOVEL     'P'           KKPTYP
 001 C                   MOVEL     EOSYS         KKSYS
 001 C     CIKY          CHAIN     PCI01                              99
B002 C                   if        *IN99 = *OFF
     C**** Beg Add *** 07/14/16 ****************************************
     C                   Eval      isFndWMCIP = *On
     C**** End Add *** 07/14/16 ****************************************
 002 *
B003 C                   if        CMCVFL = 'Y'
 003 C                   MOVE(P)   CIP1CV        RPBR2
X003 C                   ELSE
B004 C                   if        EOBTYP = 'X'
 004 C                   MOVE(P)   CIXBR2        RPBR2
X004 C                   ELSE
 004 C                   MOVE(P)   CIBR2         RPBR2
E004 C                   ENDIF
E003 C                   ENDIF
 002 *
B003 C                   if        CMLTFL = 'I'
 003 C                   MOVE(P)   CIBR7L        RPBR7
X003 C                   ELSE
 003 C                   MOVE(P)   CIBR7N        RPBR7
E003 C                   ENDIF
 002 *
 002 C                   MOVEL(P)  CISTS         RPSTS
 002 *
X002 C                   ELSE
 002 *
B003 C                   if        EOSYS = 'P'
 003 C                   MOVEL(P)  'MD'          RPBR2
X003 C                   ELSE
 003 C                   MOVEL(P)  'HP'          RPBR2
E003 C                   ENDIF
 002 *
B003 C                   if        CMLTFL = 'I'
 003 C                   MOVEL(P)  'LT'          RPBR7
X003 C                   ELSE
 003 C                   MOVEL(P)  'NL'          RPBR7
E003 C                   ENDIF
 002 *
 001 *** BEGIN CHG *** 09/12/08 **************************************
 002 C*****************  MOVEL(P)  'PD'          RPSTS
 001  *** BEGIN CHG *** 10/01/08 **************************************
 002 C*****************  MOVEL(P)  'PY'          RPSTS
 002 C                   MOVEL(P)  'PD'          RPSTS
 001  *** END   CHG *** 10/01/08 **************************************
 001 *** END   CHG *** 09/12/08 **************************************
 002 *
E002 C                   ENDIF
 001 *** BEGIN ADD *** 08/15/05 **************************************
 001 *
 001 * CHECK TO SEE IF THIS BILL SHOULD BE AUTO-RELEASED
 001 *
B002 C                   if        C132 = *ON
 002 *
B003 C                   if        RPAMT > 250
 003 C                             or CMDNY = 'Y'
 003 C                             or CMSTAT = 'C'
 003 C                   MOVEL(P)  'PR'          RPSTS
X003 C                   ELSE
 003 C                   MOVE      *ON           RELEAS
E003 C                   ENDIF
 002 *
E002 C                   ENDIF
 001 *** END   ADD *** 08/15/05 **************************************
 001 *
 001 C                   MOVEL(P)  CMNCCI        RPNCCI
 001 C                   eval      RPFRDT = EOFRDT
 001 C                   eval      RPTODT = EOTODT
 001 C                   MOVEL(P)  'MP'          RPTRCD
 001 C                   MOVEL(P)  'P'           RPTYPE
 001 C                   MOVEL(P)  'M'           RPRETP
 001 C                   MOVEL(P)  'ME'          RPRECD
     C***** Beg Add *** 03/26/18 ***************************
      /Free
         if isFndWMCIP;
           if EObtyp = 'N' Or EObtyp = 'D';
             Exsr getChkCodes;
           endif;
         endif;
      /End-Free
     C***** End Add *** 03/26/18 ***************************
 001 *
 001 C                   MOVEL(P)  RPTRCD        KKKEY
 001 C     TBKY          CHAIN     PPC01                              99
B002 C                   if        *IN99 = *OFF
 002 C                   MOVEL(P)  PCDESC        RPDESC
     C***** Beg Add *** 03/26/18 ***************************
      /Free
                         RPreCd = PCreCd;
      /End-free
     C***** End Add *** 03/26/18 ***************************
E002 C                   ENDIF
 001 *
 001 C                   MOVEL(P)  EOPNAM        RPPNAM
 001 C                   MOVEL(P)  EOADR1        RPADR1
 001 C                   MOVEL(P)  EOADR2        RPADR2
 001 C                   MOVEL(P)  EOCTY         RPCTY
 001 C                   MOVEL(P)  EOST          RPST
 001 C                   eval      RPZIP = EOZIP
 001  *** BEGIN CHG *** 09/12/08 **************************************
 001 C************       MOVEL     'P'           RPSTAT
 001  *** BEGIN CHG *** 10/01/08 **************************************
 001 C*****************  MOVEL     ' '           RPSTAT
 001 C                   MOVEL     'P'           RPSTAT
 001  *** END   CHG *** 10/01/08 **************************************
 001  *** END   CHG *** 09/12/08 **************************************
 001 C                   MOVEL(P)  EOUSER        RPUSER
 001 C                   MOVEL(P)  EOPGM         RPPGM
 001 C                   eval      RPTIME = EOTIME
 001 C                   eval      RPDT = EODT
 001 C                   eval      RPEOB# = EOEOB#
 001 C                   eval      RPEOBS = EORCN
 001 C                   eval      RPRCVD = EODT
 001 *
 001 C     TRANS         CAT(P)    U1TRID:1      RPRMDS
 001 *
 001 C                   MOVEL(P)  RPSTS         KKKEY
 001 C     TBKY          CHAIN     PPS01                              99
B002 C                   if        *IN99 = *OFF
 002 C                   MOVE(P)   PSSRT         RPSORT
E002 C                   ENDIF
 001 *
 001 C                   MOVEL(P)  'N'           RPSENT
 001 C                   MOVEL(P)  USER          RPRQST
 001 *
 001 *  Call API to determine the Bank "Code".
 001 *
 001 C                   MOVE      RPCO#         $$CO#
 001 C                   MOVE      RPFND         $$FND
 001 C                   MOVE      RPFYR         $$FYR
 001 C                   MOVE      RPCASE        $$CASE
 001 C                   eval      QXTRDT = RPTRDT
 001 C                   eval      QXFRDT = RPFRDT
 001 C                   eval      QXTODT = RPTODT
 001 C                   MOVEL     RPTRCD        QXTRCD
 001 C                   MOVEL     RPRECD        QXRECD
 001 C                   MOVEL     RPBR2         QXPV1
 001 C                   MOVEL     RPBR7         QXPV2
 001 *
     C*** Beg Add *** 07/14/16 ****************************************
      /Free
           QXseq# = RPcmsq;
           QXeob# = %Char(RPeob#);
           QXfein = RPfein;
           QXmod  = RPmod ;
           QXdsq# = RPdsq#;
      /End-Free
     C*** End Add *** 07/14/16 ****************************************

 001 C                   CALL      'WA023R1'
 001 C                   PARM      '2'           $$MODE                         Mode
 001 C                   PARM                    $$CO#                          Company
 001 C                   PARM                    $$FND                          Group #
 001 C                   PARM                    $$FYR                          Year
 001 C                   PARM                    $$CASE                         Case
 001 C                   PARM      'WC'          $$APPL                         Applic Code
 001 C                   PARM      DATA          $$DATA                         Selection criteria
 001 C                   PARM      *BLANKS       $$CKTP                         Bank Acct Code
 001 C                   PARM      *BLANKS       $$RTCD                         Return Code
 001 *
 001 C                   MOVEL(P)  $$CKTP        RPCKTP                         Bank Acct# Code
 001 *
 001 C                   MOVEL(P)  EAIUSR        RPIUSR
 001 C                   MOVEL(P)  EAPGM         RPIPGM
 001 C                   eval      RPITM = EAITIM
 001 C                   eval      RPIDT = EAIDT
 001 *
B002 C                   if        RPAMT > *ZEROS
     C***** Beg Add *** 07/14/16 ***************************
     C***** Beg Del *** 03/26/18 ***************************
        //***/Free
        //*** if isFndWMCIP;
        //***   if EObtyp = 'N' Or EObtyp = 'D';
        //***     Exsr getChkCodes;
        //***   endif;
        //*** endif;
        //***/End-Free
     C***** End Del *** 03/26/18 ***************************
     C***** End Add *** 07/14/16 ***************************
 002 C                   WRITE     PRP01
E002 C                   ENDIF
E001 C                   ENDIF
     *
     C                   ENDSR
     C***** Beg Add *** 07/14/16 ***************************
      /Free
        Begsr getChkCodes;

         // For Bill Type = 'N'
          If EOBTYP = 'N';
            isMatch = *Off;
            setll (U1co#:U1sub#:U1fnd:U1fyr:U1case:KKeob#) FDDHL;
            Reade (U1co#:U1sub#:U1fnd:U1fyr:U1case:KKeob#) FDDHL;
            Dow Not %Eof (FDDHL) and isMatch = *Off;
              xxPRCD = DHPRCD;

              setll (U1co#:'CASV') FTGTL;
              reade (U1co#:'CASV') FTGTL;
              Dow Not %Eof (FTGTL) and isMatch = *Off;
                xxPRCDLowr = *Blanks;
                xxPRCDHigh = *Blanks;

                xxPRCDLowr = %Subst(%Trim(GTkey):1:5);
                xxPRCDHigh = %Subst(%Trim(GTkey):6:5);
                If xxPRCD >= xxPRCDLowr and  xxPRCD <= xxPRCDHigh;

                 // Copy Services
                  If GTDESC ='Copy Services';
                    If CMCVFL <> 'Y' and CICTRCD<> *Blanks;
                      RPTRCD = CICTRCD;
                    Endif;
                    If CMCVFL = 'Y' and CICTRCDCV <> *Blanks;
                      RPTRCD = CICTRCDCV;
                    Endif;

                    If CMCVFL <> 'Y' and CICPV1 <> *Blanks;
                      RPBR2  = CICPV1 ;
                    Endif;
                    If CMCVFL = 'Y' and CICPV1CV <> *Blanks;
                      RPBR2  = CICPV1CV ;
                    Endif;
                  Endif;

                 // QME/AME Services
                  If GTDESC ='QME/AME';
                    If CMCVFL <> 'Y' and CIQTRCD <> *Blanks;
                      RPTRCD = CIQTRCD;
                    EndIf;
                    If CMCVFL = 'Y' and CIQTRCDCV <> *Blanks;
                      RPTRCD = CIQTRCDCV;
                    Endif;
                    If CMCVFL <> 'Y' and CIQPV1 <> *Blanks;
                      RPBR2  = CIQPV1;
                    EndIf;
                    If CMCVFL = 'Y' and CIQPV1CV <> *Blanks;
                      RPBR2  = CIQPV1CV;
                    Endif;
                  Endif;

                 // PT Services
                  If GTDESC ='PT';
                    If CMCVFL <> 'Y' and CIPTRCD <> *Blanks;
                      RPTRCD = CIPTRCD;
                    EndIf;
                    If CMCVFL = 'Y' and  CIPTRCDCV <> *Blanks;
                      RPTRCD = CIPTRCDCV;
                    Endif;
                    If CMCVFL <> 'Y' and  CIPPV1 <> *Blanks;
                      RPBR2  = CIPPV1  ;
                    Endif;
                    If CMCVFL = 'Y' and  CIPPV1CV <> *Blanks;
                      RPBR2  = CIPPV1CV;
                    Endif;
                  Endif;

                 // DME Services
                  If GTDESC ='DME';
                    If CMCVFL <> 'Y' and  CIDTRCD <> *Blanks;
                      RPTRCD = CIDTRCD  ;
                    Endif;
                    If CMCVFL = 'Y' and  CIDTRCDCV <> *Blanks;
                      RPTRCD = CIDTRCDCV;
                    Endif;
                    If CMCVFL <> 'Y' and  CIDPV1 <> *Blanks;
                      RPBR2  = CIDPV1  ;
                    Endif;
                    If CMCVFL = 'Y' and  CIDPV1CV <> *Blanks;
                      RPBR2  = CIDPV1CV;
                    Endif;
                  Endif;
                  isMatch = *ON;
                Endif;
                reade (U1co#:'CASV') FTGTL;
              EndDo;                           //Dow Not %Eof (FTGTL);
              Reade (U1co#:U1sub#:U1fnd:U1fyr:U1case:KKeob#) FDDHL;
            EndDo;                             //Dow Not %Eof (FDDHL);
          Endif;                               //If EOBTYP = 'N';

         // For Bill Type = 'D'
          If EOBTYP = 'D';
            isMatch = *Off;
            setll (U1co#:U1sub#:U1fnd:U1fyr:U1case:KKeob#) FDDDL;
            Reade (U1co#:U1sub#:U1fnd:U1fyr:U1case:KKeob#) FDDDL;
            Dow Not %Eof (FDDDL) and isMatch = *Off;
              xxPRCD = DDprcd;

              setll (U1co#:'CASV') FTGTL;
              reade (U1co#:'CASV') FTGTL;
              Dow Not %Eof (FTGTL) and isMatch = *Off;
                xxPRCDLowr =*Blanks;
                xxPRCDHigh =*Blanks;

                xxPRCDLowr = %Subst(%Trim(GTKEY):1:5);
                xxPRCDHigh = %Subst(%Trim(GTKEY):6:5);
                If xxPRCD >= xxPRCDLowr and  xxPRCD <= xxPRCDHigh;

                 // Copy Services
                  If GTDESC ='Copy Services';
                    If CMCVFL <> 'Y' and CICTRCD<> *Blanks;
                      RPTRCD = CICTRCD;
                    Endif;
                    If CMCVFL = 'Y' and CICTRCDCV <> *Blanks;
                      RPTRCD = CICTRCDCV;
                    Endif;

                    If CMCVFL <> 'Y' and CICPV1 <> *Blanks;
                      RPBR2  = CICPV1 ;
                    Endif;
                    If CMCVFL = 'Y' and CICPV1CV <> *Blanks;
                      RPBR2  = CICPV1CV ;
                    Endif;
                  Endif;

                 // QME/AME Services
                  If GTDESC ='QME/AME';
                    If CMCVFL <> 'Y' and CIQTRCD <> *Blanks;
                      RPTRCD = CIQTRCD;
                    EndIf;
                    If CMCVFL = 'Y' and CIQTRCDCV <> *Blanks;
                      RPTRCD = CIQTRCDCV;
                    Endif;
                    If CMCVFL <> 'Y' and CIQPV1 <> *Blanks;
                      RPBR2  = CIQPV1;
                    EndIf;
                    If CMCVFL = 'Y' and CIQPV1CV <> *Blanks;
                      RPBR2  = CIQPV1CV;
                    Endif;
                  Endif;

                 // PT Services
                  If GTDESC ='PT';
                    If CMCVFL <> 'Y' and CIPTRCD <> *Blanks;
                      RPTRCD = CIPTRCD;
                    EndIf;
                    If CMCVFL = 'Y' and  CIPTRCDCV <> *Blanks;
                      RPTRCD = CIPTRCDCV;
                    Endif;
                    If CMCVFL <> 'Y' and  CIPPV1 <> *Blanks;
                      RPBR2  = CIPPV1  ;
                    Endif;
                    If CMCVFL = 'Y' and  CIPPV1CV <> *Blanks;
                      RPBR2  = CIPPV1CV;
                    Endif;
                  Endif;

                 // DME Services
                  If GTDESC ='DME';
                    If CMCVFL <> 'Y' and  CIDTRCD <> *Blanks;
                      RPTRCD = CIDTRCD  ;
                    Endif;
                    If CMCVFL = 'Y' and  CIDTRCDCV <> *Blanks;
                      RPTRCD = CIDTRCDCV;
                    Endif;
                    If CMCVFL <> 'Y' and  CIDPV1 <> *Blanks;
                      RPBR2  = CIDPV1  ;
                    Endif;
                    If CMCVFL = 'Y' and  CIDPV1CV <> *Blanks;
                      RPBR2  = CIDPV1CV;
                    Endif;
                  Endif;
                  isMatch = *ON;
                Endif;
                reade (U1co#:'CASV') FTGTL;
              EndDo;                           //Dow Not %Eof (FTGTL);
              Reade (U1co#:U1sub#:U1fnd:U1fyr:U1case:KKeob#) FDDDL;
            EndDo;                             //Dow Not %Eof (FDDDL);
          Endif;                               //If EOBTYP = 'D';
        Endsr;
      /End-Free
     C***** End Add *** 07/14/16 ***************************
