     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE U0060R - WORK WITH PRE-CERTIFICATIONS
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
      *                PRE-CERTIFICATIONS FOR A SINGLE CLAIM.         *
      *                THE PRE-CERT MAY BE CHOSEN                     *
      *                FOR UPDATE, DISPLAY OR SELECTION DEPENDING     *
      *                UPON THE CALLING PROGRAM.                      *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    F0269R   - WORK WITH MEDICAL CASES (OPT 15)                *
      *  CALLS TO:                                                    *
      *    U0060R01 - PRE/RE-CERT MAINTENANCE                         *
      *    U0061R   - WORK WITH PRE-CERT PROCEDURE CODES              *
      *    U0060R02 - PRE/RE-CERT ADDITIONAL INFO MAINTENANCE         *
      *                                                               *
      *  PROGRAM: U0060R                                              *
      *  CREATED: 12/09/97    C:47      PROJECT: 87MCB                *
      * ***************************************************************
      *  FILE USAGE:                                                  *
      *
      * VIEW 1 - UMPCL6
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT
      *   - Select PCEXT = 000
      *
      * VIEW 2 - UMPCL7
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/ADMIT DATE(DESC)/CERT/EXT
      *   - Select PCEXT = 000
      *
      * VIEW 3 - UMPCL8
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/FEIN/MOD/CERT/EXT
      *   - Select PCEXT = 000
      *
      * UMPCL9
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT Descend
      *   - Used to get the Re-Cert# and Status
      * ***************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *  03/22/01  C:62  PROJECT: MCMNT                               *
      *                  -DEACTIVATE OPTION 8 AND 11 WHEN IN INQUIRY  *
      *                   MODE.                                       *
      *                                                               *
      *  01/26/01  C:62  PROJECT: MCMNT  BASE ONLY                    *
      *                  -CORRECT STATUS SUBSET                       *
      *                  -ALLOW EDITING OF SUBSET FIELDS.             *
      *                                                               *
      *  10/27/00  C:62  PROJECT: MC071 24795                         *
      *                  ACTIVATE OPTION "5" DISPLAY IF IN SEARCH MD. *
      *                                                               *
      *  05/19/00  C:62  PROJECT: MC172                               *
      *                  RECON IS NOW IMPORTANT SO ADD TO PARM LIST.  *
      *                                                               *
      *  05/10/00 C:62  Project: AR001                                *
      *                 -ADD PARM FOR CERT TO SUBSET TO.              *
      *                                                               *
      *  03/07/00 C:62  Project: UR025                                *
      *                 -CHANGE OLD PROVIDER SEARCH TO NEW PROVIDER   *
      *                  SEARCH.                                      *
      *  NOT FOR CLT098 -IF STATUS IS CLOSED THEN RE-CERTS SHOULD BE  *
      *                  IN DISPLAY MODE ONLY.                        *
      *  NOT FOR CLT098 -DISPLAY MODE IF PRE-CERT CLOSED.             *
      *                   =OPT12 - SAVINGS                            *
      *                   =OPT13 - ADDITIONAL INFO                    *
      *                   =OPT17 - T/M ACTIVITY                       *
      *                                                               *
      *  03/03/00 C:62  Project: URMNT  **BASE ONLY**                 *
      *                 ADDED OPTION 16                               *
      *                                                               *
      *  04/13/99 C:53  Project: MC145                                *
      *                 STANDARDIZE T/M Activity                      *
      *                                                               *
      *  04/09/99 C:62 PROJECT: UR024                                 *
      *                CHANGE ADDT FROM ASCEND TO DESCEND.            *
      *                                                               *
      *  11/25/98 C:62 PROJECT: MC098 14996                           *
      *                MAKE SURE T/M FOR CERT IS ACCESSED NOT RE-CERT.*
      *                                                               *
      *  09/25/98 C:56 PROJECT: MC152                                 *
      *                CLEAR OUT @@RRN SO THAT CALLING PROGRAM WILL   *
      *                KNOW WHETER OR NOT THE USER SELECTED A PRECERT *
      *                                                               *
      *  08/21/98 C:62 PROJECT: MC177                                 *
      *                - GET CALL TO OPT7 TO BE IN DISPLAY MODE IF    *
      *                  CALLED IN DISPLAY MODE.                      *
      *                                                               *
      *  08/19/98 C:62 PROJECT: MC177                                 *
      *                - VARIOUS CHANGES TO T/M.                      *
      *                                                               *
      *  08/06/98 C:94                                                *
      *                MAKE OPT17 - TIME AND MATERIALS COND. ON       *
      *                SFBPF/FBAL01, POS.9                            *
      *                                                               *
      * ***************************************************************
      * INDICATORS:
      *                                                               *
      *  01 - Used on the PAGEDOWN keyword                            *
      *  02 - Used on the PAGEUP keyword                              *
      *  03 - Used to condition SFLCLR                                *
      * N03 - Used to condition SFLDSPCTL                             *
      *  04 - Used to condition SFLDSP                                *
      *  05 - Used to condition DSPATR(RI) on the selection field     *
      * N06 - Used to condition DSPATR(PC) on the position to field   *
      *  07 - Used to condition DSPATR(PC) on the selection field     *
      *  08 - Used to condition SFLNXTCHG                             *
      *  09 - INQUIRY                                                 *
      * N09 - UPDATE                                                  *
      * N10 - Used to condition SFLEND (on message subfile)           *
      *  11 - Used to indicate the list is subsetted                  *
      *  12 - Used to condition SFLDROP/SFLFOLD                       *
      *  71 - These indicators are to used as error indicators for    *
      *       thru the various screens. They will set off after       *
      *       displaying each input capible screen.                   *
      *  90 - Used to general purpose error indicator                 *
      *  91 - Used to display CPF5203; roll up/down past last record  *
      *  95 - Used to condition SFLEND; also used to indicate EOF and *
      *       BOF on the file being read.                             *
      * ***************************************************************
     FU0060F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      * PRE-CERT FILE
     FUMPCP     IF   E             DISK
     F                                     RENAME(PPC01:PFRCDF)
      * VIEW 1
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT
     FUMPCL6    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPC01:DBRCDF)
      * VIEW 2
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/ADMIT DATE/CERT/EXT
     F*** BEGIN CHG *** 04/09/99 **************************************
     F**********UMPCL7  IF  E           K        DISK
     FUMPCL15   IF   E           K DISK
     F*** END   CHG *** 04/09/99 **************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPC01:DBRCDF2)
      * VIEW 3
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/FEIN/MOD/CERT/EXT
     FUMPCL8    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PPC01:DBRCDF3)
      * PRE-CERT FILE BY CO/SUB/FND/FYR/CASE/CERT/EXT  Descend
     FUMPCL9    IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PPC01:PPC09)
      *
      * Case Master file
     FFMCMP     IF   E             DISK
      * Provider Master file
     FFMPDL     IF   E           K DISK
      * Generic Table file
     FFTGTP     IF   E             DISK
     F                                     INFDS(DBFDS5)
     F                                     RENAME(PGT01:PGT0P)
     FFTGTL     IF   E           K DISK
     F*** BEGIN CHG *** 08/19/98 **************************************
     F**********FAEOL4  IF  E           K        DISK
     FFAEOLL    IF   E           K DISK
     F*** END   CHG *** 08/14/98 **************************************
     F                                     INFDS(DBFDSA)
      *
      /COPY SCOPYWW01D
     D HDA             S             76    DIM(3) CTDATA PERRCD(1)              Column Headings L1
     D HD              S             76    DIM(3) CTDATA PERRCD(1)              Column Headings L2
     D OF              S              1    DIM(27) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(27) ALT(OF)
     D DATA            S             74    DIM(3)
     D DTA1            S              1    DIM(74)
     D DTA2            S              1    DIM(74)
     D DTA3            S              1    DIM(74)
     D*** END   ADD *** 01/26/01 **************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
      *
     D***PY SCOPYWW01I
      *
      * /COPY required constants
      *
     D OPTNM           C                   CONST('Pre-Certifications')
     D ACDPGM          C                   CONST('U0060R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA013R')
     D USPAPI          C                   CONST('F0006R')
      *
      * Program specific constants
      *
     D SF1PAG          C                   CONST(8)
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D XCPROT          C                   CONST('Prov Tel#:')
     D XCPRDI          C                   CONST('Prim Diag:')
     D XCLIAB          C                   CONST('Liab:')
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D*** BEG ADD *** 08/06/98 ************************************
     D CLROPT          C                   CONST('                    -
     D                                            NNN')
     D*** END ADD *** 08/06/98 ************************************
      *
     D                 DS                  INZ
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS                  INZ
     D  PCSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D                 DS                  INZ
     D  HLADDT                 1      8
     D  H1ADDT                 1      2
     D  H2ADDT                 3      4
     D  H3ADDT                 5      8
      *
     I***PY SCOPYWW01E

     I*
     I*** BEGIN ADD *** 01/26/01 **************************************
     IPPC09
     I              PCCO#                       P2CO#
     I              PCSUB#                      P2SUB#
     I              PCFND                       P2FND
     I              PCFYR                       P2FYR
     I              PCCASE                      P2CASE
     I              PCLIAB                      P2LIAB
     I              PCSTAT                      P2STAT
     I              PCADDT                      P2ADDT
     I              PCRVDT                      P2RVDT
     I              PCCERT                      P2CERT
     I              PCEXT                       P2EXT
     I              PCFEIN                      P2FEIN
     I              PCMOD                       P2MOD
     I              PCICD1                      P2ICD1
     I              PCICD2                      P2ICD2
     I              PCICD3                      P2ICD3
     I              PCICD4                      P2ICD4
     I              PCICD5                      P2ICD5
     I              PCCPT1                      P2CPT1
     I              PCCPT2                      P2CPT2
     I              PCCPT3                      P2CPT3
     I              PCCPT4                      P2CPT4
     I              PCCPT5                      P2CPT5
     I              PCCPT6                      P2CPT6
     I              PCCPT7                      P2CPT7
     I              PCCPT8                      P2CPT8
     I              PCSPD1                      P2SPD1
     I              PCSPD2                      P2SPD2
     I              PCSPD3                      P2SPD3
     I              PCSPD4                      P2SPD4
     I              PCSPD5                      P2SPD5
     I              PCSPD6                      P2SPD6
     I              PCSPD7                      P2SPD7
     I              PCSPD8                      P2SPD8
     I              PCEPD1                      P2EPD1
     I              PCEPD2                      P2EPD2
     I              PCEPD3                      P2EPD3
     I              PCEPD4                      P2EPD4
     I              PCEPD5                      P2EPD5
     I              PCEPD6                      P2EPD6
     I              PCEPD7                      P2EPD7
     I              PCEPD8                      P2EPD8
     I              PCA#V1                      P2A#V1
     I              PCA#V2                      P2A#V2
     I              PCA#V3                      P2A#V3
     I              PCA#V4                      P2A#V4
     I              PCA#V5                      P2A#V5
     I              PCA#V6                      P2A#V6
     I              PCA#V7                      P2A#V7
     I              PCA#V8                      P2A#V8
     I              PCTOS1                      PCTOS1
     I              PCTOS2                      P2TOS2
     I              PCTOS3                      P2TOS3
     I              PCTOS4                      P2TOS4
     I              PCTOS5                      P2TOS5
     I              PCTOS6                      P2TOS6
     I              PCTOS7                      P2TOS7
     I              PCTOS8                      P2TOS8
     I              PCINTR                      P2INTR
     I              PCPASG                      P2PASG
     I              PCREQD                      P2REQD
     I              PCCERD                      P2CERD
     I              PCRECD                      P2RECD
     I              PCPOS                       P2POS
      *

     C     *LIKE         DEFINE    PCSUB#        KKSUB#
     C     *LIKE         DEFINE    PCFND         KKFND
     C     *LIKE         DEFINE    PCFYR         KKFYR
     C     *LIKE         DEFINE    PCCASE        KKCASE
     C     *LIKE         DEFINE    PCCERT        KKCERT
     C     *LIKE         DEFINE    PCEXT         KKEXT
     C     *LIKE         DEFINE    PCADDT        KKADDT
     C     *LIKE         DEFINE    PCFEIN        KKFEIN
     C     *LIKE         DEFINE    PCMOD         KKMOD
     C     *LIKE         DEFINE    PCSUB#        SCSUB#
     C     *LIKE         DEFINE    PCFND         SCFND
     C     *LIKE         DEFINE    PCFYR         SCFYR
     C     *LIKE         DEFINE    PCCASE        SCCASE
     C     *LIKE         DEFINE    PCCO#         XXCO#
     C     *LIKE         DEFINE    PCSUB#        XXSUB#
     C     *LIKE         DEFINE    PCFND         XXFND
     C     *LIKE         DEFINE    PCFYR         XXFYR
     C     *LIKE         DEFINE    PCCASE        XXCASE
     C     *LIKE         DEFINE    PCCERT        XXCERT
     C     *LIKE         DEFINE    PCADDT        XXADDT
     C     *LIKE         DEFINE    PCADDT        XXDT
     C     *LIKE         DEFINE    PCFEIN        XXFEIN
     C     *LIKE         DEFINE    PCMOD         XXMOD
     C     *LIKE         DEFINE    PCCO#         ##CO#
     C     *LIKE         DEFINE    PCSUB#        ##SUB#
     C     *LIKE         DEFINE    PCFND         ##FND
     C     *LIKE         DEFINE    PCFYR         ##FYR
     C     *LIKE         DEFINE    PCSUB#        @@SUB#
     C     *LIKE         DEFINE    PCFND         @@FND
     C     *LIKE         DEFINE    PCFYR         @@FYR
     C     *LIKE         DEFINE    PCCASE        @@CASE
     C     *LIKE         DEFINE    PCSS#         @@SS#
     C     *LIKE         DEFINE    GTKEY         @@KEY
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        @@TBID
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*** BEGIN ADD *** 05/10/00 **************************************
     C                   PARM                    @@CERT           15
     C*** END   ADD *** 05/10/00 **************************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      *OFF          ##KC              1
     C     *INKL         PARM      *OFF          ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C                   PARM      *BLANK        QQOCST            1
     C                   PARM      'Y'           QQCONT            1
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C*** BEGIN CHG *** 08/21/98 **************************************
     C**********           PARM           ##MODE  1
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           PARM @@CALL    ##MODE  1
     C                   PARM                    ##MODE            1
     C*** END   CHG *** 03/07/00 **************************************
     C*** END   CHG *** 08/21/98 **************************************
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQCERT           15
     C                   PARM                    QQEXT             3
     C                   PARM                    ##OCST            1
     C*
     C     ACDPL3        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      *BLANK        QQCONT
     C                   PARM      *BLANK        QQMODE            1
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKADDT
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C     KEY1B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    PCCERT
     C                   KFLD                    XXEXT
     C*
     C     KEY2B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    PCADDT
     C                   KFLD                    PCCERT
     C                   KFLD                    XXEXT
     C*
     C     KEY3B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    PCFEIN
     C                   KFLD                    PCMOD
     C                   KFLD                    PCCERT
     C                   KFLD                    XXEXT
     C*
     C     KEY9          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    PCCERT
     C*
     C**  KEY LIST FOR CASE MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     EOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEGIN ADD *** 08/19/98 **************************************
     C*** BEGIN CHG *** 11/25/98 **************************************
     C**********           KFLD           KKCERT
     C**********           KFLD           KKEXT
     C                   KFLD                    PCCERT
     C                   KFLD                    PCEXT
     C*** END   CHG *** 11/25/98 **************************************
     C*** END   ADD *** 08/19/98 **************************************
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE GROUP AUXILLARY FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C**  KEY LIST FOR PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
      *
     C                   MOVE      *BLANK        SSLIAD
     C                   MOVE      *BLANK        SSPNAM
      *
     C     SSLIAB        IFNE      *BLANK
     C     SSFEIN        ORNE      *ZEROS
     C     SSMOD         ORNE      *ZEROS
     C     SSSTAT        ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* CMDKEY - COMMAND KEY PROCESSING
     C*================================================================
     C     CMDKEY        BEGSR
     C*
     C* F10-SEARCH    *
     C                   MOVEA     '0'           *IN(20)
     C                   MOVEA     '000'         *IN(32)
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C**  GENERIC TABLE FILE SEARCH
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSLIAB'
     C                   CALL      'F2380R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C*** BEG CHG *** 09/25/98 ****************************************
     C**********           PARM           @@RRN   90
     C                   PARM                    #1RRN             9 0
     C*** END CHG *** 09/25/98 ****************************************
     C                   PARM      'LIAB'        @@TBID
     C*
     C*** BEG CHG *** 09/25/98 ****************************************
     C********** @@RRN     CHAINPGT0P                60
     C     #1RRN         CHAIN     PGT0P                              60
     C*** END CHG *** 09/25/98 ****************************************
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SSLIAB
     C                   MOVEL(P)  GTDESC        SSLIAD
     C                   MOVE      *ON           *IN33
     C                   SETON                                        90
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * PROVIDER SEARCH
      *
     C     HFLD          WHENEQ    'SSFEIN'
     C     HFLD          OREQ      'SSMOD'
     C                   MOVE(P)   SSFEIN        QQFEIN
     C                   MOVE(P)   SSMOD         QQMOD
     C*** BEGIN CHG *** 03/07/00 **************************************
     C**********           CALL 'F0031R'
     C                   CALL      'F0032R'
     C*** END   CHG *** 03/07/00 **************************************
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM      *BLANK        QQTYPE            1
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      '1'           OUTFLG
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      'B'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE(P)   QQFEIN        XXFEIN
     C                   MOVE(P)   QQMOD         XXMOD
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PDFEIN        SSFEIN
     C                   Z-ADD     PDMOD         SSMOD
     C                   MOVEL(P)  PDNAME        SSPNAM
     C                   MOVE      *ON           *IN34
     C                   ENDIF
     C*
     C                   SETON                                        90
     C                   ENDIF                                                  QQRTCD = *
     C*
     C                   OTHER
     C                   MOVE      *ON           *IN20                          invalid curs pos
     C                   MOVE      *ON           *IN34                          POS CURSOR
     C                   MOVE      *ON           *IN90
     C                   ENDSL
     C*
     C                   ENDIF                                                  *INKJ
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  CAN NOT CHANGE INFO IF PRE-CERT IS CLOSED.
     C*
     C     SCSEL         IFEQ      '2 '
     C     SCSEL         OREQ      '11'
     C*
     C     SCSTAT        IFEQ      'C'
     C                   SETON                                        9074
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     *IN97         IFEQ      *ON
     C                   EXSR      CMDKEY
     C                   ENDIF                                                   *IN97 IFEQ
     C*
     C*** BEGIN DEL *** 01/26/01 **************************************
     C********** *IN12     IFEQ *OFF
     C*** END   DEL *** 01/26/01 **************************************
     C*
     C     SSLIAB        IFNE      *BLANK
     C                   MOVEL     'LIAB'        XXTBID
     C                   MOVEL     SSLIAB        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9071
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSFEIN        IFNE      *ZERO
     C     SSMOD         ORNE      *ZERO
     C                   MOVE(P)   SSFEIN        XXFEIN
     C                   MOVE(P)   SSMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9072
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      'O'
     C     SSSTAT        ANDNE     'C'
     C     SSSTAT        ANDNE     'R'
     C     SSSTAT        ANDNE     *BLANK
     C                   SETON                                        9073
     C                   ENDIF
     C*
     C*** BEGIN DEL *** 01/26/01 **************************************
     C**********           END                             *IN12
     C*** END   DEL *** 01/26/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   ENDSR
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14 = DARTS
     C*===============================================================
     C*
     C     KNDO          BEGSR
      *
      * CALL API TO ACCESS DARTS
      *
     C                   MOVE      ##CO#         XACO#
     C                   MOVE      CMSUB#        XASUB#
     C                   MOVE      CMFND         XAFND
     C                   MOVE      CMFYR         XAFYR
     C                   MOVE      CMCASE        XACASE
      *
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@WCIN
     C                   PARM                    @@WCDK
     C                   PARM                    XACO#             3
     C                   PARM                    XASUB#            5
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C                   PARM                    XACASE            7
     C                   PARM      *BLANK        ##KEY            40
      *
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   MOVE(P)   *LOVAL        KKCERT
     C                   MOVE(P)   *LOVAL        KKEXT
     C*** BEGIN CHG *** 04/09/99 **************************************
     C**********           MOVE *LOVAL    KKADDT    P
     C                   MOVE(P)   *HIVAL        KKADDT
     C*** END   CHG *** 04/09/99 **************************************
     C                   MOVE(P)   *LOVAL        KKFEIN
     C                   MOVE(P)   *LOVAL        KKMOD
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL(P)  SCPST1        KKCERT
     C   21              MOVE      *HIVAL        KKEXT
     C   22              MOVE      SCPST2        SRDTI
     C   22              CALL      'SRCVTDAT'    DTCYMD
     C   22              MOVEL     SRDTO         KKADDT
     C   23              MOVE(P)   SCPST3        KKFEIN
     C   23              MOVE(P)   SCPS3B        KKMOD
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   MOVEL(P)  SCCERT        KKCERT
     C                   MOVE      *ZERO         KKEXT
     C                   Z-ADD     SCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKADDT
     C                   MOVE(P)   SCFEIN        KKFEIN
     C                   MOVE(P)   SCMOD         KKMOD
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEGIN ADD *** 03/07/00 **************************************
     C                   MOVEA     '0'           *IN(20)
     C                   MOVEA     '000'         *IN(32)
     C*** END   ADD *** 03/07/00 **************************************
      *
      * GET INFO FOR SUBFILE
      *
     C                   MOVEL(P)  PCCERT        SCCERT
     C                   MOVEL(P)  PCCERT        QQCERT
     C                   MOVE(P)   PCEXT         QQEXT
     C*** BEGIN ADD *** 11/25/98 **************************************
     C                   MOVE(P)   PCEXT         SCEXT2
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C                   Z-ADD     DBRRNA        SCDBR2
     C*** END   ADD *** 11/25/98 **************************************
      * PROVIDER
      *
     C                   Z-ADD     *ZERO         XXFEIN
     C                   Z-ADD     *ZERO         XXMOD
     C                   Z-ADD     *ZERO         SCFEIN
     C                   Z-ADD     *ZERO         SCMOD
     C                   MOVE      *BLANK        SCPNAM
     C                   MOVE      *ZERO         XPAREA
     C                   MOVE      *ZERO         XPPHN1
     C                   MOVE      *ZERO         XPPHN2
      *
     C                   Z-ADD     PCFEIN        XXFEIN                         KEY FIELDS
     C                   Z-ADD     PCMOD         XXMOD
     C                   Z-ADD     PCFEIN        SCFEIN                         HIDDEN FLDS
     C                   Z-ADD     PCMOD         SCMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PDNAME        SCPNAM
     C                   MOVE(P)   PDAREA        XPAREA            3
     C                   MOVEL(P)  PDPHN#        XPPHN1            3
     C                   MOVE(P)   PDPHN#        XPPHN2            4
     C                   END
      *
      * ZERO SUPPRESS FEIN NUMBER
      *
     C                   MOVE      *ZERO         XAFEIN
     C                   MOVE      *BLANK        ALFEIN
     C                   MOVE(P)   PCFEIN        XAFEIN            9
     C     '0'           CHECK     XAFEIN        N                 5 0
     C     N             IFGT      *ZERO
     C                   SUBST(P)  XAFEIN:N      ALFEIN            9
     C                   END
      *
      * ZERO SUPPRESS MOD  NUMBER
      *
     C                   MOVE      *ZERO         XXNUM
     C                   MOVE      *ZERO         ALMOD
     C                   MOVEL(P)  PCMOD         XXNUM
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALMOD             3
      * ADMIT DATE
     C                   Z-ADD     PCADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         HLADDT                         LOAD DS
     C                   MOVEL(P)  SRDTO         SCADDT
      *
      * ZERO SUPPRESS ADMIT DATE
      *
     C                   MOVEL(P)  HLADDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALADDT           10
      *
      * GET LATEST RE-CERT RECORD TO LOAD SCEXT
      *
     C     KEY9          CHAIN     PPC09                              98
      *
     C*** BEGIN ADD *** 01/26/01 **************************************
     C     *IN98         IFEQ      *OFF
     C*
     C                   MOVE(P)   P2EXT         SCEXT                          HIDDEN FLD
      *
      * ZERO SUPPRESS RE-CERT NUMBER
      *
     C                   MOVEL(P)  P2EXT         XXNUM
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALEXT             4
      *
     C                   MOVEL(P)  P2STAT        SCSTAT
     C*
     C                   ELSE
     C*** END   ADD *** 01/26/01 **************************************
     C                   MOVE(P)   PCEXT         SCEXT                          HIDDEN FLD
      *
      * ZERO SUPPRESS RE-CERT NUMBER
      *
     C                   MOVEL(P)  PCEXT         XXNUM
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALEXT             4
      *
     C                   MOVEL(P)  PCSTAT        SCSTAT
     C*** BEGIN ADD *** 01/26/01 **************************************
     C                   ENDIF
     C*** END   ADD *** 01/26/01 **************************************
      *
      * RESET FILE TO CONTINUE USING ORIGINAL PRE-CERT RECORD
      *
     C                   MOVE      *ZERO         XXEXT             3 0
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY1B         CHAIN     DBRCDF                             60
      *
     C     *IN22         WHENEQ    *ON
     C     KEY2B         CHAIN     DBRCDF2                            60
      *
     C     *IN23         WHENEQ    *ON
     C     KEY3B         CHAIN     DBRCDF3                            60
     C                   ENDSL
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SECOND DATA LINE FOR SUBFILE
      *
     C                   MOVEL(P)  PCICD1        XXICD1            5
     C     PCICD1        IFEQ      *BLANK
     C                   MOVE      *BLANK        XXICD1
     C                   END
      *
     C     XCPROT        CAT(P)    '(':1         SCDTA2
     C                   CAT       XPAREA:0      SCDTA2                         AREA CODE
     C                   CAT       ')':0         SCDTA2
     C                   CAT       XPPHN1:1      SCDTA2                         PHONE #
     C                   CAT       '-':0         SCDTA2
     C                   CAT       XPPHN2:0      SCDTA2
     C                   CAT       XCPRDI:2      SCDTA2                         PRIMARY DIAGNOSIS
     C                   CAT       XXICD1:1      SCDTA2
      *
     C**                   CAT  XCLIAB:2  SCDTA2           LIABILITY
     C**                   CAT  PCLIAB:1  SCDTA2
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LOAD SUBFILE - VIEW 1
      *
     C                   SELECT
     C     HD#           WHENEQ    1
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     SCCERT        DTA1(Y)
     C                   ADD       16            Y
     C                   MOVEA     SCPNAM        DTA1(Y)
     C                   ADD       18            Y
     C                   MOVEA     ALFEIN        DTA1(Y)
     C                   ADD       10            Y
     C                   MOVEA     ALMOD         DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     ALADDT        DTA1(Y)
     C                   ADD       12            Y
     C                   MOVEA     ALEXT         DTA1(Y)
     C                   ADD       6             Y
     C                   MOVEA     SCSTAT        DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     PCLIAB        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C                   MOVEA(P)  HDA(1)        SCHDLA
     C* VIEW 2
     C     HD#           WHENEQ    2
     C                   Z-ADD     1             Y
     C                   MOVEA     ALADDT        DTA2(Y)
     C                   ADD       11            Y
     C                   MOVEA     SCCERT        DTA2(Y)
     C                   ADD       16            Y
     C                   MOVEA     SCPNAM        DTA2(Y)
     C                   ADD       18            Y
     C                   MOVEA     ALFEIN        DTA2(Y)
     C                   ADD       10            Y
     C                   MOVEA     ALMOD         DTA2(Y)
     C                   ADD       5             Y
     C                   MOVEA     ALEXT         DTA2(Y)
     C                   ADD       6             Y
     C                   MOVEA     SCSTAT        DTA2(Y)
     C                   ADD       4             Y
     C                   MOVEA     PCLIAB        DTA2(Y)
     C                   MOVEA(P)  DTA2          SCDATA
     C                   MOVEA(P)  HDA(2)        SCHDLA
     C* VIEW 3
     C     HD#           WHENEQ    3
     C                   Z-ADD     1             Y
     C                   MOVEA     ALFEIN        DTA3(Y)
     C                   ADD       10            Y
     C                   MOVEA     ALMOD         DTA3(Y)
     C                   ADD       4             Y
     C                   MOVEA     SCCERT        DTA3(Y)
     C                   ADD       16            Y
     C                   MOVEA     SCPNAM        DTA3(Y)
     C                   ADD       18            Y
     C                   MOVEA     ALADDT        DTA3(Y)
     C                   ADD       12            Y
     C                   MOVEA     ALEXT         DTA3(Y)
     C                   ADD       6             Y
     C                   MOVEA     SCSTAT        DTA3(Y)
     C                   ADD       4             Y
     C                   MOVEA     PCLIAB        DTA3(Y)
     C                   MOVEA(P)  DTA3          SCDATA
     C                   MOVEA(P)  HDA(3)        SCHDLA
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - OPTION 7 RE-CERTS
     C*================================================================
     C     OPT7          BEGSR
     C*
     C*** BEGIN ADD *** 03/07/00 **************************************
     C     SCSTAT        IFEQ      'C'
     C*
     C                   MOVEL     '1'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVEL     @@CALL        ##MODE
     C*
     C                   ENDIF
     C*** END   ADD *** 03/07/00 **************************************
     C*
     C                   CALL      'U0062R'      ACDPL1
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 12/01/98 **************************************
     C*================================================================
     C* OPT8 - OPTION 8  LETTERS
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   CALL      'F4110C'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZERO         ##RRN1            9 0
     C                   PARM      SCDBRR        ##RRN             9 0          UMPCP
     C                   PARM      'P'           ##TYPE            1
     C*
     C                   ENDSR
     C*** END   ADD *** 12/01/98 **************************************
     C*** BEGIN DEL *** 12/01/98 **************************************
     C*================================================================
     C* OPT8 - OPTION 8  LETTERS
     C*================================================================
     C********** OPT8      BEGSR
     C*
     C**********           MOVE PCCO#     QLCO#   3
     C**********           MOVE PCSUB#    QLSUB#  5
     C**********           MOVE PCFND     QLFND   3
     C**********           MOVE SCCERT    QLCERT 15
     C**                   MOVE SCEXT     QLEXT   3
     C**********           MOVE *ZERO     QLEXT   3
     C**********           CALL 'U0040R'
     C**********           PARM           QQPRDS 40
     C**********           PARM           QLCO#
     C**********           PARM           QLSUB#
     C**********           PARM           QLFND
     C**********           PARM           QLCERT
     C**********           PARM           QLEXT
     C**********           PARM           QQRTCD
     C*
     C********** QQRTCD    IFEQ 'A'
     C**********           MOVE *ON       *INKC
     C**********           END
     C*
     C**********           ENDSR
     C*** END   DEL *** 12/01/98 **************************************
     C*================================================================
     C* OPT9  - Option  9  PROCEDURE CODES
     C*================================================================
     C     OPT9          BEGSR
     C* SEND RRN OF UMPCP
     C                   CALL      'U0061R'      ACDPL3
     C*
     C                   ENDSR
     C*================================================================
     C* OPT10- Option 10 PRE-CERT HISTORY
     C*================================================================
     C     OPT10         BEGSR
      *
     C                   CALL      'U0063R'      ACDPL1
      *
     C                   ENDSR
     C*================================================================
     C* OPT11 - Option 11  CLOSE PRE-CERT
     C*================================================================
     C     OPT11         BEGSR
     C*
     C                   MOVEL     'A'           ##MODE
     C                   MOVEL     'C'           ##OCST
     C                   MOVEL(P)  SCCERT        QQCERT
     C                   MOVE(P)   SCEXT         QQEXT
     C                   CALL      'U0063R01'    ACDPL2
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 12/31/98 **************************************
     C*================================================================
     C* OPT12 - Option 12  SAVINGS
     C*================================================================
     C     OPT12         BEGSR
     C*
     C     @@CALL        IFEQ      ' '
     C*** BEGIN ADD *** 03/07/00 **************************************
     C     SCSTAT        IFEQ      'C'
     C*
     C                   MOVEL     'L'           ##MODE
     C*
     C                   ELSE
     C*** END   ADD *** 03/07/00 **************************************
     C*
     C                   MOVEL     'S'           ##MODE
     C*** BEGIN ADD *** 03/07/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/07/00 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     'L'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C                   MOVEL(P)  SCCERT        #1CERT
     C                   MOVE(P)   SCEXT         #1EXT
     C*
     C                   CALL      'U0060R01'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1CERT           15
     C                   PARM                    #1EXT             3
     C                   PARM      *BLANKS       ##OCST            1
     C                   PARM      'N'           ##CONT            1
     C*
     C                   ENDSR
     C*================================================================
     C* OPT13 - Option 13  ADDITIONAL INFORMATION
     C*================================================================
     C     OPT13         BEGSR
     C*
     C     @@CALL        IFEQ      ' '
     C*
     C*** BEGIN ADD *** 03/07/00 **************************************
     C     SCSTAT        IFEQ      'C'
     C*
     C                   MOVEL     'D'           ##MODE
     C*
     C                   ELSE
     C*** END   ADD *** 03/07/00 **************************************
     C                   MOVEL     'C'           ##MODE
     C*** BEGIN ADD *** 03/07/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/07/00 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     'D'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   CALL      'U0060R02'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      'N'           ##CONT            1
     C*
     C                   ENDSR
     C*** END   ADD *** 12/31/98 **************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
     C*================================================================
     C* OPT16 - BILL HISTORY
     C*================================================================
     C     OPT16         BEGSR
     C*
     C                   CALL      'F0350R'
     C                   PARM      '1'           #@CALL            1
     C                   PARM      CMSUB#        #@SUB#            5 0
     C                   PARM      CMFND         #@FND             3 0
     C                   PARM      CMFYR         #@FYR             3 0
     C                   PARM      CMCASE        #@CASE            7 0
     C                   PARM      *ZEROS        #@EOB#            9 0
     C                   PARM      SCCERT        #@CERT           15
     C*
     C                   ENDSR
     C*** END   ADD *** 03/03/00 **************************************
     C*================================================================
     C* OPT17 - OPTION 17 Time and Materials
     C*================================================================
     C     OPT17         BEGSR
     C*
     C     @@CALL        IFEQ      ' '
     C*** BEGIN ADD *** 03/07/00 **************************************
     C     SCSTAT        IFEQ      'C'
     C*
     C                   MOVEL     '1'           ##CALL
     C*
     C                   ELSE
     C*** END   ADD *** 03/07/00 **************************************
     C*
     C                   MOVEL     ' '           ##CALL
     C*** BEGIN ADD *** 03/07/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/07/00 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           ##CALL
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 11/25/98 **************************************
     C*** BEGIN ADD *** 08/19/98 **************************************
     C**********           MOVELSCCERT    KKCERT
     C**********           Z-ADDSCEXT     KKEXT
     C*** END   ADD *** 08/19/98 **************************************
     C*
     C********** EOKY      CHAINPEO01                99
     C*** END   DEL *** 11/25/98 **************************************
     C*
     C*** BEGIN DEL *** 08/19/98 **************************************
     C********** *IN99     DOWEQ*OFF
     C*
     C********** EOSYS     IFEQ 'U'
     C*
     C**********           LEAVE
     C*
     C**********           ENDIF
     C*
     C********** EOKY      READEPEO01                    99
     C*
     C**********           ENDDO
     C*** END   DEL *** 08/19/98 **************************************
     C*
     C                   CALL      'F3040R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*** BEGIN CHG *** 11/25/98 **************************************
     C**********           PARM DBRRNA    ##RRN2  90
     C                   PARM      SCDBR2        ##RRN2            9 0
     C*** END   CHG *** 11/25/98 **************************************
     C*** BEGIN ADD *** 08/19/98 **************************************
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      SCCERT        ##CERT           15
     C*** BEGIN CHG *** 11/25/98 **************************************
     C**********           PARM SCEXT     ##EXT   30
     C                   PARM      SCEXT2        ##EXT             3 0
     C*** END   CHG *** 11/25/98 **************************************
     C*** BEGIN ADD *** 09/29/99 **************************************
     C                   PARM      *BLANKS       ##EOB             1
     C*** END   ADD *** 09/29/99 **************************************
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      *ZEROS        ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*** END   ADD *** 08/19/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKN         CASEQ     *ON           KNDO                     90     DARTS
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   MOVE      *BLANK        ##MODE
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C*** BEGIN ADD *** 12/31/98 **************************************
     C     SCSEL         CASEQ     '12'          OPT12
     C     SCSEL         CASEQ     '13'          OPT13
     C*** END   ADD *** 12/31/98 **************************************
     C*** BEGIN ADD *** 03/03/00 **************************************
     C     SCSEL         CASEQ     '16'          OPT16
     C*** END   ADD *** 03/03/00 **************************************
     C     SCSEL         CASEQ     '17'          OPT17
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN12                           FOLD/TRUNCATE
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C*** BEGIN ADD *** 03/03/00 **************************************
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##PRVB        IFNE      'Y'
     C*
     C                   MOVEL     CLROPT        OFD(14)
     C*
     C                   ENDIF
     C*** END   ADD *** 03/03/00 **************************************
     C*** BEGIN ADD *** 08/19/98 **************************************
     C*
     C     @@MED         IFNE      'Y'
     C*
     C                   MOVEL     CLROPT        OFD(15)
     C*
     C                   ENDIF
     C*
     C     @@EDI         IFNE      'Y'
     C*
     C                   MOVEL     CLROPT        OFD(16)
     C                   MOVEL     CLROPT        OFD(17)
     C*
     C                   ENDIF
     C*** END   ADD *** 08/19/98 **************************************
     C*** BEG ADD *** 08/06/98 **************************************
     C********** @@MED     IFNE 'Y'
     C**********           MOVELCLROPT    OFD,15
     C**********           ELSE
     C**********           MOVELPCIOPT    OFD,15
     C**********           ENDIF
     C*** END ADD *** 08/06/98 **************************************
     C*
     C*** BEG ADD *** 09/25/98 ****************************************
     C* MOVE FMCMP RELATIVE RECORD NUMBER PASSED IN TO A PROGRAM WORK
     C* FIELD AND CLEAR OUT THE ENTRY PARM @@RRN.  THIS WILL ALLOW THE
     C* CALLING PROGRAM TO TELL WHETHER OR NOT THE USER SELECTED A
     C* PRE-CERT
     C*
     C                   Z-ADD     @@RRN         #1RRN
     C                   Z-ADD     0             @@RRN
     C*** END ADD *** 09/25/98 ****************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET INFO FOR HEADING, LOAD KEY FIELDS FOR UMPCP
      *   SUB,GROUP,YEAR,CASE
      *
     C*** BEG CHG *** 09/25/98 ****************************************
     C********** @@RRN     CHAINPCM01                99
     C     #1RRN         CHAIN     PCM01                              99
     C*** END CHG *** 09/25/98 ****************************************
     C                   Z-ADD     CMSUB#        KKSUB#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
      *
      * CALL API TO CREATE A TWO LINE HEADING
      *
     C                   MOVE      CMCO#         QQCO#             3
     C                   MOVE      CMSUB#        QQSUB#            5
     C                   MOVE      CMFND         QQFND             3
     C                   MOVE      CMFYR         QQFYR             3
     C                   MOVE      CMCASE        QQCASE            7
      *
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C     SCL1HD        PARM      *BLANK        QQL1             78
     C     SCL2HD        PARM      *BLANK        QQL2             78
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET UMPCP
      *
     C     KEY           CHAIN     DBRCDF                             99
      *
     C                   SELECT
     C     @@CALL        WHENEQ    '1'                                           INQUIRY
     C     @@CALL        OREQ      'D'                                           INQUIRY
     C                   MOVE      *ON           *IN09
     C                   MOVEL     'D'           ##MODE
      *
     C     @@CALL        WHENEQ    '2'                                           SEARCH
     C                   MOVE      *ON           *IN30
     C                   MOVEL     'D'           ##MODE
      *
     C     @@CALL        WHENEQ    'C'
     C     *IN99         IFEQ      *ON                                           ADD
     C                   MOVEL     'A'           ##MODE
     C                   END
      *
     C     *IN99         IFEQ      *OFF                                          CHANGE
     C                   MOVEL     'C'           ##MODE
     C                   END
     C                   ENDSL
      *
     C                   MOVEA(P)  HDA(1)        SCHDLA
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
      *
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY           READE     DBRCDF                                 95
      *
     C     *IN22         WHENEQ    *ON
     C     KEY           READE     DBRCDF2                                95
      *
     C     *IN23         WHENEQ    *ON
     C     KEY           READE     DBRCDF3                                95
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C   23KEY           READPE    DBRCDF3                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
      *
     C                   MOVE      SCDBRR        @@RRN
      *
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     SSLIAB        IFNE      *BLANK
     C     SSLIAB        ANDNE     PCLIAB
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSFEIN        ANDNE     PCFEIN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSMOD         IFNE      *ZEROS
     C     SSMOD         ANDNE     PCMOD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
      *
     C*** BEGIN ADD *** 01/26/01 **************************************
     C     KEY9          CHAIN     PPC09                              98
     C  N98              MOVEL(P)  P2STAT        PCSTAT
     C*** END   ADD *** 01/26/01 **************************************
      *
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     PCSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEGIN ADD *** 05/10/00 **************************************
     C*
     C     Q1PRMS        IFGT      4
     C*
     C     @@CERT        IFNE      *BLANK
     C     @@CERT        ANDNE     PCCERT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 05/10/00 **************************************
     C                   ENDSR
    1 *       2         3         4         5        6        7         8
678901*345679801234567890123465798012345678901234567901234678901234567890
** Column headings
                                                Admit/Plan  Re
Admit/Plan                                                  Re
                                                Admit/Plan  Re
** Column headings
Pre-Cert#       Provider name     Prov FEIN Mod    date    Certs Sts Liab
   date    Pre-Cert#       Provider name     Prov FEIN Mod Certs Sts Liab
Prov FEIN Mod Pre-Cert#       Provider name        date    Certs Sts Liab
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1077=Re-Certs               YYN
1088=Letters                YNN
1099=Proc Codes             YYN
11010=Pre-Cert History      YYN
11111=Close Pre-Cert        YNN
11212=Savings               YYN
11313=Addtl Info            YYN
11414=DARTS                 NNN
11515=Pre-Cert              NNN
11616=Bill Hist             YYN
11717=T/M Activity          YYN
11818=EDI                   NNN
11919=EDI Log Hist          NNN
12020=Change Status         NNN
18282=Send Subsq 148        NNN
203F3=Exit                  YYN
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
214F14=DARTS                YYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYN
3
