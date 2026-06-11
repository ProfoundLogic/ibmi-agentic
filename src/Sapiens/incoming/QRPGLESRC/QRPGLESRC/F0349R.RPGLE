     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H**********************************************************
     H* PROGRAM: F0349R
     H* DESCRIPTION: WORK WITH EOMBS
     H**********************************************************
     H*
     H* ** NOTE **
     H*    ANY CHANGES TO OPTION ARRAY WILL NEED TO ADDRESS THE
     H*    SEQUENCE CONDITIONING IN THE PGMSTR SUBROUTINE
     H*      TEST BY TURNING EDI INSTALL FLAG OFF
     H*
     H*****************************************************************
     H* INDICATORS:
     H*
     H*  51 - PROTECT FYR/CASE FROM ENTRY ON SUBSET
     H*  71 - INVALID SUBSCRIBER ON SUBSET
     H*  72 - INVALID FUND ON SUBSET
     H*  73 - INVALID FEIN/MOD ON SUBSET
     H*  74 - INVALID FROM DATE ON SUBSET
     H*  75 - PROTECT FYR/CASE FROM ENTRY ON SUBSET
     H*
     H***************************************************************
     H* REVISIONS:                                                  *
     H*
     H*  06/09/20  C:212  Project : SY420
     H*                   Mask SSN
     H*                   Recompile only(scss# is hidden)
     H*
     H*  08/17/00  C:117  PROJ: FLMNT
     H*                   Changed the logic for chg on 2/23/00; the
     H*                   condition was opposite.
     H*
     H*  05/05/00  C:079  PROJ: FLMNT
     H*                  1. Change logical used for Control # view
     H*
     H*  03/10/00 C:62  Support: MC220
     H*                 ADD EOB# PARM.
     H*
     H*  02/23/00 C:62  Support: MCMNT
     H*                 -CHANGE UPFL1 TO UPMRGR
     H*
     H*  06/12/99 C:117 Project: FL021 WORK WITH EOMBS FOR BILL ENTRY
     H*
     H*  03/08/98 C:53  Project: MC145
     H*                 SWAP F0031R TO F0032R (NEW VERSION)
     H*                                                             *
     H*  02/19/98 C:62  PROJECT: MC145                              *
     H*                 1) CHANGE OPT19 TO OPT18 = EDI LOG.         *
     H*                 2) CHANGE OPT82 TO OPT19 = SEND SUBSQ 837.  *
     H*                                                             *
     H* 09/26/97 C:59 PROJECT: SUPPORT MC087/#8828                  *
     H*               Calling incorrect program on DARTS            *
     H*               correct is F0270R NOT F0075R                  *
     H*                                                             *
     H* 09/15/97 C:56  PROJECT: MC087 #8086 SUPPORT                 *
     H*                PASS '1' IN CALL PARM FOR EDI LOG HISTORY    *
     H*                DISPLAY                                      *
     H*                                                             *
     H* 08/04/97 C:58  PROJECT: 87MBN                               *
     H*                ADD OPTION 82 TO REQUEST ADDITIONAL 837;     *
     H*                USE CALL TO EDI 835 LOG API, F0070R6.        *
     H*                                                             *
     H* 08/01/97 C:56  PROJECT: 87MAW                               *
     H*                ADD PARM TO CALL OF F0269R                   *
     H*                                                             *
     H* 07/14/97 C:32  PROJ: 87MBN                                  *
     H*                CREATED PGM                                  *
     H*                                                             *
     H***************************************************************
     F**********F0350F  CF  E                    WORKSTN
     FF0349F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FFAEOP     IF   E             DISK
     F                                     RENAME(PEO01:PFRCDF)
     F*** BEG CHG *** 06/12/99 ****************************************
     F***EOMB FILE IN CO#/EOB ORDER
     F***FAEOLG  IF  E           K        DISK
     F***                                           KINFDS DBFDS
     F***         PEO01                             KRENAMEDBRCDF
     F*
     F*  Primary logical file definition (for displaying items)
     FFAEOL31   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEO01:DBRCDF)
     F*** BEG CHG *** 05/05/00 ****************************************
     F****FAEOL32 IF  E           K        DISK
     FFAEOL45   IF   E           K DISK
     F*** END CHG *** 05/05/00 ****************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEO01:DBRCDF2)
     F*** END CHG *** 06/12/99 ****************************************
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFMFNL1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FFMPDL     IF   E           K DISK
     FFMF1L     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D*** BEG CHG *** 06/12/99 ****************************************
     D*************       HD      1   3 78               Column Headings
     D HD              S             78    DIM(6) CTDATA PERRCD(1)              Column Headings
     D*** BEGIN CHG *** 09/04/97 ******************************************
     D**********          OF      1  13  1   OFD    35   Options, Function
     D*************       OF      1  14  1   OFD    35   Options, Function
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D*** END CHANGE ** 09/04/97 ******************************************
     D*** END CHG *** 06/12/99 ****************************************
     D DTA1            S              1    DIM(74)                              Screen Data
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D* /COPY required constants
     D*
     D OPTNM           C                   CONST('EOMBS Selection')
     D ACDPGM          C                   CONST('F0349R01')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA013R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(3)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)
     D*
     D* Program specific constants
     D*
     D DL              C                   CONST('* * * DELETED * * *')
     D CHGNUM          C                   CONST('SA020R')
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D*
     D** DARTS KEY (SC010R) FOR LINK WITH WCMS
     D*
     D                 DS
     D  DSKEY                  1     40
     D  DSINFO                 1     16  0
     D  DSCO#                  1      3  0
     D  DSFND                  4      6  0
     D  DSFYR                  7      9  0
     D  DSCASE                10     16  0
     D*
     D** DARTS KEY (SC010R) FOR OUTSIDE ACCOUNT
     D*
     D                 DS
     D  DRKEY                  1     40
     D  DRINFO                 1     16  0
     D  DRCO#                  1      3  0
     D  DRSUB#                 4      8  0
     D  DRFND                  9     11  0
     D  DRFYR                 12     14  0
     D  DRCASE                15     21  0
     D*
     D*** BEGIN ADD *** 09/04/97 ******************************************
     D*
     D*   Zoned decimal parameters to call 837 Log API - F0070R6
     D*
     D                 DS
     D  QEINFO                 1     30  0
     D  QECO#                  1      3  0
     D  QESUB#                 4      8  0
     D  QEFND                  9     11  0
     D  QEFYR                 12     14  0
     D  QECASE                15     21  0
     D  QEEOB#                22     30  0
     D*
     D*** END ADD ***** 09/04/97 ******************************************
     D*
     D** DATE DS
     D*
     D                 DS
     D  XXMDY                  1      8
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXCC                   5      6  0
     D  XXYY                   7      8  0
     D*
     D                 DS
     D  XFMDY                  1     10
     D  XFMM                   1      2
     D  XFS1                   3      3
     D  XFDD                   4      5
     D  XFS2                   6      6
     D  XFCC                   7      8
     D  XFYY                   9     10
     D*** Data Structure for VIEW1
     D                 DS
     D  VIEW1                  1     37
     D  X1EOB#                 1      9
     D  X1NAME                11     36
     D*** Data Structure for VIEW2
     D                 DS
     D  VIEW2                  1     37
     D  X2CNTL                 1     20
     D  X2NAME                22     36

     C*
     C*===============================================================*
     C* *LIKE DEFN STATEMENTS                                         *
     C*===============================================================*
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOCNTL        XXCNTL
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOFRDT        XZFRDT
     C     *LIKE         DEFINE    EOFRDT        XXFRDT
     C     *LIKE         DEFINE    EOTODT        XXTODT
     C     *LIKE         DEFINE    EOFEIN        XXFEIN
     C     *LIKE         DEFINE    EOMOD         XXMOD
     C     *LIKE         DEFINE    UPUSR         XXUSR
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CDLSN         XXLSN
     C     *LIKE         DEFINE    CDFSN         XXFSN
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@RTN             1
     C*** BEGIN ADD *** 03/10/00 **************************************
     C                   PARM                    @@EOB#            9 0
     C*** END   ADD *** 03/10/00 **************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C*
     C     EDIPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##CASE            7 0
     C                   PARM                    ##SS#             9
     C                   PARM                    ##ACDT            6 0
     C                   PARM                    ##EOB#            9 0
     C*
     C*** BEGIN ADD *** 09/04/97 ******************************************
     C*
     C*   Key list to call EDI 837 Log API
     C*
     C     E37PL         PLIST
     C                   PARM                    QECO#
     C                   PARM                    QESUB#
     C                   PARM                    QEFND
     C                   PARM                    QEFYR
     C                   PARM                    QECASE
     C                   PARM                    QEEOB#
     C                   PARM      'B'           QEACTF            1
     C                   PARM      '       '     QEMGID            7
     C                   PARM      ' '           QERTCD            1
     C                   PARM      '837'         QETSET            3
     C*
     C*** END ADD ***** 09/04/97 ******************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*** KEY LIST FOR USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSR
     C*
     C*** BEG DEL *** 02/27/98 *************************8
     C*** FAEOP FILE IN EOB# SEQ
     C*******    KEY1      KLIST
     C*******              KFLD           ##CO#
     C*******              KFLD           XXEOB#
     C*******
     C****FAEOP FILE IN CO/SUBS/FND/FYR/CASE
     C*******    KEY2      KLIST
     C*** END DEL *** 02/27/98 *************************
     C*** BEG CHG *** 06/12/99 ****************************************
     C*** FAEOP FILE IN CO/SUBS/FND/FYR/CASE               ADD 02/27/98
     C***********KEY1      KLIST                           ADD 02/27/98
     C***********          KFLD           ##CO#
     C***********          KFLD           XXSUB#
     C***********          KFLD           XXFND
     C***********          KFLD           XXFYR
     C***********          KFLD           XXCASE
     C***********          KFLD           XXEOB#           ADD 02/27/98
     C*** FAEOP FILE IN CO#
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*** END CHG *** 06/12/99 ****************************************
     C*** BEG ADD *** 06/12/99 ****************************************
     C*** FAEOP FILE IN EOB# SEQ
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXEOB#
     C*
     C*** FAEOP FILE IN CO/CNTL#
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXCNTL
     C                   KFLD                    XXEOB#
     C*
     C*** END ADD *** 06/12/99 ****************************************
     C*
     C*** KEY LIST FOR CASE MASTER FILE
     C     CMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C*
     C*** KEY LIST FOR CLAIM DEMOGRAPHICS FILE
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*** KEY LIST FOR SMUPP FILE
     C     SMKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C*
     C*** KEY LIST FOR FUND/GRP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    UPFND
     C*
     C*
     C*** KEY LIST FOR FUND/GRP MASTER FILE
     C     FNKEY2        KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*** KEY LIST FOR PROVIDER FILE
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*** KEY LIST FOR FND AUXILIARY FILE
     C     F1KEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSSUB#        IFNE      *ZEROS
     C     SSFND         ORNE      *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C     SSFEIN        ORNE      *ZEROS
     C     SSMOD         ORNE      *ZEROS
     C     SSFRDT        ORNE      *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* CMDKEY - COMMAND KEY PROCESSING
     C*================================================================
     C     CMDKEY        BEGSR
     C*
     C*
     C*******************************
     C* F10 SEARCH ON SUBSET FIELDS *
     C*******************************
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C*** SUBSCRIBER SEARCH
     C*
     C     HFLD          IFEQ      'SSSUB#'
     C     *IN30         ANDEQ     '1'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SSSUB#'
     C     *IN30         ANDEQ     '0'
     C                   MOVEL     OPTNM         QQPRDS           40
     C                   MOVE      '2'           QQCALL            1
     C                   MOVE      SSSUB#        QQSUB#            5 0
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'F0545R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQSUB#
     C                   PARM                    QQRTCD
     C                   MOVEL     QQSUB#        SSSUB#
     C*
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      '1'           OUTFLG
     C*                    SETON                     90
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      'B'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     QQRTCD        IFEQ      '*'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF                                                  SSSUB# IFEQ
     C*
     C     HFLD          IFEQ      'SSFND'
     C     *IN30         ANDEQ     '1'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SSFND'
     C     *IN30         ANDEQ     '0'
     C                   MOVEL     OPTNM         QQPRDS
     C                   MOVE      '2'           QQCALL
     C     SSSUB#        IFEQ      *ZEROS
     C                   MOVE      UPSUB#        QQSUB#
     C                   ELSE
     C                   MOVE      SSSUB#        QQSUB#
     C                   ENDIF
     C                   MOVE      SSFND         QQFND             3 0
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'F0508R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQRTCD
     C                   MOVE      QQFND         SSFND
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
     C                   MOVE      QQSUB#        SSSUB#
     C                   SETON                                        90
     C                   ENDIF
     C*
     C                   ENDIF                                                  SSFND IFEQ
     C*
     C*** CASE# SEARCH
     C*
     C     HFLD          IFEQ      'SSCASE'
     C     HFLD          OREQ      'SSFYR'
     C                   MOVEL     OPTNM         QQPRDS
     C                   MOVE      '2'           QQCALL            1
     C                   MOVE      SSCASE        QQCASE
     C                   MOVE      SSFYR         QQFYR
     C                   MOVE      *BLANK        QQSS#
     C                   MOVE      *BLANK        QQRTCD            1
     C                   CALL      'F0269R'
     C                   PARM                    QQCALL
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQSS#             9
     C*** BEGIN ADD *** 08/01/97 **************************************
     C                   PARM                    QQACDT            8 0
     C*** END ADD *** 08/01/97 ****************************************
     C                   PARM                    QQRTCD            1
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
     C                   SETON                                        90
     C                   MOVE      QQSUB#        SSSUB#
     C                   MOVE      QQFND         SSFND
     C                   MOVE      QQFYR         SSFYR
     C                   MOVE      QQCASE        SSCASE
     C                   MOVE      QQSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              99
     C     *IN99         IFEQ      '0'
     C*
     C*** Get Claimant name
     C                   MOVE      *BLANKS       SSNM3
     C*
     C                   MOVEL(P)  CDLSN         ##LAST           56
     C                   MOVEL(P)  CDFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SSNM3            25
     C*
     C                   ENDIF                                                  *IN99
     C                   ENDIF                                                  SSCASE IFEQ
     C                   ENDIF                                                  QQRTCD '*'
     C*
     C*** FEIN SEARCH
     C*
     C     HFLD          IFEQ      'SSFEIN'
     C                   MOVE      '2'           QQCALL            1
     C                   MOVE      SSFEIN        QQFEIN            9
     C                   MOVE      SSMOD         QQMOD             3
     C                   MOVE      *BLANK        QQRTCD            1
     C******************   CALL 'F0031R'                   DEL 03/06/98
     C                   CALL      'F0032R'                                     ADD 03/06/98
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQRTCD
     C                   PARM      ' '           QQTYPE            1            ADD 03/08/98
     C                   MOVEL     QQFEIN        SSFEIN
     C                   MOVEL     QQMOD         SSMOD
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
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF                                                  SSFEIN IFEQ
     C*
     C                   ENDIF                                                  *INKJ IFEQ
     C*
     C*** GET NAMES OF SUBSET FIELDS
     C                   MOVEL     *BLANKS       SSNM1
     C     SSSUB#        IFNE      *ZEROS
     C                   MOVEL     SSSUB#        XXSUB#
     C     SMKEY         CHAIN     PSM01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     SMNAME        SSNM1
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       SSNM2
     C     SSFND         IFNE      *ZEROS
     C                   MOVE      SSFND         XXFND
     C     SSSUB#        IFEQ      *ZEROS
     C                   MOVE      ##SUB#        XXSUB#
     C                   ENDIF
     C     FNKEY2        CHAIN     PFN02                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     FNNAME        SSNM2
     C                   ENDIF
     C                   ENDIF
     C***
     C                   MOVEL     *BLANKS       SSNM4
     C     SSFEIN        IFNE      *ZEROS
     C                   MOVE      SSFEIN        XXFEIN
     C                   MOVE      SSMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     PDNAME        SSNM4
     C                   ENDIF
     C                   ENDIF
     C***
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     SCSEL         IFEQ      '2'
     C     SCAST         ANDNE     'FL'
     C     SCSEL         OREQ      '2'
     C     SCCNTL        ANDEQ     *BLANKS
     C                   SETON                                        9085
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C     *IN97         IFEQ      '1'
     C                   EXSR      CMDKEY
     C     *IN90         CABEQ     '1'           CHKEND
     C                   ENDIF                                                   *IN97 IFEQ
     C*
     C                   MOVEL     *BLANKS       SSNM1
     C     SSSUB#        IFNE      *ZEROS
     C                   MOVEL     SSSUB#        XXSUB#
     C     SMKEY         CHAIN     PSM01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9071
     C     *IN90         CABEQ     '1'           CHKEND
     C                   ELSE
     C                   MOVEL     SMNAME        SSNM1
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       SSNM2
     C     SSFND         IFNE      *ZEROS
     C                   MOVE      SSFND         XXFND
     C     SSSUB#        IFEQ      *ZEROS
     C                   MOVE      ##SUB#        XXSUB#
     C                   ENDIF
     C     FNKEY2        CHAIN     PFN02                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9072
     C     *IN90         CABEQ     '1'           CHKEND
     C                   ELSE
     C                   MOVEL     FNNAME        SSNM2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       SSNM4
     C     SSFEIN        IFNE      *ZEROS
     C                   MOVE      SSFEIN        XXFEIN
     C                   MOVE      SSMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              99
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9073
     C     *IN90         CABEQ     '1'           CHKEND
     C                   ELSE
     C                   MOVEL     PDNAME        SSNM4
     C                   ENDIF
     C                   ENDIF
     C*
     C     SSFYR         IFNE      *ZEROS
     C                   MOVE      SSFYR         XXFYR
     C                   ENDIF
     C*
     C     SSCASE        IFNE      *ZEROS
     C                   MOVE      SSCASE        XXCASE
     C                   ENDIF
     C*
     C     SSFRDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSFRDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9074
     C     *IN90         CABEQ     '1'           CHKEND
     C                   ENDIF
     C                   Z-ADD     SSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XZFRDT
     C                   ENDIF
     C*
     C*
     C     CHKEND        ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   MOVE      *BLANKS       SSSUB#
     C                   MOVE      *BLANKS       SSFND
     C                   MOVE      *BLANKS       SSFYR
     C                   MOVE      *BLANKS       SSCASE
     C                   MOVE      *BLANKS       SSFEIN
     C                   MOVE      *BLANKS       SSMOD
     C                   MOVE      *BLANKS       SSFRDT
     C*
     C*** INITIALIZE SUB# AND GRP#
     C*** CHECK FLAG IN FUND MASTER FOR INHIBITING SUB/FND SEARCH
     C*
     C                   MOVEL     SCUSER        XXUSR
     C                   MOVE      '0'           *IN30
     C     UPKEY         CHAIN     PUP01                              99
     C     *IN99         IFEQ      '0'
     C*
     C*** BEGIN CHG *** 02/23/00 **************************************
     C********** UPFL1     IFEQ 'N'
     C*** BEG CHG *** 08/17/00 ****************************************
     C********** UPMRGR    IFEQ 'N'
     C     UPMRGR        IFEQ      'Y'
     C*** END CHG *** 08/17/00 ****************************************
     C*** END   CHG *** 02/23/00 **************************************
     C     FNKEY         CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   Z-ADD     FNSUB#        SSSUB#
     C                   Z-ADD     FNFND         SSFND
     C                   MOVE      '1'           *IN30
     C                   ENDIF                                                  END *IN99
     C                   ENDIF                                                  END UPFL1
     C                   ENDIF                                                  END *IN99
     C*
     C*** BEG DEL *** 08/17/00 ****************************************
     C***  IF @@ ENTRY FIELDS HAVE VALUES, USE THESE VALUES
     C***  AS SUBSET VALUES
     C***        @@SUB#    IFNE *ZEROS
     C***                  MOVE @@SUB#    SSSUB#
     C***                  MOVE @@FND     SSFND
     C***                  MOVE @@FYR     SSFYR
     C***                  MOVE @@CASE    SSCASE
     C***  IF *IN51 IS ON,  PROTECT SUBSET FIELDS FROM ENTRY/SEARCH
     C***                  MOVE *ON       *IN51
     C***                  ENDIF
     C*** END DEL *** 08/17/00 ****************************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVEL     *LOVAL        XXEOB#
     C                   MOVEL     *LOVAL        XXCASE
     C                   MOVEL     *LOVAL        XXFYR
     C                   MOVEL     *LOVAL        XXFEIN
     C                   MOVEL     *LOVAL        XXMOD
     C                   MOVEL     *LOVAL        XXLSN
     C                   MOVEL     *LOVAL        XXFSN
     C                   MOVEL     *LOVAL        XXSS#
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        XXEOB#
     C   22              MOVEL(P)  SCPST2        XXCNTL
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCEOB#        XXEOB#
     C                   MOVEL(P)  SCCNTL        XXCNTL
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C     CMKEY         CHAIN     PCM01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZEROS        CMSS#
     C                   MOVE      *ZEROS        CMACDT
     C                   ENDIF
     C*
     C                   MOVE      CMACDT        SCACDT
     C                   MOVE      CMSS#         XXSS#
     C                   MOVE      CMSS#         SCSS#
     C     CDKEY         CHAIN     PCD01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      *BLANKS       CDLSN
     C                   MOVE      *BLANKS       CDFSN
     C                   ENDIF
     C*
     C*** Get Claimant name
     C                   MOVE      *BLANK        XXCNME
     C*
     C                   MOVEL     CDLSN         SCLSN
     C                   MOVEL     CDFSN         SCFSN
     C                   MOVEL(P)  CDLSN         ##LAST           56
     C                   MOVEL(P)  CDFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL(P)  ##LAST        XXCNME           25
     C*
     C*** BEG ADD *** 06/12/99 ****************************************
     C                   MOVE(P)   EOCNTL        SCCNTL
     C                   MOVE      EOAST         SCAST
     C*** END ADD *** 06/12/99 ****************************************
     C                   MOVE      EOEOB#        SCEOB#
     C                   MOVE      EOSUB#        SCSUB#
     C                   MOVE      EOFND         SCFND
     C                   MOVE      EOFYR         SCFYR
     C                   MOVE      EOCASE        SCCASE
     C                   MOVE      EOFEIN        SCFEIN
     C                   MOVE      EOMOD         SCMOD
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C*
     C*** MOVE TO ALPHA FIELDS FOR MOVE TO SCDATA
     C                   MOVEL     EOEOB#        XAEOB#            9
     C*** BEG ADD *** 12/18/97 ************************
     C                   MOVEL(P)  XAEOB#        ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         XAEOB#
     C*** END ADD *** 12/18/97 ************************
     C                   MOVEL     EOSUB#        XASUB#            5
     C                   MOVEL     EOFND         XAFND             3
     C                   MOVEL     EOFYR         XAFYR             3
     C                   MOVEL     EOCASE        XACASE            7
     C                   MOVEL     EOFEIN        XAFEIN            9
     C                   MOVEL     EOMOD         XAMOD             3
     C*** BEG ADD *** 06/12/99 ****************************************
     C                   MOVEL     EOCNTL        XACNTL           20
     C*** END ADD *** 06/12/99 ****************************************
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C*** BEG ADD *** 01/08/98 ************************
     C                   MOVEL(P)  SCFRDT        ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         SCFRDT
     C                   MOVEL     ##NUM         X1FRDT           10
     C*** END ADD *** 01/08/98 ************************
      ********
     C********             MOVELSCFRDT    XXMDY
     C********             MOVELXXMM      XFMM
     C********             MOVELXXDD      XFDD
     C********             MOVELXXCC      XFCC
     C********             MOVELXXYY      XFYY
     C********             MOVE '/'       XFS1
     C********             MOVE '/'       XFS2
     C********             MOVELXFMDY     X1FRDT 10
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*** BEG ADD *** 01/08/98 ************************
     C                   MOVEL(P)  SCTODT        ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         SCTODT
     C                   MOVEL     ##NUM         X1TODT           10
     C*** END ADD *** 01/08/98 ************************
     C********             MOVELSCTODT    XXMDY
     C********             MOVELXXMM      XFMM
     C********             MOVELXXDD      XFDD
     C********             MOVELXXCC      XFCC
     C********             MOVELXXYY      XFYY
     C********             MOVE '/'       XFS1
     C********             MOVE '/'       XFS2
     C*  GET PROVIDER NAME
     C                   MOVE      EOFEIN        XXFEIN
     C                   MOVE      EOMOD         XXMOD
     C                   MOVE(P)   *BLANKS       XXPNME
     C     PDKEY         CHAIN     PPD01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     PDNAME        XXPNME           16
     C                   ENDIF
     C*
     C*  MOVE DATA TO SCREEN ARRAY
     C                   CLEAR                   DTA1
     C*** BEG CHG *** 06/12/99 ****************************************
     C                   CLEAR                   VIEW1
     C                   CLEAR                   VIEW2
     C*
     C     *IN21         IFEQ      *ON
     C******               Z-ADD1         Y       20
     C******               MOVEAXAEOB#    DTA1,Y
     C******               ADD  10        Y
     C******               MOVEAXXCNME    DTA1,Y
     C******               ADD  26        Y
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEL(P)  XAEOB#        ##NUM
     C                   MOVEL(P)  *BLANKS       ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  ##NUM         X1EOB#
     C                   MOVEL(P)  XXCNME        X1NAME
     C                   MOVEA(P)  VIEW1         DTA1(Y)
     C                   ADD       37            Y
     C                   ELSE
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEL     XACNTL        X2CNTL
     C                   MOVEL     XXCNME        X2NAME
     C                   MOVEA     VIEW2         DTA1(Y)
     C                   ADD       37            Y
     C                   ENDIF
     C                   SELECT
     C     INFO#         WHENEQ    1
     C                   MOVEA     XXPNME        DTA1(Y)
     C                   ADD       16            Y
     C     INFO#         WHENEQ    2
     C                   MOVEA     XAFEIN        DTA1(Y)
     C                   ADD       10            Y
     C                   MOVEA     '-'           DTA1(Y)
     C                   ADD       2             Y
     C                   MOVEA     XAMOD         DTA1(Y)
     C                   ADD       4             Y
     C     INFO#         WHENEQ    3
     C                   MOVEL(P)  EOAMTC        ##NUM
     C                   MOVEL     'X,XXX.XX'    ##MASK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     ##NUM         XFAMT            12
     C                   MOVEA     XFAMT         DTA1(Y)
     C                   ADD       16            Y
     C                   ENDSL
     C*
     C                   MOVEA     X1FRDT        DTA1(Y)
     C                   ADD       11            Y
     C                   MOVEA     X1TODT        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*** END CHG *** 06/12/99 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT14 - Option 14 (DARTS)
     C*================================================================
     C     OPT14         BEGSR
     C     F1KEY         CHAIN     PF101                              99
     C     *IN99         IFEQ      '0'
     C*
     C     F1OUTA        IFEQ      'N'                                          LINKED WITH
     C                   MOVE      *BLANKS       DSKEY                          WCMS
     C                   MOVE      *ZEROS        DSINFO
     C                   MOVE      ##CO#         DSCO#
     C                   MOVE      SCFND         DSFND
     C                   MOVE      SCFYR         DSFYR
     C                   MOVE      SCCASE        DSCASE
     C                   MOVEL     DSKEY         ##KEY            40
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       DRKEY                          STANDALONE
     C                   MOVE      *ZEROS        DRINFO
     C                   MOVE      ##CO#         DRCO#
     C                   MOVE      SCSUB#        DRSUB#
     C                   MOVE      SCFND         DRFND
     C                   MOVE      SCFYR         DRFYR
     C                   MOVE      SCCASE        DRCASE
     C                   MOVEL     DRKEY         ##KEY
     C                   ENDIF                                                  F1OUTA IFEQ
     C*
     C*** BEGIN CHG *** 09/26/97 **************************************
     C*** USING F0270R INSTEAD OF F0075R
     C*********            MOVEL'F0075R'  ##PGMN 40
     C                   MOVEL(P)  'F0270R'      ##PGMN           40
     C*** END CHG ***** 09/26/97 **************************************
     C*
     C                   CALL      'SC010R'
     C                   PARM                    ##KEY
     C                   PARM                    ##PGMN
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* OPT16 - Option 16 (Delete EOMB)
     C*================================================================
     C     OPT16         BEGSR
     C                   Z-ADD     SFRRN1        SCSF1R
     C                   MOVE      OPTNM         QQPRDS
     C                   MOVE      ##CO#         QQCO#
     C                   MOVE      SCSUB#        Q1SUB#
     C                   MOVE      SCFND         Q1FND
     C                   MOVE      SCEOB#        QQEOB#
     C                   MOVE      *BLANK        QQRTCD
     C                   CALL      'F0150R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '4'           QQCALL            1            DELETE CALL
     C                   PARM                    QQCO#             3
     C                   PARM                    Q1SUB#            5
     C                   PARM                    Q1FND             3
     C                   PARM                    QQEOB#            9
     C                   PARM                    QQRTCD            1
     C*
     C     QQRTCD        IFEQ      '*'                                          COMPLETED
     C     SCSF1R        CHAIN     SCR01S                             99
     C     *IN99         IFEQ      *OFF
     C                   CLEAR                   SCR01S
     C                   EXSR      SF1UPD
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* OPT17 - Option 17 (Display/print EOMB)
     C*================================================================
     C     OPT17         BEGSR
     C                   ENDSR
     C*================================================================
     C* OPT18 - Option 18 (EDI LOG Info)
     C*================================================================
     C     OPT18         BEGSR
     C                   MOVE      SCEOB#        ##EOB#
     C                   MOVE      *BLANKS       ##CALL            1
     C*** BEGIN ADD *** 09/15/97 **************************************
     C     @@CALL        IFEQ      '1'
     C                   MOVE      '1'           ##CALL
     C                   ENDIF
     C*** END ADD *** 09/15/97 ****************************************
     C                   MOVE      'C'           ##MODE            1
     C                   MOVE      SCSUB#        ##SUB#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   MOVE      SCSS#         ##SS#
     C                   MOVE      SCACDT        ##ACDT
     C                   MOVE      SCEOB#        ##EOB#
     C                   CALL      'F0875R'      EDIPL
     C                   ENDSR
     C*** BEGIN ADD *** 09/04/97 ******************************************
     C*================================================================
     C* OPT19 - Option 19 - Resend EDI 837 Transmission
     C*================================================================
     C     OPT19         BEGSR
     C                   MOVE      ##CO#         QECO#
     C                   MOVE      SCSUB#        QESUB#
     C                   MOVE      SCFND         QEFND
     C                   MOVE      SCFYR         QEFYR
     C                   MOVE      SCCASE        QECASE
     C                   MOVE      SCEOB#        QEEOB#
     C*
     C                   CALL      'F0070R6'     E37PL
     C                   ENDSR
     C*================================================================
     C* OPT20 - Option 20 (View Check)
     C*================================================================
     C     OPT20         BEGSR
     C                   ENDSR
     C*================================================================
     C*                                                                ****
     C*** END ADD ***** 09/04/97 ******************************************
     C*                                                                ****
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     c*
     C*** BEG CHG *** 06/12/99 ****************************************
     C     SCSEL         CASEQ     '14'          OPT14
     C********   SCSEL     CASEQ'16'      OPT16
     C********   SCSEL     CASEQ'17'      OPT17
     C********   SCSEL     CASEQ'18'      OPT18
     C********   SCSEL     CASEQ'19'      OPT19
     C********   SCSEL     CASEQ'20'      OPT20
     C*** END CHG *** 06/12/99 ****************************************
     C                   ENDCS
     c*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      '1'           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   MOVE      *ON           *IN12
     C*
     C*** IS EDI INSTALLED ?
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C     ##EDI         IFNE      'Y'
     C*
     C                   MOVEL     CLRFKY        OFD(5)
     C                   MOVEL     CLRFKY        OFD(6)
     C*
     C                   ENDIF                                                              '1'
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*** BEG CHG *** 06/12/99 ****************************************
     C***21******          READ DBRCDF                   95
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*** END CHG *** 06/12/99 ****************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*** BEG CHG *** 06/12/99 ****************************************
     C***21******          READPDBRCDF                   95
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*** END CHG *** 06/12/99 ****************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*** BEG CHG *** 06/12/99 ****************************************
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C*** END CHG *** 06/12/99 ****************************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*** BEG CHG *** 06/12/99 ****************************************
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C*** END CHG *** 06/12/99 ****************************************
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL     DL            SCDATA
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*** BEGIN ADD *** 03/10/00 **************************************
     C     Q1PRMS        IFGT      7
     C*
     C                   Z-ADD     SCFND         @@FND
     C                   Z-ADD     SCFYR         @@FYR
     C                   Z-ADD     SCCASE        @@CASE
     C                   Z-ADD     SCEOB#        @@EOB#
     C*
     C                   ENDIF
     C*** END   ADD *** 03/10/00 **************************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSUB#        IFNE      *ZEROS
     C     SSSUB#        ANDNE     EOSUB#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     EOFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFYR         IFNE      *ZEROS
     C     SSFYR         ANDNE     EOFYR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSCASE        IFNE      *ZEROS
     C     SSCASE        ANDNE     EOCASE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSFEIN        ANDNE     EOFEIN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSMOD         IFNE      *ZEROS
     C     SSMOD         ANDNE     EOMOD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFRDT        IFNE      *ZEROS
     C     XZFRDT        IFLT      EOFRDT
     C     XZFRDT        ORGT      EOTODT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOSYS         IFNE      'P'
     C     EOSYS         ANDNE     'H'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** HD Column headings
Opt   EOMB#   Name                       Prov Name          From       To
Opt    FL Control#       Name            Prov Name          From       To
Opt   EOMB#   Name                       FEIN        Mod    From       To
Opt    FL Control#       Name            FEIN        Mod  From         To
Opt   EOMB#   Name                        Total Charged    From        To
Opt    FL Control#       Name             Total Charged    From        To
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                NYN
11414=DARTS                 YYN
11616=Delete EOMB           NNN
11717=Dspl/Prnt EOMB        NNN
11818=EDI Log Info          NYY
11919=Send Subsq 837        NNN
12020=View Check            NNN
203F3=Exit                  YYN
209F9=Change Info           YYY
211F11=Change View          YNN
212F12=Cancel               NNY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
