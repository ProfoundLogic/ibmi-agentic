     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0079R - WORK WITH ADDITIONAL NAMED INSUREDS
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION: THIS PROGRAM WILL DISPLAY ALL ADDITIONAL NAMED   *
     H*              INSUREDS BY NAME. OPTION 4 = DELETE WILL ALLOW   *
     H*              ENTRY OF A DELETION DATE.                        *
     H*                                                               *
     H* CALLED FROM: W0050R (OPT90)                                   *
     H*                                                               *
     H* CALLS TO: W0079R02                                            *
     H*                                                               *
     H* CREATED: 01/16/01 PROGRAMMER: C:61  PROJECT: WC996            *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/10/05 C:61  WC577                                          *
     H*                Do not allow option "2=Change" or "20=Change   *
     H*                status" or "F6=Add" in INQUIRY mode.           *
     H*                                                               *
     H* 05/03/05 C:132 SUPPORT WA114 46526                           *
     H*                Remove delete option, to allow delete dates    *
     H*                to be done on change.                          *
     H*                                                               *
     H* 02/03/05 C:61  SUPPORT WA114 45642                            *
     H*                Add seq# to file key to correct paging issue,  *
     H*                with multiple records with same name.          *
     H*                                                               *
     H* 12/21/04 C:132 PROJECT WA114  45135                           *
     H*                Chg subfile page number to correct not filling *
     H*                display, which allowed adding two more lines   *
     H*                on RollUP key, now fills first screen.         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
      *
     FW0079F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FWMANP     IF   E             DISK
     F                                     RENAME(PAN01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWMANL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAN01:DBRCDF)
     FWMEMP     IF   E             DISK
     FWMFNL     IF   E           K DISK
      *
      *
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
      *
     D***PY SCOPYWW01E
     D*
      *
      *
      *
     D***PY SCOPYWW01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Additional Named Ins-
     D                                     ureds')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0079R02')
     D CHGNUM          C                   CONST('WA003R')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
      ***** BEG CHG ***** 12/21/04 ************************************
     D************  10                    C         SF1PAG
     D SF1PAG          C                   CONST(12)
      ***** END CHG ***** 12/21/04 ************************************
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM      *BLANK        #1KL              1
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKNAME
      ***** BEG ADD ******* 02/03/05 **********************************
     C                   KFLD                    KKSEQ#
      ***** END ADD ******* 02/03/05 **********************************
      *
     C     KEY1A         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKNAME
      ***** BEG ADD ******* 02/03/05 **********************************
     C                   KFLD                    KKSEQ#
      ***** END ADD ******* 02/03/05 **********************************
      *
     C     KEY2          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    ANCO#         KKCO#
     C     *LIKE         DEFINE    ANFND         KKFND
     C     *LIKE         DEFINE    ANEMP#        KKEMP#
     C     *LIKE         DEFINE    ANDIV         KKDIV
     C     *LIKE         DEFINE    ANNAME        KKNAME
      ***** BEG ADD ******* 02/03/05 **********************************
     C     *LIKE         DEFINE    ANSEQ#        KKSEQ#
      ***** END ADD ******* 02/03/05 **********************************
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    SCDBRR        SVDBRR
      *
     C/COPY SCOPYWW01C
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
      *
      *
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
      *
      ***** BEG DEL ***** 05/03/05 ************************************
     C**********           SELEC
      **********
     C********** SCSEL     WHEQ '4'
      **********
     C**N90***** SCDLDT    IFNE *ZERO
     C**********           SETON                     71  90
     C**********           ENDIF
      **********
     C**********           ENDSL
      **********
     C**********           Z-ADDSCDBRR    SVDBRR
      ***** END DEL ***** 05/03/05 ************************************
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
      *
      *
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKNAME
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     SCPST1        KKNAME
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KEYSET
      *
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   MOVEL     SCNAME        KKNAME
      ***** BEG ADD ******* 02/03/05 **********************************
     C                   Z-ADD     SCSEQ#        KKSEQ#
      ***** END ADD ******* 02/03/05 **********************************
      *
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *===============================================================
      * KNDO - SUBROUTINE FOR F14
      *===============================================================
      *
      *  F14=DARTS
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      XXEMP#        QCEMP#
     C                   MOVE      XXDIV         QCDIV
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL(P)  ANNAME        SCNAME
     C                   MOVEL(P)  ANNAME        S1NAME
      ***** BEG ADD ******* 02/03/05 **********************************
     C                   Z-ADD     ANSEQ#        SCSEQ#
      ***** END ADD ******* 02/03/05 **********************************
      *
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   MOVEL(P)  ANFEIN        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCFEIN
      *
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   MOVEL(P)  ANSEQ#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         SCINS#
      *
     C                   Z-ADD     ANEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT            8 0
      *
     C                   Z-ADD     ANDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDLDT            8 0
      *
     C                   MOVE      ANSTAT        SCSTAT
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * OPT20 - Inactivate
      *================================================================
      *
      *
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR                                                  OPT20
      *
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20                          Inactivate
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C     @@RRN         CHAIN     PEM01                              60
      *
      * COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   Z-ADD     EMFND         SCFND
     C                   MOVEL(P)  XXEDN         SCEDN
      *
      * GROUP INFORMATION
      *
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNAM
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21KEY2          READE     DBRCDF                                 95
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21KEY2          READPE    DBRCDF                                 95
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21KEY1A         SETGT     DBRCDF                             96
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21KEY1A         SETLL     DBRCDF                             96
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
      *
     C                   MOVE      *ON           *IN20
     C     SVDBRR        CHAIN     PFRCDF                             60
     C                   EXSR      MVDBSC
      *
     C     #1KL          IFEQ      *ON
     C                   MOVE      *ON           AUTORF
     C                   ENDIF
      *
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
      *
      *
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Column headings
Opt Name                               FEIN#  Ins#   Eff date    Del date  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Chg status            YNN
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
214F14=DARTS                YYN
217F17=Subset               NNN
3
