     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE U0063R - WORK WITH PRE-CERT HISTORY STATUS
      * ***************************************************************
      *                                                               *
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL THE    *
      *                HISTORY RECORDS FOR A SINGLE PRE-CERT.         *
      *                THE HISTORY RECORD MAY BE CHOSEN               *
      *                FOR DISPLAY ONLY.                              *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    U0060R   - WORK WITH PRE-CERTS     (OPT 10)                *
      *  CALLS TO:                                                    *
      *    U0063R01 - PRE/RE-CERT HISTORY STATUS MAINTENANCE          *
      *                                                               *
      *  CREATED:  2/27/98    C:47      PROJECT: 87MCB                *
      * ***************************************************************
      ** REVISIONS:
      *
      *  01/29/01 C:62 PROJECT:MCMNT  BASE ONLY
      *                CHANGED LOGICAL TO HAVE EFFECTIVE DATE BE IN
      *                DESCENDING ORDER.
      *
      *****************************************************************
     FU0063F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FUMCHP     IF   E             DISK
     F                                     RENAME(PCH01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEGIN CHG *** 01/29/01 **************************************
     F**********UMCHL2  IF  E           K        DISK
     FUMCHL3    IF   E           K DISK
     F*** END   CHG *** 01/29/01 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCH01:DBRCDF)
     F*
     F*  Pre-Cert Master
     FUMPCP     IF   E             DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PPC01:PPC0P)
      *  GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HDA             S             76    DIM(1) CTDATA PERRCD(1)              Column Headings L1
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D DATA            S             74    DIM(3)
     D DTA1            S              1    DIM(74)
     D DTA2            S              1    DIM(74)
     D*
     D***PY SCOPYWW01E
     D*
      *
      ***PY SCOPYWW01I
      *
     D                 DS                  INZ
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pre-Cert Status -
     D                                     History')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('U0063R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM      SCDBRR        @@RRN             9 0
     C*
     C     ACDPL         PLIST
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
     C                   PARM      *BLANK        QQOCST            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKEXT
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CHSUB#        KKSUB#
     C     *LIKE         DEFINE    CHFND         KKFND
     C     *LIKE         DEFINE    CHFYR         KKFYR
     C     *LIKE         DEFINE    CHCASE        KKCASE
     C     *LIKE         DEFINE    CHCERT        KKCERT
     C     *LIKE         DEFINE    CHEXT         KKEXT
     C     *LIKE         DEFINE    CHDT          XXDT
     C     *LIKE         DEFINE    CHDT          KKEFDT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL(P)  PCCERT        KKCERT
     C*** BEGIN CHG *** 01/29/01 **************************************
     C**********           Z-ADD*LOVAL    KKEFDT
     C                   Z-ADD     *HIVAL        KKEFDT
     C*** END   CHG *** 01/29/01 **************************************
     C                   Z-ADD     *LOVAL        KKEXT
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *
     C   21              MOVE      SCPST1        SRDTI
     C   21              CALL      'SRCVTDAT'    DTCYMD
     C   21              MOVEL(P)  SRDTO         KKEFDT
     C   21              MOVE(P)   *LOVAL        KKEXT
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKEFDT
     C                   MOVE(P)   SCEXT         KKEXT
     C                   MOVE(P)   SCEXT         KKEXT
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE(P)   CHEXT         SCEXT
      *  STATUS
     C                   MOVEL(P)  'PCST'        XXTBID
     C                   MOVEL(P)  CHOCST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        XAOCSD           20
     C                   ENDIF
      *
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         CYMD --> MDCY
     C                   MOVEL(P)  SRDTO         SCEFDT            8 0
      *
      * EDIT EFFECTIVE DATE
      *
     C                   MOVEL(P)  SCEFDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALEFDT           10
      *
      * ZERO SUPPRESS RE-CERT NUMBER
      *
     C                   MOVEL(P)  CHEXT         XXNUM
     C                   MOVEL(P)  'XXX,XXX'     EDTMSK
     C                   CALL      'SA020R'
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   MOVEL(P)  XXNUM         ALEXT             3
      *
     C                   MOVEL(P)  CHEXPL        XAEXPL           43
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LOAD SUBFILE - VIEW 1
      *
     C                   SELECT
     C     HD#           WHENEQ    1
     C                   Z-ADD     2             Y                 2 0
     C                   MOVEA     ALEFDT        DTA1(Y)
     C                   ADD       13            Y
     C                   MOVEA     CHOCST        DTA1(Y)
     C                   ADD       4             Y
     C                   MOVEA     ALEXT         DTA1(Y)
     C                   ADD       5             Y
     C**                   MOVEAXAOCSD    DTA1,Y
     C**                   ADD  22        Y
     C                   MOVEA     XAEXPL        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C                   MOVEA(P)  HDA(1)        SCHDLA
      *
     C                   OTHER
     C                   MOVEA(P)  HDA(1)        SCHDLA
     C                   ENDSL
      *
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
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
     C                   EVAL      @@KC = '0'
     C                   EVAL      @@KL = '0'
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET INFO FOR HEADING, LOAD KEY FIELDS FOR UMPCP
      *   SUB,GROUP,YEAR,CASE
      *
     C     @@RRN         CHAIN     PPC0P                              99
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL     PCCERT        KKCERT
     C                   Z-ADD     PCEXT         KKEXT
     C                   MOVEL     PCCERT        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C**                   MOVE PCCERT    QQCERT 15
     C**                   MOVE PCEXT     QQEXT   3
      *
      * CALL API TO CREATE A TWO LINE HEADING
      *
     C                   MOVE      PCCO#         QQCO#             3
     C                   MOVE      PCSUB#        QQSUB#            5
     C                   MOVE      PCFND         QQFND             3
     C                   MOVE      PCFYR         QQFYR             3
     C                   MOVE      PCCASE        QQCASE            7
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY           SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY           SETLL     DBRCDF                             96
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
     C                   MOVE      SCDBRR        @@RRN
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
    1 *       2         3         4         5        6        7         8
678901*345679801234567890123465798012345678901234567901234678901234567890
** Column headings

** Column headings
Opt   Eff date   Sts  Ext  Explanation
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  NNN
205F5=Refresh               NNN
206F6=Add                   NNN
211F11=Change view          NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
