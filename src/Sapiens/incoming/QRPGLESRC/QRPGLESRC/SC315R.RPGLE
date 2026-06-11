     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC315R - WORK WITH TABLE BACKUP
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the TABLE BACKUP    **
     H**     file.  This program requires reading all recods of the  **
     H**     file, but only processing into the subfile the records  **
     H**     with UA or UN or UD or UB.  Suset was used to accomplish**
     H**     the breakout.  Four views were used to display the      **
     H**     records by setting on the proper indicators based on the**
     H**     type of field.  This requires four logicals and reading **
     H**     records based on indicators.                            **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU   - Work with Custom Group ID                      **
     H**              Option 7 Work with Custom ID Field             **
     H**              Option 7 Work with Table Back-up               **
     H**   CALLS TO:                                                 **
     H**     SC315F01 - Work With TABLE BACKUP INFORMATION           **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   01/28/98      PROJECT: DR049                   **
     H*****************************************************************
     H* REVISIONS:
     H*
     H*
     H* 02/28/01 C:128 PROJECT: DR108
     H*                Wrong Message file used.
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H**   02/26/99  C:03  SUPP# 16862
     H**             NOT ALL ENTRIES FOR THE UDXX FIELDS ARE ACTUALLY
     H**             DATES.  SOME CLIENT USE THESE FIELDS FOR NUMERIC
     H**             NON DECIMAL ENTRIES.
     H*
     H*  08/05/98  C:94  PROJECT: DR049
     H*                  CORRECT SUBSET SO THAT IT WOULD BRING UP
     H*                  THE CORRECT VALUES WHETHER DONE IN UPPER
     H*                  OR LOWER CASE.
     H*
     H*  06/25/98  C:94  PROJECT: DR049
     H*                  CHANGED SUBSET SO THAT IT WOULD RECOGNIZE
     H*                  UPPER AND LOWER CASE LETTERS.
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      01   Used on the PAGEDOWN keyword
     H**      02   Used on the PAGEUP keyword
     H**      03   Used to condition SFLCLR
     H**     N03   Used to condition SFLDSPCTL
     H**      04   Used to condition SFLDSP
     H**      05   Used to condition DSPATR(RI) on the sel field
     H**     N06   Used to condition DSPATR(PC) on the pos to field
     H**      07   Used to condition DSPATR(PC) on the sel field
     H**      08   Used to condition SFLNXTCHG
     H**      09   Reserved for maintenance/inquiry conditions
     H**     N10   Used to condition SFLEND (on message subfile)
     H**      11   Used to indicate the list is subsetted
     H**      12   Used to condition SFLDROP/SFLFOLD
     H**      13   Used to condition SFLCLR (on confirm delete)
     H**     N13   Used to condition SFLDSPCTL (on confirm delete)
     H**      14   Used to condition SFLDSP (on confirm delete)
     H**      15   Used to condition SFLEND (on confirm delete)
     H**      21   View 1 - UA Sequence
     H**      22   View 2 - UD Sequence
     H**      23   View 3 - UN Sequence
     H**      24   View 4 - UB Sequence
     H**      25 - 29 are reserved for other views
     H**      54   SCM0631 Error - Invalid value (Summary code)
     H**      60   Used for chaining
     H**      75 - 88 are reserved for error screen error indicators
     H**      90   Used as general purpose error indicator
     H**      91   Used to display CPF5203; roll up/down past last rcd
     H**      92   Used to condition SFLMSGID for more than one opt 1
     H**      95   Used to condition SFLEND; also used to indicate EOF
     H**           and BOF on the file being read.
     H**      99   Used for misc purpose; error on calls, on CHAINs
     H**
     H*****************************************************************
     FSC315F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  PHYSical file definition (for refreshing subfile record)
     FSTTBP     IF   E             DISK
     F                                     RENAME(PTB01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL   IF  E           K        DISK
     FSTTBL4    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PTB01:DBRCD1)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL2  IF  E           K        DISK
     FSTTBL6    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PTB01:DBRCD2)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL3  IF  E           K        DISK
     FSTTBL7    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PTB01:DBRCD3)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL1  IF  E           K        DISK
     FSTTBL5    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PTB01:DBRCD4)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********SMCGL   IF  E           K        DISK
     FSMCGL3    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  02/01/01  *********************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Table Backup')
     D SLMSGI          C                   CONST('WC39998')
      *** 02/28/01 Begin Change ***************************************
     D******        'SCMESG'              C         SLMSGF
     D SLMSGF          C                   CONST('WMESGG')
      *** 02/28/01 End   Change ***************************************
     D ACDPGM          C                   CONST('SC315R01')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D*** BEG ADD *** 06/25/98 ************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D*** END ADD *** 06/25/98 ************************************
     D*================================================================
     D*
     D                 DS
     D  DA                     1    100
     D                                     DIM(10)                              UAXX FLD HDGS
     D  CGDA01                 1     10
     D  CGDA02                11     20
     D  CGDA03                21     30
     D  CGDA04                31     40
     D  CGDA05                41     50
     D  CGDA06                51     60
     D  CGDA07                61     70
     D  CGDA08                71     80
     D  CGDA09                81     90
     D  CGDA10                91    100
     D                 DS
     D  DD                     1     50
     D                                     DIM(5)                               UDXX FLD HDGS
     D  CGDD01                 1     10
     D  CGDD02                11     20
     D  CGDD03                21     30
     D  CGDD04                31     40
     D  CGDD05                41     50
     D                 DS
     D  DN                     1     50
     D                                     DIM(5)                               UNXX FLD HDGS
     D  CGDN01                 1     10
     D  CGDN02                11     20
     D  CGDN03                21     30
     D  CGDN04                31     40
     D  CGDN05                41     50
     D*
     D                 DS
     D  DB                     1     60
     D                                     DIM(6)                               UBXX FLD HDGS
     D  CGDB01                 1     10
     D  CGDB02                11     20
     D  CGDB03                21     30
     D  CGDB04                31     40
     D  CGDB05                41     50
     D  CGDB06                51     60
     D*
     D                 DS
     D  XXFLD                  1      4
     D  XXFLDP                 1      2
     D  XXFLDS                 3      4
     D* ---------------------------------------------------------------
     D* SCDATA For VALUES UP TO 10 CHARACTERS
     D* ---------------------------------------------------------------
     D                 DS
     D  X1DATA                 1     74
     D  X1VALU                 3     16
     D  X1DESC                19     58
     D  X1STAT                72     72
     D*
     D* ---------------------------------------------------------------
     D* SCDATA For VALUES OF 35 CHARACTERS
     D* ---------------------------------------------------------------
     D                 DS
     D  X2DATA                 1     74
     D  X2VALU                 3     37
     D  X2DESC                38     67
     D  X2STAT                72     72

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Blank = Maint
     C*                                                    '1' = Inquiry
     C*                                                    '2' = Search
     C                   PARM                    @@CSID            2
     C                   PARM                    @@FLD             4
     C                   PARM                    @@RTCD            1
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM                    @@GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCCSID        @@CSID            2
     C                   PARM      SCFLD         @@FLD             4
     C                   PARM      SCHDGH        @@HDG            35
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM      SCGRP         ##GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
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
     C*   KEY LIST FOR CUSTOM GROUP ID MASTER FILE
     C*
     C     CGKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C*   MASTER KEY LIST FOR W/W
     C     KEY           KLIST
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*   KEY LIST FOR THE TABLE BACK-UP FILE - FOR RECORD FORMAT 'A'
     C     TAKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C                   KFLD                    KKFLD
     C                   KFLD                    KKVUA
     C*
     C*
     C*   KEY LIST FOR THE TABLE BACK-UP FILE - FOR RECORD FORMAT 'B'
     C     TBKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C                   KFLD                    KKFLD
     C                   KFLD                    KKVUB
     C*
     C*   KEY LIST FOR THE TABLE BACK-UP FILE - FOR RECORD FORMAT 'N'
     C     TNKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C                   KFLD                    KKFLD
     C                   KFLD                    KKVUN
     C*
     C*
     C*   KEY LIST FOR THE TABLE BACK-UP FILE - FOR RECORD FORMAT 'D'
     C     TDKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C                   KFLD                    KKFLD
     C                   KFLD                    KKVUD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    TBGRP         KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    TBCSID        KKCSID
     C     *LIKE         DEFINE    TBFLD         KKFLD
     C     *LIKE         DEFINE    TBVUA         KKVUA
     C     *LIKE         DEFINE    TBVUB         KKVUB
     C     *LIKE         DEFINE    TBVUD         KKVUD
     C     *LIKE         DEFINE    TBVUN         KKVUN
     C*** BEG ADD *** 06/25/98 **************************************
     C     *LIKE         DEFINE    TBDESC        U1PDSC
     C     *LIKE         DEFINE    TBDESC        U2PDSC
     C     *LIKE         DEFINE    TBVUA         U1VALU
     C     *LIKE         DEFINE    TBVUA         U2VALU
     C*** END ADD *** 06/25/98 **************************************
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    DRFL11        USEGRP
     C     *LIKE         DEFINE    SCGRP         @@GRP
     C     *LIKE         DEFINE    SCGRP         ##GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C     SSSTAT        IFNE      *BLANKS
     C*** BEG ADD *** 06/25/98 **************************************
     C     SSDESC        ORNE      *BLANKS
     C     SSVALU        ORNE      *BLANKS
     C*** END ADD *** 06/25/98 **************************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF
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
     C*
     C* CHECK ACTIVE/INACTIVE
     C*
     C     SSSTAT        IFNE      'A'
     C     SSSTAT        ANDNE     'I'
     C     SSSTAT        ANDNE     *BLANKS
     C                   SETON                                        9054
     C                   ENDIF
     C*
     C     ERRSTP        ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKCSID
     C                   MOVE      *LOVAL        KKFLD
     C                   MOVE      *LOVAL        KKVUA
     C                   MOVE      *LOVAL        KKVUB
     C                   MOVE      *LOVAL        KKVUD
     C                   MOVE      *LOVAL        KKVUN
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C     XXFLDP        IFEQ      'UA'
     C                   MOVE      *ON           *IN21
     C                   ELSE
     C     XXFLDP        IFEQ      'UD'
     C                   MOVE      *ON           *IN22
     C                   ELSE
     C     XXFLDP        IFEQ      'UN'
     C                   MOVE      *ON           *IN23
     C                   ELSE
     C     XXFLDP        IFEQ      'UB'
     C                   MOVE      *ON           *IN24
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     SCPST1        KKVUA
     C                   MOVE      SCPST2        HDPST2            8 0
     C                   Z-ADD     HDPST2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKVUD
     C** BGN ADD  02/26/99
     C     SRERR         IFEQ      '1'
     C                   MOVEL     HDPST2        KKVUD
     C                   END
     C** END ADD  02/26/99
     C                   MOVE      SCPST3        KKVUN
     C                   MOVEL     SCPST4        KKVUB
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   SUB       1             @RCDS
     C                   MOVEL     SCCSID        KKCSID
     C                   MOVEL     SCFLD         KKFLD
     C                   MOVEL     SCVALA        KKVUA
     C                   MOVEL     SCVALB        KKVUB
     C                   MOVE      SCVALN        KKVUN
     C                   Z-ADD     SCVALD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKVUD
     C** BGN ADD  02/26/99
     C     SRERR         IFEQ      '1'
     C                   MOVEL     SCVALD        KKVUD
     C                   END
     C** END ADD  02/26/99
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C     XXFLDP        CASEQ     'UA'          UASR
     C     XXFLDP        CASEQ     'UD'          UDSR
     C     XXFLDP        CASEQ     'UN'          UNSR
     C     XXFLDP        CASEQ     'UB'          UBSR
     C                   ENDCS
     C   71              MOVEL     X1DATA        SCDATA
     C   72              MOVEL     X2DATA        SCDATA
     C                   ENDSR
     C*================================================================
     C* UASR - UA FIELD SUBROUTINE
     C*================================================================
     C     UASR          BEGSR
     C*** BEG DEL *** 06/25/98 **************************************
     C***                  MOVEL*OFF      *IN71
     C*** END DEL *** 06/25/98 **************************************
     C                   MOVEL     TBDESC        X1DESC
     C                   MOVEL     TBVUA         X1VALU
     C                   MOVEL     TBVUA         SCVALA
     C                   MOVEL     TBSTAT        X1STAT
     C                   MOVEL     *ON           *IN71
     C                   ENDSR
     C*================================================================
     C* UDSR - UD FIELD SUBROUTINE
     C*================================================================
     C     UDSR          BEGSR
     C*** BEG DEL *** 06/25/98 **************************************
     C***                  MOVEL*OFF      *IN71
     C*** END DEL *** 06/25/98 **************************************
     C                   MOVEL     TBDESC        X1DESC
     C                   Z-ADD     TBVUD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1VALU
     C                   MOVE      TBVUD         SCHDGH           35
     C                   Z-ADD     TBVUD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVALD
     C** BGN ADD  02/26/99
     C     SRERR         IFEQ      '1'
     C                   MOVEL     TBVUD         X1VALU
     C                   MOVEL     TBVUD         SCVALD
     C                   END
     C** END ADD  02/26/99
     C                   MOVEL     TBSTAT        X1STAT
     C                   MOVEL     *ON           *IN71
     C                   ENDSR
     C*================================================================
     C* UNSR - UN FIELD SUBROUTINE
     C*================================================================
     C     UNSR          BEGSR
     C*** BEG DEL *** 06/25/98 **************************************
     C***                  MOVEL*OFF      *IN71
     C*** END DEL *** 06/25/98 **************************************
     C                   MOVEL     TBDESC        X1DESC
     C                   MOVEL     TBVUN         X1VALU
     C                   MOVE      TBVUN         SCHDGH
     C                   MOVE      TBVUN         SCVALN
     C                   MOVEL     TBSTAT        X1STAT
     C                   MOVEL     *ON           *IN71
     C                   ENDSR
     C*================================================================
     C* UBSR - UB FIELD SUBROUTINE
     C*================================================================
     C     UBSR          BEGSR
     C*** BEG DEL *** 06/25/98 **************************************
     C***                  MOVEL*OFF      *IN72
     C*** END DEL *** 06/25/98 **************************************
     C                   MOVEL     TBDESC        X2DESC
     C                   MOVEL     TBVUB         X2VALU
     C                   MOVEL     TBVUB         SCHDGH
     C                   MOVEL     TBVUB         SCVALB
     C                   MOVEL     TBSTAT        X2STAT
     C                   MOVEL     *ON           *IN72
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
     C*
     C*          Process option 20 - Change status
     C*          ------------------------------------------------------
     C     SCSEL         IFEQ      '20'
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF                                                  SEL = '20'
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEG ADD *** 06/25/98 ****************************************
     C                   MOVEL     *ON           *INLR
     C*** END ADD *** 06/25/98 ****************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** BEG DEL *** 06/25/98 ****************************************
     C***                  MOVEL*OFF      *IN21
     C*** END DEL *** 06/25/98 ****************************************
     C                   MOVE      @@CSID        KKCSID
     C                   MOVE      @@FLD         KKFLD
     C                   MOVE      @@FLD         XXFLD
     C                   MOVE      @@CSID        XXCSID            2
     C                   MOVEL     @@FLD         SCFLD
     C                   MOVEL     @@CSID        SCCSID
     C                   MOVEL     SCCSID        KKCSID
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   SCGRP
     C*
     C     Q1PRMS        IFGE      5
     C                   MOVE      @@GRP         SCGRP
     C                   ENDIF                                                  Q1PRMS>=5
     C*
     C                   MOVE      SCGRP         KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C     CGKY          CHAIN     PCG01                              60
     C                   MOVEL     CGRFMT        SCFRMT
     C                   MOVEL     CGDESC        SCDESC
     C     XXFLDP        CASEQ     'UA'          UAS2
     C     XXFLDP        CASEQ     'UD'          UDS2
     C     XXFLDP        CASEQ     'UN'          UNS2
     C     XXFLDP        CASEQ     'UB'          UBS2
     C                   ENDCS
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   DRFL11
     C     'SC'          CHAIN     SDR01                              60
     C     DRFL11        COMP      'Y'                                    41
     C                   MOVEL     DRFL11        USEGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* UAS2 - UA FIELD INITIALIZE
     C*================================================================
     C     UAS2          BEGSR
     C                   MOVEL     '10   A'      SCATTR
     C                   MOVE      XXFLDS        W                 2 0
     C                   MOVEL     DA(W)         SCHDG
     C                   MOVEL     TBVUA         SCVALA
     C                   MOVEL     TBVUA         SCHDGH
     C                   MOVEL     SCVALA        KKVUA
     C                   MOVEL     *ON           *IN21
     C                   ENDSR
     C*================================================================
     C* UDS2 - UD FIELD INITIALIZE
     C*================================================================
     C     UDS2          BEGSR
     C                   MOVEL     ' 8/0 N'      SCATTR
     C                   MOVE      XXFLDS        W
     C                   MOVEL     DD(W)         SCHDG
     C                   Z-ADD     TBVUD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVALD
     C                   MOVE      SCVALB        KKVUB
     C** BGN ADD  02/26/99
     C     SRERR         IFEQ      '1'
     C                   MOVE      TBVUD         SCVALD
     C                   END
     C** END ADD  02/26/99
     C                   MOVEL     *ON           *IN22
     C                   ADD       1             VIEW#
     C                   ENDSR
     C*================================================================
     C* UNS2 - UN FIELD INITIALIZE
     C*================================================================
     C     UNS2          BEGSR
     C                   MOVEL     ' 9/2 N'      SCATTR
     C                   MOVE      XXFLDS        W
     C                   MOVEL     DN(W)         SCHDG
     C                   MOVE      TBVUN         SCVALN
     C                   MOVE      SCVALN        KKVUN
     C                   MOVEL     *ON           *IN23
     C                   ADD       2             VIEW#
     C                   ENDSR
     C*================================================================
     C* UBS2 - UB FIELD SUBROUTINE
     C*================================================================
     C     UBS2          BEGSR
     C                   MOVEL     '35   A'      SCATTR
     C                   MOVE      XXFLDS        W
     C                   MOVEL     DB(W)         SCHDG
     C                   MOVEL     TBVUB         SCVALB
     C                   MOVEL     SCVALB        KKVUB
     C                   MOVEL     *ON           *IN24
     C                   ADD       3             VIEW#
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C***21*****           READ DBRCD1                   95
     C***22*****           READ DBRCD2                   95
     C***23*****           READ DBRCD3                   95
     C***24*****           READ DBRCD4                   95
     C   21KEY           READE     DBRCD1                                 95
     C   22KEY           READE     DBRCD2                                 95
     C   23KEY           READE     DBRCD3                                 95
     C   24KEY           READE     DBRCD4                                 95
     C***  END   CHG  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C***21*****           READPDBRCD1                   95
     C***22*****           READPDBRCD2                   95
     C***23*****           READPDBRCD3                   95
     C***24*****           READPDBRCD4                   95
     C   21KEY           READPE    DBRCD1                                 95
     C   22KEY           READPE    DBRCD2                                 95
     C   23KEY           READPE    DBRCD3                                 95
     C   24KEY           READPE    DBRCD4                                 95
     C***  END   CHG  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21TAKY          SETGT     DBRCD1                             96
     C   22TDKY          SETGT     DBRCD2                             96
     C   23TNKY          SETGT     DBRCD3                             96
     C   24TBKY          SETGT     DBRCD4                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21TAKY          SETLL     DBRCD1                             96
     C   22TDKY          SETLL     DBRCD2                             96
     C   23TNKY          SETLL     DBRCD3                             96
     C   24TBKY          SETLL     DBRCD4                             96
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     XXCSID        IFNE      TBCSID
     C     XXFLD         ORNE      TBFLD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C*** BEG ADD *** 06/25/98 **************************************
     C     SSDESC        IFNE      *BLANKS
     C     LO:HI         XLATE(P)  TBDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES = 0
     C                   ENDIF                                                  SCDESC NE BL
     C*
     C     SSVALU        IFNE      *BLANKS
     C*** BEG CHG *** 08/05/98 *************************************
     C********** LO:HI     XLATETBVUA     U1VALU    P
     C     LO:HI         XLATE(P)  TBVUB         U1VALU
     C*** END CHG *** 08/05/98 *************************************
     C     LO:HI         XLATE(P)  SSVALU        U2VALU
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1VALU
     C                   PARM      10            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2VALU
     C                   PARM      10            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES = 0
     C                   ENDIF                                                  SCDESC NE BL
     C*** END ADD *** 06/25/98 **************************************
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     TBSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Value           Description                                       Status
Opt  Value           Description                                       Status
Opt  Value           Description                                       Status
Opt  Value                              Description                    Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYN
12020=Change Status         YNN
203F3=Exit                  YYY
205F5=Refresh               YNN
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YNN
3
