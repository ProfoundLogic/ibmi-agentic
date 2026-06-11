     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W1555R - WORK WITH APPROVED STATES
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0051R - Work with Employer Policy Years           *
     H*                                                               *
     H* CALLS TO:  W1555R02 Approved States Info Maint                *
     H*            SY601R - Search for valid state code               *
     H*                                                               *
     H* CREATED:                                                      *
     H*                                                               *
     H* 11/01/07 C:184 PROJECT WC1530                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
      * 12/12/19 C:291 Project: WC2693                                *
      *                Add Expiration Date to grid.                   *
      *                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
      *
      *
     FW1555F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FWTESP     IF   E             DISK
     F                                     RENAME(PES01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWTESL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PES01:DBRCDF)
      *
      *  State
     FSTSTL1    IF   E           K DISK
      *
      *  ENDORSEMENT TABLE
     FWTENP     IF   E             DISK
      *
      *
      *
      *** Beg Chg ***** 12/12/19 ******************************
      **/COPY SCOPYWW01D
      /copy SCOPYWW41D
      /copy sprdate
      /copy sPrMessage
      /copy sPrRunCmd
      /copy sPrString
      /copy sPrWrkWth
      *** End Chg ***** 12/12/19 ******************************
     D HD              S             90    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Approved/Excluded St-
     D                                     ates')
     D APPROV          C                   CONST('Approved')
     D EXCLUD          C                   CONST('Excluded')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1555R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          WTESP (Return)
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN1            9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      *  Approved States
      *
     C     KEY1          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFORM
      *
     C     KEY2          KLIST
     C                   KFLD                    ENCO#
     C                   KFLD                    ENFORM
     C                   KFLD                    KKST
      *
      *  State
      *
     C     STKY          KLIST
     C                   KFLD                    KKKEY
      *
      *  EMPLOYER MASTER
      *
     C*          EMKY      KLIST
     C*                    KFLD           ELCO#
     C*                    KFLD           ELFND
     C*                    KFLD           ELEMP#
     C*                    KFLD           ELDIV
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    ESCO#         KKCO#
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    ESST          KKST
      *
      *** Beg Chg ***** 12/12/19 ******************************
      **/COPY SCOPYWW01C
     c/copy SCOPYWW41C
      *** End Chg ***** 12/12/19 ******************************
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CKSSM
      *
      *
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
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
      *
     C                   MOVE      *LOVAL        KKST
      *
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C   21              MOVE      SCPST1        KKST
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
     C                   MOVE      SCST          KKST
      *
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE      ESST          SCST
     C                   MOVEL(P)  ESST          SCDATA
     C     ESAPEX        IFEQ      'A'
     C                   MOVEL     APPROV        SCAPEX
     C                   ENDIF
     C     ESAPEX        IFEQ      'E'
     C                   MOVEL     EXCLUD        SCAPEX
     C                   ENDIF
     C                   MOVEL     ESST          KKKEY
     C     STKY          CHAIN     PST01                              60
     C  N60              CAT(P)    STNAME:2      SCDATA

      //*** Beg Add ***** 12/12/19 ******************************
      /Free
         Clear scExpDt;
         If ESExDt <> 0;
            scExpDt = CvtToDate(ESExDt);
         Endif;
      /End-Free
      //*** End Add ***** 12/12/19 ******************************
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
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
      * Get record
      *
     C*
     C     @@RRN         CHAIN     PEN01                              60
     C*
     C   60              SETON                                        LR
     C   60              RETURN
      *
     C                   Z-ADD     ENCO#         KKCO#
     C*
     C     ENFORM        CAT       ENDESC:2      SCFORD
     C                   MOVE      ENFND         SCFND
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
      *
     C   21KEY1          READE     DBRCDF                                 95
      *
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
      *
     C   21KEY1          READPE    DBRCDF                                 95
      *
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
      *
     C   21KEY2          SETGT     DBRCDF                             96
      *
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
      *
     C   21KEY2          SETLL     DBRCDF                             96
      *
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
      *
     C                   MOVEL(P)  DELETD        SCDATA
      *
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C                   MOVE      SCDBRR        @@RRN
      *
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
Opt  State                   Approved/Excluded     Expiration Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
