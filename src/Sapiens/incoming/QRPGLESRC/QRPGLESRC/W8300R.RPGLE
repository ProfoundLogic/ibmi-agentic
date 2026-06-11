     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8300R - BUILD EMPLOYER PAYROLL WORK FILE
     H*************************************************************
     H** PROGRAM DESCRIPTION:
     H**   THIS PROGRAM GENERATES A WORK FILE FOR SUBMITTING BSI-5
     H**   STATE REPORTS.  BSI-5'S SHOW AUDITED PAYROLL AMOUNTS AND
     H**   RATES FOR EACH ACTIVE PAYROLL CLASS FOR AND EMPLOYER WITHIN
     H**   THE ENTERED FYR.
     H**   CALLED FROM:
     H**         W8300C
     H**   CALLS TO:
     H**         NONE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/20/04  C:33   Seq#: WA072/43732                            *
     H*                  Ignore flat cx div's when getting the        *
     H*                   earliest policy effective date.             *
     H*                                                               *
     H* 11/09/98  C:35   PROJECT: SUPPORT OP090 14971                 *
     H*                  USE EARLIEST POLICY EFFECTIVE DATE ACROSS    *
     H*                  ALL DIVISIONS.                               *
     H*                                                               *
     H* 05/19/98  C:35 PROJECT: SUPP                                  *
     H*                DO NOT REPORT FLAT CANCELLED POLICIES.         *
     H*                                                               *
     H* 04/30/98  C:35  PROJECT: SUPPORT                              *
     H*                 ADD CODE TO PREVENT POLICY TO DATE FROM BEING *
     H*                 LESS THAN POLICY FROM DATE.                   *
     H*                                                               *
     H* 04/09/98  C:35  PROJECT: SUPPORT                              *
     H*                 RANGE TO DATE WAS NOT BEING INITIALIZED.      *
     H*                                                               *
     H*****************************************************************
     FWXB5P     UF A E           K DISK
     FWMEPL1    IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMPRL1    IF   E           K DISK

     D*
     D*  THE FOLLOWING ARRAYS ARE FOR TO SPECIFY RATE CHANGES AND
     D*  PAYROLL EFFECTIVE DATES
     D*  PDAT - DATES IN WHICH PAYROLLS TAKE ON DIFFERENT RATING
     D*         THERE CAN BE UP TO 3 DIFF RATING PERIODS
     D*  RDAT - CORRESPONDING RATES IN EFFECT FOR PAYROLLS AS OF PDAT
     D*
     D PDAT            S              7  0 DIM(4) ASCEND                        PAYROLL DATES
     D RDAT            S              7  0 DIM(4) ASCEND                        RATING DATES
     D SMD             S              4  3 DIM(4)                               Split Modifier
     D SMDT            S              6  0 DIM(4)                               Split Mod Date
      *
      *  Experience Modifier fields assigned to array
     D                 DS
     D  XMD                    1     15  4
     D                                     DIM(3)                               Exp Modifier
     D  ELEXM1                 1      5  4
     D  ELEXM2                 6     10  4
     D  ELEXM3                11     15  4
      *  Exp Mod Effective Date fields assigned to array
     D                 DS
     D  XDT                    1    021  0
     D                                     DIM(3)                               Exp Mod Eff. DT
     D  ELEMD1               001    007  0
     D  ELEMD2               008    014  0
     D  ELEMD3               015    021  0
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXQQMY                 1      6  0
     D  XXQQMM                 1      2  0
     D  XXQQYY                 3      6  0
     D                 DS
     D  XXDATE                 1      8  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D  XXYY                   5      8  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE EMP DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
      **** BEGIN ADD **** 11/09/98 *********************************************
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      **** END   ADD **** 11/09/98 *********************************************
     C*
     C*   KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C*
     C*  BSI-5 WORKFILE
     C*
     C     B5KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    XXPFDT
     C     B5KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    XXEFDT            7 0
     C                   KFLD                    EPCLAS
     C*
     C*  KEY LISTS FOR THE PAYROLL CLASS CODE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C*
     C     *LIKE         DEFINE    ELPTDT        XXPFDT
     C     *LIKE         DEFINE    PRCLAS        XXCLAS
     C     *LIKE         DEFINE    PRRTCD        XXRTCD
     C     *LIKE         DEFINE    ELPFDT        XXRFDT
     C     *LIKE         DEFINE    ELPFDT        XXRTDT
      **** BEGIN ADD **** 11/09/98 *********************************************
     C     *LIKE         DEFINE    ELEMP#        XXEMPH
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIVH
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELPFDT        XXPFDH
      **** END   ADD **** 11/09/98 *********************************************
      *
     C                   Z-ADD     0             XXPDAT            8 0
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRPMY            6
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C     QQRPMY        IFNE      '000000'
     C*
     C                   MOVE      QQRPMY        XXQQMY
     C                   Z-ADD     XXQQMM        XXMM
     C                   Z-ADD     XXQQYY        XXYY
     C                   Z-ADD     1             XXDD
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRFDT
     C*
     C                   Z-ADD     XXQQMM        XXMM
     C                   Z-ADD     XXQQYY        XXYY
     C     XXMM          IFEQ      4
     C     XXMM          OREQ      6
     C     XXMM          OREQ      9
     C     XXMM          OREQ      11
     C                   Z-ADD     30            XXDD
     C                   ELSE
     C     XXMM          IFEQ      2
     C                   Z-ADD     29            XXDD
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRTDT
     C*
     C     SRERR         IFEQ      *ON
     C                   Z-ADD     28            XXDD
     C                   ENDIF                                                  SRERR  IFEQ
     C                   ELSE
     C                   Z-ADD     31            XXDD
     C                   ENDIF                                                  XXMM   IFNE
     C                   ENDIF                                                  XXMM   IFNE
      **** BEGIN ADD **** 04/09/98 *********************************************
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRTDT
      **** END   ADD **** 04/09/98 *********************************************
     C                   ELSE
     C*
     C                   Z-ADD     0             XXRFDT
     C                   Z-ADD     9999999       XXRTDT
     C*
     C                   ENDIF                                                  QQRPMY IFNE
     C*
     C                   TIME                    DSDTTM
     C*
     C*   GET THE FUND DETAIL FILE
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Initialize Arrays based on Employer Detail Fields
     C*
     C                   MOVEA     *ZERO         XMD
     C                   MOVEA     *ZERO         XDT
     C*
     C*   START READING EMPLOYER DETAIL FILE
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
      **** BEGIN ADD **** 11/09/98 *********************************************
      *
      * FIND THE EARLIEST EFFECTIVE DATE FOR THIS EMPLOYER/YEAR
      *
     C                   Z-ADD     ELEMP#        XXEMPH
     C                   Z-ADD     ELDIV         XXDIVH
     C                   Z-ADD     ELPFDT        XXPFDH
      *
     C***  Begin Add  ***  10/20/04  **************************************
     C     ELPTDT        IFEQ      ELPFDT
     C                   Z-ADD     *HIVAL        XXPFDH
     C                   ENDIF
     C***  End   Add  ***  10/20/04  **************************************
      *
     C     ELKEY         READE     PEL01                                  60
      *
     C     *IN60         DOWEQ     *OFF
T    C     ELEMP#        ANDEQ     XXEMPH
      *
     C     ELPFDT        IFLT      XXPFDH
     C***  Begin Add  ***  10/20/04  **************************************
     C     ELPTDT        ANDNE     ELPFDT
     C***  End   Add  ***  10/20/04  **************************************
     C                   Z-ADD     ELDIV         XXDIVH
     C***  Begin Add  ***  10/20/04  **************************************
     C                   Z-ADD     ELPFDT        XXPFDH
     C***  End   Add  ***  10/20/04  **************************************
     C                   ENDIF                                                  ELPFDT IFLT XXPFDH
      *
     C     ELKEY         READE     PEL01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   Z-ADD     XXEMPH        XXEMP#
     C                   Z-ADD     XXDIVH        XXDIV
      *
     C     ELKY2         CHAIN     PEL01                              60
      **** END   ADD **** 11/09/98 *********************************************
     C*
     C     ELFAF         IFEQ      'A'
     C     ELPFDT        ANDGE     XXRFDT
     C     ELPFDT        ANDLE     XXRTDT
      **** BEGIN ADD **** 05/19/98 ************************************
     C     ELPTDT        ANDGT     ELPFDT
      **** END   ADD **** 05/19/98 ************************************
     C*
     C*  CLEAR PAYROLL ARRAYS
     C*
     C                   Z-ADD     0             PDAT
     C                   Z-ADD     0             RDAT
     C                   Z-ADD     *ZERO         SMD
     C                   Z-ADD     *ZERO         SMDT
     C*
     C*   LOAD POLICY FROM DATE INTO ARRAY AS 1ST PAYROLL DATE
     C*
     C                   Z-ADD     ELPFDT        PDAT(1)
     C                   Z-ADD     ELPFDT        RDAT(1)
     C*
     C*   USE ANNIVERSARY DATE TO RETRIEVE RATE IF NOT = 0
     C*
     C                   Z-ADD     0             XXANDT
     C     ELANDT        COMP      0                                      78
     C  N78ELANDT        COMP      ELPFDT                                 78
     C  N78              Z-ADD     ELANDT        XXANDT
     C  N78              ADD       1             XXANYR
     C  N78XXANDT        COMP      ELPFDT                               7878
     C*
     C*  IF ARD IS NOT WITHIN POLICY PERIOD, DO NOT SPLIT PAYROLLS
     C*  BUT GET RATE EFFECTIVE AS OF ARD
     C*
     C  N78              Z-ADD     ELANDT        RDAT(1)
     C     XXANDT        IFGE      F1ENPO
     C     XXANDT        ORLE      ELPFDT
     C                   MOVE      '1'           *IN78
     C                   END
     C*
     C*  LOAD PAYROLL DATE & RATE ARRAYS
     C*
     C                   Z-ADD     2             X
     C     *IN78         IFEQ      '0'
     C*
     C     XXANDT        IFLT      ELPTDT
     C                   Z-ADD     XXANDT        PDAT(X)
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ADD       1             X
     C                   END
     C*
     C                   END                                                      *IN78='0'
     C*
     C                   Z-ADD     ELPTDT        PDAT(X)
     C     X             SUB       1             ZMAX              3 0
     C*
     C*  Assign Experience Modifier values to the Period array.
     C*
     C     XDT(1)        IFGT      *ZERO
     C*
     C*  Find 'Highest Index' of Modifier Changes
     C*
     C                   SELECT
     C     XDT(3)        WHENGT    *ZERO
     C                   Z-ADD     3             K                 3 0
     C     XDT(2)        WHENGT    *ZERO
     C                   Z-ADD     2             K                 3 0
     C     XDT(1)        WHENGT    *ZERO
     C                   Z-ADD     1             K                 3 0
     C                   ENDSL
     C*
     C                   Z-ADD     ZMAX          J                 3 0
     C*
     C*  Process the Period array from the last index backwards
     C*  inserting Modifiers.
     C*
     C     LOOP1         TAG
     C*
     C                   MOVE      *OFF          *IN60
     C     *IN60         DOWEQ     *OFF
     C*
     C     XDT(K)        IFGT      PDAT(J)
     C                   SUB       1             K
     C*
     C     K             IFEQ      *ZERO
     C                   Z-ADD     1             K
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   ENDDO
     C*
     C                   Z-ADD     XDT(K)        SMDT(J)
     C                   Z-ADD     XMD(K)        SMD(J)
     C*
     C                   SUB       1             J
     C     J             IFGT      *ZERO
     C                   GOTO      LOOP1
     C                   ENDIF
     C*
     C                   ENDIF                                                  XDT > 0
     C*
     C*   START READING EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   MOVE DATA IF ACTIVE
     C*
     C     EPSTAT        IFEQ      'A'
     C*
     C*  MOVE DATA IF EPSAFL = 'A'                         LJQ 8-10-88
     C*                                                          "
     C     EPSAFL        IFEQ      'A'                                                "
     C                   MOVE      ELRTCD        XXRTCD
     C                   Z-ADD     EPCLAS        XXCLAS
     C                   Z-ADD     ELPTDT        XXEFDT
      *
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     PRPCF         IFEQ      'C'
     C                   MULT      10            EPAAPR
     C                   ENDIF                                                  PRPCF IFEQ
      *
     C                   ENDIF                                                  *IN60 IFEQ
     C*                                                          "
     C*  IF A BSI-5 RECORD ALREADY EXISTS FOR THIS CLASS CODE, ADD TO
     C*  THE AUDITED PAYROLLS, OTHERWISE WRITE A NEW RECORD.
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C  N60              GOTO      WRITE
     C                   SUB       1             X
     C                   Z-ADD     PDAT(X)       XXEFDT
     C*
     C     B5KY1         CHAIN     PB501                              60
     C     *IN60         IFEQ      '0'
     C                   ADD       EPAAPR        B5AAPR
     C                   ADD       EPAMPR        B5AMPR
     C                   UPDATE    PB501
     C*
     C                   ELSE
     C     WRITE         TAG
     C*
     C                   Z-ADD     XXCO#         B5CO#
     C                   Z-ADD     XXFND         B5FND
     C                   Z-ADD     XXFYR         B5FYR
     C                   Z-ADD     EPEMP#        B5EMP#
     C                   Z-ADD     EPCLAS        B5CLAS
     C*
     C     EPSPEC        IFEQ      'S'
     C                   MOVE      ' '           B5SPEC
     C                   ELSE
     C                   MOVE      EPSPEC        B5SPEC
     C                   END
     C*
     C                   Z-ADD     EPAAPR        B5AAPR
     C*
     C                   Z-ADD     EPARAT        B5ARAT
     C*
     C*  LOAD EFFECTIVE DATE OF PAYROLL, RATING DATE, AND RATING END
     C*  DATE
     C*
     C                   Z-ADD     ELPFDT        B5EFDT
     C                   Z-ADD     ELPFDT        B5RDAT
     C                   Z-ADD     ELPTDT        B5TODT
     C*
     C                   Z-ADD     1             X                 2 0
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C     *IN60         IFEQ      '1'
     C     X             SUB       1             Y                 2 0
     C                   Z-ADD     PDAT(Y)       B5EFDT
     C                   Z-ADD     RDAT(Y)       B5RDAT
     C                   Z-ADD     RDAT(Y)       B5EXDT
     C                   Z-ADD     SMD(Y)        B5EXMD
     C*
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPDAT
     C                   MOVE      XXPDAT        SRDTE
     C*
     C*  (CANCELLED ACCOUNTS ALSO WILL BE SUBT BY 1)
     C*
     C                   Z-ADD     1             SRPST             5 0
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE             8 0
     C                   PARM                    SRPST
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C                   MOVE      SRDTE         XXTODT            8 0
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B5TODT
     C                   END
      **** BEGIN ADD **** 04/30/98 *********************************************
     C     B5TODT        IFLT      B5EFDT
     C                   Z-ADD     B5EFDT        B5TODT
     C                   ENDIF                                                  B5TODT IFLT B5EFDT
      **** END   ADD **** 04/30/98 *********************************************
     C                   MOVE      EPAMPR        B5AMPR
     C*
     C*   WRITE RECORD
     C*
     C                   WRITE     PB501
     C*
     C                   END                                                    CHAIN WXB5P
     C                   END                                                    LJQ 8-10-88
     C*                                                    EPSAFL LOOP
     C*
     C                   END                                                    STATUS LOOP
     C*
     C     EPKEY         READE     PEP01                                  60
     C                   END
     C*
     C*   CHECK TO MAKE SURE ALL PERIODS ARE WRITTEN
     C*
     C                   Z-ADD     2             Y
     C     Y             DOWLE     4
     C     PDAT(Y)       ANDNE     0
     C     Y             SUB       1             X
     C                   Z-ADD     PDAT(X)       XXPFDT
     C     B5KY          CHAIN     PB501                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     XXCO#         B5CO#
     C                   Z-ADD     XXFND         B5FND
     C                   Z-ADD     XXFYR         B5FYR
     C                   Z-ADD     ELEMP#        B5EMP#
     C                   Z-ADD     0             B5CLAS
     C                   MOVE      *BLANK        B5SPEC
     C                   Z-ADD     0             B5AAPR
     C                   Z-ADD     0             B5ARAT
     C                   Z-ADD     PDAT(X)       B5EFDT
     C                   Z-ADD     RDAT(X)       B5RDAT
     C*
     C                   Z-ADD     PDAT(Y)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPDAT
     C                   MOVE      XXPDAT        SRDTE
     C*
     C                   Z-ADD     1             SRPST             5 0
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE             8 0
     C                   PARM                    SRPST
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C                   MOVE      SRDTE         XXTODT            8 0
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         B5TODT
     C*
     C     B5TODT        IFLT      B5EFDT
     C                   Z-ADD     B5EFDT        B5TODT
     C                   END
     C                   Z-ADD     0             B5AMPR
     C                   WRITE     PB501
     C                   END                                                    B5KY = 1
     C                   ADD       1             Y
     C                   END                                                    DOWHILE
     C*
     C                   END                                                    ELFAF = A
     C*
     C*
     C*  Initialize Arrays based on Employer Detail Fields
     C*
     C                   MOVEA     *ZERO         XMD
     C                   MOVEA     *ZERO         XDT
     C     ELKY1         SETGT     PEL01
     C     ELKEY         READE     PEL01                                  60
     C                   END
     C*
     C                   END                                                    F1KEY
     C*
     C                   MOVE      '1'           *INLR
     C*
