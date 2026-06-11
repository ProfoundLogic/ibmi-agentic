     H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2001-2008
     H*****************************************************************
    H/TITLE MERGE MSC TN PHARMACY DATA
    H*
    H*  Program ID:   CVTFXXMP
    H*
    H*  CALLED FROM:  COMMAND LINE
    H*
    H*  CALLS TO:     NONE
    H*
    H*  CREATED: 05/02/08      C:180     PROJECT: 10MLN
     H*
     H*
     H*****************************************************************
     H*  REVISIONS
     H*
     H*
     FFXXMP     IF   E             DISK
     FFXXPL2    IF   E           K DISK    RENAME(PXP01:PXPL2)
     FSMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FFMCMLW    IF   E           K DISK
     FFXXPP     O    E             DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
      *
     D IREC            DS
     D    XMDATA
     D      XMCASE                   26    OVERLAY(XMDATA:35)
     D      XMBLID                   21    OVERLAY(XMDATA:154)
     D      XMFDAT                   14    OVERLAY(XMDATA:214)
     D      XMPAID                   11    OVERLAY(XMDATA:228)
     D      XMNDC                    14    OVERLAY(XMDATA:327)
     D      XMCRDT                   11    OVERLAY(XMDATA:341)
     D      XMPODT                    8    OVERLAY(XMDATA:352)
      *
     D MYPSDS         SDS
     D PROC_NAME         *PROC
     D USER                  254    263
      *
      * STAND ALONE
      *
     D XXBLID          S                   LIKE(XPBLID) INZ
     D XXNDC           S                   LIKE(XPNDC) INZ
     D XXFDAT          S                   LIKE(XPFDAT) INZ
     D XXPAID          S                   LIKE(XPPAID) INZ
     D XXCLM#          S                   LIKE(XPCLM#) INZ
     D CASE#           S              7  0 INZ
     D CASE7A          S              7A   INZ
     D TIME60          S              6  0 INZ
     D FLD40A          S             40A   INZ
     D FLD60A          S             60A   INZ
     D OUT30A          S             30A   INZ
     D CO3A            S              3A   INZ
     D SUB5A           S              5A   INZ
     D FND3A           S              3A   INZ
     D FYR3A           S              3A   INZ
     D FileCode        S              1    INZ
     d ERRCNT          S              5  0 INZ
     D NUM             S              5  0 INZ
      *
      * CONSTANTS
     D DFTITL          C                   CONST('MERGE MSC TN PHARMACY -
     D                                     DATA ERROR REPORT')
     D MSG1            C                   CONST('**** NO ERRORS FOUND *-
     D                                     **')
     D MSG2            C                   CONST('**** RECORD ALREADY EX-
     D                                     ISTS.')
     D MSG3            C                   CONST('**** CASE # NOT FOUND -
     D                                     ****.')
     D MSG4            C                   CONST('*** INVALID CASE# LENG-
     D                                     TH')
     D/COPY SPRSTRING
      *
      *
     C     KYfmcmlw      KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    CASE#
      *
     C     KYsmorl       KLIST
     C                   KFLD                    PROC_NAME
     C                   KFLD                    FileCode
      *
     C     KYfxxpl2      KLIST
     C                   KFLD                    XXCLM#
     C                   KFLD                    XXBLID
     C                   KFLD                    XXNDC
     C                   KFLD                    XXFDAT
     C                   KFLD                    XXPAID
      *
      * Reading MSC file
      *
     C                   READ      FXXMP
      *
     C                   DOW       NOT %EOF(FXXMP)
     C     ' '           CHECKR(E) XMCASE        NUM
      *
     C                   IF        %FOUND AND NUM = 6
     C                   EVAL      CASE7A = '0' + %SUBST(XMCASE:1:6)
     C                   MOVE      CASE7A        CASE#
      *
      * Check if claim exists
      *
     C     KYfmcmlw      CHAIN     FMCMLW
      *
     C                   IF        %FOUND(FMCMLW)
     C                   EXSR      PROCESS
     C                   ELSE
     C                   MOVEL     MSG3          OUT30A
     C                   EXSR      ERROR
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVEL     MSG4          OUT30A
     C                   EXSR      ERROR
     C                   ENDIF
      *
     C                   READ      FXXMP
     C                   ENDDO
      *
     C                   EVAL      *INLR = *ON
     C*---------------------------------------------------------------------
      * PROCESS SUBROUTINE
     C*---------------------------------------------------------------------
     C     PROCESS       BEGSR
     C                   EXSR      BLDKEY
      *
      * Check if bill exists.
      *
     C     KYfxxpl2      CHAIN     FXXPL2
      *
     C                   IF        %FOUND(FXXPL2)
     C                   MOVEL     MSG2          OUT30A
     C                   EXSR      ERROR
     C                   ELSE
     C                   EXSR      WRTREC
     C                   ENDIF
     C                   ENDSR
     C*---------------------------------------------------------------------
      * BUILD KEY FOR FILE FXXPL2
     C*---------------------------------------------------------------------
     C     BLDKEY        BEGSR
     C                   MOVE      CMCO#         CO3A
     C                   MOVE      CMSUB#        SUB5A
     C                   MOVE      CMFND         FND3A
     C                   MOVE      CMFYR         FYR3A
     C                   EVAL      XXCLM# = CO3A  + FND3A +
     C                                      FYR3A + CASE7A
      *
     C                   MOVE      *BLANKS       XXBLID
     C                   EVAL      XXBLID = %SUBST(XMBLID:1:17)
      *
     C                   MOVE      *BLANKS       XXNDC
     C                   EVAL      XXNDC = %SUBST(XMNDC:1:13)
      *
     C                   MOVE      *BLANKS       XXFDAT
     C                   EVAL      XXFDAT = %SUBST(XMFDAT:1:8)
      *
     C                   MOVE      *BLANKS       XXPAID
     C                   EVAL      XXPAID = %SUBST(XMPAID:1:8)
     C                   ENDSR
     C*---------------------------------------------------------------------
      * WRITE RECORD SUBROUTINE
     C*---------------------------------------------------------------------
     C     WRTREC        BEGSR
     C                   CLEAR                   PXP01
     C                   MOVEL     XXCLM#        XPCLM#
     C                   MOVEL     XMBLID        XPBLID
     C                   MOVEL     XMNDC         XPNDC
     C                   MOVEL     XMFDAT        XPFDAT
     C                   MOVEL     XMPAID        XPPAID
     C                   MOVEL     XMCRDT        XPCRDT
     C                   MOVE      XMPODT        XPPODT
     C                   MOVE      *BLANKS       XPSTAT
     C                   WRITE     PXP01
     C                   ENDSR
     C*---------------------------------------------------------------------
      * ERROR SUBROUTINE
     C*---------------------------------------------------------------------
     C     ERROR         BEGSR
     C                   EVAL      ERRCNT = ERRCNT + 1
     C                   IF        *INOF
     C                   EXCEPT    HDG
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ENDSR
     C*---------------------------------------------------------------------
      * INITIALIZATION
     C*---------------------------------------------------------------------
     C     *INZSR        BEGSR
      *
      * Get Report Title
      *
     C     KYsmorl       CHAIN     SMORL
     C                   IF        %FOUND
     C                   ELSE
     C                   MOVE      *BLANKS       ORTITL
     C                   MOVE      DFTITL        ORTITL
     C                   ENDIF
      *
     C                   EVAL      ORTITL = CenterFld(ORTITL)
      *
      * Get Company/Subscriber codes
      *
     C     USER          CHAIN     SMUPL
     C                   IF        %FOUND(SMUPL)
      *
      * Get Company Name
      *
     C     UPCO#         CHAIN     SMCOL
     C                   IF        %FOUND(SMCOL)
     C                   TIME                    TIME60
     C                   EVAL      CONAME = CenterFld(CONAME)
     C                   EXCEPT    HDG
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
     OQSYSPRT   E            HDG            1 02
     O                       TIME60                 '  :  :  '
     O                       CONAME              90
     O                       UDATE         Y    132
     O          E            HDG            2
     O                       PROC_NAME
     O                       ORTITL             100
     O                                          125 'PAGE:'
     O                       PAGE          Z  +0001
     O          E            HDG            1
     O                                           54 'DATE OF'
     O                                           85 'AMOUNT'
     O          E            HDG            1
     O                                              'CASE #'
     O                                           36 'BILL ID'
     O                                           54 'SERVICE'
     O                                           71 'NDC CODE'
     O                                           83 'PAID'
     O                                          107 'ERROR MESSAGE'
     O          E            DET            1
     O                       XMCASE
     O                       XMBLID              50
     O                       XMFDAT              61
     O                       XMNDC               77
     O                       XMPAID              90
     O                       OUT30A         B   124
