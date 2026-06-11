     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0051R26 - CALL Work With Waiver Subrogation - DLT
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R1  - EMPLOYER MID TERM ENDORSEMENTS                  *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0815R   - Work With Waiver of Subrogation                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 12/14/09 C:190 SEQ#70250
     H*                Add processing to delete the WC000313,
     H*                End # 0 record if it is the last Waiver that
     H*                that is being deleted on the policy.
     H*
     H* 04/26/06 C:61  PROJECT 132WFI
     H*                Created.
     H*
     H*****************************************************************
      *
      *** BEG ADD ***** 12/14/09*************************************
     F** Employer Detail File
     FWDELP     IF   E             DISK
     F*
     FWMMUL     UF   E           K DISK
     F*
     FWMWVL1    IF   E           K DISK
      *** END ADD ***** 12/14/09*************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *================================================================
      *  PLIST definitions
      *================================================================
      *

     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0          PEM01 RRN
     C                   PARM                    @@RRN1            9 0          PEL01 RRN
     C                   PARM                    @@RRN2            9 0          PMY01 RRN
     C                   PARM                    @@ENDT            8 0
      *** BEG ADD ***** 12/14/09*************************************
     C* Waiver Detail file
      *
     C     *LIKE         DEFINE    MUEND#        XXEND#
      *
     C     WVKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C* Employer Attached Endorsement Forms file
     C*
     C     MUKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    XXFORM
     C                   KFLD                    XXEND#
     C*
      *** END ADD ***** 12/14/09*************************************
      *
      *
     C                   CALL      'W0815R'                             99
     C     @@KC          PARM                    ##KC              1
     C     @@KL          PARM                    ##KL              1
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      @@RRN1        ##RRN2            9 0          RRN WDELP
     C                   PARM      Q1PGM         ##PGM            10            Program
     C                   PARM      'D'           ##WTYP            1            Waiver Type
      *** BEG ADD ***** 12/14/09*************************************
     C*
     C*   Fetch current Year WDELP record.
     C     @@RRN1        CHAIN     WDELP                              60
     C*
     C*   Check if the last waiver record (WMWVP) is being deleted.
      *
     C     WVKY1         SETGT     PWV01
     C     WVKY1         READPE    PWV01                                  60
      *
     C*   If yes, delete the Waiver Endorsement record in WMMUP (WC000313)
     C*   with Endorsement # = 0. Chis will ensure we do not roll an
     C*   "empty" Waiver Endorsement into the new policy year.
      *
     C     *IN60         IFEQ      *ON
      *
     C                   Z-ADD     *ZEROS        XXEND#
     C                   MOVEL     'WC000313'    XXFORM           15
      *
     C     MUKY          DELETE    PMU01                              60
      *
     C                   ENDIF                                                  *IN60=*ON
      *** END ADD ***** 12/14/09*************************************
      *
      *
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
