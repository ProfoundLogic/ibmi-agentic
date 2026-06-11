     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA006R - API to Access FMPDP & Return RRN
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*    Program receives PARMS FEIN/MOD.  FMPDL is accessed and the  *
     H*    RRN for the FEIN/MOD is returned as PARM.                    *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    W0030R1                                                      *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  12/05/96  C:30  Project:  68MDI                      *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    40 *OFF - FMPDP rec found.  Update record.                   *
     H*    40 *ON  - FMPDP rec not found.  Write record.                *
     H*******************************************************************
      *
      * MCMS Provider file
     FFMPDL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
      *

     D DBFDS1          DS
     D  DBRRN1               397    400B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *

     C     PDKY          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
      *
      *          M A I N     L O G I C
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    @@RRN             9 0          FMPDP RRN
      *
      *  Access WCMC Provider
      *
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
      *
     C     PDKY          CHAIN     PPD01                              40
      *
     C     *IN40         IFEQ      *OFF
     C                   Z-ADD     DBRRN1        @@RRN
     C                   ELSE
     C                   Z-ADD     *ZERO         @@RRN
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
