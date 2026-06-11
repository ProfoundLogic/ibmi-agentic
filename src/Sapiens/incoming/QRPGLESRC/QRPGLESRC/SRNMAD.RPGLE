     H Copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE SRNMAD - Name/Address API for WCPOLS
     H DFTNAME(SRNMAD)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H BNDDIR('WTBNDDIR')
     H**********************************************************************
    *===============================================================*
    *  Created    : 07/31/14                                        *
    *  Programmer : C:65                                            *
    *  Project    : WC2210                                          *
    *  Description: This API will generate name/address records     *
    *               for WCPOLS to be used for printing/reporting    *
    *               policy information.                             *
    *                                                               *
    *===============================================================*
    *  Revisions:                                                   *
     *                                                               *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)

     D xxCO#           s                   like(ELCO#)
     D xxFnd           s                   like(ELFnd)
     D xxEmp#          s                   like(ELEmp#)
     D xxDiv           s                   like(ELDiv)
     D xxFyr           s                   like(ELFyr)

     D PSDS          ESDS                  extname(SZQ1P)

     d* Proto-types

     d* Main Processing
     d Main            pr

     d/copy sprDate
     d/copy wPrEmpLr

     C*
     C*   Key List for the Employer Detail File
     C*
     C     keyWDELL      Klist
     C                   Kfld                    xxCo#
     C                   Kfld                    xxFnd
     C                   Kfld                    xxEmp#
     C                   Kfld                    xxDiv
     C                   Kfld                    xxFyr
     C*
     c* Entry of Program.
     c*
     C     *ENTRY        PLIST
     C                   PARM                    @@rrn             9            wdelp
     c
      /free

       Main();

       *InLr = *On;
      /end-free

       //***************************************************************************//
       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI
      /free

      /end-free
     P Main            E

       //***************************************************************************//

