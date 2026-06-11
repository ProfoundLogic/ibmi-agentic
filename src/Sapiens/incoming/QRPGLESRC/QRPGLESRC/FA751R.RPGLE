     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*********************************************************************
    H/TITLE Multi-line Retrieve Consoldated Values
     H*OPTION(*NODEBUGIO: *SRCSTMT)
     H*DEBUG(*YES)
    H*********************************************************************
    H*  CALLED FROM: F9711RAFL                                           *
    H*                                                                   *
    H*  CREATED: 08/10/07      C:183     PROJECT: FL053                  *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
     H* 08/26/14 C:307 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
     H*                                                                   *
    H*  06/15/11   C:257   Support:77152                                 *
    H*                     Count the number of units (caunts) for the    *
    H*                     MSEQ number and return this to the calling    *
    H*                     program.                                      *
    H*                                                                   *
    H*  11/30/10   C:227   Support:74760                                 *
    H*                     EDI extract not pulling correct EOB code.     *
    H*                     Added an entry parameter to fix the issue.    *
    H*                                                                   *
    H*********************************************************************
     F*================================================================
     F*  File Specification
     F*================================================================
     FFDCAL28   IF   E           K DISK
      *
     D*** BEG ADD *** 11/30/10 ****************************************
     D @@dseq          s                   like(cadseq)
     D*** END ADD *** 11/30/10 ****************************************
     D*** BEG ADD *** 06/15/11  ***************************************
     D@@units          s              3  0
     D*** END ADD *** 06/15/11  ***************************************
     C*================================================================
     C*  Key Lists
     C*================================================================
      *
     C     CAKY          KLIST
     C                   KFLD                    @@CO#             3 0
     C                   KFLD                    @@SUB#            5 0
     C                   KFLD                    @@FND             3 0
     C                   KFLD                    @@FYR             3 0
     C                   KFLD                    @@CASE            7 0
     C                   KFLD                    @@EOB#            9 0
     C                   KFLD                    @@MSEQ            5 0
      *
     C*================================================================
     C*  *Entry PLIST
     C*================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@EOB#
     C                   PARM                    @@MSEQ
     C                   PARM                    @@CHRG           11 2
     C                   PARM                    @@PAID           11 2
     C*** BEG ADD *** 11/30/10 ****************************************
     C                   PARM                    @@DSEQ
     C*** END ADD *** 11/30/10 ****************************************
     C*** BEG ADD *** 06/15/11 ****************************************
     C                   PARM                    @@units
     C*** END ADD *** 06/15/11 ****************************************
      *
     C*================================================================
     C*  Main Line of Code
     C*================================================================
      *
     C     CAKY          SETLL     PCA01
     C     CAKY          READE     PCA01
      *
     C                   DOW       NOT %EOF(FDCAL28)
      *
     C                   ADD       CACHRG        @@CHRG
     C                   ADD       CAPAID        @@PAID
     C*** BEG ADD *** 06/15/11 ****************************************
      ***** Beg Add ***** 08/26/14 ************************************
     C                   IF        %PARMS > 10
      ***** End Add ***** 08/26/14 ************************************
     C                   ADD       CAUNTS        @@units
      ***** Beg Add ***** 08/26/14 ************************************
     C                   Endif
      ***** End Add ***** 08/26/14 ************************************
     C*** END ADD *** 06/15/11 ****************************************
     C*** BEG ADD *** 11/30/10 ****************************************
     C* Return the sequence# for line item with paid amount
     C                   IF        %PARMS > 9
     C                   IF        @@DSEQ = 0
     C                   EVAL      @@DSEQ = CADSEQ
     C                   ELSE                                                   @@DSEQ = 0
     C                   IF        CAPAID > 0
     C                   EVAL      @@DSEQ = CADSEQ
     C                   ENDIF                                                  CAPAID > 0
     C                   ENDIF                                                  @@DSEQ = 0
     C                   ENDIF                                                  %PARMS > 9
     C*** END ADD *** 11/30/10 ****************************************
      *
     C     CAKY          READE     PCA01
      *
     C                   ENDDO
      *
     C                   EVAL      *INLR = *ON
