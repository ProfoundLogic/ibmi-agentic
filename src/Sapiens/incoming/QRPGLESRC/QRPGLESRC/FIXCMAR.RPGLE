     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      ***************************************************************
      *                                                            **
      * PROGRAM:     FIXIQ                                         **
      * DESCRIPTION: Update the first position of the field CMIQUE **
      *              in the file SDCMP. This was changed to allow  **
      *              Visual Darts to load faster when there are    **
      *              many remarks.                                 **
      * CREATED BY:  C:128  PROJ:         DATE:  03/15/04          **
     H***************************************************************
     FSDCMP     UF   E             DISK
      *
     FSMARL1    IF   E           K DISK
      *

     I******      DS
     I******                                  1  10 CMIQUE
     I******                                  1   1 ADDRMK
     I******                                  2  10 FILLER

     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
      *
     C*** Read the Darts remark file.
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     ARKY          CHAIN     PAR01                              61
      *
     C     *IN61         IFEQ      *OFF
     C     ARCM75        IFNE      *BLANK
     C                   MOVEL     'Y'           CMIQUE
     C                   UPDATE    PCM01
     C                   ENDIF
     C                   ENDIF
      *
     C                   READ      PCM01                                  60
     C                   ENDDO                                                  ENDDOW *IN60
     C*
     C*** End the program
     C*
     C                   MOVE      *ON           *INLR
