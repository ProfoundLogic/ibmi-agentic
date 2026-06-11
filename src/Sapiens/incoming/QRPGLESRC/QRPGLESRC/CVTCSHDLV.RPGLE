     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/title CVTCSHDLV- Fix program for Post Cash Values
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*   THIS PROGRAM WILL DEFAULT NEW FLAGS BASED UPON VALUES       *
     H*   OF OLD FLAGS.                                               *
     H* PROGRAMMER: C:246                                             *
     H* CREATED: 10/20/10                                             *
     H* REVISIONS:                                                    *
     H*                                                               *
     ******************************************************************
     FWDF2P     UF   E             DISK
     ******************************************************************
     ** MAIN LOOP **
     ******************************************************************
     C                   READ      WDF2P                                  30
      *
     C                   DOW       *IN30 = *OFF
     C                   IF        F2CPMT <> *BLANKS
      **
     C                   SELECT
     C                   WHEN      F2CPMT = '1'
     C                   EVAL      F2CPYF = 'Y'
     C                   EVAL      F2CCYF = 'Y'
     C                   EVAL      F2RPYF = 'Y'
     C                   EVAL      F2RCYF = 'Y'
     C                   EVAL      F2APYF = 'Y'
     C                   EVAL      F2ACYF = 'Y'
     C                   EVAL      F2LCYF = 'Y'
      *
     C                   WHEN      F2CPMT = '2'
     C                   EVAL      F2CCYF = 'Y'
     C                   EVAL      F2RCYF = 'Y'
     C                   EVAL      F2ACYF = 'Y'
     C                   EVAL      F2LCYF = 'Y'
      *
     C                   OTHER
     C                   ENDSL
     **
     C                   IF        F2RNEW  = 'Y'
     C                   EVAL      F2CRYF = 'Y'
     C                   EVAL      F2RRYF = 'Y'
     C                   EVAL      F2ARYF = 'Y'
     C                   ENDIF
      **
     C                   EVAL      F2CPDT = F2CDTY
     C                   EVAL      F2RPDT = F2CDTY
     C                   EVAL      F2APDT = 'C'
     C                   EVAL      F2LPDT = 'B'
     C                   EVAL      F2RNEW = *blank
     C                   EVAL      F2CPMT = *blank
     C                   EVAL      F2CDTY = *blank
      **
     C                   UPDATE    PF201
     C                   ENDIF
     C                   CLEAR                   PF201
     C                   READ      WDF2P                                  30
      **
     C                   ENDDO
     C                   SETON                                        LR

