     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N7924R - CREATE WORK FILE FOR HOSPITAL UTILIZATION REPORT
      *****************************************************************
      ** THIS PROGRAM IS USED TO CREATE A WORK FILE FROM FAEOP       **
      ** (EOB FILE)        AND NATRP (NETWORK TRANSACTION FILE).     **
      ** THE WORK FILE CREATED WILL CONSIST OF ALL THE BILLED        **
      ** EOB RECORDS AND UNBILLED EOB RECORDS WITH ASSOCIATED        **
      ** BILLED NETWORK RECORDS.                                     **
      *****************************************************************
     H*  REVISIONS:                                                  **
     H*  01/06/99 C:94  PROJECT: MC152                               **
     H*                 CHANGE PROGRAM TO USE FAEAP IF PROVIDER BILLS**
     H*                 IS LOADED.                                   **
     H*                                                              **
     H*  08/26/92 C:05  PROJ: 99AAC                                  **
     H*                 Changed all instances of EOFLG2 to EOHRFL    **
     H*                 Changed all instances of EOFLG4 to EOBLFL    **
     H*                 Changed all instances of TRFLG2 to TRHRFL    **
     H*                 Changed all instances of TRFLG4 to TRBLFL    **
     H*                 Changed FAEOLA TO FAEOLC                     **
     H*                                                              **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FFAEOLC    IF   E           K DISK
     FNATRL     IF   E           K DISK
     FTAEOP     O  A E           K DISK
     F                                     RENAME(PEO01:PEO02)
     F*** BEG ADD *** 01/06/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/06/99 ***************************************

     C*
     C* KEY LIST TO READ MATCHING NETWORK RECORD
     C*
     C     TRKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*** BEG ADD *** 01/06/99 ***************************************
     C*
     C*
     C     EAKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C*** END ADD *** 01/06/99 ***************************************
     C                   MOVE      '0'           *IN60
     C*** BEG ADD *** 01/06/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     '0'
     C**
     C** IF WE ARE WORKING WITH A NON-BILLED EOB RECORD, CHECK TO
     C** SEE IF THERE IS A BILLED NETWORK RECORD ASSOCIATED WITH IT.
     C** IF YES, THEN WRITE A WORK FILE RECORD.
     C     EOBLFL        IFNE      'P'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      '0'
     C     EANBLF        ANDEQ     'P'
     C                   MOVE      EANBLF        EOBLFL
     C                   MOVE      EANHRF        EOHRFL
     C                   MOVE      EANAMT        EOBAMT
     C                   MOVE      'N'           EOSYS
     C                   WRITE     PEO02
     C                   END                                                    END EQ 0/P
     C                   ELSE                                                   ELSE NE P
     C*
     C** IF THIS IS A BILLED EOB RECORD, WRITE A WORK FILE RECORD
     C*
     C                   WRITE     PEO02
     C                   END                                                    END EQ P
     C**
     C                   READ      PEO01                                  60
     C                   END                                                    END DOWEQ 60
     C*
     C                   ELSE                                                   ##PRVB = Y
     C*** END ADD *** 01/06/99 ***************************************
     C*
     C                   READ      PEO01                                  60
     C     *IN60         DOWEQ     '0'
     C**
     C** IF WE ARE WORKING WITH A NON-BILLED EOB RECORD, CHECK TO
     C** SEE IF THERE IS A BILLED NETWORK RECORD ASSOCIATED WITH IT.
     C** IF YES, THEN WRITE A WORK FILE RECORD.
     C     EOBLFL        IFNE      'P'
     C     TRKEY         CHAIN     PTR01                              65
     C     *IN65         IFEQ      '0'
     C     TRBLFL        ANDEQ     'P'
     C                   MOVE      TRBLFL        EOBLFL
     C                   MOVE      TRHRFL        EOHRFL
     C                   MOVE      TRBAMT        EOBAMT
     C                   MOVE      'N'           EOSYS
     C                   WRITE     PEO02
     C                   END                                                    END EQ 0/P
     C                   ELSE                                                   ELSE NE P
     C*
     C** IF THIS IS A BILLED EOB RECORD, WRITE A WORK FILE RECORD
     C*
     C                   WRITE     PEO02
     C                   END                                                    END EQ P
     C**
     C                   READ      PEO01                                  60
     C                   END                                                    END DOWEQ 60
     C*
     C*** BEG ADD *** XX/XX/XX ***************************************
     C                   ENDIF                                                  ##PRVB = Y
     C*** END ADD *** XX/XX/XX ***************************************
     C                   MOVE      '1'           *INLR
     C*
