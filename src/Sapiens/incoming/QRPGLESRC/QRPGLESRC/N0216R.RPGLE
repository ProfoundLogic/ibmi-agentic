     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE N0216R - CREATE WORK FILE FOR INTERACTIVE HOLD/RELEASE
      *****************************************************************
      **                                                             **
      ** THIS PROGRAM IS USED TO CREATE A WORK FILE FROM FAEOP       **
      ** (EOB WORK   FILE) AND NATRP (NETWORK TRANSACTION FILE).     **
      ** THE WORK FILE CREATED WILL CONSIST OF ALL THOSE RECORDS     **
      ** FROM FAEOP THAT HAVE A BILLING AMOUNT AND HAVE NOT BEEN     **
      ** INVOICED PLUS ANY ADDITIONAL NETWORK TRANSACTION RECORDS    **
      ** THAT A BILLING AMOUNT AND HAVE NOT BEEN INVOICED            **
      **                                                             **
      *****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                              **
     H*  12/29/98  C:94 PROJECT: MC152                               **
     H*                 CHANGE PROGRAM SO THAT IT NO LONGER USES     **
     H*                 NATRP, NAMBP, NAPBP, AND FAHBP BUT INSTEAD   **
     H*                 USE FAEAP                                    **
     H*                                                              **
     H*  07/05/95  C:35 REQ/PROJ: NW008                              **
     H*                 1. ADDED M-MEDICAL MANAGEMENT BILLING FILE   **
     H*                 2. CONDITION DISPLAY OF SYSTEM TYPE M RECORDS**
     H*                    ON POS. 9 OF FBAL01 EQ 'Y'.               **
      *****************************************************************
     FFAEOL6    IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FNATRL1    IF   E           K DISK
     FHAEOL     UF A E           K DISK
     F                                     RENAME(PEO01:PEO03)
     FFAHBL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FNAMBL1    IF   E           K DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F*** BEG ADD *** 12/29/98 ****************************************
     FFAEAL1    IF   E           K DISK
     FFAEAP     IF   E             DISK
     F                                     RENAME(PEA01:PEA02)
     F*** END ADD *** 12/29/98 ****************************************

     C*
     C* KEY LIST TO READ MATCHING EOB RECORD
     C*
     C     EOKEY         KLIST
     C                   KFLD                    TRCO#
     C                   KFLD                    TRSUB#
     C                   KFLD                    TRFND
     C                   KFLD                    TRFYR
     C                   KFLD                    TRCASE
     C                   KFLD                    TREOB#
     C*
     C     HBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     PBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     MBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*** BEG ADD *** 12/29/98 ****************************************
     C*
     C     EOK1          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
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
     C*** END ADD *** 12/29/98 ****************************************
     C*
     C                   MOVE      '0'           *IN60
     C     *IN60         DOWEQ     '0'
     C                   READ      PEO01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C*** BEG ADD *** 12/29/98 ****************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBAMT        ANDNE     *ZEROS
     C                   WRITE     PEO03
     C                   END
     C*
     C     EOSYS         IFEQ      'H'
     C     EOSYS         OREQ      'U'
     C     EOSYS         OREQ      'M'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      '0'
     C     EABAMT        IFNE      *ZEROS
     C                   Z-ADD     EABAMT        EOBAMT
     C                   WRITE     PEO03
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*** END ADD *** 12/29/98 ****************************************
     C     EOSYS         IFEQ      'P'
     C     EOBAMT        ANDNE     *ZEROS
     C                   WRITE     PEO03
     C                   END
     C*
     C     EOSYS         IFEQ      'H'
     C     HBKEY         CHAIN     PHB01                              65
     C     *IN65         IFEQ      '0'
     C     HB1AAM        IFNE      *ZEROS
     C                   Z-ADD     HB1AAM        EOBAMT
     C                   WRITE     PEO03
     C                   END
     C                   END
     C                   END
     C*
     C     EOSYS         IFEQ      'U'
     C     PBKEY         CHAIN     PPB01                              65
     C     *IN65         IFEQ      '0'
     C     PBBAMT        IFNE      *ZEROS
     C                   Z-ADD     PBBAMT        EOBAMT
     C                   WRITE     PEO03
     C                   END
     C                   END
     C                   END
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'M'
     C     MBKEY         CHAIN     PMB01                              65
     C     *IN65         IFEQ      '0'
     C     MBBAMT        IFNE      *ZEROS
     C                   Z-ADD     MBBAMT        EOBAMT
     C                   WRITE     PEO03
     C                   END
     C                   END
     C                   END
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C                   END
     C*** BEG ADD *** 12/29/98 ***************************************
     C                   ENDIF                                                  ##PRVB = Y
     C*** BEG ADD *** 12/29/98 ***************************************
     C                   END
     C*
     C                   MOVE      '0'           *IN60
     C     *IN60         DOWEQ     '0'
     C*** BEG ADD *** 12/29/98 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   READ      PEA02                                  60
     C     *IN60         IFEQ      '0'
     C     EANAMT        IFNE      *ZEROS
     C     EASYS         OREQ      'N'
     C*
     C     EOKEY         CHAIN     PEO02                              65
     C     *IN65         IFEQ      '0'
     C*
     C     EOKEY         CHAIN     PEO03                              65
     C     *IN65         IFEQ      '0'
     C                   MOVE      *ZEROS        EOAMT
     C                   MOVE      *ZEROS        EOAMTC
     C                   MOVE      *ZEROS        EOOTHR
     C                   MOVE      *ZEROS        EONDIS
     C                   MOVE      *BLANKS       EOFLG8
     C                   END
     C                   MOVE      EANBLF        EOBLFL
     C                   MOVE      EANHRF        EOHRFL
     C                   MOVE      EANAMT        EOBAMT
     C                   MOVE      'N'           EOSYS
     C                   WRITE     PEO03
     C                   END
     C                   END
     C                   END
     C                   ELSE
     C*** END ADD *** 12/29/98 ***************************************
     C                   READ      PTR01                                  60
     C     *IN60         IFEQ      '0'
     C     TRBAMT        IFNE      *ZEROS
     C     EOKEY         CHAIN     PEO02                              65
     C     *IN65         IFEQ      '0'
     C*
     C     EOKEY         CHAIN     PEO03                              65
     C     *IN65         IFEQ      '0'
     C                   MOVE      *ZEROS        EOAMT
     C                   MOVE      *ZEROS        EOAMTC
     C                   MOVE      *ZEROS        EOOTHR
     C                   MOVE      *ZEROS        EONDIS
     C                   MOVE      *BLANKS       EOFLG8
     C                   END
     C                   MOVE      TRBLFL        EOBLFL
     C                   MOVE      TRHRFL        EOHRFL
     C                   MOVE      TRBAMT        EOBAMT
     C                   MOVE      'N'           EOSYS
     C                   WRITE     PEO03
     C                   END
     C                   END
     C                   END
     C                   ENDIF
     C*** BEG ADD *** 12/29/98 ***************************************
     C                   ENDDO
     C*** END ADD *** 12/29/98 ***************************************
     C*
     C                   MOVE      '1'           *INLR
     C*
