     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N7902R - CREATE WORK FILE FOR EOB EDIT REPORT
      *****************************************************************
      **                                                             **
      ** THIS PROGRAM IS USED TO CREATE A WORK FILE FROM FAEOP       **
      ** (EOB FILE), AND NATRP (NETWORK TRANSACTION FILE)            **
      ** THE NEW FILE CREATED TAEOP WILL CONSIST                     **
      ** OF ALL THE RECORDS FROM FAEOP THAT HAVE A BILLING           **
      ** AMOUNT PLUS ANY ASSOCIATED NETWORK TRANSACTION RECORDS      **
      ** FROM NATRP THAT HAVE A BILLING AMOUNT.                      **
      **                                                             **
      *****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                              **
     H*  01/11/99  C:94  PROJECT: MC152
     H*                  CHANGE PROGRAM TO USE FAEAP INSTEAD OF
     H*                  NAPBP, NAMBP, FAHBP, NATRP
     H*
     H*  07/05/95 C:35  REQ/PROJ: NW008                              **
     H*                 ADDED MEDICAL MANAGEMENT BILLING FILE        **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     FFAEOL4    IF   E           K DISK
     FNATRL     IF   E           K DISK
     FTAEOL1    UF A E           K DISK
     F                                     RENAME(PEO01:PEO02)
     FFAHBL     IF   E           K DISK
     FNAPBL1    IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FNAMBL1    IF   E           K DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     FFDEOL     IF   E           K DISK
     F                                     RENAME(PEO01:PEO03)
     F*** BEG ADD *** 01/11/99 ***************************************
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/11/99 ***************************************

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
     C*
     C     HBKEY         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C*** BEG ADD *** 01/11/99 ***************************************
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
     C*** END ADD *** 01/11/99 ***************************************
     C                   MOVE      '0'           *IN60
     C     *IN60         DOWEQ     '0'
     C                   READ      PEO01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     EOSYS         IFEQ      'P'
     C                   WRITE     PEO02
     C                   END
     C*
     C     EOSYS         IFEQ      'U'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EABAMT        EOBAMT
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     PBKEY         CHAIN     PPB01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     PBBAMT        EOBAMT
     C                   WRITE     PEO02
     C                   END
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C                   END
     C*
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'M'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EABAMT        EOBAMT
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     MBKEY         CHAIN     PMB01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     MBBAMT        EOBAMT
     C                   WRITE     PEO02
     C                   END
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C                   END
     C*
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'H'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EABAMT        EOBAMT
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     HBKEY         CHAIN     PHB01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     HB1AAM        EOBAMT
     C                   WRITE     PEO02
     C                   END
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN60
     C     *IN60         DOWEQ     '0'
     C*** BEG CHG *** 01/11/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C                   READ      PEA01                                  60
     C     *IN60         IFEQ      '0'
     C     EOKEY         CHAIN     PEO01                              65
     C     *IN65         IFEQ      '0'
     C*
     C     EOKEY         CHAIN     PEO02                              65
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
     C                   WRITE     PEO02
     C                   END
     C                   END
     C                   ELSE                                                   ##PRVB = Y
     C                   READ      PTR01                                  60
     C     *IN60         IFEQ      '0'
     C     EOKEY         CHAIN     PEO01                              65
     C     *IN65         IFEQ      '0'
     C*
     C     EOKEY         CHAIN     PEO02                              65
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
     C                   WRITE     PEO02
     C                   END
     C                   END
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/11/99 ***************************************
     C                   END
     C*
     C                   MOVE      '0'           *IN60
     C     *IN60         DOWEQ     '0'
     C                   READ      PEO03                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'D'           EOSTAT
     C                   WRITE     PEO02
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
