     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: FS902R
     H*    DESCRIPTION: REMOVE PREVIOUS DATA
     H*                                                                *
     H*    CALLED FROM: FS900C
     H*    CALLS TO: NONE
     H******************************************************************
     H*  CREATED:                                                      *
     H*                                                                *
     H*  11/30/04 C:079 PROJECT 25MNU                                  *
     H*                 HCPCS MISSING G,H CODES                        *
     H*                                                                *
     H*  08/26/04 C:079 PROJECT: 129MGV                                *
     H*                 DELETE DENTAL BEGINNING WITH 'D'
     H*                                                                *
     H*  02/27/01 C:117 PROJECT: FS225                                 *
     H*                 CREATED ANES MODULE
     H*                 MOVED MEDICODE PGMS TO BASE
     H*                                                                *
     H******************************************************************
     H*  REVISIONS:                                                    *
     H*                                                                *
     H******************************************************************
     H*  INDICATORS:                                                   *
     H*  60    READ/DOWEQ ON PCR01                                     *
     H******************************************************************
     H*  FILES:                                                        *
     H*  FMRCP  (PRC01): CO#/ZIPA/CPT4/EFDT                            *
     H******************************************************************
     FFMRCL     UF   E           K DISK

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* ENTRY PARMS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQSEL             1
     C*
     C* READ FAEOP FILE
     C*
     C                   READ      PRC01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*** SELECT THE CORRECT SBR
     C*
     C     QQSEL         IFEQ      'A'
     C                   EXSR      DLTASR
     C                   ENDIF
     C*
     C     QQSEL         IFEQ      'M'
     C                   EXSR      DLTMSR
     C                   ENDIF
     C*
     C     QQSEL         IFEQ      'D'
     C                   EXSR      DLTDSR
     C                   ENDIF
     C*
     C     QQSEL         IFEQ      'H'
     C                   EXSR      DLTHSR
     C                   ENDIF
     C*
     C                   READ      PRC01                                  60
     C                   ENDDO
     C*
     C*  END THE PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*** DELETE 'A' SUBROUTINE
     C*** DELETE CPT4 RECORDS THAT ARE IN RANGE 00100 - 01999
     C*
     C     DLTASR        BEGSR
     C                   MOVEL     RCCPT4        XXCPT4            5
     C     XXCPT4        IFGE      '00100'
     C     XXCPT4        ANDLE     '01999'
     C                   DELETE    PRC01
     C                   ELSE
     C                   GOTO      NODLTA
     C                   ENDIF
     C*
     C     NODLTA        TAG
     C*
     C                   ENDSR
     C*
     C*** DELETE 'M' SUBROUTINE
     C*** DELETE CPT4 RECORDS THAT 'DO NOT' BEGIN WITH O,A,J,K,L,D,E
     C*
     C     DLTMSR        BEGSR
     C                   MOVEL     RCCPT4        XXCKCP            1
     C                   MOVEL     RCCPT4        XXCPT4            5
     C     XXCKCP        IFGE      'A'
     C     XXCKCP        ANDLE     'Z'
     C*** BEG DEL *** 11/30/04 ****************************************
     C*********  XXCKCP    IFEQ 'O'
     C*********  XXCKCP    OREQ 'A'
     C*********  XXCKCP    OREQ 'J'
     C*********  XXCKCP    OREQ 'K'
     C*********  XXCKCP    OREQ 'L'
     C****BEGIN ADD *** 08/25/97 **************************************
     C***ADDING THE REMOVAL OF EXXXX CPT4 CODES WHEN SEL = 'M'
     C*********  XXCKCP    OREQ 'E'
     C****END*ADD ***** 08/25/97 **************************************
     C****BEG*ADD *** 08/26/04 ****************************************
     C*********  XXCKCP    OREQ 'D'
     C*** END ADD *** 08/26/04 ****************************************
     C*** BEG END *** 11/30/04 ****************************************
     C                   GOTO      NODLTM
     C                   ELSE
     C** DO NOT INCLUDE ANES CODES
     C     XXCPT4        IFGE      '00100'
     C     XXCPT4        ANDLE     '01999'
     C                   GOTO      NODLTM
     C                   ELSE
     C                   DELETE    PRC01
     C                   ENDIF
     C                   ENDIF
     C*
     C     NODLTM        TAG
     C*
     C                   ENDSR
     C*
     C*** DELETE 'D' SUBROUTINE
     C*** DELETE CPT4 RECORDS THAT BEGIN WITH O.
     C*
     C     DLTDSR        BEGSR
     C                   MOVEL     RCCPT4        XXCKCP
     C     XXCKCP        IFEQ      'O'
     C*** BEG ADD *** 08/26/04 ****************************************
     C     XXCKCP        OREQ      'D'
     C*** END ADD *** 08/26/04 ****************************************
     C                   DELETE    PRC01
     C                   ELSE
     C                   GOTO      NODLTD
     C                   ENDIF
     C*
     C     NODLTD        TAG
     C*
     C                   ENDSR
     C*
     C*** DELETE 'H' SUBROUTINE
     C*** DELETE CPT4 RECORDS THAT BEGIN WITH A,J,K,L.
     C*
     C     DLTHSR        BEGSR
     C                   MOVEL     RCCPT4        XXCKCP
     C*** BEG ADD *** 11/30/04 ****************************************
     C     XXCKCP        IFGE      'A'
     C     XXCKCP        ANDLE     'C'
     C     XXCKCP        ORGE      'E'
     C     XXCKCP        ANDLE     'Z'
     C*** END ADD *** 11/30/04 ****************************************
     C*** BEG DEL *** 11/30/04 ****************************************
     C********** XXCKCP    IFEQ 'A'
     C********** XXCKCP    OREQ 'J'
     C********** XXCKCP    OREQ 'K'
     C********** XXCKCP    OREQ 'L'
     C****BEGIN*ADD *** 08/25/97 **************************************
     C***ADDING*THE REMOVAL OF EXXXX CPT4 CODES WHEN SEL = 'H'
     C********** XXCKCP    OREQ 'E'
     C*** END ADD ***** 08/25/97 **************************************
     C*** END DEL *** 11/30/04 ****************************************
     C                   DELETE    PRC01
     C                   ELSE
     C                   GOTO      NODLTH
     C                   ENDIF
     C*
     C     NODLTH        TAG
     C*
     C                   ENDSR
