     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC811R - QUERY INTERFACE WORKFILE CREATION - CRT PGM
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/30/15 C:62 PROJECT 48WYI AND 114WUU                       *
     H*                NO NEED TO READ THRU THE ENTIRE FILE FOR EACH
     H*                GROUP                                          *
     H*                                                               *
     H*  11/17/98 C:33 SEQ#: DR048/14470                              *
     H*                CHECK FOR CMKEY TO BE NUMERIC PRIOR TO MOVING  *
     H*                INTO DATA STRUCTURES                           *
     H*                                                               *
     H*  02/12/92 LJD  PROJ: SC100                                    *
     H*                Allow for processing of all cases within FYR   *
     H*****************************************************************
     H*  INDICATORS:                                                  *
     H*  60    Primary Chain/Usage on PCM0x                           *
     H*  61    General Chain/Usage on PAR0x, PHD0x, PH10x             *
     H*****************************************************************
     F*
     F* DARTS Master Files: KEY
     FSMARL1    IF   E           K DISK
     FSDCMLC    IF   E           K DISK
     FSMHDL1    IF   E           K DISK
     FSMH1L1    IF   E           K DISK
     F*
     F* Claim Interface Workfiles
     FSXARPWC   O    E           K DISK
     F                                     RENAME(PAR01:PAR0C)
     FSXCMPWC   O    E           K DISK
     F                                     RENAME(PCM01:PCM0C)
     FSXHDPWC   O    E           K DISK
     F                                     RENAME(PHD01:PHD0C)
     FSXH1PWC   O    E           K DISK
     F                                     RENAME(PH101:PH10C)
     F*
     F* Employer Interface Workfiles
     FSXARPWE   O    E           K DISK
     F                                     RENAME(PAR01:PAR0E)
     FSXCMPWE   O    E           K DISK
     F                                     RENAME(PCM01:PCM0E)
     FSXHDPWE   O    E           K DISK
     F                                     RENAME(PHD01:PHD0E)
     FSXH1PWE   O    E           K DISK
     F                                     RENAME(PH101:PH10E)
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** Claim DARTS Key
     D***  BEGIN CHG  ***  11/17/98  *********************************
     D**********  DS
     D                 DS                  INZ
     D***  END   CHG  ***  11/17/98  *********************************
     D  XCKEY                  1     40
     D  XCCO#                  1      3  0
     D  XCFND                  4      6  0
     D  XCFYR                  7      9  0
     D  XCCASE                10     16  0
     D*
     D*** Employer DARTS Key
     D***  BEGIN CHG  ***  11/17/98  *********************************
     D**********  DS
     D                 DS                  INZ
     D***  END   CHG  ***  11/17/98  *********************************
     D  XEKEY                  1     40
     D  XECO#                  1      3  0
     D  XEFND                  4      6  0
     D  XEEMP#                 7     15  0
     D  XEDIV                 16     20  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Major Remarks File - Primary File read - Full Key
     C     CMKYF         KLIST
     C                   KFLD                    XXPNAM
     C                   KFLD                    XXKEY
     C     CMKYP         KLIST
     C                   KFLD                    XXPNAM
     C*
     C* Additional Remarks File
     C     ARKY          KLIST
     C                   KFLD                    XXKEY
     C                   KFLD                    CMSEQ#
     C*
     C* Database File - Record Format A
     C     HDKYF         KLIST
     C                   KFLD                    XXPNAM
     C                   KFLD                    XXKEY
     C     HDKYP         KLIST
     C                   KFLD                    XXPNAM
     C*
     C* Database File - Record Format B
     C     H1KYF         KLIST
     C                   KFLD                    XXPNAM
     C                   KFLD                    XXKEY
     C     H1KYP         KLIST
     C                   KFLD                    XXPNAM
     C*
     C     *LIKE         DEFINE    CMEMP#        HEEMP#
     C     *LIKE         DEFINE    CMDIV         HEDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QCCMFL            1
     C                   PARM                    QCCO#             3
     C                   PARM                    QCFND             3
     C                   PARM                    QCFYR             3
     C                   PARM                    QCCASE            7
     C                   PARM                    QEEMFL            1
     C                   PARM                    QECO#             3
     C                   PARM                    QEFND             3
     C                   PARM                    QEEMP#            9
     C                   PARM                    QEDIV             5
     C*
     C                   MOVE      QCCO#         HCCO#             3 0
     C                   MOVE      QCFND         HCFND             3 0
     C                   MOVE      QCFYR         HCFYR             3 0
     C                   MOVE      QCCASE        HCCASE            7 0
     C*
     C                   MOVE      QECO#         HECO#             3 0
     C                   MOVE      QEFND         HEFND             3 0
     C                   MOVE      QEEMP#        HEEMP#
     C                   MOVE      QEDIV         HEDIV
     C*
     C* Initialize Variable workfields
     C     *LIKE         DEFINE    CMPNAM        XXPNAM
     C     *LIKE         DEFINE    CMKEY         XXKEY
     C*
     C* ============================================================= *
     C* Do Claim Conversion:
     C     QCCMFL        IFNE      *BLANKS
     C*
     C* Establish program name for Claim Master Maintenance to work with
     C                   MOVE      *BLANKS       XXPNAM
     C                   MOVEL     'W0070R'      XXPNAM
     C*
     C* Do Claim Conversion:
     C     QCCASE        IFNE      *ZEROS
     C                   EXSR      CLFULL
     C                   ELSE                                                   ELS CASE NE *ZERO
     C     QCFYR         IFNE      *ZEROS
     C                   EXSR      CLFYR
     C                   ELSE                                                   ELS FYR NE *ZERO
     C     QCFND         IFNE      *ZEROS
     C                   EXSR      CLPART
     C                   ELSE                                                   ELS FUND NE *ZERO
     C                   EXSR      CLNONE
     C                   END                                                    END FUND NE *ZERO
     C                   END                                                    END FYR NE *ZERO
     C                   END                                                    END CASE NE *ZERO
     C*
     C* Establish program name for FS Bill Entry to work with
     C                   MOVE      *BLANKS       XXPNAM
     C                   MOVEL     'F0070R'      XXPNAM
     C*
     C* Do Claim Conversion:
     C     QCCASE        IFNE      *ZEROS
     C                   EXSR      CLFULL
     C                   ELSE                                                   ELS CASE NE *ZERO
     C     QCFYR         IFNE      *ZEROS
     C                   EXSR      CLFYR
     C                   ELSE                                                   ELS FYR NE *ZERO
     C     QCFND         IFNE      *ZEROS
     C                   EXSR      CLPART
     C                   ELSE                                                   ELS FUND NE *ZERO
     C                   EXSR      CLNONE
     C                   END                                                    END FUND NE *ZERO
     C                   END                                                    END FYR NE *ZERO
     C                   END                                                    END CASE NE *ZERO
     C*
     C                   END                                                    END NE *BLANKS
     C*
     C* ============================================================= *
     C* Do Employer Conversion
     C     QEEMFL        IFNE      *BLANKS
     C*
     C* Establish program name for Employer Master Maintenance to work with
     C                   MOVE      *BLANKS       XXPNAM
     C                   MOVEL     'W0050R'      XXPNAM
     C*
     C     QEEMP#        IFNE      *ZEROS
     C                   EXSR      EMFULL
     C                   ELSE                                                   ELS EMP# NE *ZERO
     C     QEFND         IFNE      *ZEROS
     C                   EXSR      EMPART
     C                   ELSE                                                   ELS FUND NE *ZERO
     C                   EXSR      EMNONE
     C                   END                                                    END FUND NE *ZERO
     C                   END                                                    END EMP# NE *ZERO
     C                   END                                                    END NE *BLANKS
     C*
     C*
     C*  End of program
     C     END           TAG
     C                   MOVE      '1'           *INLR
     C***********************************************************************
     C*  CLFULL - Full (Case selected)  Claim Key Conversion
     C***********************************************************************
     C     CLFULL        BEGSR
     C*
     C*  Get Major Comment Records
     C*
     C*  Setup DARTS file key
     C                   MOVE      QCCO#         XCCO#
     C                   MOVE      QCFND         XCFND
     C                   MOVE      QCFYR         XCFYR
     C                   MOVE      QCCASE        XCCASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*  Setup workfile's new fields
     C                   MOVE      QCCO#         CMCO#
     C                   MOVE      QCFND         CMFND
     C                   MOVE      QCFYR         CMFYR
     C                   MOVE      QCCASE        CMCASE
     C*
     C     CMKYF         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PCM0C
     C*
     C*          Get Additional Comment Records
     C                   MOVE      QCCO#         ARCO#
     C                   MOVE      QCFND         ARFND
     C                   MOVE      QCFYR         ARFYR
     C                   MOVE      QCCASE        ARCASE
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         DOWEQ     '0'
     C                   WRITE     PAR0C
     C     ARKY          READE     PAR01                                  61
     C                   END                                                    END 0 *IN61
     C*
     C     CMKYF         READE     PCM01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt A Records
     C                   MOVE      QCCO#         HDCO#
     C                   MOVE      QCFND         HDFND
     C                   MOVE      QCFYR         HDFYR
     C                   MOVE      QCCASE        HDCASE
     C     HDKYF         CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PHD0C
     C     HDKYF         READE     PHD01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt B Records
     C                   MOVE      QCCO#         H1CO#
     C                   MOVE      QCFND         H1FND
     C                   MOVE      QCFYR         H1FYR
     C                   MOVE      QCCASE        H1CASE
     C     H1KYF         CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PH10C
     C     H1KYF         READE     PH101                                  60
     C                   END                                                    END 0 *IN60
     C*
     C                   ENDSR
     C***********************************************************************
     C*  CLFYR - Fund Year (Fund/Fund Year only): Claim Key Conversion
     C***********************************************************************
     C     CLFYR         BEGSR
     C*
     C*  Get Major Comment Records
     C     CMKYP         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELCMKEY     XCKEY
     C*
     C                   MOVEL(P)  CMKEY         CKEY             16
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CMKEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         CMCO#
     C                   MOVE      XCFND         CMFND
     C                   MOVE      XCFYR         CMFYR
     C                   MOVE      XCCASE        CMCASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C     XCFND         ANDEQ     HCFND
     C     XCFYR         ANDEQ     HCFYR
     C                   WRITE     PCM0C
     C*
     C*  Get Additional Comment Records
     C                   MOVE      XCCO#         ARCO#
     C                   MOVE      XCFND         ARFND
     C                   MOVE      XCFYR         ARFYR
     C                   MOVE      XCCASE        ARCASE
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         DOWEQ     '0'
     C                   WRITE     PAR0C
     C     ARKY          READE     PAR01                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END XCCO# EQ HCCO#
     C*
     C*  Read next Major Remarks record
     C     CMKYP         READE     PCM01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt A Records
     C     HDKYP         CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELHDKEY     XCKEY
     C*
     C                   MOVEL(P)  HDKEY         CKEY
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  HDKEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         HDCO#
     C                   MOVE      XCFND         HDFND
     C                   MOVE      XCFYR         HDFYR
     C                   MOVE      XCCASE        HDCASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C     XCFND         ANDEQ     HCFND
     C     XCFYR         ANDEQ     HCFYR
     C                   WRITE     PHD0C
     C                   END                                                    END XCCO# EQ HCCO#
     C*
     C     HDKYP         READE     PHD01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt B Records
     C     H1KYP         CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELH1KEY     XCKEY
     C*
     C                   MOVEL(P)  H1KEY         CKEY
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  H1KEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         H1CO#
     C                   MOVE      XCFND         H1FND
     C                   MOVE      XCFYR         H1FYR
     C                   MOVE      XCCASE        H1CASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C     XCFND         ANDEQ     HCFND
     C     XCFYR         ANDEQ     HCFYR
     C                   WRITE     PH10C
     C                   END                                                    END XCCO# EQ HCCO#
     C*
     C     H1KYP         READE     PH101                                  60
     C                   END                                                    END 0 *IN60
     C*
     C                   ENDSR
     C***********************************************************************
     C*  CLNONE - No Selection: Claim Key Conversion
     C***********************************************************************
     C     CLNONE        BEGSR
     C*
     C*  Get Major Comment Records
     C     CMKYP         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELCMKEY     XCKEY
     C*
     C                   MOVEL(P)  CMKEY         CKEY             16
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CMKEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         CMCO#
     C                   MOVE      XCFND         CMFND
     C                   MOVE      XCFYR         CMFYR
     C                   MOVE      XCCASE        CMCASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C                   WRITE     PCM0C
     C*
     C*  Get Additional Comment Records
     C                   MOVE      XCCO#         ARCO#
     C                   MOVE      XCFND         ARFND
     C                   MOVE      XCFYR         ARFYR
     C                   MOVE      XCCASE        ARCASE
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         DOWEQ     '0'
     C                   WRITE     PAR0C
     C     ARKY          READE     PAR01                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END XCCO# EQ HCCO#
     C*
     C*  Read next Major Remarks record
     C     CMKYP         READE     PCM01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt A Records
     C     HDKYP         CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELHDKEY     XCKEY
     C*
     C                   MOVEL(P)  HDKEY         CKEY
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  HDKEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         HDCO#
     C                   MOVE      XCFND         HDFND
     C                   MOVE      XCFYR         HDFYR
     C                   MOVE      XCCASE        HDCASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C                   WRITE     PHD0C
     C                   END                                                    END XCCO# EQ HCCO#
     C*
     C     HDKYP         READE     PHD01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt B Records
     C     H1KYP         CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELH1KEY     XCKEY
     C*
     C                   MOVEL(P)  H1KEY         CKEY
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  H1KEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         H1CO#
     C                   MOVE      XCFND         H1FND
     C                   MOVE      XCFYR         H1FYR
     C                   MOVE      XCCASE        H1CASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C                   WRITE     PH10C
     C                   END                                                    END XCCO# EQ HCCO#
     C*
     C     H1KYP         READE     PH101                                  60
     C                   END                                                    END 0 *IN60
     C*
     C                   ENDSR
     C***********************************************************************
     C*  CLPART - Part (Fund only): Claim Key Conversion
     C***********************************************************************
     C     CLPART        BEGSR
     C*
     C*** BEGIN ADD *** 06/30/15 ******************************************
     C                   MOVE      QCCO#         XCCO#
     C                   MOVE      QCFND         XCFND
     C                   Z-ADD     0             XCFYR
     C                   Z-ADD     0             XCCASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*** END   ADD *** 06/30/15 ******************************************
     C*
     C*  Get Major Comment Records
     C*** BEGIN ADD *** 06/30/15 ******************************************
     C     CMKYF         SETLL     PCM01
     C*** END   ADD *** 06/30/15 ******************************************
     C*** BEGIN CHG *** 06/30/15 ******************************************
     C*****CMKYP         CHAIN     PCM01                              60
     C     CMKYP         READE     PCM01                                  60
     C*** END   CHG *** 06/30/15 ******************************************
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELCMKEY     XCKEY
     C*
     C                   MOVEL(P)  CMKEY         CKEY             16
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CMKEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         CMCO#
     C                   MOVE      XCFND         CMFND
     C                   MOVE      XCFYR         CMFYR
     C                   MOVE      XCCASE        CMCASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C     XCFND         ANDEQ     HCFND
     C                   WRITE     PCM0C
     C*
     C*  Get Additional Comment Records
     C                   MOVE      XCCO#         ARCO#
     C                   MOVE      XCFND         ARFND
     C                   MOVE      XCFYR         ARFYR
     C                   MOVE      XCCASE        ARCASE
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         DOWEQ     '0'
     C                   WRITE     PAR0C
     C     ARKY          READE     PAR01                                  61
     C                   END                                                    END 0 *IN61
     C*
     C*** BEGIN ADD *** 06/30/15 ******************************************
     C                   ELSE
     C                   LEAVE
     C*** END   ADD *** 06/30/15 ******************************************
     C                   ENDIF                                                  END XCCO# EQ HCCO#
     C*
     C*  Read next Major Remarks record
     C     CMKYP         READE     PCM01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt A Records
     C*** BEGIN ADD *** 06/30/15 ******************************************
     C     HDKYF         SETLL     PHD01
     C*** END   ADD *** 06/30/15 ******************************************
     C*** BEGIN CHG *** 06/30/15 ******************************************
     C*****HDKYP         CHAIN     PHD01                              60
     C     HDKYP         READE     PHD01                                  60
     C*** END   CHG *** 06/30/15 ******************************************
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELHDKEY     XCKEY
     C*
     C                   MOVEL(P)  HDKEY         CKEY
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  HDKEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         HDCO#
     C                   MOVE      XCFND         HDFND
     C                   MOVE      XCFYR         HDFYR
     C                   MOVE      XCCASE        HDCASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C     XCFND         ANDEQ     HCFND
     C                   WRITE     PHD0C
     C*** BEGIN ADD *** 06/30/15 ******************************************
     C                   ELSE
     C                   LEAVE
     C*** END   ADD *** 06/30/15 ******************************************
     C                   ENDIF                                                  END XCCO# EQ HCCO#
     C*
     C     HDKYP         READE     PHD01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt B Records
     C*** BEGIN ADD *** 06/30/15 ******************************************
     C     H1KYF         SETLL     PH101
     C*** END   ADD *** 06/30/15 ******************************************
     C*** BEGIN CHG *** 06/30/15 ******************************************
     C*****H1KYP         CHAIN     PH101                              60
     C     H1KYP         READE     PH101                                  60
     C*** END   CHG *** 06/30/15 ******************************************
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XCKEY
     C**********           MOVELH1KEY     XCKEY
     C*
     C                   MOVEL(P)  H1KEY         CKEY
     C*
     C                   TESTN                   CKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  H1KEY         XCKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        CKEY
     C                   MOVEL(P)  CKEY          XCKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XCCO#         H1CO#
     C                   MOVE      XCFND         H1FND
     C                   MOVE      XCFYR         H1FYR
     C                   MOVE      XCCASE        H1CASE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XCKEY         XXKEY
     C*
     C     XCCO#         IFEQ      HCCO#
     C     XCFND         ANDEQ     HCFND
     C                   WRITE     PH10C
     C*** BEGIN ADD *** 06/30/15 ******************************************
     C                   ELSE
     C                   LEAVE
     C*** END   ADD *** 06/30/15 ******************************************
     C                   ENDIF                                                  END XCCO# EQ HCCO#
     C*
     C     H1KYP         READE     PH101                                  60
     C                   END                                                    END 0 *IN60
     C*
     C                   ENDSR
     C***********************************************************************
     C*  EMFULL - Full (Employer selected)  Employer Key Conversion
     C***********************************************************************
     C     EMFULL        BEGSR
     C*
     C*  Get Major Comment Records
     C*
     C*  Setup DARTS file key
     C                   MOVE      QECO#         XECO#
     C                   MOVE      QEFND         XEFND
     C                   MOVE      QEEMP#        XEEMP#
     C                   MOVE      QEDIV         XEDIV
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XEKEY         XXKEY
     C*  Setup workfile's new fields
     C                   MOVE      QECO#         CMCO#
     C                   MOVE      QEFND         CMFND
     C                   MOVE      QEEMP#        CMEMP#
     C                   MOVE      QEDIV         CMDIV
     C*
     C     CMKYF         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PCM0E
     C*
     C*          Get Additional Comment Records
     C                   MOVE      QECO#         ARCO#
     C                   MOVE      QEFND         ARFND
     C                   MOVE      QEEMP#        AREMP#
     C                   MOVE      QEDIV         ARDIV
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         DOWEQ     '0'
     C                   WRITE     PAR0E
     C     ARKY          READE     PAR01                                  61
     C                   END                                                    END 0 *IN61
     C*
     C     CMKYF         READE     PCM01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt A Records
     C                   MOVE      QECO#         HDCO#
     C                   MOVE      QEFND         HDFND
     C                   MOVE      QEEMP#        HDEMP#
     C                   MOVE      QEDIV         HDDIV
     C     HDKYF         CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PHD0E
     C     HDKYF         READE     PHD01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt B Records
     C                   MOVE      QECO#         H1CO#
     C                   MOVE      QEFND         H1FND
     C                   MOVE      QEEMP#        H1EMP#
     C                   MOVE      QEDIV         H1DIV
     C     H1KYF         CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C                   WRITE     PH10E
     C     H1KYF         READE     PH101                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*
     C                   ENDSR
     C***********************************************************************
     C*  EMNONE - No Selection: Employer Key Conversion
     C***********************************************************************
     C     EMNONE        BEGSR
     C*
     C*  Get Major Comment Records
     C     CMKYP         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XEKEY
     C**********           MOVELCMKEY     XEKEY
     C*
     C                   MOVEL(P)  CMKEY         EKEY             20
     C*
     C                   TESTN                   EKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CMKEY         XEKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        EKEY
     C                   MOVEL(P)  EKEY          XEKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XECO#         CMCO#
     C                   MOVE      XEFND         CMFND
     C                   MOVE      XEEMP#        CMEMP#
     C                   MOVE      XEDIV         CMDIV
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XEKEY         XXKEY
     C*
     C     XECO#         IFEQ      HECO#
     C                   WRITE     PCM0E
     C*
     C*  Get Additional Comment Records
     C                   MOVE      CMCO#         ARCO#
     C                   MOVE      CMFND         ARFND
     C                   MOVE      CMEMP#        AREMP#
     C                   MOVE      CMDIV         ARDIV
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         DOWEQ     '0'
     C                   WRITE     PAR0E
     C     ARKY          READE     PAR01                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END XECO# EQ HECO#
     C*
     C*  Read next Major Remarks record
     C     CMKYP         READE     PCM01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt A Records
     C     HDKYP         CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XEKEY
     C**********           MOVELHDKEY     XEKEY
     C*
     C                   MOVEL(P)  HDKEY         EKEY             20
     C*
     C                   TESTN                   EKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  HDKEY         XEKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        EKEY
     C                   MOVEL(P)  EKEY          XEKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XECO#         HDCO#
     C                   MOVE      XEFND         HDFND
     C                   MOVE      XEEMP#        HDEMP#
     C                   MOVE      XEDIV         HDDIV
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XEKEY         XXKEY
     C*
     C     XECO#         IFEQ      HECO#
     C                   WRITE     PHD0E
     C                   END                                                    END XECO# EQ HECO#
     C*
     C     HDKYP         READE     PHD01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt B Records
     C     H1KYP         CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XEKEY
     C**********           MOVELH1KEY     XEKEY
     C*
     C                   MOVEL(P)  H1KEY         EKEY             20
     C*
     C                   TESTN                   EKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  H1KEY         XEKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        EKEY
     C                   MOVEL(P)  EKEY          XEKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XECO#         H1CO#
     C                   MOVE      XEFND         H1FND
     C                   MOVE      XEEMP#        H1EMP#
     C                   MOVE      XEDIV         H1DIV
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XEKEY         XXKEY
     C*
     C     XECO#         IFEQ      HECO#
     C                   WRITE     PH10E
     C                   END                                                    END XECO# EQ HECO#
     C*
     C     H1KYP         READE     PH101                                  60
     C                   END                                                    END 0 *IN60
     C*
     C                   ENDSR
     C***********************************************************************
     C*  EMPART - Part (Fund only): Employer Key Conversion
     C***********************************************************************
     C     EMPART        BEGSR
     C*
     C*  Get Major Comment Records
     C     CMKYP         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XEKEY
     C**********           MOVELCMKEY     XEKEY
     C*
     C                   MOVEL(P)  CMKEY         EKEY             20
     C*
     C                   TESTN                   EKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  CMKEY         XEKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        EKEY
     C                   MOVEL(P)  EKEY          XEKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XECO#         CMCO#
     C                   MOVE      XEFND         CMFND
     C                   MOVE      XEEMP#        CMEMP#
     C                   MOVE      XEDIV         CMDIV
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XEKEY         XXKEY
     C*
     C     XECO#         IFEQ      HECO#
     C     XEFND         ANDEQ     HEFND
     C                   WRITE     PCM0E
     C*
     C*  Get Additional Comment Records
     C                   MOVE      CMCO#         ARCO#
     C                   MOVE      CMFND         ARFND
     C                   MOVE      CMEMP#        AREMP#
     C                   MOVE      CMDIV         ARDIV
     C     ARKY          CHAIN     PAR01                              61
     C     *IN61         DOWEQ     '0'
     C                   WRITE     PAR0E
     C     ARKY          READE     PAR01                                  61
     C                   END                                                    END 0 *IN61
     C*
     C                   END                                                    END XECO# EQ HECO#
     C*
     C*  Read next Major Remarks record
     C     CMKYP         READE     PCM01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt A Records
     C     HDKYP         CHAIN     PHD01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XEKEY
     C**********           MOVELHDKEY     XEKEY
     C*
     C                   MOVEL(P)  HDKEY         EKEY             20
     C*
     C                   TESTN                   EKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  HDKEY         XEKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        EKEY
     C                   MOVEL(P)  EKEY          XEKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XECO#         HDCO#
     C                   MOVE      XEFND         HDFND
     C                   MOVE      XEEMP#        HDEMP#
     C                   MOVE      XEDIV         HDDIV
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XEKEY         XXKEY
     C*
     C     XECO#         IFEQ      HECO#
     C     XEFND         ANDEQ     HEFND
     C                   WRITE     PHD0E
     C                   END                                                    END XECO# EQ HECO#
     C*
     C     HDKYP         READE     PHD01                                  60
     C                   END                                                    END 0 *IN60
     C*
     C*  Get Database Fmt B Records
     C     H1KYP         CHAIN     PH101                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Break apart key to get separate fields
     C***  BEGIN CHG  ***  11/17/98  *********************************
     C**********           MOVE *BLANKS   XEKEY
     C**********           MOVELH1KEY     XEKEY
     C*
     C                   MOVEL(P)  H1KEY         EKEY             20
     C*
     C                   TESTN                   EKEY                 99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  H1KEY         XEKEY
     C                   ELSE
     C                   MOVEL     *ZEROS        EKEY
     C                   MOVEL(P)  EKEY          XEKEY
     C                   ENDIF                                                  *IN99=*ON
     C***  END   CHG  ***  11/17/98  *********************************
     C*
     C                   MOVE      XECO#         H1CO#
     C                   MOVE      XEFND         H1FND
     C                   MOVE      XEEMP#        H1EMP#
     C                   MOVE      XEDIV         H1DIV
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XEKEY         XXKEY
     C*
     C     XECO#         IFEQ      HECO#
     C     XEFND         ANDEQ     HEFND
     C                   WRITE     PH10E
     C                   END                                                    END XECO# EQ HECO#
     C*
     C     H1KYP         READE     PH101                                  60
     C                   END                                                    END 0 *IN60
     C*
     C                   ENDSR
     C***********************************************************************
