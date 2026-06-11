     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC315R01 - TABLE BACKUP -  UPDATE/DISPLAY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates and displays Table backup file     **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     SC315R - Work with Table Backup                         **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   02/02/98      PROJECT: DR049                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H**   02/26/99  C:03  SUPP# 16862
     H**             NOT ALL ENTRIES FOR THE UDXX FIELDS ARE ACTUALLY
     H**             DATES.  SOME CLIENT USE THESE FIELDS FOR NUMERIC
     H**             NON DECIMAL ENTRIES.
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      09   Reserved for maintenance/inquiry conditions
     H**      40   Add mode
     H**      60   Used for chaining
     H**   69-78   SCR01 SC40002 Error - Custom Code not valid (CSID)
     H**      71   Used to SHOW A UA VALUE
     H**      72   Used to SHOW A UD VALUE
     H**      73   Used to SHOW A UN VALUE
     H**      74   Used to SHOW A UB VALUE
     H**      80   SCR01 SCM0503 Error - Required field (UA)
     H**      81   SCR01 SC40002 Error - Duplicate record (UA)
     H**      82   SCR01 SCM0503 Error - Required field (UD)
     H**      83   SCR01 SC40002 Error - Duplicate record (UD)
     H**      84   SCR01 SCM0503 Error - Required field (UN)
     H**      85   SCR01 SC40002 Error - Duplicate record (UN)
     H**      86   SCR01 SCM0503 Error - Required field (UB)
     H**      87   SCR01 SC40002 Error - Duplicate record (UB)
     H**      88   SCR01 SCM0503 Error - Required field (DS40)
     H**      90   Used as general purpose error indicator
     H**      99   Used when chaining to file being updated
     H**
     H*****************************************************************
     FSC315F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FSTTBP     UF A E             DISK
     F                                     RENAME(PTB01:DBRCDF)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL1  IF  E           K        DISK
     FSTTBL5    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     RENAME(PTB01:DBRCD1)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL2  IF  E           K        DISK
     FSTTBL6    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     RENAME(PTB01:DBRCD2)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL3  IF  E           K        DISK
     FSTTBL7    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     RENAME(PTB01:DBRCD3)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********STTBL   IF  E           K        DISK
     FSTTBL4    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     RENAME(PTB01:DBRCD4)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********SMCGL   IF  E           K        DISK
     FSMCGL3    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  02/01/01  *********************************
     F*
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Table Backup Informa-
     D                                     tion')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*
     D                 DS
     D  DA                     1    100
     D                                     DIM(10)                              UAXX FLD HDGS
     D  CGDA01                 1     10
     D  CGDA02                11     20
     D  CGDA03                21     30
     D  CGDA04                31     40
     D  CGDA05                41     50
     D  CGDA06                51     60
     D  CGDA07                61     70
     D  CGDA08                71     80
     D  CGDA09                81     90
     D  CGDA10                91    100
     D                 DS
     D  DD                     1     50
     D                                     DIM(5)                               UDXX FLD HDGS
     D  CGDD01                 1     10
     D  CGDD02                11     20
     D  CGDD03                21     30
     D  CGDD04                31     40
     D  CGDD05                41     50
     D                 DS
     D  DN                     1     50
     D                                     DIM(5)                               UNXX FLD HDGS
     D  CGDN01                 1     10
     D  CGDN02                11     20
     D  CGDN03                21     30
     D  CGDN04                31     40
     D  CGDN05                41     50
     D*
     D                 DS
     D  DB                     1     60
     D                                     DIM(6)                               UBXX FLD HDGS
     D  CGDB01                 1     10
     D  CGDB02                11     20
     D  CGDB03                21     30
     D  CGDB04                31     40
     D  CGDB05                41     50
     D  CGDB06                51     60
     D*
     D                 DS
     D  XXFLD                  1      4
     D  XXFLDP                 1      2
     D  XXFLDS                 3      4

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CSID            2
     C                   PARM                    @@FLD             4
     C                   PARM                    @@HDG            35
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM                    @@GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*   KEY LIST FOR CUSTOM GROUP ID MASTER FILE
     C*
     C     CGKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C*
     C*   KEY LIST FOR UA FIELDS
     C     TAKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C                   KFLD                    KKFLD
     C                   KFLD                    KKVUA
     C*
     C*   KEY LIST FOR UD FIELDS
     C     TDKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C                   KFLD                    KKFLD
     C                   KFLD                    KKVUD
     C*
     C*   KEY LIST FOR UN FIELDS
     C     TNKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C                   KFLD                    KKFLD
     C                   KFLD                    KKVUN
     C*
     C*   KEY LIST FOR UB FIELDS
     C     TBKY          KLIST
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKCSID
     C                   KFLD                    KKFLD
     C                   KFLD                    KKVUB
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    TBGRP         KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    TBCSID        KKCSID
     C     *LIKE         DEFINE    TBFLD         KKFLD
     C     *LIKE         DEFINE    TBVUB         KKVUB
     C     *LIKE         DEFINE    TBVUA         KKVUA
     C     *LIKE         DEFINE    TBVUD         KKVUD
     C     *LIKE         DEFINE    TBVUN         KKVUN
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    DRFL11        USEGRP
     C     *LIKE         DEFINE    SCGRP         @@GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   DO
     C*
     C     XXFLDP        IFEQ      'UA'
     C*
     C* CHECK VALUE 1 - REQUIRED
     C*
     C  N90SCVAL1        IFEQ      *BLANKS
     C                   SETON                                        908071
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN UPDATE MODE  ****
     C     SCVAL1        IFNE      CPVAL1
     C                   MOVEL     SCVAL1        KKVUA
     C     TAKY          CHAIN     DBRCD4                             60
     C  N60              SETON                                        819071
     C  N60              LEAVE                                                              s
     C                   ENDIF                                                  NE ORIGINALPATH
     C                   ENDIF                                                  SCPATH = BLANKS
     C*
     C                   ELSE
     C*
     C     XXFLDP        IFEQ      'UD'
     C*
     C* CHECK VALUE 2 - REQUIRED
     C*
     C  N90SCVAL2        IFEQ      *ZEROS
     C                   SETON                                        908272
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN UPDATE MODE  ****
     C     SCVAL2        IFNE      CPVAL2
     C                   Z-ADD     SCVAL2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKVUD
     C** BGN ADD  02/26/99
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     SCVAL2        KKVUD
     C                   END
     C** END ADD  02/26/99
     C     TDKY          CHAIN     DBRCD2                             60
     C  N60              SETON                                        839072
     C  N60              LEAVE                                                              s
     C                   ENDIF                                                  NE ORIGINALPATH
     C                   ENDIF                                                  SCPATH = BLANKS
     C*
     C                   ELSE
     C*
     C     XXFLDP        IFEQ      'UN'
     C*
     C* CHECK VALUE 3 - REQUIRED
     C*
     C  N90SCVAL3        IFEQ      *ZEROS
     C                   SETON                                        908473
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN UPDATE MODE  ****
     C     SCVAL3        IFNE      CPVAL3
     C                   MOVEL     SCVAL3        KKVUN
     C     TNKY          CHAIN     DBRCD3                             60
     C  N60              SETON                                        859073
     C  N60              LEAVE                                                              s
     C                   ENDIF                                                  NE ORIGINALPATH
     C                   ENDIF                                                  SCPATH = BLANKS
     C*
     C                   ELSE
     C*
     C     XXFLDP        IFEQ      'UB'
     C*
     C* CHECK VALUE 4 - REQUIRED
     C*
     C  N90SCVAL4        IFEQ      *BLANKS
     C                   SETON                                        908674
     C                   LEAVE                                                              s
     C                   ELSE                                                               s
     C****  TO PREVENT DUPLICATE RECORD WHILE IN UPDATE MODE  ****
     C     SCVAL4        IFNE      CPVAL4
     C                   MOVEL     SCVAL4        KKVUB
     C     TBKY          CHAIN     DBRCD1                             60
     C  N60              SETON                                        879074
     C  N60              LEAVE                                                              s
     C                   ENDIF                                                              ATH
     C                   ENDIF                                                              NKS
     C*
     C                   ENDIF                                                              NKS
     C                   ENDIF                                                              NKS
     C                   ENDIF                                                              NKS
     C                   ENDIF                                                              NKS
     C*
     C* CHECK DESCRIPTION
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        9088
     C                   LEAVE                                                              s
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   TIME                    DFDTTM
     C                   MOVEL     @@FLD         SCFLD
     C                   MOVEL     SCFLD         XXFLD
     C                   MOVEL     'A'           SCSTAT
     C                   MOVEL(P)  @@CSID        SCCSID
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** SCCSID    CHAINPCG01                60
     C     CGKY          CHAIN     PCG01                              60
     C***  END   CHG  ***  02/01/01  *********************************
     C                   MOVEL     CGRFMT        SCFRMT
     C                   MOVEL     CGDESC        SCCSDS
     C                   MOVE      XXFLDS        W                 2 0
     C     XXFLDP        CASEQ     'UA'          UASR
     C     XXFLDP        CASEQ     'UD'          UDSR
     C     XXFLDP        CASEQ     'UN'          UNSR
     C     XXFLDP        CASEQ     'UB'          UBSR
     C                   ENDCS
     C                   MOVE      *BLANKS       SCDESC
     C     *IN40         IFEQ      *OFF
     C                   MOVEL     TBDESC        SCDESC
     C                   MOVEL     TBSTAT        SCSTAT
     C     XXFLDP        IFEQ      'UA'
     C                   MOVEL     TBVUA         SCVAL1
     C                   ELSE
     C     XXFLDP        IFEQ      'UD'
     C                   Z-ADD     TBVUD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVAL2
     C** BGN ADD  02/26/99
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     TBVUD         SCVAL2
     C                   END
     C** END ADD  02/26/99
     C                   ELSE
     C     XXFLDP        IFEQ      'UN'
     C                   MOVEL     TBVUN         SCVAL3
     C                   ELSE
     C     XXFLDP        IFEQ      'UB'
     C                   MOVEL     TBVUB         SCVAL4
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   DRFL11
     C     'SC'          CHAIN     SDR01                              60
     C     DRFL11        COMP      'Y'                                    41
     C                   MOVEL     DRFL11        USEGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* UASR - UA INITIALIZE SUBROUTINE
     C*================================================================
     C     UASR          BEGSR
     C                   MOVEL     '10   A'      SCATTR
     C                   MOVEL     DA(W)         SCHDG
     C                   MOVEL     *ON           *IN71
     C                   MOVEL     *BLANKS       SCVAL1
     C                   MOVEL     TBVUA         CPVAL1           10
     C                   ENDSR
     C*================================================================
     C* UDSR - UD INITIALIZE SUBROUTINE
     C*================================================================
     C     UDSR          BEGSR
     C                   MOVEL     ' 8/0 N'      SCATTR
     C                   MOVEL     DD(W)         SCHDG
     C                   MOVEL     *ON           *IN72
     C                   MOVEL     *BLANKS       SCVAL2
     C                   Z-ADD     TBVUD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CPVAL2            8 0
     C** BGN ADD  02/26/99
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     TBVUD         CPVAL2
     C                   END
     C** END ADD  02/26/99
     C                   ENDSR
     C*================================================================
     C* UNSR - UN INITIALIZE SUBROUTINE
     C*================================================================
     C     UNSR          BEGSR
     C                   MOVEL     ' 9/2 N'      SCATTR
     C                   MOVEL     DN(W)         SCHDG
     C                   MOVEL     *ON           *IN73
     C                   MOVEL     *BLANKS       SCVAL3
     C                   MOVEL     TBVUN         CPVAL3            9 2
     C                   ENDSR
     C*================================================================
     C* UBSR - UB INITIALIZE SUBROUTINE
     C*================================================================
     C     UBSR          BEGSR
     C                   MOVEL     '35   A'      SCATTR
     C                   MOVEL     DB(W)         SCHDG
     C                   MOVEL     *ON           *IN74
     C                   MOVEL     *BLANKS       SCVAL4
     C                   MOVEL     TBVUB         CPVAL4           35
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVEL     SCGRP         TBGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     SCCSID        TBCSID
     C                   MOVEL     SCFLD         TBFLD
     C                   MOVEL     SCDESC        TBDESC
     C                   MOVEL     SCSTAT        TBSTAT
     C     XXFLDP        IFEQ      'UA'
     C                   MOVEL     SCVAL1        TBVUA
     C                   ELSE
     C     XXFLDP        IFEQ      'UD'
     C                   Z-ADD     SCVAL2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TBVUD
     C** BGN ADD  02/26/99
     C     SRERR         IFEQ      '1'
     C                   Z-ADD     SCVAL2        TBVUD
     C                   END
     C** END ADD  02/26/99
     C                   ELSE
     C     XXFLDP        IFEQ      'UN'
     C                   MOVEL     SCVAL3        TBVUN
     C                   ELSE
     C     XXFLDP        IFEQ      'UB'
     C                   MOVEL     SCVAL4        TBVUB
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      MVUPDB
     C                   ENDSR
     C*================================================================
     C* MVUPDB - Move update fields to data base fields
     C*================================================================
     C     MVUPDB        BEGSR
     C                   MOVEL     Q1USER        TBUSER
     C                   MOVEL     Q1PGM         TBPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TBDT
     C                   MOVEL     DSTIME        TBTIME
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVEL     @@FLD         KKFLD
     C                   MOVEL     @@FLD         XXFLD
     C                   MOVEL     @@CSID        KKCSID
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   SCGRP
     C*
     C     Q1PRMS        IFGE      8
     C                   MOVE      @@GRP         SCGRP
     C                   ENDIF                                                  Q1PRMS>=8
     C*
     C                   MOVE      SCGRP         KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C     XXFLDP        IFEQ      'UA'
     C                   MOVEL     @@HDG         KKVUA
     C                   ELSE
     C     XXFLDP        IFEQ      'UD'
     C                   MOVEL     @@HDG         KKVUD
     C                   ELSE
     C     XXFLDP        IFEQ      'UN'
     C                   MOVEL     @@HDG         KKVUN
     C                   ELSE
     C     XXFLDP        IFEQ      'UB'
     C                   MOVEL     @@HDG         KKVUB
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*          MODE 'S' - Change status
     C*
     C     @@MODE        IFEQ      'S'
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     TBSTAT        IFEQ      'I'
     C                   MOVE      'A'           TBSTAT
     C                   ELSE
     C                   MOVE      'I'           TBSTAT
     C                   ENDIF
     C                   EXSR      MVUPDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C                   RETURN
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        TBIUSR
     C                   MOVEL     Q1PGM         TBIPGM
     C                   MOVEL     DFTIME        TBITIM
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         TBIDT
     C                   WRITE     DBRCDF
     C                   FEOD      STTBP
     C                   ENDSR
