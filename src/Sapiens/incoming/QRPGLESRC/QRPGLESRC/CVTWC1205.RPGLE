     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC. 2005
     H*****************************************************************
     H*
     H*----------------------------------------------------------------
     H/TITLE MOVE ACTIVITY STATUS DATA FROM  WMEMP TO WDELP
     H*                                    EMASTS ==> ELASCD
     H*----------------------------------------------------------------
     H*****************************************************************
     H* Description:  IF WMEMP/EMASTS not blank, CHAIN and update
     H*                 new data field in WDELP -- all fund years
     H*                 for this employer.
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWDELL     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C**************************************************************************
     C*-------------------------------------------------------------------------
     C*  Employer file drives.
     C                   READ      PEM01                                  60
     C*
     C*  Start of main loop
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             NOEMP             7 0
     C*
     C     EMASTS        IFEQ      *BLANK
     C                   ADD       1             BLANS             7 0
     C                   ELSE
     C*
     C*  Get next company.
     C     EMCO#         IFNE      XXCO#
     C                   ADD       1             NOCOS             7 0
     C                   Z-ADD     EMCO#         XXCO#
     C     COKY          CHAIN     PCO01                              65
     C     *IN65         IFEQ      *ON
     C                   MOVEL(P)  'No Co. '     CONAME
     C                   ENDIF                                                  e- 65 ON
     C                   ENDIF                                                  e- CO#<>CO
     C*
     C*  Process all detail for this employer.
     C     ELKY          CHAIN     PEL01                              61
     C     *IN61         IFEQ      *ON
     C                   ADD       1             NODEL             7 0
     C                   ELSE
     C                   Z-ADD     0             EMPYR
     C*
     C     *IN61         DOWEQ     *OFF
     C*
     C*  Add to totals
     C                   ADD       1             TOTYR             7 0
     C                   ADD       1             EMPYR             7 0
     C*
     C*  Update
     C                   MOVEL(P)  EMASTS        ELASCD
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        ELUSER
     C                   MOVEL(P)  Q1PGM         ELPGM
     C                   Z-ADD     DSTIME        ELTIME
     C                   Z-ADD     XXDT          ELDT
     C                   UPDATE    PEL01
     C                   EXSR      DPRT
     C*
     C     ELKY          READE     PEL01                                  61
     C                   ENDDO                                                  e-61 DOW
     C                   ENDIF                                                  e-61 = ON
     C*
     C                   ENDIF                                                  e-ASTS=BLANK
     C*
     C*  Read next employer
     C                   READ      PEM01                                  60
     C                   ENDDO
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    TOTS
     C                   SETON                                        LR
     C*--------------------------------------------------------------
     C*
     C*--------------------------------------------------------------
     C     DPRT          BEGSR
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ENDSR
     C*
     C*--------------------------------------------------------------
     C     *INZSR        BEGSR
     C*-------------------------------------------------------------------------
     C* ---  K  E  Y  ---  L  I  S  T  S  ----------------------------------
     C*
     C*  Key list for employer detail -WDELP.
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C*  Key list for the Company file
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C* ---- P A R A M E T E R --- L I S T S --------------------------
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    ELDT          XXDT
     C*
     C* --------------------------------------------------------------
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   MOVE      *ON           *INOF
     C*
     C                   ENDSR
     C*
     C*
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           44 'Update WDELP: activity'
     O                                           66 ' status code          '
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           22 'Grp Employer #    Div '
     O                                           44 'FYR  YR nbr ST Emp RR#'
     O          E            DET            1
     O                       ELFND         4      3
     O                       ELEMP#        4     14
     O                       ELDIV         4     21
     O                       ELFYR         4     25
     O                       EMPYR         4     33
     O                       ELASCD              36
     O                       NOEMP         4     44
     O          E            TOTS           1
     O                                           22 'Number of Employers . '
     O                                           33 '. . . . . .'
     O                       NOEMP         J     44
     O*
     O          E            TOTS           1
     O                                           22 '     SKIP - act status'
     O                                           33 ' blank  . .'
     O                       BLANS         J     44
     O*
     O          E            TOTS           1
     O                                           22 '     SKIP - no details'
     O                                           33 '  . . . . .'
     O                       NODEL         J     44
     O          E            TOTS           1
     O                                           22 'Number of detail years'
     O                                           33 ' updated  .'
     O                       TOTYR         J     44
