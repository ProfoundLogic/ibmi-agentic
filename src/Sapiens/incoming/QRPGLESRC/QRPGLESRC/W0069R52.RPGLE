     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0069R52 : EDI Claim Control Maintenance - A49.
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: EDI Claim Control Maintenance - A49.            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0069R5 - EDI Claim Control Maintenance                    *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/24/10  PROGRAMMER: C:232 PROJECT: 139WNN         *
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  04/24/19 C:180  Project: WC2669
     H*                  Add A49 start date override field.
     H*
     H*  08/19/10 C:189  Project: 139WLM  Priss 49
     H*                  Remove the two A49 setup fields added prior
     H*                  to 139WNN
     H*
     H*  04/05/10 C:189  Project: 139WLM
     H*                  EDI-Create diary when EDI Log History
     H*
     H*
     H*****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0069f52  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmixp     uf a e             disk
     F                                     rename(pix01:dbrcdf)

     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F***WMIXL     UF A E           K DISK
     FWMIXL     IF   E           K DISK    PREFIX(@)
     FSTCTL1    IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPP     IF   E             DISK
     F                                     RENAME(PUP01:PUP03)

     F*  GENERIC TABLE FILE
     FWTGTL     IF   E           K DISK
     F*  STATE MASTER
     FSTSTL3    IF   E           K DISK
      *
     FWMI1L     IF   E           K DISK
      *
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'EDI Claim Control Maintenance'
     D comapi          c                   'WA001R'
     D** BEG CHG *** 04/05/10 *****************************************
     D********** #ofscr          c                   1
     D #ofscr          c                   2
     D** END CHG *** 04/05/10 *****************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
      *
     C     UPRMPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
      *
    C*================================================================
    C*  KLIST definitions
    C*================================================================
      *
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF

      *
     C*   KEY LIST for Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY

     C*  EDI CLAIMS CONTROL FILE
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTRID
     C                   KFLD                    XXEDIF
      *
     C*  DARTS TABLE CODE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
      *
     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
     C     IXKEY2        klist
     C                   kfld                    XXCO#
     C                   kfld                    SCJRST
     C                   kfld                    SCTRID
     C                   kfld                    SCEDIF
      *
    C*================================================================
    C*  *LIKE DEFN statements
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CONAME        XXCONM
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    SCJRST        XXJRST
     C     *LIKE         DEFINE    IXTRID        XXTRID
     C     *LIKE         DEFINE    IXEDIF        XXEDIF
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    STKEY         XXKEY
     C     *LIKE         DEFINE    IXCSDT        XXCSDT
     C     *LIKE         DEFINE    IXCSDT        XXDSDT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      *
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
      *
     C*  If EDI Format not entered, check the fund file to see if it is required.
      *
     C                   IF        *IN90 <> '1' AND SCEDIF = *BLANKS
     C                   CALL      'WA075R'
     C                   PARM                    XXCO#
     C                   PARM      000           ##fnd
     C                   PARM      *BLANK        QQRTCD
     C     QQRTCD        IFEQ      'F'
     C                   SETON                                        556690
     C                   ENDIF
     C                   ENDIF
      *
     C*  The EDI Format ID Must be in the table file if entered.
     C                   IF        *IN90 <> '1'
     C                   IF        SCEDIF <> *BLANKS
     C                   MOVEL     'EDIF'        XXTBID
     C                   MOVEL     SCEDIF        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C                   IF        *IN60 = '0' AND GTSTAT = 'A'
     C                   ELSE
     C                   SETON                                        555690
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C* System should not allow the duplicate records...
     C                   IF        *IN90 <> '1'
     C                   IF        SCEDIF <> IXEDIF
      *
     C     IXKEY2        CHAIN     PIX01
     C                   IF        %FOUND(WMIXL)
     C                   SETON                                        555790
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *
     C*  THE ADJUSTER DARTS REMARK AND # OF DAYS ARE MUTUALLY
     C*  INCLUSIVE.
     C*
     C                   IF        *IN90 <> '1'
     C     SCAD9R        IFEQ      *BLANKS
     C     SCAD9M        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C**  VALIDATE THE ADJUSTER DARTS DIARY REMARK
     C*
     C  N90SCAD9R        IFNE      *BLANKS
     C                   MOVEL     SCAD9R        XXRMCD
     C     CTKY          CHAIN     PCT01                              21
     C                   IF        *IN21 = '1'
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  THE SUPERVISOR DARTS REMARK AND # OF DAYS ARE MUTUALLY
     C*  INCLUSIVE.
     C*
     C  N90SCSD9R        IFEQ      *BLANKS
     C     SCSD9M        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C**  VALIDATE THE SUPERVISOR DARTS DIARY REMARK
     C*
     C  N90SCSD9R        IFNE      *BLANKS
     C                   MOVEL     SCSD9R        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C                   IF        *IN60 = '1'
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN24
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  THE LATE REVIEWER STAFF CODE, # OF DAYS, AND DARTS REMARK ARE
     C*  MUTUALLY INCLUSIVE.
     C*
     C  N90SCLR9C        IFNE      *BLANKS
     C     SCLR9R        ANDEQ     *BLANKS
     C     SCLR9C        OREQ      *BLANKS
     C     SCLR9M        ANDNE     *ZEROS
     C     SCLR9C        OREQ      *BLANKS
     C     SCLR9C        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C**  VALIDATE THE LATE REVIEWER DARTS DIARY REMARK
     C*
     C  N90SCLR9R        IFNE      *BLANKS
     C                   MOVEL     SCLR9R        XXRMCD
     C     CTKY          CHAIN     PCT01                              26
     C                   IF        *IN26 = '1'
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C**  VALIDATE THE LATE REVIEWER STAFF CODE
     C*
     C  N90SCLR9C        IFNE      *BLANKS
     C                   MOVEL     SCLR9C        XXSTAF
     C     UPKY1         CHAIN     PUP02                              28
     C                   IF        *IN28 = '1'
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C*
     C**  VALIDATE THE ERROR, ADJUSTER DARTS DIARY REMARK
     C*
     C  N90SCEA9R        IFNE      *BLANKS
     C                   MOVEL     SCEA9R        XXRMCD
     C     CTKY          CHAIN     PCT01                              29
     C                   IF        *IN29 = '1'
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C**  Validate the Final Payment DARTS remark
     C*
     C  N90SCFNRC        IFNE      *BLANKS
     C                   MOVEL     SCFNRC        XXRMCD
     C     CTKY          CHAIN     PCT01                              31
     C                   IF        *IN31 = '1'
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN32
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Validate the Final Pmt MTC code, if entered.
     C*
     C  N90SCMTC1        IFNE      *BLANKS
     C                   MOVEL(P)  'SMTC'        XXTBID
     C                   MOVEL(P)  SCMTC1        XXKEY
     C     GTKY          CHAIN     PGT01                              43
     C*
     C                   IF        *IN43 = '1'
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     GTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN43
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C**  The '# of days after basis date A49 due' cannot be less than 0
     C*
     C  N90SCSDU9        IFLT      0
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     c** BEG DEL *** 08/19/10 *****************************************
     C***********  N90SCCECR        IFNE      *BLANKS
     **************      MOVEL     SCCECR        XXRMCD
     C***  CTKY ***      CHAIN     PCT01                              60
     C**** *IN60***      IFEQ      *ON
     C************       MOVE      '1'           *IN45
     C************       MOVE      '1'           *IN47
     C************       MOVE      '1'           *IN90
     C************       ENDIF
     C*
     C**** *IN60         IFEQ      *OFF
     C**** CTFL01        ANDNE     'N'
     C************       MOVE      '1'           *IN45
     C************       MOVE      '1'           *IN46
     C************       MOVE      '1'           *IN90
     C************       ENDIF
     C*
     C************       ENDIF
     C* N90SCCEIR        IFNE      *BLANKS
     C************       MOVEL     SCCEIR        XXRMCD
     C**   CTKY          CHAIN     PCT01                              60
     C**   *IN60         IFEQ      *ON
     C************       MOVE      '1'           *IN48
     C************       MOVE      '1'           *IN50
     C************       MOVE      '1'           *IN90
     C************       ENDIF
     C************
     C***  *IN60         IFEQ      *OFF
     C**   CTFL01        ANDNE     'Y'
     C************       MOVE      '1'           *IN48
     C************       MOVE      '1'           *IN49
     C************       MOVE      '1'           *IN90
     C************       ENDIF
      ************
     C************       ENDIF
     c** END DEL **   08/19/10 ****************************************
      *
      *** Beg Add *** 04/24/19 ****************************************
     C                   IF        *IN90 <> '1'
     C                   MOVE      *OFF          *IN61
     C     SCA49D        IFGT      0
     C                   Z-ADD     SCA49D        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCA49D
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN61
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *** End Add *** 04/24/19 ****************************************
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     c** BEG ADD *** 04/05/10 *****************************************
     c                   move      '0'           *in90
     C  N90SCCECR#       IFNE      *BLANKS
     C                   MOVEL     SCCECR#       XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     CTFL01        ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C  N90SCCTAR        IFNE      *BLANKS
     C                   MOVEL     SCCTAR        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     CTFL01        ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C  N90SCCRER        IFNE      *BLANKS
     C                   MOVEL     SCCRER        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     CTFL01        ANDNE     'Y'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C  N90SCCEIR#       IFNE      *BLANKS
     C                   MOVEL     SCCEIR#       XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     CTFL01        ANDNE     'Y'
     C                   MOVE      '1'           *IN29
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C  N90SCCTRR        IFNE      *BLANKS
     C                   MOVEL     SCCTRR        XXRMCD
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     CTFL01        ANDNE     'Y'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C                   ENDIF
    C*================================================================
     c** END ADD *** 04/05/10 *****************************************
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
      *
     C     @@RRN         CHAIN     DBRCDF
     C                   IF        %FOUND
     C                   MOVEL     IXJRST        SCJRST
      *
     C                   MOVEL     'A49'         SCTRID
      *
     C                   MOVE      *BLANKS       SCTRDS
     C     SCTRID        IFNE      *BLANK
     C                   MOVEL     'TSET'        XXTBID
     C                   MOVEL     SCTRID        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C                   IF        *IN60 = '0' AND GTSTAT = 'A'
     C                   MOVEL     GTDESC        SCTRDS
     C                   ENDIF
     C                   ENDIF
      *
     C                   EVAL      SCEDIF = IXEDIF
     C                   EVAL      SCAD9R = IXADDR
     C                   EVAL      SCAD9M = IXADRC
     C                   EVAL      SCSD9R = IXSDDR
     C                   EVAL      SCSD9M = IXSDIM
     C                   EVAL      SCLR9R = IXDDRC
     C                   EVAL      SCLR9M = IXLRIM
     C                   EVAL      SCLR9C = IXLRSC
     C                   EVAL      SCEA9R = IXEADC
     C                   EVAL      SCMTC1 = IXMTC1
     C                   EVAL      SCSDU9 = IXSDUE
     c** BEG DEL *** 08/19/10 *****************************************
     C************       EVAL      SCCECR = IXCECR
     C************       EVAL      SCCEIR = IXCEIR
     c** END DEL *** 08/19/10 *****************************************
      *
     C                   ELSE
      *
     C                   MOVEL     IXJRST        SCJRST
     C                   MOVEL     'A49'         SCTRID
      *
     C                   EVAL      SCEDIF = IXEDIF
     C                   EVAL      SCAD9R = *blanks
     C                   EVAL      SCAD9M = *zeros
     C                   EVAL      SCSD9R = *blanks
     C                   EVAL      SCSD9M = *zeros
     C                   EVAL      SCLR9R = *blanks
     C                   EVAL      SCLR9M = *zeros
     C                   EVAL      SCLR9C = *blanks
     C                   EVAL      SCEA9R = *blanks
     C                   EVAL      SCMTC1 = *blanks
     C                   EVAL      SCSDU9 = *zeros
     c** BEG DEL ***  08/19/10 ****************************************
     C************       EVAL      SCCECR = *blanks
     C************       EVAL      SCCEIR = *blanks
     c** END DEL ***  08/19/10 ****************************************
     C                   ENDIF
      *
     c** BEG ADD *** 04/05/10 *****************************************
     C* Screen #4....
      *
     C                   MOVEL     IXCRER        SCCRER
     C                   MOVEL     IXCECR        SCCECR#
     C                   MOVEL     IXCTAR        SCCTAR
     C*
     C                   MOVEL     IXCEIR        SCCEIR#
     C                   MOVEL     IXCTRR        SCCTRR
      *
     c** END ADD *** 04/05/10 *****************************************
     C** Begin Add *** 04/24/19 ***********************************
     C                   Z-ADD     IXA49D        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCA49D
     C** End   Add *** 04/24/19 ***********************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   Z-ADD     XXCO#         IXCO#
     C                   MOVE      SCJRST        IXJRST
     C                   MOVE      SCTRID        IXTRID
      *
     c                   EVAL      IXEDIF =  SCEDIF
     c                   EVAL      IXADDR =  SCAD9R
     c                   EVAL      IXADRC =  SCAD9M
     c                   EVAL      IXSDDR =  SCSD9R
     c                   EVAL      IXSDIM =  SCSD9M
     c**                 EVAL      IXSEIN =  SCSEIN
     c                   EVAL      IXDDRC =  SCLR9R
     c                   EVAL      IXLRIM =  SCLR9M
     c                   EVAL      IXLRSC =  SCLR9C
     c                   EVAL      IXEADC =  SCEA9R
     c                   EVAL      IXMTC1 =  SCMTC1
     c                   EVAL      IXSDUE =  SCSDU9
     c** BEG DEL *** 08/19/10 *****************************************
     c********           EVAL      IXCECR =  SCCECR
     c********           EVAL      IXCEIR =  SCCEIR
     c** END DEL *** 08/19/10 *****************************************
      *
     C                   MOVE      Q1PGM         IXPGM
     C                   MOVE      Q1USER        IXUSER
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        IXTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXDT
      *
     C     IXKEY2        CHAIN     PI101
     C                   IF        %FOUND(WMI1L)
     C                   MOVEL     'Y'           IXAUTO
     C                   ELSE
     C                   MOVEL     *BLANKS       IXAUTO
     C                   ENDIF
      *
     c** BEG ADD *** 04/05/10 *****************************************
     C* Screen #4....
      *
     C                   MOVEL     SCCRER        IXCRER
     C                   MOVEL     SCCECR#       IXCECR
     C                   MOVEL     SCCTAR        IXCTAR
     C*
     C                   MOVEL     SCCEIR#       IXCEIR
     C                   MOVEL     SCCTRR        IXCTRR
      *
     C                   MOVE      Q1PGM         IXPGM
     C                   MOVE      Q1USER        IXUSER
     C                   TIME                    DSDTTM
     C                   MOVE      DSTIME        IXTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXDT
      *
     c** END ADD *** 04/05/10 *****************************************
      ** Begin Add *** 04/24/19 ***********************************
     C                   Z-ADD     SCA49D        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXA49D
      ** End   Add *** 04/24/19 ***********************************
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
      *
     C* Process search key
      *
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   endcs
      *
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
      *
     C                   MOVEL     *BLANKS       QQRTC             1
     C                   MOVEL     'W0069R52'    Q1PGM
      *
     C     q1user        chain     pup01
     C                   IF        %FOUND
     C                   EVAL      XXCO# = UPCO#
     c                   endif
      *
     C                   EVAL      SCEDIF = *blanks
     C                   EVAL      SCAD9R = *blanks
     C                   EVAL      SCAD9M = *zeros
     C                   EVAL      SCSD9R = *blanks
     C                   EVAL      SCSD9M = *zeros
     C                   EVAL      SCLR9R = *blanks
     C                   EVAL      SCLR9M = *zeros
     C                   EVAL      SCLR9C = *blanks
     C                   EVAL      SCEA9R = *blanks
     C                   EVAL      SCMTC1 = *blanks
     C                   EVAL      SCSDU9 = *zeros
     c** BEG DEL *** 08/19/10 *****************************************
     C************       EVAL      SCCECR = *blanks
     C************       eval      SCCEIR = *blanks
     c** END DEL *** 08/19/10 *****************************************
      *
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
     C                   MOVE      *ON           OUTFLG
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
      *
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
      *
     C                   EVAL      IXSTAT = 'A'
     C                   MOVE      Q1USER        IXIUSR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXIDT
      *
     C                   write     dbrcdf
      *
     C                   feod      wmixp
     C                   MOVE      *ON           OUTFLG
      *
     C                   endsr
    C*================================================================
    C* KJDO - Processing for F10
    C*================================================================
     C     kjdo          begsr
      *
     C                   SETOFF                                       553465
     C                   SETOFF                                       353637
     C                   SETOFF                                       383940
     C                   MOVEA     '00000000'    *IN(41)
      *
      * Search EDI Format ID...
      *
     C     HFLD          IFEQ      'SCEDIF'
      *
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      'EDIF'        QQTBID
     C                   PARM      *BLANK        QQKEY
     C                   PARM      ' '           QQRTCD            1
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCEDIF
     C                   MOVE      *ON           *IN65
     C                   ELSE
     C                   MOVE      *ON           *IN55
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *A49 adjuster diary code search
     C     HFLD          IFEQ      'SCAD9R'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C                   SELECT
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCAD9R
     C                   MOVE      *ON           *IN34
     C                   OTHER
     C                   MOVE      *ON           *IN65
     C                   ENDSL
     C*
     C                   ENDIF
      *
      * A49 supervisor diary code search
      *
     C     HFLD          IFEQ      'SCSD9R'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C                   SELECT
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCSD9R
     C                   MOVE      *ON           *IN36
     C                   OTHER
     C                   MOVE      *ON           *IN35
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C*  A49 EDI reviewer diary code search
      *
     C     HFLD          IFEQ      'SCLR9R'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
      *
     C                   SELECT
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCLR9R
     C                   MOVE      *ON           *IN38
     C                   OTHER
     C                   MOVE      *ON           *IN37
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C*  A49 EDI reviewer staff code search
      *
     C     HFLD          IFEQ      'SCLR9C'
     C*
     C                   MOVE      *BLANKS       XXSTAF
     C                   CALL      'S0005R'      UPRMPL
      *
     C     ##RRN         CHAIN     PUP03                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPSTAF        SCLR9C
     C                   MOVE      '1'           *IN40
     C                   ELSE
     C                   MOVE      '1'           *IN39
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C*  A49 Acknowledgement error adjuster diary code search
     C*
     C     HFLD          IFEQ      'SCEA9R'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C                   SELECT
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCEA9R
     C                   MOVE      *ON           *IN41
     C                   OTHER
     C                   MOVE      *ON           *IN40
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD = EA9R
     C*
     C*  A49 Final payment DARTS code search
     C*
     C     HFLD          IFEQ      'SCFNRC'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C                   SELECT
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCFNRC
     C                   MOVE      *ON           *IN42
     C                   OTHER
     C                   MOVE      *ON           *IN41
     C                   ENDSL
     C*
     C                   ENDIF                                                  HFLD = FNRC
     C*
     C*  A49 Final payment MTC code search
     C*
     C     HFLD          IFEQ      'SCMTC1'
     C*
     C                   MOVEL(P)  'SMTC'        XXTBID
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM                    QQRTC
      *
     C                   IF        XXKEY <> ' '
     C                   MOVEL     XXKEY         SCMTC1
     C                   MOVEL     *ON           *IN44
     C                   ELSE
     C                   MOVEL     *ON           *IN42
     C                   ENDIF
     C*
     C                   ENDIF                                                  HFLD = MTC1
     C*
     C*  "EC" SEARCH
     C*
     c** BEG DEL *** 08/19/10 *****************************************
     C**** HFLD ****     IFEQ      'SCCECR'
     C************       CALL      'SC305R'
     C************       PARM      '2'           ##CALL            1
     C**** *INKC         PARM                    ##KC              1
     C**** *INKL         PARM                    ##KL              1
     C************       PARM      *BLANKS       ##RMCD            5
     C************       PARM      *BLANKS       ##DESC           40
     C************       PARM      *BLANK        ##RTCD            1
     C************
     C***  *INKL         IFEQ      *ON
     C************       MOVEL     *ON           *IN45
     C************       ENDIF
     C*
     C***  ##RMCD        IFNE      *BLANKS
     C************       MOVEL     ##RMCD        SCCECR
     C************       MOVEL     *ON           *IN48
     C************       ENDIF
     C************
     C************       ENDIF
     C*
     C*  "EI" SEARCH
     C*
     C***  HFLD****      IFEQ      'SCCEIR'
     C************       CALL      'SC305R'
     C************       PARM      '2'           ##CALL            1
     C**** *INKC**       PARM                    ##KC              1
     C***  *INKL**       PARM                    ##KL              1
     C************       PARM      *BLANKS       ##RMCD            5
     C************       PARM      *BLANKS       ##DESC           40
     C************       PARM      *BLANK        ##RTCD            1
     C************
     C**** *INKL**       IFEQ      *ON
     C************       MOVEL     *ON           *IN48
     C************       ENDIF
      *
     C***  ##RMCD        IFNE      *BLANKS
     C************       MOVEL     ##RMCD        SCCEIR
     C************       ENDIF
      *
     C**************     ENDIF
     c** END DEL *** 08/19/10 *****************************************
     c** BEG ADD *** 04/05/10 *****************************************
      *
     C*  Process SCR02 searches
     C*
     C*  A49    SEARCH
     C*
     C*  "EC" SEARCH
     C*
     C     HFLD          IFEQ      'SCCECR#'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     ##RMCD        IFNE      *BLANKS
     C                   MOVEL     ##RMCD        SCCECR#
     C                   MOVEL     *ON           *IN46
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  "TA" SEARCH
     C*
     C     HFLD          IFEQ      'SCCTAR'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     *ON           *IN46
     C                   ENDIF
     C*
     C     ##RMCD        IFNE      *BLANKS
     C                   MOVEL     ##RMCD        SCCTAR
     C                   MOVEL     *ON           *IN48
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  "RE" SEARCH
     C*
     C     HFLD          IFEQ      'SCCRER'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     *ON           *IN48
     C                   ENDIF
     C*
     C     ##RMCD        IFNE      *BLANKS
     C                   MOVEL     ##RMCD        SCCRER
     C                   MOVEL     *ON           *IN50
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  "EI" SEARCH
     C*
     C     HFLD          IFEQ      'SCCEIR#'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     *ON           *IN50
     C                   ENDIF
     C*
     C     ##RMCD        IFNE      *BLANKS
     C                   MOVEL     ##RMCD        SCCEIR#
     C                   MOVEL     *ON           *IN52
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  "TR" SEARCH
     C*
     C     HFLD          IFEQ      'SCCTRR'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     *ON           *IN52
     C                   ENDIF
     C*
     C     ##RMCD        IFNE      *BLANKS
     C                   MOVEL     ##RMCD        SCCTRR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     c** END ADD *** 04/05/10 *****************************************
     C                   endsr
