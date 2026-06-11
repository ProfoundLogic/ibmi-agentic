     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE W0069R51 : EDI Claim Control Maintenance - 148.
     H*****************************************************************
     H*****************************************************************
     H*  DESCRIPTION: EDI Claim Control Maintenance - 148.            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0069R5 - EDI Claim Control Maintenance                    *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 03/24/10  PROGRAMMER: C:232 PROJECT: 139WNN         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/19/20 C:180 Project: WC2738                               *
     H*                 Add Base on Date Last Worked check box.       *
     H*                                                               *
     H*  12/19/19 C:180 Support: 85936                                *
     H*                 Prevent record lock on WMIXP.                 *
     H*                                                               *
     H*  03/14/19 C:212 Project: WC2658                               *
     H*                 Add processing related to addition of new eff *
     H*                 dates for override                            *
     H*                 Retrofit WC2087 04/05/12 under this           *
     H*                                                               *
     H* 04/05/12  C:255 Project WC2087                                *
     H*                 Added processing for field "If 'M' more than  *
     H*                 'X' days, where X is particular No. of days.  *
     H*****************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0069f51  cf   e             workstn
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
     D #ofscr          c                   4
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
     C/COPY SCOPYMT05C
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

     C*  DARTS TABLE CODE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXRMCD

     C*  KEY LIST FOR THE SYSTEM-WIDE STATE CODE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXKEY
      *
     C     IXkey2        klist
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
     C                   EVAL      *IN90 = '0'
      *
     C*  If EDI Format not entered, check the fund file to see if it is required.
      *
     C                   IF        *IN90 <> '1' AND SCEDIF = *BLANKS
     C                   CALL      'WA075R'
     C                   PARM                    XXCO#
     C                   PARM      000           ##fnd
     C                   PARM      *BLANK        QQRTCD
     C     QQRTCD        IFEQ      'F'
     C                   SETON                                        676890
     C                   ENDIF
     C                   ENDIF
      *
     C*  The EDI Format ID Must be in the table file if entered.
      *
     C                   IF        *IN90 <> '1'
     C                   IF        SCEDIF <> *BLANKS
     C                   MOVEL     'EDIF'        XXTBID
     C                   MOVEL     SCEDIF        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C                   IF        *IN60 = '0' AND GTSTAT = 'A'
     C                   ELSE
     C                   EVAL      *IN66 = '1'
     C                   EVAL      *IN90 = '1'
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      **
     C* System should not allow the duplicate records...
     C                   IF        *IN90 <> '1'
     C                   IF        SCEDIF <> IXEDIF
      *
     C     IXKEY2        CHAIN     PIX01
     C                   IF        %FOUND(WMIXL)
     C                   SETON                                          6990
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*    Basis code required, must be A, R, or D.
      *
     C                   IF        *IN90 <> '1'
     C     SCBASE        IFNE      'A'
     C     SCBASE        ANDNE     'R'
     C     SCBASE        ANDNE     'D'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C*    Days to submission must be present, GT zero.
      *
     C  N90SCSDUE        IFLE      0
     C                   MOVE      *ON           *IN21
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C*
     C*  VALID THE HARDCOPY VS. EDI FLAG.  MUST BE 'H' OR 'E'.
     C*
     C  N90SCHCED        IFNE      'H'
     C     SCHCED        ANDNE     'E'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  THE FROI PRINT DARTS REMARK AND THE FROI FORMAT ARE MUTUALLY
     C*  INCLUSIVE.
      *
     C  N90SCHCPR        IFNE      *BLANKS
     C     SCFROI        ANDEQ     *BLANKS
     C     SCHCPR        OREQ      *BLANKS
     C     SCFROI        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  Validate the Hardcopy format, if entered.
      *
     C  N90SCFROI        IFNE      *BLANKS
     C                   MOVEL(P)  SCFROI        XXKEY
     C     STKY          CHAIN     PST01                              26
     C     *IN26         IFEQ      '1'
     C     SCFROI        ANDNE     'A1'
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF                                                  SCFROI IFNE *BLANKS
      *
     C*  Validate the FROI print DARTS remark code, if entered.
      *
     C  N90SCHCPR        IFNE      *BLANKS
     C                   MOVEL(P)  SCHCPR        XXRMCD
     C     CTKY          CHAIN     PCT01                              24
     C                   IF        *IN24 = '1'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      /Free
       //*** Begin Add *** 06/19/20 **********************************
        *in58 = *Off;
       //*** End   Add *** 06/19/20 **********************************
      /End-Free
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C*
     C                   MOVE      '0'           *IN90
     C*
     C*  The 'All Claims' flag must be Y or N if not blank.
     C*
     C     SCEALL        IFNE      *BLANKS
     C     SCEALL        ANDNE     'Y'
     C     SCEALL        ANDNE     'N'
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Include Record-Only claims' should be Y or N if not blank.
      *
     C     SCRO          IFNE      *BLANKS
     C     SCRO          ANDNE     'Y'
     C     SCRO          ANDNE     'N'
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Include Medical Only claims' should be Y or N if not blank.
      *
     C     SCMO          IFNE      *BLANKS
     C     SCMO          ANDNE     'Y'
     C     SCMO          ANDNE     'N'
     C                   MOVE      '1'           *IN61
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
      *** Beg Add *** 04/05/12 ***********************************************
     C*  The 'Number of days lost' cannot be less than 0.
      *
     C     SCMLTD        IFLT      0
     C                   MOVE      '1'           *IN62
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *** End Add *** 04/05/12 ***********************************************
      *
     C*  The 'Lost time claims' flag must be Y or N if not blank.
      *
     C     SCELT         IFNE      *BLANKS
     C     SCELT         ANDNE     'Y'
     C     SCELT         ANDNE     'N'
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Number of days lost time' cannot be less than 0.
      *
     C     SCELTD        IFLT      0
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Comp payment' flag must be Y or N if not blank.
      *
     C     SCECMP        IFNE      *BLANKS
     C     SCECMP        ANDNE     'Y'
     C     SCECMP        ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Comp threshold amount' cannot be less than 0.
      *
     C     SCECOA        IFLT      0
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Med payment' flag must be Y or N if not blank.
      *
     C     SCEMED        IFNE      *BLANKS
     C     SCEMED        ANDNE     'Y'
     C     SCEMED        ANDNE     'N'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Comp threshold amount' cannot be less than 0.
      *
     C     SCEMEA        IFLT      0
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Salary cont' flag must be Y or N if not blank.
      *
     C     SCESAL        IFNE      *BLANKS
     C     SCESAL        ANDNE     'Y'
     C     SCESAL        ANDNE     'N'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Occupational disease' flag must be Y or N if not blank.
      *
     C     SCEOCD        IFNE      *BLANKS
     C     SCEOCD        ANDNE     'Y'
     C     SCEOCD        ANDNE     'N'
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Death claim' flag must be Y or N if not blank.
      *
     C     SCEDTH        IFNE      *BLANKS
     C     SCEDTH        ANDNE     'Y'
     C     SCEDTH        ANDNE     'N'
     C                   MOVE      '1'           *IN29
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Denied' flag must be Y or N if not blank.
      *
     C     SCEDNY        IFNE      *BLANKS
     C     SCEDNY        ANDNE     'Y'
     C     SCEDNY        ANDNE     'N'
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      /Free
       //*** Begin Add *** 06/19/20 *****************************************************
       // The 'Denied' check box must be Y if 'Base on Date Last Worked' is checked.
       //
       If scDnyDlw = 'Y' and scEdny <> 'Y';
          *in58= *On;
          *in90 = *On;
       Else;
          *in58 = *Off;
       Endif;
       //*** End   Add *** 06/19/20 *****************************************************
      /End-Free
      *
     C*  The 'Acquired' flag must be Y or N if not blank.
      *
     C     SCEACQ        IFNE      *BLANKS
     C     SCEACQ        ANDNE     'Y'
     C     SCEACQ        ANDNE     'N'
     C                   MOVE      '1'           *IN31
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  The 'Acquired' flag must be Y or N if not blank.
      *
     C     SCECVT        IFNE      *BLANKS
     C     SCECVT        ANDNE     'Y'
     C     SCECVT        ANDNE     'N'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN90
     C                   ENDIF

     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
      *
     C                   MOVE      '0'           *IN90
      *
     C*  THE ADJUSTER DARTS REMARK AND # OF DAYS ARE MUTUALLY
     C*  INCLUSIVE.
      *
     C     SCADDR        IFEQ      *BLANKS
     C     SCADRC        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN20
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  VALIDATE THE ADJUSTER DARTS DIARY REMARK
      *
     C  N90SCADDR        IFNE      *BLANKS
     C                   MOVEL     SCADDR        XXRMCD
     C     CTKY          CHAIN     PCT01                              21
     C                   IF        *IN21 = '1'
     C                   MOVE      '1'           *IN21
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN22
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*  THE SUPERVISOR DARTS REMARK AND # OF DAYS ARE MUTUALLY
     C*  INCLUSIVE.
      *
     C  N90SCSDDR        IFEQ      *BLANKS
     C     SCSDIM        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN23
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  VALIDATE THE SUPERVISOR DARTS DIARY REMARK
      *
     C  N90SCSDDR        IFNE      *BLANKS
     C                   MOVEL     SCSDDR        XXRMCD
     C     CTKY          CHAIN     PCT01                              24
     C                   IF        *IN24 = '1'
     C                   MOVE      '1'           *IN24
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN25
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*  THE LATE REVIEWER STAFF CODE, # OF DAYS, AND DARTS REMARK ARE
     C*  MUTUALLY INCLUSIVE.
      *
     C                   IF        *IN90 <> '1'
     C     SCDDRC        IFEQ      *BLANKS
     C     SCLRSC        ANDNE     *BLANKS
     C     SCDDRC        OREQ      *BLANKS
     C     SCLRIM        ANDNE     *ZEROS
     C     SCLRSC        OREQ      *BLANKS
     C     SCDDRC        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C**  VALIDATE THE LATE REVIEWER DARTS DIARY REMARK
      *
     C  N90SCDDRC        IFNE      *BLANKS
     C                   MOVEL     SCDDRC        XXRMCD
     C     CTKY          CHAIN     PCT01                              27
     C                   IF        *IN27 = '1'
     C                   MOVE      '1'           *IN27
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN28
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*  VALIDATE THE LATE REVIEWER STAFF CODE
      *
     C  N90SCLRSC        IFNE      *BLANKS
     C                   MOVEL     SCLRSC        XXSTAF
     C     UPKY1         CHAIN     PUP02                              29
     C                   IF        *IN29 = '1'
     C                   MOVE      '1'           *IN29
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C*  THE ADJUSTER DARTS REMARK AND # OF DAYS ARE MUTUALLY
     C*  INCLUSIVE.
      *
     C  N90SCCARC        IFEQ      *BLANKS
     C     SCCAIM        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C*  VALIDATE THE ADJUSTER DARTS DIARY REMARK
      *
     C  N90SCCARC        IFNE      *BLANKS
     C                   MOVEL     SCCARC        XXRMCD
     C     CTKY          CHAIN     PCT01                              31
     C                   IF        *IN31 = '1'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*  THE SUPERVISOR DARTS REMARK AND # OF DAYS ARE MUTUALLY
     C*  INCLUSIVE.
      *
     C  N90SCCSRC        IFEQ      *BLANKS
     C     SCCSIM        ANDNE     *ZEROS
     C                   MOVE      '1'           *IN33
     C                   MOVE      '1'           *IN90
     C                   ENDIF
      *
     C**  VALIDATE THE SUPERVISOR DARTS DIARY REMARK
      *
     C  N90SCCSRC        IFNE      *BLANKS
     C                   MOVEL     SCCSRC        XXRMCD
     C     CTKY          CHAIN     PCT01                              34
     C                   IF        *IN34 = '1'
     C                   MOVE      '1'           *IN34
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN35
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*  THE LATE REVIEWER STAFF CODE, # OF DAYS, AND DARTS REMARK ARE
     C*  MUTUALLY INCLUSIVE.
      *
     C                   IF        *IN90 <> '1'
     C     SCLDDC        IFEQ      *BLANKS
     C     SCCLSC        ANDNE     *BLANKS
     C     SCLDDC        OREQ      *BLANKS
     C     SCCLIM        ANDNE     *ZEROS
     C     SCCLSC        OREQ      *BLANKS
     C     SCLDDC        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C*  VALIDATE THE LATE REVIEWER DARTS DIARY REMARK
      *
     C  N90SCLDDC        IFNE      *BLANKS
     C                   MOVEL     SCLDDC        XXRMCD
     C     CTKY          CHAIN     PCT01                              37
     C                   IF        *IN37 = '1'
     C                   MOVE      '1'           *IN37
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN38
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C*  VALIDATE THE LATE REVIEWER STAFF CODE
      *
     C  N90SCCLSC        IFNE      *BLANKS
     C                   MOVEL     SCCLSC        XXSTAF
     C     UPKY1         CHAIN     PUP02                              39
     C                   IF        *IN39 = '1'
     C                   MOVE      '1'           *IN39
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C*  VALIDATE THE ERROR, ADJUSTER DARTS DIARY REMARK
      *
     C  N90SCEADC        IFNE      *BLANKS
     C                   MOVEL     SCEADC        XXRMCD
     C     CTKY          CHAIN     PCT01                              40
     C                   IF        *IN40 = '1'
     C                   MOVE      '1'           *IN40
     C                   MOVE      '1'           *IN90
     C                   ELSE
     C     CTSTAT        IFNE      'A'
     C                   MOVE      '1'           *IN41
     C                   MOVE      '1'           *IN90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C*================================================================
    C* ERRCK4 - Error checking for srceen 4 (SCR04)
    C*================================================================
     C     errck4        begsr
     c                   move      '0'           *in90
     C  N90SCCECR        IFNE      *BLANKS
     C                   MOVEL     SCCECR        XXRMCD
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
     C  N90SCCEIR        IFNE      *BLANKS
     C                   MOVEL     SCCEIR        XXRMCD
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
     C                   endsr
    C*================================================================
    C*================================================================
    C* ERRCK5 - Error checking for srceen 5 (SCR05)
    C*================================================================
     C     errck5        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
      *
     C* Screen #1...
      *
     C                   MOVE      '148'         SCTRID
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
     C                   MOVE      IXEDIF        SCEDIF
     C                   MOVE      IXJRST        SCJRST
     C                   Z-ADD     IXSDUE        SCSDUE
     C                   MOVE      IXHCED        SCHCED
     C                   MOVEL     IXBASE        SCBASE
     C                   MOVE      IXHCPR        SCHCPR
     C                   MOVE      IXFROI        SCFROI
      *
     C* Screen# 2....
      *
     C                   MOVE      '148'         SCTRID
     C                   MOVE      IXEALL        SCEALL
     C                   MOVE      IXRO          SCRO
     C                   MOVE      IXMO          SCMO
     C***** Beg Add *** 03/14/19 ***********************************************
     C/free
        Monitor;
         scMSDt = %date(IXmsdt:*cymd);
        On-error;
         scMSDt = *loval;
        EndMon;
        Monitor;
         scRSDt = %date(IXrsdt:*cymd);
        On-error;
         scRSDt = *loval;
        EndMon;
      /end-free
     C***** End Add *** 03/14/19 ***********************************************
     C                   MOVE      IXELT         SCELT
     C                   Z-ADD     IXELTD        SCELTD
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       SCmltd = IXmltd;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
     C                   MOVE      IXECMP        SCECMP
     C                   Z-ADD     IXECOA        SCECOA
     C                   MOVE      IXEMED        SCEMED
     C                   Z-ADD     IXEMEA        SCEMEA
     C                   MOVE      IXESAL        SCESAL
     C                   MOVE      IXEOCD        SCEOCD
     C                   MOVE      IXEDTH        SCEDTH
     C*
     C                   MOVE      IXEDNY        SCEDNY
      /Free
       //*** Begin Add *** 06/19/20 ********************************************
       scDnyDlw = IxDnyDlw;
       //*** End   Add *** 06/19/20 ********************************************
      /End-Free
     C                   MOVE      IXEACQ        SCEACQ
     C                   MOVE      IXECVT        SCECVT
     C***** Beg Add *** 03/14/19 ***********************************************
     C/free

        if IXmo = 'N';
           // dsbMSDT = *on; this is implemented in screen through JS
           scMSDt = *loval;
        else;
           // dsbMSDT = *off;
        endif;

        if IXro = 'N';
           // dsbRSDT = *on; this is implemented in screen through JS
           scRSDt = *loval;
        else;
           // dsbRSDT = *off;
        endif;

      /end-free
     C***** End Add *** 03/14/19 ***********************************************
     C*
     C* Screen #3...
     C                   MOVE      '148'         SCTRID
     C                   MOVE      IXADDR        SCADDR
     C                   Z-ADD     IXADRC        SCADRC
     C                   MOVE      IXSDDR        SCSDDR
     C                   Z-ADD     IXSDIM        SCSDIM
     C                   MOVE      IXDDRC        SCDDRC
     C                   Z-ADD     IXLRIM        SCLRIM
     C                   MOVE      IXLRSC        SCLRSC
     C                   MOVE      IXCARC        SCCARC
     C                   Z-ADD     IXCAIM        SCCAIM
     C                   MOVE      IXCSRC        SCCSRC
     C                   Z-ADD     IXCSIM        SCCSIM
     C                   MOVE      IXLDDC        SCLDDC
     C                   Z-ADD     IXCLIM        SCCLIM
     C                   MOVE      IXCLSC        SCCLSC
     C                   MOVE      IXEADC        SCEADC
     C*
     C* Screen #4....
      *
     C                   MOVEL     IXCRER        SCCRER
     C                   MOVEL     IXCECR        SCCECR
     C                   MOVEL     IXCTAR        SCCTAR
     C*
     C                   MOVEL     IXCEIR        SCCEIR
     C                   MOVEL     IXCTRR        SCCTRR
      *
      *
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
      *
     C     @@RRN         CHAIN     DBRCDF
      *
     C* Screen #1...
     C                   MOVE      SCEDIF        IXEDIF
     C                   Z-ADD     XXCO#         IXCO#
     C                   MOVE      SCJRST        IXJRST
     C                   MOVE      SCTRID        IXTRID
     C                   Z-ADD     SCSDUE        IXSDUE
     C                   MOVE      SCHCED        IXHCED
     C                   MOVEL     SCBASE        IXBASE
     C                   MOVE      SCHCPR        IXHCPR
     C                   MOVE      SCFROI        IXFROI
     C*
     C* Screen# 2....
     C                   MOVE      SCTRID        IXTRID
     C                   MOVE      SCEALL        IXEALL
     C                   MOVE      SCRO          IXRO
     C                   MOVE      SCMO          IXMO
     C***** Beg Add *** 03/14/19 ***********************************************
     C/free
        Monitor;
         IXmsdt = %dec(%char(scMSDt:*cymd0):7:0);
        On-error;
         IXmsdt = *zeros;
        EndMon;
        Monitor;
         IXrsdt = %dec(%char(scRSDt:*cymd0):7:0);
        On-error;
         IXrsdt = *zeros;
        EndMon;
      /end-free
     C***** End Add *** 03/14/19 ***********************************************
     C                   MOVE      SCELT         IXELT
     C                   Z-ADD     SCELTD        IXELTD
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       IXmltd = SCmltd;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
     C                   MOVE      SCECMP        IXECMP
     C                   Z-ADD     SCECOA        IXECOA
     C                   MOVE      SCEMED        IXEMED
     C                   Z-ADD     SCEMEA        IXEMEA
     C                   MOVE      SCESAL        IXESAL
     C                   MOVE      SCEOCD        IXEOCD
     C                   MOVE      SCEDTH        IXEDTH
     C                   MOVE      SCEDNY        IXEDNY
      /Free
       //*** Begin Add *** 06/19/20 ********************************************
       IxDnyDlw = scDnyDlw;
       //*** End   Add *** 06/19/20 ********************************************
      /End-Free
     C                   MOVE      SCEACQ        IXEACQ
     C                   MOVE      SCECVT        IXECVT
     C***** Beg Add *** 03/14/19 ***********************************************
     C/free

        if SCmo = 'N';
           IXmsdt = *zeros;
        endif;

        if SCro = 'N';
           IXrsdt = *zeros;
        endif;

      /end-free
     C***** End Add *** 03/14/19 ***********************************************
     C*
     C* Screen #3...
     C                   MOVE      SCTRID        IXTRID
     C                   MOVE      SCADDR        IXADDR
     C                   Z-ADD     SCADRC        IXADRC
     C                   MOVE      SCSDDR        IXSDDR
     C                   Z-ADD     SCSDIM        IXSDIM
     C                   MOVE      SCDDRC        IXDDRC
     C                   Z-ADD     SCLRIM        IXLRIM
     C                   MOVE      SCLRSC        IXLRSC
     C                   MOVE      SCCARC        IXCARC
     C                   Z-ADD     SCCAIM        IXCAIM
     C                   MOVE      SCCSRC        IXCSRC
     C                   Z-ADD     SCCSIM        IXCSIM
     C                   MOVE      SCLDDC        IXLDDC
     C                   Z-ADD     SCCLIM        IXCLIM
     C                   MOVE      SCCLSC        IXCLSC
     C                   MOVE      SCEADC        IXEADC
     C*
     C* Screen #4....
      *
     C                   MOVEL     SCCRER        IXCRER
     C                   MOVEL     SCCECR        IXCECR
     C                   MOVEL     SCCTAR        IXCTAR
     C*
     C                   MOVEL     SCCEIR        IXCEIR
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
     C     IXKEY2        CHAIN     PI101
     C                   IF        %FOUND(WMI1L)
     C                   MOVEL     'Y'           IXAUTO
     C                   ELSE
     C                   MOVEL     *BLANKS       IXAUTO
     C                   ENDIF
      *
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
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
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
     C     Q1USER        CHAIN     PUP01
     C                   IF        %FOUND
     C                   EVAL      XXCO# = UPCO#
     c                   ENDIF
      *
      /Free
       //*** Begin Add *** 06/19/20 **********************************
        *in58 = *Off;
       //*** End   Add *** 06/19/20 **********************************
      /End-Free
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
      *
      *** Begin change *** 12/19/19 ***********************************
     C***  @@RRN         CHAIN     DBRCDF                             99
     C     @@RRN         CHAIN(n)  DBRCDF                             99
      *** End   change *** 12/19/19 ***********************************
B01  C                   IF        *IN99 = *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   MOVE      *ON           OUTFLG
E01  C                   ENDIF
      *
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
      *
     C                   CLEAR                   DBRCDF
      *
     C                   EXSR      MVSCDB
      *
     C                   EVAL      IXSTAT = 'A'
     C                   MOVE      Q1USER        IXIUSR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IXIDT
      *
     C                   WRITE     DBRCDF
      *
     C                   FEOD      WMIXP
     C                   MOVE      *ON           OUTFLG
      *
     C                   endsr
    C*================================================================
    C* KJDO - Processing for F10
    C*================================================================
     C     kjdo          begsr
      *
     C*  Process SCR01 searches
     C*  EDI FORMAT SEARCH.....
     C     HFLD          IFEQ      'SCEDIF'
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM      'EDIF'        QQTBID
     C                   PARM      *BLANK        QQKEY
     C                   PARM      ' '           QQRTCD            1
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCEDIF
     C                   MOVE      *ON           *IN65
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C*  Hardcopy DARTS remark search
      *
     C     HFLD          IFEQ      'SCHCPR'
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
     C                   MOVEL     ##RMCD        SCHCPR
     C                   OTHER
     C                   MOVE      *ON           *IN30
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C*  1st adjuster diary code search
     C     HFLD          IFEQ      'SCADDR'
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
     C                   MOVEL     ##RMCD        SCADDR
     C                   MOVE      *ON           *IN43
     C                   OTHER
     C                   MOVE      *ON           *IN42
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C*  1st supervisor diary code search
     C*
     C     HFLD          IFEQ      'SCSDDR'
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
     C                   MOVEL     ##RMCD        SCSDDR
     C                   MOVE      *ON           *IN45
     C                   OTHER
     C                   MOVE      *ON           *IN44
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C*  1st EDI reviewer diary code search
     C*
     C     HFLD          IFEQ      'SCDDRC'
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
     C                   MOVEL     ##RMCD        SCDDRC
     C                   MOVE      *ON           *IN47
     C                   OTHER
     C                   MOVE      *ON           *IN46
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C*  1st EDI Reviewer staff code search
     C*
     C     HFLD          IFEQ      'SCLRSC'
     C                   MOVE      *BLANKS       XXSTAF
     C                   CALL      'S0005R'      UPRMPL
     C                   IF        ##RRN = 0
     C                   MOVE      '1'           *IN48
     C                   ENDIF

     C*
     C     ##RRN         CHAIN     PUP03                              60

     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPSTAF        SCLRSC
     C                   MOVE      '1'           *IN49
     C                   ELSE
     C                   MOVE      '1'           *IN48
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C*  2nd adjuster diary code search
      *
     C     HFLD          IFEQ      'SCCARC'
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
     C                   MOVEL     ##RMCD        SCCARC
     C                   MOVE      *ON           *IN50
     C                   OTHER
     C                   MOVE      *ON           *IN49
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C*  2nd supervisor diary code search
     C*
     C     HFLD          IFEQ      'SCCSRC'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C                   SELECT
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCCSRC
     C                   MOVE      *ON           *IN52
     C                   OTHER
     C                   MOVE      *ON           *IN51
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*  2nd EDI reviewer diary code search
     C*
     C     HFLD          IFEQ      'SCLDDC'
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
     C                   MOVEL     ##RMCD        SCLDDC
     C                   MOVE      *ON           *IN54
     C                   OTHER
     C                   MOVE      *ON           *IN53
     C                   ENDSL
     C*
     C                   ENDIF
     C*  2nd EDI Reviewer staff code search
     C*
     C     HFLD          IFEQ      'SCCLSC'
     C*
     C                   MOVE      *BLANKS       XXSTAF
     C                   CALL      'S0005R'      UPRMPL
     C**   ##RRN         CABEQ     *ZEROS        STRT                     55
     C                   IF        ##RRN = 0
     C                   MOVE      '1'           *IN55
     C                   ENDIF
     C*
     C     ##RRN         CHAIN     PUP03                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPSTAF        SCCLSC
     C                   MOVE      '1'           *IN56
     C                   ELSE
     C                   MOVE      '1'           *IN55
     C                   ENDIF
     C                   ENDIF
     C*
     C*  148 Acknowledgement error EDI reviewer diary code search
     C*
     C     HFLD          IFEQ      'SCEADC'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C                   SELECT
     C     ##RMCD        WHENNE    *BLANKS
     C                   MOVEL     ##RMCD        SCEADC
     C                   OTHER
     C                   MOVE      *ON           *IN56
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*
     C*  Process SCR05 searches
     C*
     C*  148/00 SEARCH
     C*
     C*  "EC" SEARCH
     C*
     C     HFLD          IFEQ      'SCCECR'
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C     ##RMCD        IFNE      *BLANKS
     C                   MOVEL     ##RMCD        SCCECR
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
     C     HFLD          IFEQ      'SCCEIR'
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
     C                   MOVEL     ##RMCD        SCCEIR
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
     C                   endsr
