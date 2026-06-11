     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*====================================================================
      /TITLE W0325R02 - Work with Seasonal Adjustment
     H Bnddir('WTBNDDIR')
      * ***************************************************************
      * DESCRIPTION:  This program allows the user to maintain        *
      *               Seasonal Adjustments on a Claim.                *
      *                                                               *
      * CALLED FROM:  W0325R02                                        *
      * CALLS TO:     NONE                                            *
     H*********************************************************************
     H* Revisions:                                                        *
     H*                                                                   *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*********************************************************************
    H*
     Fw0325f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmsap     uf a e             disk
     F                                     rename(psa01:dbrcdf)
     FWMSAL     UF A E           K DISK    INFDS(SALDS)
     F                                     RENAME(psa01:wmsalr)
     Fsmcol     IF   E           K DISK
     Fwmfnl     IF   E           K DISK
     Fwmeml     IF   E           K DISK
     Fwmupl     IF   E           K DISK
     Fwmcdl     IF   E           K DISK
     FWMCMP     IF   E             DISK
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Seasonal Adjustment'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1

     D SALDS           DS
     D  SALRRN               397    400B 0

     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9

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
     D @@rrn_cmp       s              9  0
     D @@rrn           s              9  0
     D @@call          s              1
     D kkco#           s                   like(saco#)
     D kkfnd           s                   like(safnd)
     D kkfyr           s                   like(safyr)
     D kkcase          s                   like(sacase)
     D kkbgdt          s                   like(sabgdt)
     D kkendt          s                   like(saendt)
     D SCBGDT_dt       S               D
     D SCENDT_dt       S               D
     D SCBGDT_sv       S                   Like(SCBGDT)
     D SCENDT_sv       S                   Like(SCENDT)
     D SCLCYC_sv       S                   Like(SCLCYC)
     D SCCPCT_sv       S                   Like(SCCPCT)
     D SCCWCR_sv       S                   Like(SCCWCR)
     D SCBGDT_CYMD     S                   Like(SABGDT)
     D SCENDT_CYMD     S                   Like(SAENDT)
     D KGDO_Call       S                   Like(*INKC)
     D @@Sav_Chg       S                   Like(SaSaSv)

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
     C                   parm                    @@rrn                          WMSAP RRN
     C                   parm                    @@rrn_cmp                      WMCMP RRN
     C                   parm                    @@Sav_Chg                      WMCMP RRN
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam

     C     DteISO        PLIST
     C                   PARM                    srdti
     C                   PARM      '*MDYY'       srffmt
     C                   PARM      '*ISO'        srtfmt
     C                   PARM                    srdto
     C                   PARM                    srerr

    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkbgdt
     C                   kfld                    kkendt

     C     FNKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND

     C     EMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV

     C     CMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE

     C     CDKEY         KLIST
     C                   KFLD                    CMSS#

     C     SAKList       KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE

    C*================================================================
    C*  *LIKE DEFN statements
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

     C                   Move      '0'           *IN90

     C                   Z-ADD     SCBGDT        SrDtI
     C                   Call      'SRCVTDAT'    DteISO
     C                   IF        srerr  = '1'
     C                   Move      '1'           *IN90
     C                   Move      '1'           *IN72
     C                   Else
     C                   MOVEL     srdto         SCBGDT_Dt
     C                   Call      'SRCVTDAT'    DtCYMD
     C                   MOVEL     srdto         SCBGDT_CYMD
     C                   ENDIF

     C                   Z-ADD     SCENDT        SrDtI
     C                   Call      'SRCVTDAT'    DteISO
     C                   IF        srerr  = '1'
     C                   Move      '1'           *IN90
     C                   Move      '1'           *IN73
     C                   Else
     C                   MOVEL     srdto         SCENDT_Dt
     C                   Call      'SRCVTDAT'    DtCYMD
     C                   MOVEL     srdto         SCENDT_CYMD
     C                   ENDIF

     C                   IF        SCCPCT =0
     C                   MOVE      '1'           *IN74
     C                   Move      '1'           *IN90
     C                   ENDIF

     C                   IF        SCCWCR =0
     C                   MOVE      '1'           *IN75
     C                   Move      '1'           *IN90
     C                   ENDIF

     C                   If        SCBGDT_Dt > SCENDT_Dt
     C                   MOVE      '1'           *IN76
     C                   Move      '1'           *IN90
     C                   Endif

      * Check for overlapping periods with existing records
     C     SAKList       Setll     WMSAL
     C     SAKList       Reade(N)  WMSAL                                  22
     C                   Dow       Not *in22
     C                   If        @@rrn <> SALRRN               AND
     C                             ((SCBGDT_CYMD >=  SABGDT  and
     C                               SCBGDT_CYMD <=  SAENDT     ) OR
     C                              (SCENDT_CYMD >=  SABGDT  and
     C                               SCENDT_CYMD <=  SAENDT     ) OR
     C                              (SCBGDT_CYMD <   SABGDT  and
     C                               SCENDT_CYMD >   SAENDT     ))
     C                   MOVE      '1'           *IN77
     C                   Move      '1'           *IN90
     C                   Endif
     C     SAKList       Reade(N)  WMSAL                                  22
     C                   Enddo

      * If No Error and Any entry has changed, force F7.
     C                   If        Not *IN90 and Not KGDO_Call and
     C                             (SCBGDT <> SCBGDT_sv or
     C                              SCENDT <> SCENDT_sv or
     C                              SCLCYC <> SCLCYC_sv or
     C                              SCCPCT <> SCCPCT_sv or
     C                              SCCWCR <> SCCWCR_sv )
     C*@@@               Eval      SCBGDT_sv =  SCBGDT
     C*@@@               Eval      SCENDT_sv =  SCENDT
     C*@@@               Eval      SCLCYC_sv =  SCLCYC
     C*@@@               Eval      SCCPCT_sv =  SCCPCT
     C*@@@               Eval      SCCWCR_sv =  SCCWCR
     C                   Move      '1'           *IN71
     C                   Move      '1'           *IN90
     C                   Endif

     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
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

     C                   Z-ADD     SABGDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL     SrDtO         SCBGDT
     C                   Z-ADD     SAENDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL     SrDtO         SCENDT
     C                   Eval      SCLCYC =  SALCYC
     C                   Eval      SCCPCT =  SACPCT
     C                   Eval      SCCWCR =  SACWCR
     C                   Eval      SC#DAY =  SA#DAY
     C                   Eval      SCRATE =  SARATE
     C                   Eval      SCSASV =  SASASV
     C                   If        @@mode = 'C'
     C                   Eval      @@Sav_Chg = SaSaSv
     C                   Endif
     C
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr

     C                   Eval      SACO#  =  CMCO#
     C                   Eval      SAFND  =  CMFND
     C                   Eval      SAFYR  =  CMFYR
     C                   Eval      SACASE =  CMCASE
     C                   Z-ADD     SCBGDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtCYMD
     C                   MOVEL     SrDtO         SABGDT
     C                   Z-ADD     SCENDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtCYMD
     C                   MOVEL     SrDtO         SAENDT
     C                   Eval      SARATE =  SCRATE
     C                   Eval      SALCYC =  SCLCYC
     C                   Eval      SA#DAY =  SC#DAY
     C                   Eval      SASASV =  SCSASV
     C                   Eval      SACPCT =  SCCPCT
     C                   Eval      SACWCR =  SCCWCR
     C                   Eval      SAIUSR =  SCUSER
     C                   Eval      SAIPGM =  'W0325R02'
     C                   Z-ADD     DSDATE        SrDtI
     C                   CALL      'SRCVTDAT'    DtCYMD
     C                   MOVEL     SrDtO         SAIDT
     C                   Eval      SAITM  =  DSTIME
     C                   Eval      SAUSR  =  SCUSER
     C                   Eval      SAPGM  =  'W0325R02'
     C                   Z-ADD     DSDATE        SrDtI
     C                   CALL      'SRCVTDAT'    DtCYMD
     C                   MOVEL     SrDtO         SADT
     C                   Eval      SATM   =  DSTIME
     C                   Eval      @@Sav_Chg = @@Sav_Chg - SaSaSv


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

     C     *INKG         CASEQ     *ON           KGDO                     90
     C                   EndCS

     C                   endsr
    C*================================================================
    C* KGDO - SubRoutine for F7 = Calculate
    C*================================================================
     C     KGDO          begsr

     C                   Move      '1'           KGDO_Call
     C                   Exsr      errck1
     C                   If        not *IN90
     C                   Eval      SCBGDT_sv =  SCBGDT
     C                   Eval      SCENDT_sv =  SCENDT
     C                   Eval      SCLCYC_sv =  SCLCYC
     C                   Eval      SCCPCT_sv =  SCCPCT
     C                   Eval      SCCWCR_sv =  SCCWCR

     C     SCENDT_Dt     Subdur    SCBGDT_Dt     SC#DAY:*d
     C                   Add       1             SC#DAY                         Include begin & End
     C                   Eval(h)   SCRATE = SCLCYC*7*SCCPCT/100/SC#DAY
     C                   Eval(h)   SCSASV = (SCCWCR-SCRATE)*SC#Day/7
     C                   EndIF
     C                   Move      '0'           KGDO_Call
     C                   Move      '1'           *IN90

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr

     C                   If        (*inKL or *inKC)
     C                             and  @@mode = 'C'
     C                   Eval      @@Sav_Chg = *Zero
     C                   Endif
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

      *Get Key Record from WMCMP
     C     @@rrn_cmp     CHAIN(N)  PCM01                              99
      * Company Name
     C     CMCO#         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD           80
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
      * Fund Name
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      * Employer Name
     C     EMKEY         CHAIN     PEM01                              60
     C                   If        not *in60
      * Concatenate Emp#, Div & Name
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   Endif
      *SSN
     C     CMSS#         IFNE      *BLANK
      *****  Begin Chg  ***  06/09/20  **********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       scss# = DisplaySS#(MaskData('SSN':'cmss#':cmss#));
      /End-free
      *****  End Chg  ***  06/09/20  **********************
     C                   END
      *  DOI
     C                   Z-ADD     CMACDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL(P)  SrDtO         SCACDT
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCL
     C     SCYRCL        CAT       XACASE:0      SCYRCL

      *  Date of Disability
     C                   Z-ADD     CMDBDT        SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL(P)  SrDtO         SCDBDT
      *  RTW DATE
     C                   Z-ADD     CMRTW         SrDtI
     C                   CALL      'SRCVTDAT'    DtMDYY
     C                   MOVEL(P)  SrDtO         SCRTW
      *  Claimant Name
     C     CDKEY         CHAIN     PCD01                              60
     C                   If        not *IN60
     C                   MOVE      *BLANKS       SCCNAM
     C     CDLSN         CAT       ',':0         SCCNAM
     C                   CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C                   CAT       CDMID:1       SCCNAM
     C                   CAT       '.':0         SCCNAM
     C                   ENDIF
     C                   ENDIF
      *  AWW
     C                   MOVE      CMAWW         SCAWW

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
E01  C                   endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wmsap
     C                   Move      '1'           *inKL
     C                   endsr
