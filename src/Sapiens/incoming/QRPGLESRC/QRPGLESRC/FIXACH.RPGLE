     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

      /title FIXACH - Move WMCBP VALUES TO SMCOP FILE
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *****************************************************************
      * CALLED FROM:                                                  *
      * CALLED TO:                                                    *
      *                                                               *
      * Created 08/31/10  C:255  Project: SY190                       *
      *****************************************************************
      * Revision:                                                     *
      *                                                               *
      *  09/15/10  C:191 Project: SY190 priss #179                    *
      *                  Include checking/savings flag setup          *
      *                  Duplicate Provider EFT records for all groups*
      *                                                               *
      *****************************************************************
    F*  Physical file definition
     FWMCBL     IF   E           K DISK
     FSMCOL     UF A E           K DISK
     F*** BEGIN ADD ****** 09/15/10 ************************************
     FWMEFP     UF A E             DISK
     FWMBIL     IF   E           K DISK
     FWMEFL7    IF   E           K DISK    rename(pef01:pef07) prefix(e7:2)
     F*** END   ADD ****** 09/15/10 ************************************
      *
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D XXDT            s              7  0
     D @@CO#           s                   LIKE(CBCO#)
     D arEftGrp        s              3  0 dim(999)
     DPSDS           ESDS                  Extname(SZQ1P)
     DCODS             DS
     DCOSYCD
     DCOARCD
     DCOECAR
     DCOIFAR
     DCOMFAR
     DCOSFAR
     DCOTXAR
     DCO1AAR
     DCO2AAR
     DCO3AAR
     DCO4AAR
     DCO5AAR
     DCO6AAR
     DCO7AAR
     DCOACHT
    C**********************************************************************
    C*Parm Lists/Key Lists
    C**********************************************************************
    C
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     efky          klist
     C                   kfld                    xxtype            1
     C                   kfld                    xxco#             3 0
     C                   kfld                    xxfnd             3 0
     C                   kfld                    xxkey            30
     C
     C     *Loval        setll     WMCBL
     C                   read      PCB01
     C                   dow       Not %EOF(WMCBL)
     C                   move      CBCO#         @@CO#
     C*Chain Company Master file
     C     @@CO#         Chain     PCO01                              60
     C                   if        *in60 = *off
     C*
     C                   if        cods = *blanks and COACHL=0
     C                   eval      COSYCD = CBSYCD
     C                   eval      COARCD = CBARCD
     C                   eval      COECAR = CBECAR
     C                   eval      COIFAR = CBIFAR
     C                   eval      COMFAR = CBMFAR
     C                   eval      COSFAR = CBSFAR
     C                   eval      COTXAR = CBTXAR
     C                   eval      CO1AAR = CB1AAR
     C                   eval      CO2AAR = CB2AAR
     C                   eval      CO3AAR = CB3AAR
     C                   eval      CO4AAR = CB4AAR
     C                   eval      CO5AAR = CB5AAR
     C*                  eval      CO6AAR = CB6AAR
     C*                  eval      CO7AAR = CB7AAR
     C                   eval      COACHL = CBACHL
     C                   eval      COACHT = CBACHT
     C                   eval      COUSER =Q1USER
     C                   eval      COPGM  =Q1PGM
     C                   exsr      initz
     C                   MOVE      DSTIME        COTIME
     C                   MOVE      XXDT          CODT
     C                   UPDATE    PCO01
     C                   endif
     C*
     C                   endif
     C                   read      PCB01
     C                   enddo
     C*** BEGIN ADD ****** 09/15/10 ************************************
     C* Load EFT groups to an array for use in provider records
     C                   z-add     0             i                 3 0
     C     *Loval        setll     pbi01
     C                   read      pbi01
     C                   dow       not %eof
     C                   if        bistat<>'I' and biappl='WC' and bicktp='EFT'
     C                   eval      i = i + 1
     C                   eval      arEftGrp(i) = bifnd
     C                   endif
     C                   read      pbi01
     C                   enddo
     C*
     C* Load checking/savings flag in WMEFP
     C                   read      pef01
     C                   dow       not %eof
     C*
     C* Update account type checking/savings
     C                   if        efactp = *blanks
     C                   exsr      srPrcActp
     C                   update    pef01
     C                   endif                                                  efactp = *blanks
     C*
     C* Create provider per group records
     C                   exsr      initz
     C                   if        (efexdt=0 or efexdt>=xxdt) and eftype='P'
     C                   exsr      srCrtPvdr
     C                   endif                                                  efexdt = 0
     C*
     C                   read      pef01
     C                   enddo
     C*** END   ADD ****** 09/15/10 ************************************
     C                   EVAL      *Inlr = *On
     C*=====================================================================
     C     initz         begsr
     C*=====================================================================
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         XXDT
     C*
     C                   endsr
     C*=====================================================================
     C* Process for updating account type
     C*=====================================================================
     C     srPrcActp     begsr
     C                   select
     C                   when      eftrcd >= '22' and eftrcd <= '29'
     C                   eval      efactp = 'C'
     C                   when      eftrcd >= '32' and eftrcd <= '39'
     C                   eval      efactp = 'S'
     C                   other
     C                   eval      efactp = 'C'
     C                   endsl
     C*
     C                   eval      efuser =Q1USER
     C                   eval      efpgm  =Q1PGM
     C                   exsr      initz
     C                   move      DSTIME        eftime
     C                   move      XXDT          efdt
     C                   endsr
     C*=====================================================================
     C* Create provider per group records
     C*=====================================================================
     C     srCrtPvdr     begsr
     C* Loop thru EFT Groups array to duplicate provider records
     C                   z-add     0             x                 3 0
     C                   z-add     effnd         savfnd            3 0
     C                   movel(p)  effein        xxwk12           12
     C                   move      efmod         xxwk12
     C                   movel(p)  xxwk12        xxkey
     C                   eval      xxtype = 'P'
     C                   eval      xxco# = efco#
     C*
     C                   for       x = 1 to  i
     C                   eval      xxfnd = arEftGrp(x)
     C* Skip same fund
     C                   if        xxfnd <> savfnd
     C* ignore if already exists
     C     efky          chain     pef07
     C                   if        not %found
     C                   eval      effnd = xxfnd
     C                   eval      efuser =Q1USER
     C                   eval      efiusr =Q1USER
     C                   eval      efipgm =Q1PGM
     C                   exsr      initz
     C                   move      DSTIME        eftime
     C                   move      DSTIME        efitm
     C                   move      XXDT          efidt
     C                   write     pef01
     C                   endif                                                  not %found
     C                   endif                                                  xxfnd <> effnd
     C                   endfor
     C                   endsr
