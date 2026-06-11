     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
     H/TITLE F6216R01                                                                      *
     H**************************************************************************************
     H*  Description:  997 Acknowledgement Maintenance                                     *
     H*                                                                                    *
     H*  Created: 07/18/14   Programmer C:079   Project:MC376                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     Ff6216F01  cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
    F*
    F*  Physical file definition (for maintaining record)
     FFEAE997P  uf a e             disk
     F                                     rename(P99701:dbrcdf)
     FFTER997L1 if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   '997 Ack Errors'
     D comapi          c                   'FA007R'
     D #ofscr          c                   1
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
     D kkErCd          s                   like(AE997ErCd)
     D kkSgId          s                   like(AE997SgId)
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
    C*================================================================
    C*  KLIST definitions
     C*
     C     erky          klist
     C                   kfld                    kkSgId
     C                   kfld                    kkErCd
     C*
    C*================================================================
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHGSTS - Subroutine for changing status
    C*================================================================
    C*
     C     chgsts        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
     C                   if        *in99 = *off
    C*
     C                   if        ae997stat = 'I'
     C                   eval      ae997stat = 'A'
     C                   else
     C                   eval      ae997stat = 'I'
     C                   endif
    C*
     C                   movel     q1user        ae997user
     C                   movel     q1pgm         ae997pgm
    C*
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ae997dt
     C                   movel     dstime        ae997time
    C*
     C                   update    dbrcdf
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*** End Add *** 10/11/11 ****************************************
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
     C*
     C                   eval      sc997TstId = AE997TstId
     C                   z-add     AE997PrDt     srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   Movel     srdto         sc997PrDt
     C                   eval      sc997SgId = AE997SgId
     C                   eval      sc997IdCd = AE997IdCd
     C                   eval      sc997ErCd = AE997ErCd
     C*
     C* Get the error code desription
     C*
     C                   eval      kkSgId = sc997SgId
     C                   eval      kkErCd = sc997ErCd
     C     ErKy          Chain     FTer997L1                          99
     C*
     C                   if        *in99 = *off
     C                   eval      scErCdDsc = ER997Dsc
     C                   endif
     C*
     C                   eval      sc997Pos = AE997Pos
     C                   eval      sc997Ref = AE997Ref
     C                   eval      sc997Data = AE997Data
     C                   eval      sc997Data = AE997Data
     C                   eval      scStat = AE997Stat
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
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
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C     @@mode        comp      'C'                                    10
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
    C*
     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   endsr
