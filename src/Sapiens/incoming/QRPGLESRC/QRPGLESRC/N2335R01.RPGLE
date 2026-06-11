     H*  (C) Copyright Progressive Data Solutions, Inc., 2002
    H*
     H**********************************************************************
     H**   BILL RATE CODE INFORMATION                                     **
     H**                                                                  **
     H**   CREATED 09/27/00 BY C:062 FOR PROJECT:AR032                    **
     H**********************************************************************
     H** REVISIONS:
     H*
     H*  05/30/02 C:62 Project AR037
     H*                Add edit that keeps a discount code from being added
     H*                if it already exists under a different discount type.
     H*
     H**********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) actgrp(*caller)
      /ENDIF
     Fn2335f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fnmbdp     uf a e             disk
     F                                     rename(pbd01:dbrcdf)
     F*** BEGIN CHG *** 05/30/02 *******************************************
     F**********nmbdl3    if a e           k disk
     Fnmbdlb    if a e           k disk
     F*** END   CHG *** 05/30/02 *******************************************
     Fftgtp     if a e             disk
     Fftgtl     if a e           k disk
     F                                     rename(pgt01:pgt02)
     D pgm             s             70    dim(2) ctdata perrcd(1)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Bill Rate Codes'
     D optn2           c                   'Bill Rate Code Information'
     D comapi          c                   'SA005R'
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
     D kkcode          s                   like(bdcode)
     D*** BEGIN DEL *** 05/30/02 *******************************************
     D*kktype***       s                   like(bdtype)
     D*** END   DEL *** 05/30/02 *******************************************
     D kkefdt          s                   like(bdefdt)
     D kkco#           s                   like(gtco#)
     D kktbid          s                   like(gttbid)
     D kkkey           s                   like(gtkey)
     D xxcmd           ds           140    inz
     D xxtype                  9     10
     D xxprds                 19     58
     D xxcall                 62     62
     D xxcode                 72     74
     D xxefdt                 78     85
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
     C                   parm                    ##conm
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C     bdky          klist
     C                   kfld                    kkcode
     C*** BEGIN DEL *** 05/30/02 *******************************************
     C**********         kfld                    kktype
     C*** END   DEL *** 05/30/02 *******************************************
     C                   kfld                    kkefdt
     C*** BEGIN ADD *** 05/30/02 *******************************************
    C*
     C     bdky2         klist
     C                   kfld                    kkcode
     C*** END   ADD *** 05/30/02 *******************************************

     C     coky          klist
     C                   kfld                    bdcode
     C                   kfld                    bdefdt
    
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* WRTPGM = Add record by calling add screen.
    C*================================================================
     C     wrtpgm        begsr

     C** Call QCMDEXC to get program based on type entered.

     C                   z-add     140           xxlen
     C                   movel(p)  pgm(1)        xxcmd
     C                   move      pgm(2)        xxcmd
     C                   movel(p)  sctype        xxtype
     C                   movel(p)  sccode        xxcode
     C                   move      scefdt        xxefdt
     C                   movel(p)  optn2         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##DATA           80
     C                   parm      40            ##LENG            3 0
     C                   movel     ##data        xxprds
     C                   movel     ' '           xxcall

     C                   call      'QCMDEXC'
     C                   parm                    xxcmd
     C                   parm                    xxlen            15 5

     C                   endsr
     C*================================================================
     C* CHGSTS - Subroutine for changing status
     C*================================================================
     C     chgsts        begsr

     C     @@rrn         chain     dbrcdf                             99

     C                   if        *in99 = *off

     C                   if        bdstat = 'I'
     C                   eval      bdstat = 'A'
     C                   else
     C                   eval      bdstat = 'I'
     C                   endif

     C                   eval      bduser = q1user
     C                   eval      bdpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      bdtime = dstime

     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         bddt

     C                   update    dbrcdf

     C                   endif

     C                   endsr
    C*================================================================
    C* DATCVT - Converts dates to MDYY when searches are used
    C*================================================================
     C     datcvt        begsr

     C                   Z-ADD     scefdt        srdti
     C                   CALL      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt

     C                   endsr
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

     C* Discount code cannot be blank

     C                   if        *in90 = *off and sccode = *blanks
     C                   seton                                        7190
     C                   endif

     C* Discount type must be valid

     C                   if        *in90 = *off
     C                   eval      kkco# = ##co#
     C                   eval      kktbid = 'BILT'
     C                   movel(p)  sctype        kkkey
     C     gtky          chain     pgt02                              99
     C   99              seton                                        7290
     C                   endif

     C* Effective date must be valid

     C                   if        *in90 = *off
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        7390
     C                   endif
     C                   endif
     C*** BEGIN ADD *** 05/30/02 *******************************************

     C* Discount code can't be used twice with different types

     C                   if        *in90 = *off
     C                   eval      kkcode = sccode
     C     bdky2         chain     pbd01                              99
     C                   if        *in99=*off
     C                   movel(p)  sctype        xxsys1            1
     C                   movel(p)  bdtype        xxsys2            1
     C                   if        xxsys1<>xxsys2
     C                   seton                                        7590
     C                   endif
     C                   endif
     C                   endif
     C*** END   ADD *** 05/30/02 *******************************************

     C* Discount code information cannot be duplicated

     C                   if        *in90 = *off
     C                   eval      kkcode = sccode
     C*** BEGIN DEL *** 05/30/02 *******************************************
     C**********         eval      kktype = sctype
     C*** END   DEL *** 05/30/02 *******************************************
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C     bdky          chain     pbd01                              99
     C  n99              seton                                        7490
     C                   endif

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
    C* KJDO - F10=Search
    C*================================================================
     C     kjdo          begsr

     C                   exsr      datcvt

     C                   select
     C     hfld          wheneq    'SCTYPE'
     C                   call      'F2380R'
     C                   parm      '2'           ##call            1
     C     *inkc         parm      '0'           ##kc              1
     C     *inkl         parm      '0'           ##kl              1
     C                   parm      *zeros        ##rrn             9 0
     C                   parm      'BILT'        ##tbid            4

     C                   if        *inkl = *on or ##rrn = *zeros
     C                   seton                                        31
     C                   else
     C     ##rrn         chain     pgt01                              99
     C                   movel(p)  gtkey         sctype
     C                   movel(p)  gtdesc        sctypd
     C                   seton                                        32
     C                   endif

     C                   endsl

     C                   seton                                        90

     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   if        *in40 = *on
     C                   movel     *blanks       sccode
     C                   movel     *blanks       sctype
     C                   movel     *blanks       sctypd
     C                   z-add     *zeros        scefdt
     C                   endif

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

     C     *inkj         caseq     *on           kjdo
     C                   endcs

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

     C     @@mode        comp      'A'                                    40

     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr

     C                   select
     C     @@mode        wheneq    'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl

     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C*    @@rrn         chain     dbrcdf                             99
B01  C*                  if        *in99 = *off
     C*                  exsr      mvscdb
     C*                  update    dbrcdf
E01  C*                  endif
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr

     C                   exsr      wrtpgm

     C                   if        *in99 = *off and *in99 = *on
     C                   write     dbrcdf
     C                   endif

     C                   endsr
** pgm
CALL N92XXR PARM('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' 'x'
'xxx' 'xxxxxxxx')
