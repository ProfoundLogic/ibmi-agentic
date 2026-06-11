     H*  (C) Copyright Progressive Data Solutions, Inc., 2005
    H*
     H*****************************************************************
     H*  CREATED:
     H* 06/02/20 C:180 PROJECT WC2711C
     H*                Add edit to PV1
     H*
     H* 12/31/19 C:180 SUPPORT 86004
     H*                Fix status on Add.
     h*                Fix Group on Add.
     H*
     H* 12/05/19 C:180 PROJECT: WC2711
     H*                Pass subset parm value of 'CO' to W2146R.
     H*
     H* 08/24/07 C:136 SUPPORT: WA135 59723
     H*                USE GROUP VALUE FROM RELATIVE RECORD NUMBER
     H*                PASSED INTO THE PROGRAM RATHER THAN FROM
     H*                USER PARMS
     H*
     H* 09/21/05 C:53  PROJECT: WC1309
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw0025f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdr1p     uf a e             disk
     F                                     rename(pr101:dbrcdf)
     Fwtabl     if   e           k disk
     Fststl3    if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'State Comp Rates by Ben Type'
     D comapi          c                   'WA001R'
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
      *** beg add ***** 08/24/07 *************************************
     C                   parm                    @@fnd             3
      *** end add ***** 08/24/07 *************************************
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
    C*================================================================
     C     t2key         klist
     C                   kfld                    ##co#
     C                   kfld                    kkkey
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
     C     *like         define    b2key         kkkey
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     chgsts        begsr
     C*
     C     @@rrn         chain     dbrcdf                             99
     C*
     C                   if        *in99=*off
     C*
     C                   if        r1stat='A'
     C                   eval      r1stat =  'I'
     C                   else
     C                   eval      r1stat = 'A'
     C                   endif                                                  fnflag='I'
     C*
     C                   update    dbrcdf
     C*
     C                   endif                                                  *in99=*off
     C*
     C                   endsr                                                  chgsts
     C*
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
     C*
     C                   if        scst = *blanks
     C                   move      *on           *in70
     C     *in70         cabeq     *on           ender1                   90
     C                   endif
     C*
     C                   movel(p)  scst          stkey            10
     C     stkey         chain     pst01                              71
     C     *in71         cabeq     *on           ender1                   90
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C*
     C                   if        srerr = *on
     C                   seton                                        9072
     C                   endif
     C*
     C                   movel(p)  scbr2         kkkey
     C     t2key         chain     pb201                              73
      *** Begin Add *** 06/02/20 *********************************************
      /Free
       If *in73 = *On;
          msBR2 = 'WCR0513';
       Else;
         If B2rc01 <> 'CO' and B2rc02 <> 'CO' and B2rc03 <> 'CO' and
            B2rc04 <> 'CO' and B2rc05 <> 'CO' and B2rc06 <> 'CO' and
            B2rc07 <> 'CO' and B2rc08 <> 'CO' and B2rc09 <> 'CO' and
            B2rc10 <> 'CO';
            msBR2 = 'WCR4834';
            *in73 = *On;
         Endif;
       Endif;

      /End-Free
      *** End   Add *** 06/02/20 *********************************************
     C     *in73         cabeq     *on           ender1                   90
     C*
     C                   if        scrate = *zeros
     C                   move      *on           *in74
     C     *in74         cabeq     *on           ender1                   90
     C                   endif
     C*
     C     ender1        endsr
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
      *===============================================================
      * KJDO - SUBROUTINE FOR F10
      *===============================================================
      *
      *  F10=TABLE SEARCH
      *
     C     kjdo          begsr
     C*
     C                   select
     C     hfld          wheneq    'SCBR2'
     C                   call      'W2146R'
     C                   parm      '2'           ##call            1
     C     *inkc         parm      '0'           ##kc              1
     C     *inkl         parm      '0'           ##kl              1
     C                   parm      *blanks       ##key            10
      *** Begin Add *** 12/05/19 ************************************
     C                   parm      'CO'          outSubset         2
      *** End   Add *** 12/05/19 ************************************
     C*
     C     *inkl         ifeq      *off
     C     ##key         andne     *blanks
     C                   movel     ##key         scbr2
     C*
     C                   eval      kkkey  = scbr2
     C     t2key         chain     pb201                              99
     C                   if        *in99 = *off
     C                   eval      scbr2d = b2desc
     C                   else
     C                   eval      scbr2d = *blanks
     C                   endif
     C*
     C                   move      *on           *in21
     C                   else
     C                   move      *on           *in22
     C                   endif
     C*
     C                   endsl
     C                   endsr
     C
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      scst   = r1st
     C                   eval      scbr2  = r1br2
     C*
     C                   eval      kkkey  = r1br2
     C     t2key         chain     pb201                              99
     C                   if        *in99 = *off
     C                   eval      scbr2d = b2desc
     C                   else
     C                   eval      scbr2d = *blanks
     C                   endif
     C*
     C                   eval      scrate = r1rate
     C                   eval      scstat = r1stat
     C                   z-add     r1efdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt            8 0
     C*
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C*
     C                   eval      r1co#  = ##co#
      *** Begin change *** 12/31/19 **********************************
     C***                eval      r1fnd  = ##fnd
     C                   eval      r1fnd  = scfnd
      *** End   change *** 12/31/19 **********************************
      *** Begin Add *** 12/31/19 *************************************
      /Free
       If scstat = *blanks and @@mode = 'A';
          scstat = 'A';
       Endif;
      /End-Free
      *** End   Add *** 12/31/19 *************************************
     C                   eval      r1stat = scstat
     C*
     C                   eval      r1st   = scst
     C                   eval      r1br2  = scbr2
     C                   eval      r1rate = scrate
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         r1efdt
     C*
     C                   eval      r1user = q1user
     C                   eval      r1pgm = q1pgm
     C                   time                    dsdttm
     C                   eval      r1time = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         r1dt
     C*
     C                   if        *in40 = *on
     C                   eval      r1iusr = q1user
     C                   eval      r1ipgm = q1pgm
     C                   eval      r1itim = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         r1idt
     C                   endif
     C*
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
     C     *inkj         caseq     *on           kjdo                     90
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
     C     @@mode        comp      '1'                                    09
     C     @@mode        comp      'D'                                    09
     C     @@mode        comp      'A'                                    40
     C                   movel(p)  ##fnam        scname
     C                   movel(p)  ##fnd         scfnd
      *** BEG ADD ***** 08/24/07 **************************************
     C                   if        q1prms > 4
     C                   move(p)   @@fnd         scfnd
     C                   endif
      *** END ADD ***** 08/24/07 **************************************
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C*
     C                   select
     C                   when      @@mode = 'S'
     C                   exsr      chgsts
     C                   return
     C                   endsl
     C*
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
     C                   feod      wdr1p
     C                   endsr
