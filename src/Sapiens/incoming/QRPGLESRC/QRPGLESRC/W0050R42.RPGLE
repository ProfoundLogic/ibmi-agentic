      * (C) Copyright Progressive Data Solutions, Inc., 2004
      *
      /title W0050R42 - New Business Tracking
      *****************************************************************
      *
      * CALLED FROM: W0050R
      *
      *****************************************************************
      * Revised:
      *
      * 05/31/05  C:156 Project 114WKB
      *                 Policy Tracking - Additional Dates
      *
      * Created 03/25/04  C:61  Project 114WHL
      *****************************************************************

      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0050f42  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     *
     *  Physical file definition (for maintaining record)
     Fwmnbp     uf a e             disk
     F                                     rename(pnb01:dbrcdf)

     Fwmnbl     if a e           k disk    infds(nbfds)
     *
     *  Employer Master
     Fwmemp     if a e             disk


     *================================================================
     *  /COPY required constants
     *================================================================
     D optnm           c                   'New Business Tracking'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1


     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn1          s              9  0
     D EM_rrn          s              9  0
     D FN_rrn          s              9  0
     D tmp7            s              7  0



     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     D nbfds           ds
     D nb_rrn                397    400b 0

     D/copy SPRSTRING
     D/copy WPRGROUP
     D/copy WPREMPLR


     C/COPY SCOPYMT01C


     *================================================================
     *  PLIST definitions
     *================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn                          WMNBP
     C                   parm                    @@rrn1                         WMEMP
     *
     * Program expects @@RRN1 from WMEMP passed in.
     *
     *
     *
     *  Company file/fund (group) retrieve program parameter list
     *
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam



     *================================================================
     *  KLIST definitions
     *================================================================
      *
      * New business
      *
     C     nbky          klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    ememp#



     *================================================================
     *  *LIKE DEFN statements
     *================================================================


     *================================================================
     * DLTPRE - Pre-processing before delete occurs
     *================================================================
     C     dltpre        begsr
     C                   endsr


     *================================================================
     * DSPLYR - Processing after displaying information
     *================================================================
     C     dsplyr        begsr
     C                   endsr


     *================================================================
     * ERRCK1 - Error checking for srceen 1 (SCR01)
     *================================================================
     C     errck1        begsr

      * Received Date
     c                   if        (*in90 = *off) and (scrcdt <> 0)

     c                   z-add     scrcdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scrcdt

     c                   if        srerr = *on
     c                   eval      *in71 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Date Entered
     c                   if        (*in90 = *off) and (scendt <>0)

     c                   z-add     scendt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scendt

     c                   if        srerr = *on
     c                   eval      *in72 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Initial Underwriter Review Date
     c                   if        (*in90 = *off) and (sciwdt <> 0)

     c                   z-add     sciwdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         sciwdt

     c                   if        srerr = *on
     c                   eval      *in73 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Date Quoted
     c                   if        (*in90 = *off) and (scqtdt <> 0)

     c                   z-add     scqtdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scqtdt

     c                   if        srerr = *on
     c                   eval      *in74 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Date Declined
     c                   if        (*in90 = *off) and (scdcdt <> 0)

     c                   z-add     scdcdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scdcdt

     c                   if        srerr = *on
     c                   eval      *in75 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Date Lost
     c                   if        (*in90 = *off) and (sclsdt <> 0)

     c                   z-add     sclsdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         sclsdt

     c                   if        srerr = *on
     c                   eval      *in76 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Agent Approval Date
     c                   if        (*in90 = *off) and (scaadt <> 0)

     c                   z-add     scaadt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scaadt

     c                   if        srerr = *on
     c                   eval      *in77 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Policy Generated Date
     c                   if        (*in90 = *off) and (scpgdt <> 0)

     c                   z-add     scpgdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpgdt

     c                   if        srerr = *on
     c                   eval      *in78 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif


      * Policy Mailed
     c                   if        (*in90 = *off) and (scpold <> 0)

     c                   z-add     scpold        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpold

     c                   if        srerr = *on
     c                   eval      *in79 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif

      ***  begin add  ***  05/31/05 ***********************************
      * Panel Requested Date
     c                   if        (*in90 = *off) and (scprdt <> 0)

     c                   z-add     scprdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scprdt

     c                   if        srerr = *on
     c                   eval      *in81 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif
      * Panel Creation date
     c                   if        (*in90 = *off) and (scpcdt <> 0)

     c                   z-add     scpcdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpcdt

     c                   if        srerr = *on
     c                   eval      *in82 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif
      ***  end   add  ***  05/31/05 ***********************************

      * Panel Mailed
     c                   if        (*in90 = *off) and (scpand <> 0)

     c                   z-add     scpand        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpand

     c                   if        srerr = *on
     c                   eval      *in80 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif



      ***  end   add  ***  05/31/05 ***********************************
      * Panel Posted Date
     c                   if        (*in90 = *off) and (scppdt <> 0)

     c                   z-add     scppdt        SRDTI
     c                   CALL      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scppdt

     c                   if        srerr = *on
     c                   eval      *in83 = *on
     c                   eval      *in90 = *on
     c                   endif

     c                   endif
      ***  end   chg  ***  05/31/05 ***********************************

     C                   endsr


     *================================================================
     * ERRCK2 - Error checking for srceen 2 (SCR02)
     *================================================================
     C     errck2        begsr
     C                   endsr


     *================================================================
     * ERRCK3 - Error checking for srceen 3 (SCR03)
     *================================================================
     C     errck3        begsr
     C                   endsr


     *================================================================
     * MVDBSC - Move data base fields to screen fields
     *================================================================
     C     mvdbsc        begsr

     C                   Z-ADD     nbrcdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scrcdt

     C                   Z-ADD     nbendt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scendt

     C                   Z-ADD     nbiwdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         sciwdt

     C                   Z-ADD     nbqtdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scqtdt

     C                   Z-ADD     nbdcdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scdcdt

     C                   Z-ADD     nblsdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         sclsdt

     C                   Z-ADD     nbaadt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scaadt

     C                   Z-ADD     nbpgdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scpgdt

     C                   Z-ADD     nbpold        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scpold

     C                   Z-ADD     nbpand        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scpand

      ****  begin chg  ***  05/31/05 ***********************************
     C                   Z-ADD     nbprdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scprdt

     C                   Z-ADD     nbpcdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scpcdt

     C                   Z-ADD     nbppdt        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         scppdt
      ****  end   chg  ***  05/31/05 ***********************************

     C                   endsr


     *================================================================
     * MVSCDB - Move screen fields to data base fields
     *================================================================
     C     mvscdb        begsr

     C                   z-add     emco#         nbco#
     C                   z-add     emfnd         nbfnd
     C                   z-add     ememp#        nbemp#

     C                   z-add     scrcdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbrcdt

     C                   z-add     scendt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbendt

     C                   z-add     sciwdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbiwdt

     C                   z-add     scqtdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbqtdt

     C                   z-add     scdcdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbdcdt

     C                   z-add     sclsdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nblsdt

     C                   z-add     scaadt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbaadt

     C                   z-add     scpgdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbpgdt

     C                   z-add     scpold        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbpold

     C                   z-add     scpand        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbpand

     ***  begin chg  ***  05/31/05 ***********************************
     C                   z-add     scprdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbprdt

     C                   z-add     scpcdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbpcdt

     C                   z-add     scppdt        SRDTI
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         nbppdt
     ***  begin chg  ***  05/31/05 ***********************************

     C                   endsr


     *================================================================
     * OTHERR - Processing for other modes
     *================================================================
     C     otherr        begsr
     C                   endsr


     *================================================================
     * OTHFKS - Processing for other functions keys
     *================================================================
     C     othfks        begsr
     C                   endsr


     *================================================================
     * PGMEND - End of program processing
     *================================================================
     C     pgmend        begsr

     c                   eval      *inlr = *on

     C                   endsr


     *================================================================
     * PGMSTR - Start of program processing
     *================================================================
     C     pgmstr        begsr

      * add mode
     C     @@MODE        COMP      'A'                                    40

     c                   eval      scprds = optnm
     C                   eval      scprds = centerFld(scprds)

     C     @@rrn1        chain     wmemp
     C                   eval      scfnd = emfnd
      *
      * Get CX date
      *
     C                   eval      tmp7 = GetCxDate(EMCO#:EMFND:
     C                                                       EMEMP#:EMDIV)
      *
     C                   Z-ADD     tmp7          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         sccxdt
      *  zzz
     C                   eval      *in50 = (sccxdt <> *zero)
      *
      * Get Employer Name
      *
     C                   eval      scedn = EmpDivName(EMEMP#:EMDIV:EM_rrn:
     C                                                EMFND:EMCO#)
      *
      *  GET New Business Tracking Information
      *
     C                   CLEAR                   dbrcdf
      *
     C     NBKY          CHAIN     PNB01
      *
     C                   if        %found
     C                   eval      *in40  = *off
     C                   eval      @@rrn  = nb_rrn

     c                   else

     C                   if        @@mode = 'C'
     C                   eval      *in40  = *on
     C                   eval      @@mode = 'A'
     C                   endif
      *
     C                   endif
      *
      *  GET Group Name
      *
     C                   eval      scfnam   = GetGrpName(EMFND:FN_rrn:EMCO#)
      *
      *  GET Agent/Producer Information
      *
     C                   CALL      'WA002R'
     C                   PARM      EMCO#         #1CO#             3 0
     C                   PARM      EMFND         #1FND             3 0
     C                   PARM      EMEMP#        #1EMP#            9 0
     C                   PARM      EMDIV         #1DIV             5 0
     C                   PARM      0             #1FYR             3 0
     C     SCAGT#        PARM                    #1AGT#            5 0
     C                   PARM                    #1ANAM           60
     C     SCPROD        PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
      *
     C                   eval      SCANAM = #1ANAM
     C                   eval      SCPNAM = #1PNAM

     C                   endsr


     *================================================================
     * PREOTH - Pre-processing for other modes
     *================================================================
     C     preoth        begsr
     C                   endsr


     *================================================================
     * UPDATR - Update data base record and after processing
     *================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb

     C                   eval      nbuser = q1user
     C                   eval      nbpgm = q1pgm
     C                   time                    dsdttm
     C                   eval      nbtime = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         nbdt

     C                   update    dbrcdf
E01  C                   endif
     c                   eval      outflg = *on
     C                   endsr


     *================================================================
     * WRITER - Write data base record and after processing
     *================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb

     C                   eval      nbuser = q1user
     C                   eval      nbiusr = q1user
     C                   eval      nbpgm = q1pgm
     C                   eval      nbipgm = q1pgm
     C                   time                    dsdttm
     C                   eval      nbtime = dstime
     C                   eval      nbitm = dstime
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel(p)  srdto         nbdt
     C                   movel(p)  srdto         nbidt

     C                   write     dbrcdf
     C                   feod      wmnbp
     c                   eval      outflg = *on
     C                   endsr


