     H*  (C) Copyright Fiserv, Inc., 2008
    H*
     H/TITLE W2305R01 - Image Routing Maintenance                     *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: Image Routing Maintenance                *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 02/18/08  C:191 PROJECT 129MQN                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* XX/XX/XX C:XXX Project XXXXXX                                 *
     H*                                                               *
     H*****************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw2305f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwairp     uf a e             disk
     F                                     rename(pir01:dbrcdf)
     Fwairl     if   e           k disk
     Fsmupl     if   e           k disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Image Routing'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##call          s              1
     D ##rrn           s              9  0
     D ##view          s              1  0
     D ##rtcd          s              1
     D ##user          s             10
     D ##usnm          s             50
     D ##lvl           s              3  0
     D ##type          s              1
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
    C*
     C     dtcmdp        plist
     C                   PARM                    SRDTI
     C                   PARM      *blanks       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM      *BLANK        SRERR
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
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
     C*
     C                   eval      *in90 = *on
     C                   if        @@mode = 'A'
     C* Validate user ID
     C**** User Id should not be blanks
     C                   if        scusr = *blanks
     C                   seton                                        81
     C                   leavesr
     C                   endif                                                  scusr=*blanks
     C*
     C**** Check for duplicate User Id
     C     scusr         chain     pir01
     C                   if        %found
     C                   seton                                        82
     C                   leavesr
     C                   endif                                                  %found
     C*
     C**** User ID must exist in SMUPL
     C     scusr         chain     pup01
     C                   if        not %found
     C                   seton                                        83
     C                   leavesr
     C                   endif                                                  %found
     C                   endif                                                  @@mode='A'
     C*
     C* Validate Folder name
     C**** Folder name should not be blanks
     C                   if        scfold = *blanks
     C                   seton                                        84
     C                   leavesr
     C                   endif                                                  scfold=*blanks
     C***
     C                   eval      ##rtcd = *blanks
     C                   call      'SVALIFS'
     C                   parm                    scfold
     C                   parm                    ##rtcd            1
     C***** Folder must exist
     C                   if        ##rtcd = '1'
     C                   seton                                        85
     C                   leavesr
     C                   endif                                                  ##rtcd = '1'
     C* Validate Status
     C**** Status should be 'I' or 'A'
     C                   if        scstat <> 'I' and scstat <> 'A'
     C                   seton                                        87
     C                   leavesr
     C                   endif                                                  scfold=*blanks
     C* No error found
     C                   eval      *in90 = *off
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
     C                   eval      scusr = irusr
     C                   eval      scfold = irfold
     C                   eval      scstat = irstat
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   eval      irusr = scusr
     C                   eval      irfold = scfold
     C                   eval      irstat = scstat
     C                   eval      irpgm = q1pgm
     C                   eval      iruser = q1user
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         irdt
     C                   eval      irtime = dstime
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
     C* Process search key
     C                   select
     C                   when      *inkj = *on
     C                   exsr      kjdo
     C                   endsl
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
     C* Set *IN40 = *On for Add mode
     C                   eval      *in40 = *off
     C                   if        @@mode = 'A'
     C                   eval      *in40 = *on
     C                   endif                                                  @@mode = 'A'
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
     C                   feod      wairp
     C                   endsr
    C*====================================================================
    C* KJDO - Processing for F10
    C*====================================================================
     C     kjdo          begsr
    C* Initial User Search
    C*
     C                   if        hfld = 'SCUSR'
     C                   movel(p)  scusr         xxusr            10
     C                   call      'S0005R'
     C                   parm      '2'           ##call
     C                   parm      *zeros        ##rrn
     C                   parm      *zeros        ##view
    C*
     C                   if        ##rrn = *zeros
     C                   movel(p)  xxusr         scusr
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   else                                                   ##rrn = *zeros
     C                   call      'SA013R'
     C                   parm      *blanks       ##rtcd
     C                   parm      ##rrn         ##rrn
     C                   parm      *blanks       ##user
     C                   parm      *blanks       ##usnm
     C                   parm      *zeros        ##lvl
     C                   parm      *blanks       ##type
     C*
     C                   if        ##rtcd = *blanks
     C                   movel(p)  ##user        scusr
     C                   eval      *in72 = *on
     C                   else                                                   ##rtcd = *blanks
     C                   clear                   scusr
     C                   eval      *in71 = *on
     C                   endif                                                  ##rtcd = *blanks
     C                   eval      *in90 = *on
     C                   endif                                                  ##rrn = *zeros
     C                   endif                                                  hfld = 'SCUSR'
     C                   endsr
