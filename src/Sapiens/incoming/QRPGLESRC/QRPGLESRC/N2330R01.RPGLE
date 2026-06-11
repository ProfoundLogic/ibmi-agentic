     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) ACTGRP(*CALLER)
     H bnddir ('FTBNDDIR')
    H*
     H**********************************************************************
     H**   DISCOUNT CODE INFORMATION                                      **
     H**                                                                  **
     H**   CREATED 09/19/00 BY C:062 FOR PROJECT:NW068                    **
     H**********************************************************************
     H** REVISIONS:
     H*
     H*  08/31/23 C:079 Support: 114/107354
     H*                 Prevent duplicate discount codes for DS and DR
     H*                                                                    *
     H*  03/18/15  c:212   Project 114MRC
     H*                    Add call to FPINETDISC/NetworkDisc using FTBNDDIR
     H*                    to calculate network discounts
     H*                                                                    *
     H*  08/12/10 C:198 Project: 114MWS                                    *
     H*                 Added an edit to check for where Medicare PPO      *
     H*                 processing module is installed or not in order to  *
     H*                 allow addition for discount type DR/DS.            *
     H*                                                                    *
     H*  09/02/08 C:212 Project: 114MQM                                    *
     H*                 Perform the check for duplicate record in NMBDP    *
     H*                 with the process generated code (NXDSP/DSCODE) in  *
     H*                 case of 'DQ' type discount code.                   *
     H*                                                                    *
     H*  02/21/08 C:183 Project: 114MPX                                    *
     H*                 Call FA753R to write Tracking Record.              *
     H*
     H**********************************************************************
     Fn2330f01  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fnmbdp     uf a e             disk
     F                                     rename(pbd01:dbrcdf)
     Fnmbdl1    if a e           k disk
     F*** Beg Add *** 08/31/23 *****************************************
     Fnmbdld    if a e           k disk    Rename(pbd01:pbd0d)
     F*** End Add *** 08/31/23 *****************************************
     Fftgtp     if a e             disk
     Fftgtl     if a e           k disk
     F                                     rename(pgt01:pgt02)
     Fndcol     uf a e           k disk
     Fnxdsl     if   e           k disk
     Fssylf     if   e           k disk
     F                                     ignore(SWC01)
     F                                     ignore(SPC01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SSY01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)
     d*
     d* network discount  (FTBNDDIR/FPINETDISC)
     d*
     d networkDisc     pr
     d inType                         2    const
     d inPrDs                        40    const
     d inCall                         1    const
     d inCode                         3    const
     d inEfdt                         8    const
     d inRrn                          9    const options(*nopass)
     d inTabInd                       5    const options(*nopass)
     d*
     D pgm             s             70    dim(2) ctdata perrcd(1)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Discount Codes'
     D optn2           c                   'Discount Code Information'
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
     D jjcode          s                   like(bdcode)
     D kkcode          s                   like(bdcode)
     D kktype          s                   like(bdtype)
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
     d yyType          s              2    inz
     d yyPrDs          s             40    inz
     d yyCall          s              1    inz
     d yyCode          s              3    inz
     d yyEfdt          s              8    inz
     d yyRrn           s              9    inz
     d yyTabInd        s              5    inz
     d*
     d NetDisInd       s               n   inz(*off)
     d*
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
     C                   kfld                    kktype
     C                   kfld                    kkefdt

     C     bdky1         klist
     C                   kfld                    jjcode
     C                   kfld                    kktype
     C                   kfld                    kkefdt

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

     C                   eval      NetDisInd = *off
     C                   If        sctype = 'DX'
     C                   eval      NetDisInd = *on
     C                   eval      yytype = sctype
     C                   eval      yyprds = xxprds
     C                   eval      yycall = xxcall
     C                   eval      yycode = xxcode
     C                   eval      yyefdt = %char(%date(xxefdt:*usa0):*iso0)
     C                   eval      yyrrn  = *blanks
     C                   eval      yytabind = *blanks
     C                   Else
     C                   EndIf

     C                   If        NetDisInd = *on

     C                   callp     NetworkDisc(yytype:yyprds:yycall:yycode:
     C                                         yyefdt:yyrrn:yytabind)

     C                   Else

     C                   call      'QCMDEXC'
     C                   parm                    xxcmd
     C                   parm                    xxlen            15 5

     C                   Endif

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

     C                   select
     C     bdtype        wheneq    'D7'
     C     bdtype        oreq      'DE'
     C     coky          chain     pco01                              99
     C     *in99         doweq     *off
     C                   delete    pco01
     C     coky          reade     pco01                                  99
     C                   enddo
     C                   endsl
     C                   IF        CLTID = '114'
     C                   EVAL      @@CODE = BDCODE
     C                   EVAL      @@TYPE = BDTYPE
     C                   EVAL      @@RCDT = BDRCDT
     C                   EVAL      @@EFDT = BDEFDT
     C                   ENDIF

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
     C                   eval      kktbid = 'DSTY'
     C                   movel(p)  sctype        kkkey
     C     gtky          chain     pgt02                              99
     C   99              seton                                        7290
     C                   endif

     C*** Retrieve flag from system control file to allow  DR/DS
     C*** as discount types.
     C                   move      *blanks       XXKEY             2
     C                   move      'FB'          XXKEY
     C     xxkey         chain     sfb01                              99
     C*
     C                   if        *in99 = '0'
     C                             and %subst(fbst08:2:1) <> 'Y'
     C*
     C                   if        sctype = 'DR' or sctype = 'DS'
     C                   eval      *in75 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C                   endif
       //*** Beg Add *** 08/31/23 **********************************
       // For discount types DR and DS prevent duplicate discount codes
       If scType = 'DR' or scType = 'DS';
          Setll (scCode) pbd0d;
          Reade (scCode) pbd0d;
          Dow not %Eof(NMBDLD);
             If BDtype <> 'DR' and BDtype <> 'DS';
                erDupCode = *on;
                *in90 = *on;
                msDupCode = 'When adding a discount code for discount +
                 type DS or DR you must use a unique discount code. +
                 The discount code you entered is already being used for +
                 other discount types.';
                Leave;
             Endif;
             Reade (scCode) pbd0d;
          Enddo;
       Endif;
       //*** End Add *** 08/31/23 *************************************

     C* Effective date must be valid

     C                   if        *in90 = *off
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C                   if        srerr = '1'
     C                   seton                                        7390
     C                   endif
     C                   endif


     C* Discount code information cannot be duplicated

     C                   if        *in90 = *off
     C                   eval      kkcode = sccode
     C                   eval      kktype = sctype
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkefdt
     C     bdky          chain     pbd01                              99
     C  n99              seton                                        7490
     C** It could be the case that there is a process generated code    **
     C** (nxdsp/dscode) for this code(kkcode) with which records are    **
     C** present in NMBDP. Check for duplicate with that code too.      **
     C*
     C                   If        *in99 = '1' and kktype = 'DQ'
     C     kkcode        chain     pds01                              98
     C                   IF        *in98 = '0'
     C                   eval      jjcode = dscode
     C     bdky1         chain     pbd01                              99
     C  n99              seton                                        7490
     C                   endIF
     C                   endIf
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
     C                   parm      'DSTY'        ##tbid            4

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
         //*** Beg Add *** 08/31/23 *************************************
       erDupCode = *off;
       //*** End Add *** 08/31/23 *************************************
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   IF        @@MODE = 'E' AND CLTID = '114'
      *
     C                   MOVEL(P)  'Deleted'     @@FLD
      *
     C                   CALL      'FA753R'
     C                   PARM                    @@CODE            3
     C                   PARM                    @@TYPE            2
     C                   PARM                    @@RCDT            1
     C                   PARM                    @@EFDT            7 0
     C                   PARM      *BLANKS       @@CPT4            5
     C                   PARM      *BLANKS       @@PMOD            2
     C                   PARM      *BLANKS       @@10A1           10
     C                   PARM      *BLANKS       @@10A2           10
     C                   PARM      *BLANKS       @@10A3           10
     C                   PARM      *BLANKS       @@10A4           10
     C                   PARM                    @@FLD            10
     C                   PARM                    @@FVAL           80
     C                   PARM                    @@TVAL           80
     C
     C                   ENDIF                                                  @@MODE = 'E'
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr

     C     @@mode        comp      'A'                                    40
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3

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
CALL N91XXR PARM('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' 'x'
'xxx' 'xxxxxxxx')
