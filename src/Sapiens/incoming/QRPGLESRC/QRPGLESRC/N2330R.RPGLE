     H*  (C) Copyright Progressive Data Solutions, Inc., 2005
    H*
     H**********************************************************************
     H**   WORK WITH DISCOUNT CODES                                       **
     H**                                                                  **
     H**   CREATED 09/19/00 BY C:062 FOR PROJECT:NW068                    **
     H**********************************************************************
     H** REVISIONS:
     H*  05/22/19  c:180   Project MC432
     H*                    Add new processing for D7 discount type.
     H*
     H*  03/18/15  c:212   Project 114MRC
     H*                    Add call to FPINETDISC/NetworkDisc using FTBNDDIR
     H*                    to calculate network discounts
     H*
     H*  03/21/05 c:62 Project 114MJD
     H*                Add parm to subset to a particular type.
     H*
     H**********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
      /ENDIF
     H BNDDIR ('FTBNDDIR')                                                      03/18/15
     Fn2330f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     sfile(scr01s:sfrrn1)
     F                                     sfile(scr02s:sfrrn2)
     F                                     infds(fids01)
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fnmbdp     if   e             disk
     F                                     rename(pbd01:pfrcdf)
    F*
    F*  Primary logical file definition (for displaying items)
     Fnmbdl1    if   e           k disk
     F                                     infds(dbfds)
     F                                     rename(pbd01:dbrcdf)
    F*
    F*  Secondary logical file definition (for displaying items)
     Fnmbdl2    if   e           k disk
     F                                     infds(dbfds2)
     F                                     rename(pbd01:dbrcdf2)
     Ffms4l10   if   e           k disk
    D*
     d***** Beg Add *** 03/18/15 **************************************
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
     d***** End Add *** 03/18/15 **************************************
     D hd              s             78    dim(2) ctdata perrcd(1)
     D of              s              1    dim(12) ctdata perrcd(1)
     D ofd             s             35    dim(12) alt(of)
     D pgm             s             70    dim(2) ctdata perrcd(1)
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Discount Codes'
     D optn2           c                   'Discount Code Information'
     D dltmsg          c                   '**Deleted**'
     D slmsgi          c                   'SYS0001'
     D slmsgf          c                   'SYMESG'
     D acdpgm          c                   'N2330R01'
     D comapi          c                   'SA005R'
     D uspapi          c                   'S0006R'
     D infomx          c                   1
     D viewmx          c                   2
     D sf1pag          c                   12
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##kc            s              1
     D ##kl            s              1
     D ##mode          s              1
     D ##rrn           s              9  0
     D @@call          s              1
     D @@kc            s              1
     D @@kl            s              1
     D @@rrn           s              9  0
     C*** begin add *** 03/21/05 *******************************************
     D @@type          s                   like(bdtype)
     C*** end   add *** 03/21/05 *******************************************
     D kkcode          s                   like(bdcode)
     D kktype          s                   like(bdtype)
     D kkefdt          s                   like(bdefdt)
     D xxsel           s                   like(scsel)
     D kkdisc          s                   like(s4disc)
     D xxdate          s                   like(s4bgdt)
     D xxcmd           ds           140    inz
     D xxtype                  9     10
     D xxprds                 19     58
     D xxcall                 62     62
     D xxcode                 72     74
     D xxefdt                 78     85
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     d*
     d***** Beg Add *** 03/18/15 **************************************
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
     d***** End Add *** 03/18/15 **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYWW01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@call
     C                   parm                    @@kc
     C                   parm                    @@kl
     C                   parm                    @@rrn
     C*** begin add *** 03/21/05 *******************************************
     C                   parm                    @@type
     C*** end   add *** 03/21/05 *******************************************
    C*
    C*  Add/change/display program parameter list
    C*
     C     acdpl         plist
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##mode
     C                   parm      scdbrr        ##rrn
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    kkcode
     C                   kfld                    kktype
     C                   kfld                    kkefdt

     C     key2          klist
     C                   kfld                    kktype
     C                   kfld                    kkcode
     C                   kfld                    kkefdt

     C     s4ky          klist
     C                   kfld                    kkdisc
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* CHKSSM - Set indicator 11 for subset message
    C*================================================================
     C     chkssm        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Additional errors on subfile selection
    C*================================================================
     C     errck1        begsr

     C                   if        scsel = '2' or scsel = '5'
     C                   eval      xxsel = scsel
     C                   eval      scsel = '99'
     C                   endif

     C                   if        scsel = '4' and scstat = 'A'
     C                   seton                                        7190
     C                   endif

     C                   if        scsel = '20' and scstat = 'A'

     C                   time                    dsdttm
     C                   movel(p)  sccode        kkdisc
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdate

     C     s4ky          chain     ps401                              99
     C     *in99         doweq     *off
     C     s4bgdt        ifle      xxdate
     C     s4endt        andeq     *zeros
     C     s4bgdt        orle      xxdate
     C     s4endt        andge     xxdate
     C                   seton                                        9072
     C                   leave
     C                   endif
     C     s4ky          reade     ps401                                  99
     C                   enddo

     C                   endif

     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for the subset screen
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* INZSST - Initialize the subset fields
    C*================================================================
     C     inzsst        begsr
     C                   endsr
    C*================================================================
    C* KEYINZ - Initialize key values for program start
    C*================================================================
     C     keyinz        begsr

     C                   eval      kkcode = *blanks
     C                   eval      kktype = *blanks
     C                   eval      kkefdt = *zeros

     C                   endsr
    C*================================================================
    C* KEYSET - Set key values when doing "position to"
    C*================================================================
     C     keyset        begsr

     C   21              eval      kkcode = scpst1
     C   21              eval      kktype = *blanks
     C   22              eval      kkcode = *blanks
     C   22              eval      kktype = scpst2
     C                   eval      kkefdt = *zeros

     C                   endsr
    C*================================================================
    C* KEYSFL - Set key values from subfile records for paging
    C*================================================================
     C     keysfl        begsr

     C                   eval      kkcode = sccode
     C                   eval      kktype = sctype
     C                   eval      kkefdt = scefdt

     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr

     C                   eval      sccode = bdcode
     C                   eval      sctype = bdtype
     C                   eval      scefdt = bdefdt

     C   21              movel(p)  bdcode        scdata
     C   21              move      bdtype        scdata

     C   22              movel(p)  bdtype        scdata
     C   22              move      bdcode        scdata

     C                   movel     bdcom1        scdesc
     C                   movel     bdstat        scstat

     C                   z-add     bdefdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefd2

     C                   endsr
    C*================================================================
    C* OPT20 = Change Status
    C*================================================================
     C     opt20         begsr

     C                   eval      ##mode = 'S'
     C                   call      acdpgm        acdpl                    99

     C                   endsr
    C*================================================================
    C* OPT99 = Change/Display
    C*================================================================
     C     opt99         begsr

     C** Call QCMDEXC to get program based on type entered.

     C                   z-add     140           xxlen
     C                   movel(p)  pgm(1)        xxcmd
     C                   move      pgm(2)        xxcmd
     C                   movel(p)  sctype        xxtype
     C                   movel(p)  sccode        xxcode

     C                   Z-ADD     scefdt        srdti
     C                   CALL      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxefdt

     C                   movel(p)  optn2         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##DATA           80
     C                   parm      40            ##LENG            3 0
     C                   movel     ##data        xxprds

     C                   select
     C     xxsel         wheneq    '2'
     C                   movel     ' '           xxcall
     C     xxsel         wheneq    '5'
     C                   movel     '1'           xxcall
     C                   endsl

     C***** Beg Add *** 03/18/15 **************************************
     C                   eval      NetDisInd = *off
      *** Begin Change *** 05/22/19 ***********************************
     C****               If        sctype = 'DX'
     C                   If        sctype = 'DX' or sctype = 'D7'
      *** End   Change *** 05/22/19 ***********************************
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
     C***** End Add *** 03/18/15 **************************************

     C***** Beg Add *** 03/18/15 **************************************
     C                   If        NetDisInd = *on

     C                   callp     NetworkDisc(yytype:yyprds:yycall:yycode:
     C                                         yyefdt:yyrrn:yytabind)

     C                   Else
     C***** End Add *** 03/18/15 **************************************

     C                   call      'QCMDEXC'
     C                   parm                    xxcmd
     C                   parm                    xxlen            15 5

     C***** Beg Add *** 03/18/15 **************************************
     C                   Endif
     C***** End Add *** 03/18/15 **************************************

     C                   endsr
    C*================================================================
    C* OTHFKS - Process other function keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* OTHOPT - Process other options
    C*================================================================
     C     othopt        begsr

     C     scsel         caseq     '20'          opt20
     C     scsel         caseq     '99'          opt99
     C                   endcs

     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr

     C                   seton                                        lr

     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C                   endsr
    C*================================================================
    C* READ   - Read next record from file
    C*================================================================
     C     read          begsr

     C                   if        *in21 = *on
     C                   read      dbrcdf                                 95
     C                   endif

     C                   if        *in22 = *on
     C                   read      dbrcdf2                                95
     C                   endif

     C                   endsr
    C*================================================================
    C* READP  - Read previous record from file
    C*================================================================
     C     readp         begsr

     C                   if        *in21 = *on
     C                   readp     dbrcdf                                 95
     C                   endif

     C                   if        *in22 = *on
     C                   readp     dbrcdf2                                95
     C                   endif

     C                   endsr
    C*================================================================
    C* SETGT  - Set greater than on file
    C*================================================================
     C     setgt         begsr

     C                   if        *in21 = *on
     C     key1          setgt     dbrcdf                             96
     C                   endif

     C                   if        *in22 = *on
     C     key2          setgt     dbrcdf2                            96
     C                   endif

     C                   endsr
    C*================================================================
    C* SETLL  - Set lower limits on file
    C*================================================================
     C     setll         begsr

     C                   if        *in21 = *on
     C     key1          setll     dbrcdf                             96
     C                   endif

     C                   if        *in22 = *on
     C     key2          setll     dbrcdf2                            96
     C                   endif

     C                   endsr
    C*================================================================
    C* SF1UPD - Update subfile record when item is deleted
    C*================================================================
     C     sf1upd        begsr

     C                   movel(p)  dltmsg        scdata

     C                   endsr
    C*================================================================
    C* SLCTRD - Move fields for selected record to parameters
    C*================================================================
     C     slctrd        begsr

     C                   eval      @@rrn = scdbrr

     C                   endsr
    C*================================================================
    C* SUBSET - Determine if record should be put in subfile
    C*================================================================
     C     subset        begsr
     C*** begin add *** 03/21/05 *******************************************
     C*
     C* subset to the type passed in
     C*
     C                   if        q1prms > 4
     C                   if        @@type <> *blanks and
     C                             @@type <> bdtype
     C                   eval      meetss = *off
     C                   endif
     C                   endif
     C*** end   add *** 03/21/05 *******************************************
     C                   endsr
** Column headings
Opt  code    type  Description                                Eff. date Sts
Opt  type    code  Description                                Eff. date Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               NNN
3
** pgm
CALL N91XXR PARM('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' 'x'
'xxx' 'xxxxxxxx')
