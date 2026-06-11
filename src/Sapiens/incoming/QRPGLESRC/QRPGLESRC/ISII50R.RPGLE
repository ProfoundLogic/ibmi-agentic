     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
    H*==================================================================*
    H/TITLE ISII50R - PMSC POLICY INDEX PROGRAM
    H*==================================================================*
    H* Description: This program indexes incoming images by PMSC's      *
    H*              Policy key.                                         *
    H*                                                                  *
    H* Called from: IS215R - Work With Queued Entries                   *
    H*              IS213R - Select Index Type                          *
    H* Calls to   :                                                     *
    H*                                                                  *
    H* Created    : 12/30/1999                                          *
    H* Programmer : C:113                                               *
    H* Project    : 81WQW                                               *
    H*==================================================================*
    H* REVSIONS:                                                     *
      *                                                               *
      * 10/17/11 C:279 Project: SY195                                 *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
      * 01/06/09 C:062 PROJECT: 114IRZ                                *
      *                Add BarCode Indexing                           *
      *                                                               *
      * 03/15/06 C:132 PROJECT: IM120 - 51809                         *
      *                Fix Diary Flag Problems                        *
      *                                                               *
      * 01/30/02 C:128 PROJECT: DR169                                 *
      *                If remark type changed,change description.     *
      *                                                               *
    H* 08/17/01 C:128 PROJECT: IM081  SUPPORT #28626                    *
    H*          Program would skip validity checking if a search was    *
    H*          done. In that case the 'Skip Error Check' indicator(97) *
    H*          would be set on and never set off.                      *
    H*                                                                  *
    H*                                                                  *
    H* ---------------------------------------------------------------- *
    H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.              *
    H* ---------------------------------------------------------------- *
    H*==================================================================*
    H* INDICATOR USAGE                                                  *
    H*                                                                  *
    H* GLOBAL---------------------------------------------------------- *
    H*  03    Office NOT installed.                                     *
    H* N03    Office IS installed.                                      *
    H*  34    Data entry program option is installed.                   *
    H*  37    Used on call to Data Entry Program (DEPGM).               *
    H*  56    Display "UNSECURE REMARK", remark is secured.             *
    H* N56    Display "SECURE REMARK", remark is unsecured.             *
    H*  59    Validate chain command match for default security level.  *
    H*  60    General chain.                                            *
    H*  97    Do not process error check.                               *
    H* N98    Either no security or meets security level.               *
    H*  98    Protect remark type because of security level.            *
    H*  99    Image product is installed.                               *
    H*                                                                  *
    H* ERROR----------------------------------------------------------- *
    H*  26    SCM0609-Document ID is not allowed with a remark type     *
    H*                that has an associated data entry program ID.     *
    H*  31    SCM0550-AS/400 Officevision is not installed.             *
    H*  32    SCM0532-Default Folder required if Document ID indicated. *
    H*  35    SCM0502-Value must be 'Y' or 'N'.                         *
    H*  42    SCM0603-Value must be 'Y', 'N' or blank.                  *
    H*  43    SCM0604-This remark type is non-purgeable.                *
    H*  44    SCM0605-Diary type must be 'Y' in order for this remark   *
    H*                to be purgeable.                                  *
    H*  46    SCM0502-Value must be 'Y' or 'N'.                         *
    H*  47    SCM0619-Must find alternative access to this data entry   *
    H*                program.                                          *
    H*  51    SCM0501-Value entered is not defined in table file.       *
    H*  52    SCM0500-Invalid date entered.                             *
    H*  53    SCM0503-Value cannot be equal to blanks - remark required.*
    H*  54    SCM0516-Document not found in specified folder.           *
    H*  58    SCM0612-User is not enrolled in DARTS. See your           *
    H*                System Administrator.                             *
    H*  62    SCM0557-Invalid Location entered.                         *
    H*  63    SCM0558-Invalid Master Company entered.                   *
    H*  64    SCM0559-Invalid Policy number entered.                    *
    H*  65    SCM0560-Invalid Symbol entered.                           *
    H*  66    SCM0561-Invalid Module entered.                           *
    H*==================================================================*
     Fisii50f   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     infds(fids01)
    F*
    F** Image Batch Administration Master File
     Fimbml     if   e           k disk
    F*
    F** Image Document Type Master File
     Fimdtl     if   e           k disk
    F*
    F** Image Document Notes
     Fimdnl2    if   e           k disk
    F*
    F** DARTS Remark Detail File
     Fsdcml     o    e             disk
    F*
     Fsdcmli    if   e           k disk
     F                                     rename(pcm01:pcm0i)
    F*
    F** DARTS Security Master File
     Fsmsll     if   e           k disk
    F*
    F** DARTS Remark Type Table File
     Fstctl1    if   e           k disk
    F*
    F** System & Application Control File
     Fssylf     if   e           k disk
    F*
    F** User Paramters File
     Fwmupl     if   e           k disk
    F*
    F** Work File
     Fwxerp1    if   e             disk    usropn
    F*
    F*** PMSC Files ***
    F*
    F** PMSC Location File
     Fasaarel1  if   e           k disk
    F*
    F** PMSC Master Company File
     Fasabrel1  if   e           k disk
    F*
    F** PMSC Policy File
     Fpmsp0200  if   e           k disk
      ** Logical for Symbol
     Fsmsyl     if   e           k disk    rename(rec02fmt:psy01)
     F*** begin add *** 01/06/2009 *****************************************
     Fimral     if   e           k disk
     F*** end   add *** 01/06/2009 *****************************************

    D*==================================================================*
    D* Array & Table Definitions
    D*==================================================================*
     D ck1             s             80    dim(5) ctdata perrcd(1)
     D dn              s             75    dim(4)
     D qca             s             55    dim(1) ctdata perrcd(1)
    D*
     D pgmnam          c                   const('Policy Index')
     D comapi          c                   const('WA001R')
    D*
     D mdoc            c                   const('To Darts:')
     D msen            c                   const('User :')
     D ddat            c                   const('Date:')
     D dtim            c                   const('Time:')
     D drea            c                   const('** INDEXED **')
    D*
    D** 'Point' application name for policy search
     D point           c                   const('BMICPXRF')
    D*
    D** DARTS PNAM
     D scpnam          c                   const('PMSPOL')
    D*
    D** Date & Time structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D*** begin add *** 01/06/2009 *****************************************
    D** PMSC Key structure
     D                 ds
     D yydkey                        40
     D  yyloc                         2    overlay(yydkey)
     D  yymco                         2    overlay(yydkey:3)
     D  yysym                         3    overlay(yydkey:5)
     D  yypol#                        7    overlay(yydkey:8)
     D  yymod                         2    overlay(yydkey:15)
     D*** end   add *** 01/06/2009 *****************************************
    D** PMSC Key structure
     D                 ds
     D ##pmky                        16
     D  ##loc                         2    overlay(##pmky)
     D  ##mco                         2    overlay(##pmky:3)
     D  ##sym                         3    overlay(##pmky:5)
     D  ##pol#                        7    overlay(##pmky:8)
     D  ##mod                         2    overlay(##pmky:15)
    D*
    D** File Information data structure
     D fids01        e ds                  extname(szq2p)
    D*
    D** Program status data structure
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D $$dcid          s             12
     D $$doct          s              8
     D $$qid           s             10
     D $$user          s             10
     D $#dcid          s             12
     D $xseq           s              5
     D #$nowd          s              8
     D #$nowt          s              6
     D #$sdt           s              8
     D #$stm           s              6
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##key           s             40
     D ##pnam          s             10
     D ##sbyl          s              1
     D ##secl          s              3  0
     D ##seq#          s              5  0
     D #diary          s              1
     D @#ddsc          s             30
     D @#flr           s              8
     D @#pth           s             63
     D @#rtc1          s              1
     D @#sflr          s             12
     D @@cdat          s              7  0
     D @@time          s              6  0
     D c               s              5  0
     D c1              s              5  0
     D dcid            s             12
     D decom1          s            256
     D decom2          s             19
     D dekey           s                   like(cmkey)
     D dertcd          s              1
     D errfle          s              1
     D fq              s             10
     D g               s              3  0
     D hhdt            s              7  0
     D hhtime          s              6  0
     D hlrecf          s                   like(q2recf)
     D kPolicy         s              7a
     D length          s             15  5
     D ofcpgm          s                   like(dral02)
     D pmsid           s              2
     D q$rtc           s              1
     D q@key           s             40
     D q@rcd           s              1
     D q@rtc           s              1
     D q@rtx           s              1
     D q@seq#          s              5  0
     D qq#pgs          s              6  0
     D qqbchi          s             12
     D qqcm75          s             75
     D qqdcid          s             12
     D qqdct           s              1
     D qqddsc          s             40
     D qqdoct          s              8
     D qqdsts          s              2
     D qqkey           s             40
     D qqpnam          s             10
     D qqprfl          s              1
     D qqpth           s             40
     D qqqdsc          s             40
     D qqqid           s             10
     D qqrtc           s              1
     D qqrtcd          s              1
     D qqsdat          s              8  0
     D qqseq#          s              5  0
     D qqsrep          s              8
     D qqstim          s              6  0
      ***** BEG ADD ***** 10/17/11 ***********************************
     d qquncn          s            255
     d qqifsp          s              1
      ***** END ADD ***** 10/17/11 ***********************************
     D rtrn            s              6
     D scbdsc          s             40
     D sccode          s              5
     D scdflr          s             80
     D scdusr          s             10
     D scqdsc          s             40
     D scqid           s             10
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D svscrn          s                   like(q2recf)
     D tu              s             10
     D uid             s             10
     D v@key           s             40
     D v@seq#          s              5
     D xxcdat          s                   like(cmcdat)
     D xxd             s              2
     D xxdate          s                   like(cmcdat)
     D xxdcid          s             12
     D xxdefl          s              1
     D xxdepg          s              1
     D xxdewr          s              1
     D xxdsdt          s                   like(cmcdat)
     D xxfold          s             80
     D xxh             s              2
     D xxhms           s              8
     D xxm             s              2
     D xxmdyy          s             10
     D xxofif          s              1
     D xxpgm           s             10
     D xxpnam          s             10
     D xxs             s              2
     D xxseq#          s                   like(cmseq#)
     D xxsyk           s              2
     D xxtime          s                   like(cmtrtm)
     D xxy             s              4
     D xx3a            s              3
     D xx5a            s              5
     D xx6a            s              6
     D xx75            s             75
     D zzrtcd          s              1
    I*==================================================================*
    I* Data Structure & Constant Definitions
    I*==================================================================*
     Ipcm0i
     I              cmkey                       pikey
     I              cmseq#                      piseq#
    C*==================================================================*
    C* KLIST Definitions
    C*==================================================================*
    C*
    C** DARTS Additional Remarks file
     C     arky          klist
     C                   kfld                    cmkey
     C                   kfld                    cmseq#
    C*
    C** Image Batch Master file
     C     bmky1         klist
     C                   kfld                    qqbchi
    C*
    C** DARTS Remarks Detail file
     C     cmkya         klist
     C                   kfld                    qqkey
     C                   kfld                    ##seq#
    C*
    C** DART Remark Type file
     C     ctky          klist
     C                   kfld                    cmrmcd
    C*
     C     ctky1         klist
     C                   kfld                    sccode
    C*
    C** Image Document Notes file
     C     dnky2         klist
     C                   kfld                    qqdcid
     c*** begin add *** 01/06/2009 *****************************************
    C*
    C** Restrict auto index file
     C     raky          klist
     C                   kfld                    qqdcid
     C*** end   add *** 01/06/2009 *****************************************
    C*
    C** Image Document Type Table file
     C     dtky          klist
     C                   kfld                    scdoct
    C*
    C** DARTS Security file
     C     slky          klist
     C                   kfld                    $$user
    C*
    C** System Control file
     C     syky          klist
     C                   kfld                    xxsyk
    C*
    C** User Parameter file
     C     upkey         klist
     C                   kfld                    q1user
    C***************
    C*  PMSC files
    C***************
    C** PMSC Location file
     C     locky         klist
     C                   kfld                    scloc
    C*
    C** PMSC Master Company file
     C     mcoky         klist
     C                   kfld                    scloc
     C                   kfld                    scmco
    C*
    C** PMSC Policy file
     C     polky         klist
     C                   kfld                    pmsid
     C                   kfld                    scloc
     C                   kfld                    kPolicy
    C*
    C** PMSC Symbol key
     C     symky         klist
     C*                  kfld                    pmsid
     C*                  kfld                    scloc
     C*                  kfld                    kPolicy
     C                   kfld                    scsym
    C*
    C** PMSC Module key
     C     modky         klist
     C                   kfld                    pmsid
     C                   kfld                    scloc
     C                   kfld                    kPolicy
     C                   kfld                    scsym
     C                   kfld                    scmco
     C                   kfld                    scmod
    C*==================================================================*
    C* PLIST Definitions
    C*==================================================================*
    C*
    C** Entry parameters
     C     *entry        plist
     C                   parm                    qqdcid
     C                   parm                    qqpth
     C                   parm                    qqdoct
     C                   parm                    qqddsc
     C                   parm                    qqqid
     C                   parm                    qqqdsc
     C                   parm                    qqbchi
     C                   parm                    qq#pgs
     C                   parm                    qqsdat
     C                   parm                    qqstim
     C                   parm                    qqsrep
     C                   parm                    qqkey
     C                   parm                    qqseq#
     C                   parm                    qqrtcd
      ***** BEG ADD ***** 10/17/11 ***********************************
     c                   parm                    qquncn
     c                   parm                    qqifsp
      ***** END ADD ***** 10/17/11 ***********************************
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C** Date check/conversion parameters
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C** PMSC Search program parameters
     C     pmscpl        plist
     C                   parm                    ##pmky
    C*==================================================================*
    C* Main
    C*==================================================================*
     C                   exsr      pgmstr
    C*
     C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or *in90 = *off
    C*
     C                   exsr      predsp
     C                   exfmt     scr01
    C*
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
    C*** 08/17/01 Begin Change ********************************************
     C                   move      *off          *in97
    C*** 08/17/01 End   Change ********************************************
    C*
     C                   if        *inkc = *off
     C                             and *inkl = *off
     C                   exsr      othfks
    C*
    C*
     C                   if        *in97 = *off
     C                   exsr      errck1
     C                   endif
    C*
     C                   if        *in90 = *off
     C                   exsr      finish
     C                   endif
    C*
     C                   endif
    C*
     C                   enddo
    C*
     C                   exsr      pgmend
    C*
     C                   move      *on           *inlr
    C*====================================================================*
    C*  CHKPMS  SUBROUTINE - EDIT PMSC KEY FIELDS
    C*====================================================================*
     C     chkpms        begsr
    C*
    C** Location
     C                   if        *in90 = *off
     C     locky         chain     asaarel1                           62
     C                   if        *in62 = *on
     C                   move      *on           *in90
     C                   endif
     C                   endif
    C*
    C** Master Company
     C                   if        *in90 = *off
     C     mcoky         chain     asabrel1                           63
     C                   if        *in63 = *on
     C                   move      *on           *in90
     C                   endif
     C                   endif
    C*
    C** Policy Number
     C                   if        *in90 = *off
     C                   move      scpol#        kPolicy
     C     polky         chain     pmsp0200                           64
     C                   if        *in65 = *on
     C                   move      *on           *in90
     C                   endif
     C                   endif
    C*
    C** Symbol
     C                   if        *in90 = *off
     C                   move      scpol#        kPolicy
     C     symky         chain     psy01                              65
     C                   if        *in64 = *on
     C                   move      *on           *in90
     C                   endif
     C                   endif
    C*
    C** Module
     C                   if        *in90 = *off
     C                   move      scpol#        kPolicy
     C     modky         chain     pmsp0200                           60
     C                   if        *in66 = *on
     C                   move      *on           *in90
     C                   endif
     C                   endif
    C*
     C                   endsr
    C*====================================================================*
    C*  DNOT DOC NOTES  - CREATE DOC NOTES AND CPY NOTES TO SMAR
    C*====================================================================*
     C     dnot          begsr
    C*
    C*  Fill all doc note fields.
    C*
     C                   time                    dsdttm
     C                   move      dsdate        #$nowd
     C                   move      dstime        #$nowt
    C*
     C                   clear                   xx75
     C                   movel     #$nowd        xxm
     C                   move      #$nowd        xxy
     C     2             subst     #$nowd:3      xxd
     C     xxm           cat(p)    '/'           xx3a
     C     xx3a          cat(p)    xxd           xx5a
     C     xx5a          cat(p)    '/'           xx6a
     C     xx6a          cat(p)    xxy           xxmdyy
    C*
     C                   movel     #$nowt        xxh
     C                   move      #$nowt        xxs
     C     2             subst     #$nowt:3      xxm
     C     xxh           cat(p)    ':'           xx3a
     C     xx3a          cat(p)    xxm           xx5a
     C     xx5a          cat(p)    ':'           xx6a
     C     xx6a          cat(p)    xxs           xxhms
    C*
     C     ddat          cat(p)    xxmdyy:1      xx75
     C                   cat(p)    dtim:1        xx75
     C                   cat(p)    xxhms:1       xx75
     C                   cat(p)    drea:5        xx75
    C*
     C                   movel     xx75          dn(1)
    C*
     C                   move      qqseq#        $xseq
     C     mdoc          cat(p)    cmkey:1       xx75
     C                   cat(p)    $xseq:5       xx75
    C*
     C                   movel     xx75          dn(2)
    C*
     C     msen          cat(p)    q1user:1      xx75
     C                   cat(p)    '.'           xx75
     C                   movel     xx75          dn(3)
     C                   clear                   dn(4)
    C*
    C*  Call the program to write doc notes.
    C*
     C                   do        4             g
    C*
     C                   movel     qqdcid        $#dcid
     C                   movel     dn(g)         qqcm75
    C*
     C                   if        g >= 4
     C                   move      '1'           q$rtc
     C                   else
     C                   move      ' '           q$rtc
     C                   endif
    C*
     C                   call      'ISDOCNR'
     C                   parm                    qqcm75
     C                   parm                    qqdcid
     C                   parm                    #$nowd
     C                   parm      q1pgm         qqpnam
     C                   parm      'H'           qqprfl
     C                   parm                    q$rtc
    C*
     C                   enddo
    C*
    C*  Now we need to create the Additional Remarks for this
    C*   DART, by read/writing all Notes to the SMARP file.
    C*   We will also BACK DATE the Remarks, so they are NOT modifiable.
    C*
     C                   call      'ISNOTAR'
     C                   parm                    qqdcid
     C                   parm      cmkey         q@key
     C                   parm      cmseq#        q@seq#
     C                   parm      *blanks       q@rcd
    C*
     C                   endsr
    C*================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*================================================================
     C     dscapi        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr
    C*====================================================================*
    C*  ERRCK1  SUBROUTINE - EDIT SCREEN 1 FIELDS
    C*====================================================================*
     C     errck1        begsr
    C*
    C*  Validate PMSC key fields.
    C*
     C                   exsr      chkpms
    C*
    C*  Date entered must be valid.
    C*
     C                   eval      srdti = sccdat
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sccdat
    C*
     C  n90              if        srerr = '1'
     C                   move      *on           *in52
     C                   move      *on           *in90
     C                   endif
    C*
     C                   move      ' '           #diary
    C*
    C*  Remark type must be entered and valid.
    C*
     C                   movel     scdept        sccode
     C                   move      scrmty        sccode
    C*
    C*  If remark is secured, the secuity level displayed on screen
    C*   is moved into the field to be stored when record is made
    C*   or updated.
    C*
     C                   if        *in56 = *on
     C                             and scsecl <> ##secl
     C                   movel     scsecl        ##secl
     C                   else
     C                   if        *in56 = *off
     C                   clear                   scsecl
     C                   movel     scsecl        ##secl
     C                   endif
     C                   endif
    C*
     C  n90              if        sccode <> *blanks
     C     sccode        chain     stctl1                             60
     C                   if        *in60 = *on
     C                   move      *on           *in51
     C                   move      *on           *in90
     C                   endif
    C*
     C                   else
     C                   move      *on           *in41
     C                   move      *on           *in90
     C                   endif
    C*
    C*  Determine if remark type is diary or not.
    C*
     C  n90              if        scditp = 'Y'
    C*
    C*  User name, if diary type, must be valid.
    C*
     C                   movel     scdusr        $$user
     C     slky          chain     psl01                              60
     C                   if        *in60 = *on
     C                   move      *on           *in58
     C                   move      *on           *in90
     C                   endif
     C                   endif
    C*
     C  n90              if        scditp <> ' '
      ***** BEG CHG ***** 03/15/06 ********************************************
     C*****************            and scditp <> 'Y'
     C                   IF        scditp <> 'Y'
      ***** END CHG ***** 03/15/06 ********************************************
     C                             and scditp <> 'N'
     C                   move      *on           *in35
     C                   move      *on           *in90
      ***** BEG ADD ***** 03/15/06 ********************************************
     C                   EndIf
      ***** END ADD ***** 03/15/06 ********************************************
     C                   else
     C                   move      ctfl01        scditp
     C                   endif
    C*
    C*  Check Data Entry Flag to see if Data Entry Program has been
    C*     called once already.
    C*
     C                   if        xxdefl = 'N'
    C*
    C*  If DRFL03 = 'Y' in file SDRPF, Remark Code is checked to see
    C*     if it has an associated Data Entry Program ID.
    C*
     C                   move      'N'           xxdepg
     C     syky          chain     sdr01                              40
     C                   if        *in40 = *off
     C                   if        drfl03 = 'Y'
     C                   if        ctdepg <> *blanks
     C                   move      'Y'           xxdepg
    C*
    C*  Doc ID not allowed if the remark type has an associated data
    C*   entry program.
    C*
     C  n90              if        scdcid <> *blanks
     C                   move      *on           *in26
     C                   move      *on           *in90
     C                   endif
    C*
    C*  Check to see if a recursive call was made.  If so... error msg
    C*
     C                   clear                   xxpgm
     C                   movel     ctdepg        xxpgm
     C                   clear                   scdcid
     C                   movel     ctdepg        scdcid
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdsdt
     C                   eval      srdti = sccdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxcdat
    C*
     C  n90              if        xxcdat <= xxdsdt
    C*
     C                   if        errfle = ' '
     C                   call      'ERROR1C'
    C*
     C                   eval      length = 55
    C*
     C                   call      'QCMDEXC'                              37
     C                   parm                    qca(1)
     C                   parm                    length
    C*
     C                   open      wxerp1
     C                   move      'Y'           errfle
     C                   endif
    C*
     C     7             chain     per01                              37
     C                   dow       *in37 = *off
    C*
    C*  Check for Recursive call to program
    C*
     C                   if        erpgm = xxpgm
     C                   move      *on           *in47
     C                   move      *on           *in90
     C                   endif
    C*
     C                   read      per01                                  37
     C                   enddo
    C*
    C*  ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
    C*  (NOTE: It was necessary to break up the comment field of size
    C*   275 into DECOM1 and DECOM2 because the largest alpha-numeric
    C*   value allowed for a parameter is 256.
    C*
     C                   if        *in90 = *off
    C*
     C                   clear                   dekey
     C                   clear                   decom1
     C                   clear                   decom2
     C                   clear                   dertcd
     C                   movel     qqkey         dekey
    C*
     C                   call      ctdepg                               37
     C                   parm                    dekey
     C                   parm                    decom1
     C                   parm                    decom2
     C                   parm                    dertcd
    C*
    C*
     C                   if        *in37 = *on
     C                   move      *on           *in90
     C                   endif
    C*
     C                   endif
    C*
     C                   move      'Y'           xxdefl
    C*
     C                   if        dertcd = 'A'
     C                             or dertcd = 'B'
     C                   move      *on           *in90
     C                   endif
    C*
     C                   if        dertcd = *blanks
     C                   move      'Y'           xxdewr
     C                   clear                   sccomm
     C                   if        decom1 = *blank
     C                             and decom2 = *blank
     C                   movel     ctdesc        sccomm
     C                   movel     ctdesc        decom1
     C                   else
     C                   movel     decom1        sccomm
     C                   move      decom2        sccomm
     C                   endif
    C*
     C                   clear                   scdcid
     C                   movel     ctdepg        scdcid
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdsdt
     C                   eval      srdti = sccdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxcdat
     C                   if        xxcdat <= xxdsdt
    C*
     C                   move      *on           *in20
     C                   move      *on           *in90
     C                   endif
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    C*
     C                   if        *in99 = *off
     C                             and xxdefl = 'N'
     C                             and *in90 = *off
    C*
    C*  Doc ID must be valid.
    C*
     C                   movel     scdcid        xxdcid
     C                   movel     scdflr        xxfold
     C                   clear                   zzrtcd
    C*
     C                   if        scdcid <> *blank
    C*
    C*  If AS/400 Office NOT Installed - Don't allow this function
    C*
     C                   if        xxofif = 'N'
     C                   move      *on           *in31
     C                   move      *on           *in90
     C                   endif
    C*
    C*  Must have a Default Folder entered for this function
    C*
     C  n90              if        xxfold = *blanks
     C                   move      *on           *in32
     C                   move      *on           *in90
     C                   endif
    C*
     C                   if        *in90 = *off
     C                   call      'SVALDOC'
     C                   parm                    xxdcid
     C                   parm                    xxfold
     C                   parm                    zzrtcd
    C*
     C                   endif
     C                   if        qqrtcd = '1'
     C                   move      *on           *in54
     C                   move      *on           *in90
     C                   endif
    C*
     C                   endif
    C*
     C                   endif
    C*
    C*  Comments cannot be blank.
    C*
     C  n90              if        sccomm = *blanks
     C                   move      *on           *in53
     C                   move      *on           *in90
     C                   endif
    C*
    C*  Additional Remarks flag is required.
    C*
     C                   if        scarmk <> 'Y'
     C                             and scarmk <> 'N'
     C                   move      *on           *in46
     C                   move      *on           *in90
     C                   endif
    C*
    C*  Purgeable item flag must be 'Y', 'N' or blank.
    C*
     C  n90              if        scpgfl <> 'Y'
     C                             and scpgfl <> 'N'
     C                             and scpgfl <> *blank
     C                   move      *on           *in42
     C                   move      *on           *in90
     C                   endif
    C*
     C                   select
    C*
    C*  If purgeable item flag is = 'Y'
    C*
     C                   when      scpgfl = 'Y'
    C*
     C  n90              if        ctdprg = *zeros
     C                   move      *on           *in43
     C                   move      *on           *in90
     C                   endif
    C*
     C  n90              if        scditp <> 'Y'
     C                   move      *on           *in44
     C                   move      *on           *in90
     C                   endif
    C*
    C*  If purgeable item flag is = *BLANKS
    C*
     C                   when      scpgfl = *blanks
    C*
     C                   if        ctdprg <> *zeros
     C                             and scditp = 'Y'
     C                   move      'Y'           scpgfl
     C                   else
     C                   move      'N'           scpgfl
     C                   endif
    C*
     C                   endsl
    C*
     C                   endsr
    C*===============================================================*
    C*  FINISH
    C*===============================================================*
     C     finish        begsr
    C*
     C                   if        *inkc = *on
     C                   move      '3'           qqrtcd
     C                   endif
    C*
     C                   if        *inkl = *on
     C                   move      'C'           qqrtcd
     C                   endif
    C*
     C                   if        qqrtcd <> '3'
     C                             and qqrtcd <> '8'
     C                             and qqrtcd <> '9'
     C                             and qqrtcd <> 'A'
     C                             and qqrtcd <> 'C'
     C                             and qqrtcd <> 'K'
     C                             and qqrtcd <> 'P'
     C                   exsr      prsctl
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C*  KEDO - F5=Policy name search
    C*===============================================================*
     C     kedo          begsr
    C*
     C*                  move      scloc         ##loc
     C*                  move      scmco         ##mco
     C*                  move      scsym         ##sym
     C*                  move      scpol#        ##pol#
     C*                  move      scmod         ##mod
     C                   Eval      ##pmky = *blanks
    C*
     C                   call      point         pmscpl
    C*
     C                   move      ##loc         scloc
     C                   move      ##mco         scmco
     C                   move      ##sym         scsym
     C                   move      ##pol#        scpol#
     C                   move      ##mod         scmod
    C*
     C                   endsr
    C*===============================================================*
    C*  KFDO - F6=Bypass (ONLY FOR USER QUEUE)
    C*===============================================================*
     C     kfdo          begsr
     C                   if        *in73 = *on
    C*
    C*  Bypass by putting a "P" in the return code and ending.
    C*
     C                   move      'P'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    C*
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C*  KGDO - F7=Document Notes
    C*===============================================================*
     C     kgdo          begsr
    C*
     C                   clear                   qqrtcd
     C                   call      'IS016R'
     C                   parm                    qqdcid
     C                   parm      q1pgm         xxpnam
    C*
     C     dnky2         chain     pdn01                              60
     C                   if        *in60 = *off
     C                   move      *on           *in69
     C                   else
     C                   move      *off          *in69
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C*  KHDO - F8=Reject Document
    C*===============================================================*
     C     khdo          begsr
    C*
     C                   if        *in73 = *off
    C*
    C* Reject the Document being Indexed.  This will
    C*  call the Reject Document program  to allow
    C*  the entry of reason for the reject, Then update
    C*  the IE record, and the DO record with RJ   status.
    C*  Code will be RJ. (later a program will be available
    C*  to see the "rejects" and perform some processing on them.
    C*  The user will be required to fill in a reason for the
    C*  Reject in the program, which will be written to the
    C*  Doc Notes.
    C*
     C                   call      'IS157R'
     C                   parm                    qqdcid
     C                   parm      'RJ'          qqdsts
     C                   parm      'ISII01R '    xxpnam
     C                   parm      ' '           qqrtc
    C*
     C                   if        qqrtc = 'A'
     C                   move      '3'           qqrtcd
     C                   movel     *on           *inlr
     C                   move      *off          *in90
     C                   move      *on           *in97
     C                   else
    C*
    C*  If valid return from reject, bypass the current record.
    C*
     C                   movel     '8'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    C*
     C                   endif
    C*
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C*  KIDO - F9=Reassign document
    C*===============================================================*
     C     kido          begsr
    C*
     C                   move      qqsdat        #$sdt
     C                   move      qqstim        #$stm
    C*
     C                   call      'IS115R'
     C                   parm                    #$sdt
     C                   parm                    #$stm
     C                   parm                    qqdoct
     C                   parm                    qqdcid
     C                   parm                    qqdct
     C                   parm      *blanks       @#flr
     C                   parm                    @#sflr
     C                   parm      qqpth         @#pth
     C                   parm                    qqqid
     C                   parm      qqddsc        @#ddsc
     C                   parm      *blanks       @#rtc1
    C*
     C                   select
    C*
     C                   when      @#rtc1 = 'A'
     C                   move      'A'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    C*
     C                   when      @#rtc1 = 'B'
     C                   move      'C'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    C*
     C                   endsl
    C*
     C                   if        qqrtcd <> 'A'
     C                             and qqrtcd <> 'C'
     C                   clear                   qqkey
     C                   movel     '9'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C*  KJDO - F10=Search
    C*===============================================================*
     C     kjdo          begsr
     C                   if        slf10 = 'Y'
    C*
     C                   select
    C*
     C                   when      fld = 'SCDUSR  '
    C*
     C                   call      'SC530R'
     C                   parm                    scdusr
     C                   parm                    qqrtcd
    C*
     C                   when      fld = 'SCDEPT  '
     C                   clear                   sccode
     C                   clear                   zzrtcd
    C*
     C                   call      'SC580R'
     C                   parm                    sccode
     C                   parm                    zzrtcd
    C*
     C                   movel     sccode        scdept
     C                   move      sccode        scrmty
     C     ctky1         chain     pct01                              60
    C*
     C                   if        *in60 = *off
     C                   movel     ctfl01        scditp
      *** Begin Chg *** 01/30/02 ***********************************************
     C                   move      *blanks       sccomm
     C                   movel     ctdesc        sccomm
      *** End   Chg *** 01/30/02 ***********************************************
     C                   endif
    C*
     C                   when      fld = 'SCRMTY '
     C                   clear                   sccode
     C                   movel     scdept        sccode
     C                   clear                   zzrtcd
    C*
     C                   call      'SC580R'
     C                   parm                    sccode
     C                   parm                    zzrtcd
    C*
     C                   movel     sccode        scdept
     C                   move      sccode        scrmty
     C     ctky1         chain     pct01                              60
     C                   if        *in60 = *off
     C                   movel     ctfl01        scditp
      *** Begin Chg *** 01/30/02 ***********************************************
     C                   move      *blanks       sccomm
     C                   movel     ctdesc        sccomm
      *** End   Chg *** 01/30/02 ***********************************************
     C                   endif
    C*
     C                   endsl
     c*
     C                   Eval      *in97 = *on
    C*
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C* KMDO - F13=Change User Parms
    C*===============================================================*
     C     kmdo          begsr
    C*
     C                   call      'W0006R'
    C*
    C*  GET THE USER PARAMETER FILE
    C*
     C     upkey         chain     pup01                              60
    C*
     C                   endsr
    C*===============================================================*
    C* KNDO - F14=Split
    C*===============================================================*
     C     kndo          begsr
    C*
     C                   move      qqsdat        #$sdt
     C                   move      qqstim        #$stm
    C*
     C                   if        qqdct = 'F'
     C                   movel     'F'           q@rtc
     C                   else
     C                   movel     'I'           q@rtc
     C                   endif
    C*
     C                   movel     'I'           q@rtx
    C*
     C                   call      'IS215R3'
     C                   parm      qqqid         $$qid
     C                   parm      qqdcid        $$dcid
     C                   parm                    #$sdt
     C                   parm                    #$stm
     C                   parm      qqdoct        $$doct
     C                   parm                    q@rtx
    C*
     C                   if        q@rtx = '3'
     C                   movel     q@rtx         qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C*  KQDO - F16=New Index
    C*===============================================================*
     C     kqdo          begsr
    C*
     C                   movel     'K'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    C*
     C                   endsr
    C*===============================================================*
    C*  KRDO - F17=User profile search
    C*===============================================================*
     C     krdo          begsr
    C*
     C                   if        slf09 = 'Y'
    C*
     C                   call      'SC530R'
     C                   parm                    scdusr
     C                   parm                    zzrtcd
    C*
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C*  KXDO - F23=Secure/Unsecure remarks
    C*===============================================================*
     C     kxdo          begsr
    C*
     C                   if        slf23 = 'Y'
    C*
    C*  Remark UNSECURED to SECURED
    C*
     C                   if        *in56 = *off
     C                   clear                   sccode
     C                   movel     scdept        sccode
     C                   move      scrmty        sccode
     C     ctky1         chain     pct01                              59
    C*
     C                   if        *in59 = *on
     C                   move      slsecl        scsecl
     C                   endif
    C*
     C                   move      *on           *in56
     C                   move      scsecl        ##secl
     C                   move      'Y'           ##sbyl
    C*
    C*  Display "UNSECURE REMARKS" if "SECURE REMARKS" was displayed.
    C*
     C                   if        c1 = 3
     C                   eval      c1 = 4
     C                   endif
    C*
     C                   else
    C*
    C*  Remark SECURED to UNSECURED
    C*
     C                   move      *off          *in56
     C                   clear                   scsecl
     C                   move      scsecl        ##secl
     C                   move      'N'           ##sbyl
    C*
    C*  Display "SECURE REMARKS" if "UNSECURE REMARKS" was displayed.
    C*
     C                   if        c1 = 4
     C                   eval      c1 = 3
     C                   endif
    C*
     C                   endif
    C*
     C                   movel     ck1(c1)       sccml1
    C*
     C                   endif
    C*
     C                   endsr
    C*===============================================================*
    C*  KYDO - F24=More Keys
    C*===============================================================*
     C     kydo          begsr
    C*
     C                   if        slf24 = 'Y'
    C*
     C                   select
    C*
     C                   when      c1 = 1
     C                             or c1 = 5
     C                   eval      c1 = 2
    C*
    C*  Remark Secured
    C*
     C                   when      *in56 = *on
     C                             and c1 = 2
     C                   eval      c1 = 4
    C*
    C*  Remark NOT Secured
    C*
     C                   when      *in56 = *off
     C                             and c1 = 2
     C                   eval      c1 = 3
    C*
     C                   when      c1 = 3
     C                             or c1 = 4
    C*
     C                   if        *in73 = *on
     C                   eval      c1 = 5
     C                   else
     C                   eval      c1 = 1
     C                   endif
    C*
     C                   endsl
    C*
     C                   movel     ck1(c1)       sccml1
    C*
     C                   endif
    C*
     C                   endsr
    C*===============================================================
    C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
    C*===============================================================
     C     othfks        begsr
    C*
     C     *inke         caseq     *on           kedo                     90
     C     *inkf         caseq     *on           kfdo                     90
     C     *inkg         caseq     *on           kgdo                     90
     C     *inkh         caseq     *on           khdo                     90
     C     *inki         caseq     *on           kido                     90
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
     C     *inkn         caseq     *on           kndo                     90
     C     *inkq         caseq     *on           kqdo                     90
     C     *inkr         caseq     *on           krdo                     90
     C     *inkx         caseq     *on           kxdo                     90
     C     *inky         caseq     *on           kydo                     90
     C                   endcs
    C*
     C                   endsr
    C*===============================================================
    C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
    C*===============================================================
     C     pgmend        begsr
    C*
     C                   if        *inkc = *on
     C                   move      '3'           qqrtcd
     C                   endif
    C*
     C                   if        *inkl = *on
     C                   move      'C'           qqrtcd
     C                   endif
    C*
     C                   endsr
    C*====================================================================*
    C* PGMSTR
    C*====================================================================*
     C     pgmstr        begsr
    C*
     C                   movel     pgmnam        sctitl
     C                   move      '02'          pmsid
     C                   movel     'SC'          xxsyk
     C                   eval      c = 0
     C                   eval      c1 = 0
     C                   move      'N'           xxdewr
     C                   clear                   dertcd
     C                   clear                   decom1
     C                   clear                   decom2
    C*
     C                   clear                   dekey
    C*
    C*  Save the RTCD (F=fax, I=image, " "= scanned image)
    C*
     C                   movel     qqrtcd        qqdct
    C*
     C                   if        qqdct <> 'F'
     C                   move      *off          *in73
     C                   else
     C                   move      *on           *in73
     C                   endif
    C*
     C                   clear                   qqrtcd
    C*
     C                   time                    dsdttm
     C                   move      *hival        @@time
     C                   eval      xxtime = 0
     C                   eval      xxdate = 0
     C                   eval      xxseq# = 0
     C                   eval      ##seq# = *hival
     C                   eval      ##secl = 0
     C                   move      'N'           ##sbyl
    C*
     C                   movel     q1user        $$user
     C     slky          chain     psl01                              60
     C                   if        *in60 = *on
     C                   clear                   $$user
     C     slky          chain     psl01                              60
     C                   endif
    C*
     C                   movel     slfold        scdflr
    C*
     C                   if        slccif <> 'Y'
     C                   move      *on           *in98
     C                   else
     C                   move      *off          *in98
     C                   endif
    C*
    C*  Check System Control File Install Flags for this Client:
    C*
     C     'SY'          chain     ssy01                              60
     C                   if        *in60 = *off
    C*
    C*  Check for AS/400 Office Installed
    C*
     C                   if        syofif = 'Y'
     C                   move      'Y'           xxofif
     C                   move      *off          *in03
     C                   else
     C                   move      'N'           xxofif
     C                   move      *on           *in03
     C                   endif
    C*
     C                   endif
    C*
    C*  Look at DARTS System Control File to determine what's installed.
    C*
     C     syky          chain     sdr01                              60
     C                   if        *in60 = *off
    C*
    C*  If Image is installed, set on *IN99.
    C*
     C                   if        drfl01 = 'Y'
     C                   move      *on           *in99
     C                   endif
    C*
     C                   if        drfl03 = 'Y'
     C                   move      *on           *in34
     C                   endif
    C*
    C*  If Summary Code Option is installed, set on *IN36.
    C*
     C                   if        drfl02 = 'Y'
     C                   move      *on           *in36
     C                   endif
    C*
     C                   endif
    C*
     C                   if        *in73 = *on
     C                   eval      c1 = 5
     C                   else
     C                   eval      c1 = 1
     C                   endif
    C*
     C                   movel     ck1(c1)       sccml1
    C*
    C*  Load the record format info for the first time.
    C*
     C                   movel     qqdcid        scdoci
     C                   movel     qqpth         scpth
     C                   movel     qqdoct        scdoct
     C                   movel     qqddsc        scddsc
     C                   movel     qqqid         scqid
     C                   movel     qqqdsc        scqdsc
     C                   movel     qqbchi        scbchi
     C                   move      qq#pgs        sc#pgs
     C                   move      qqsdat        scsdat
     C                   move      qqstim        scstim
     C                   movel     qqsrep        scsrep
    C*
     C                   movel     scddsc        sccomm
     C                   movel     'N'           scarmk
     C                   time                    dsdttm
     C                   move      dsdate        sccdat
    C*
     C     dtky          chain     pdt01                              60
     C                   if        *in60 = *off
     C                   movel     dtrmcd        scdept
     C                   move      dtrmcd        scrmty
     C                   else
     C                   clear                   scdept
     C                   clear                   scrmty
     C                   endif
    C*
      ***** BEG ADD ***** 03/15/06 ********************************************
     c*** fill in the default Diary type here *****
     C                   movel     scdept        sccode
     C                   move      scrmty        sccode
     c     ctky1         chain     pct01                              60
     c                   if        *in60 = *off
     C                   movel     ctfl01        scditp
     c                   else
     c                   movel     *blanks       scditp
     c                   endif
      ***** END ADD ***** 03/15/06 ********************************************
    C*
     C     bmky1         chain     pbm01                              60
     C                   if        *in60 = *off
     C                   movel     bmbdsc        scbdsc
     C                   else
     C                   clear                   scbdsc
     C                   endif
    C*
     C                   movel     scbdsc        sccomm
    C*
    C*  Any Non-System Doc notes exist?
    C*
     C     dnky2         chain     pdn01                              60
     C                   if        *in60 = *off
     C                   move      *on           *in69
     C                   else
     C                   move      *off          *in69
     C                   endif
     c*** begin add *** 01/06/2009 *****************************************
     C*
     C* TRY TO RETRIEVE DATA FROM IMRAP
     C*
     C     raky          chain     pra01
     C                   if        %found
     C                   movel(p)  radkey        yydkey
     C                   move      yyloc         scloc
     C                   move      yymco         scmco
     C                   move      yysym         scsym
     C                   move      yypol#        scpol#
     C                   move      yymod         scmod
     C                   endif
     c*** end   add *** 01/06/2009 *****************************************
    C*
     C                   endsr
    C*====================================================================*
    C* PREDSP
    C*====================================================================*
     C     predsp        begsr
     C                   endsr
    C*====================================================================*
    C* PRSCTL
    C*====================================================================*
     C     prsctl        begsr
    C*
    C*  We have passed the edits, move screen key into CM record.
    C*
     C                   move      scloc         ##loc
     C                   move      scmco         ##mco
     C                   move      scsym         ##sym
     C                   move      scpol#        ##pol#
     C                   move      scmod         ##mod
    C*
     C                   movel     ##pmky        ##key
    C*
     C                   movel     scpnam        ##pnam
    C*
     C                   exsr      wcom
    C*
    C*  EXSR here to write doc notes, and also to copy all doc notes
    C*  to the SMARP file for this new DART remark.
    C*
     C                   exsr      dnot
    C*
    C*  Retain the item on the users queue?
    C*
     C                   if        scretn = 'Y'
     C                   movel     'R'           qqrtcd
     C                   endif
     C                   endsr
    C*====================================================================*
    C*  WCOM  SUBROUTINE - WRITE REMARK RECORD
    C*====================================================================*
     C     wcom          begsr
    C*
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hhdt
     C                   eval      hhtime = dstime
    C*
     C                   move      ##secl        cmsecl
     C                   move      ##sbyl        cmsbyl
    C*
     C                   movel     ##key         cmkey
     C                   clear                   cmcsid
     C                   movel     ##pnam        cmpnam
     C                   clear                   cmcode
     C                   movel     sccode        cmrmcd
     C                   movel     '***IMAGE'    cmdcid
     C                   move      '*** '        cmdcid
     C                   movel     sccomm        cmcomm
     C                   move      ' '           cmfl01
    C*
    C*  Load fax folder.
    C*
     C                   movel     qqpth         cmfold
     C                   eval      srdti = sccdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmcdat
     C                   eval      cmtrdt = hhdt
     C                   eval      cmtrtm = hhtime
      ***** BEG ADD ***** 10/17/11 ***********************************
     C                   eval      cmuncn = qquncn
     C                   eval      cmifsp = qqifsp
      ***** END ADD ***** 10/17/11 ***********************************
    C*
    C*  Fill hidden fields.
    C*
     C                   movel     scique        cmique
     C                   movel     scipth        cmipth
     C                   move      scpidx        cmpidx
     C                   movel     scapid        cmapid
     C                   movel     qqdcid        cmidoc
     C                   movel     scqpty        cmqpty
     C                   movel     scdctp        cmdctp
    C*
    C*  Check fax flag.
    C*
     C                   if        qqdct <> 'F'
     C                   movel     'Y'           cmimgf
     C                   else
     C                   movel     'F'           cmimgf
     C                   movel     qqdcid        cmdcid
     C                   clear                   cmidoc
     C                   endif
    C*
     C                   clear                   cmstat
     C                   movel     q1pgm         cmpgm
    C*
    C*  If Diary user entered, write to CMUSER, else default to current user.
    C*
     C                   if        scdusr <> *blanks
     C                   movel     scdusr        cmuser
     C                   else
     C                   movel     q1user        cmuser
     C                   endif
    C*
     C                   movel     q1user        cmiusr
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmdt
     C                   eval      cmtime = dstime
    C*
     C                   if        scditp = 'Y'
     C                             or scditp = 'N'
     C                   move      scditp        cmdifl
     C                   else
     C                   move      ' '           cmdifl
     C                   endif
    C*
     C                   movel     'D'           cmfl05
    C*
     C                   move      scpgfl        cmfl02
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmidt
    C*
     C                   eval      cmdcdt = *zero
     C                   movel     q1pgm         cmipgm
    C*
     C                   clear                   cmsmcd
     C                   clear                   cmfl04
     C                   clear                   cmfl05
    C*
    C*  Get the last SEQ# used.
    C*
     C                   movel     ##key         qqkey
    C*
     C     cmkya         setgt     pcm0i
     C                   readp     pcm0i                                  60
     C                   if        *in60 = *off
     C                             and qqkey = pikey
     C                   eval      cmseq# = piseq# + 1
     C                   else
     C                   eval      cmseq# = 0
     C                   endif
    C*
     C                   write     pcm01
     C                   feod      sdcml
    C*
     C                   clear                   qqrtcd
    C*
    C*  If additional remarks are requestd, call program to add the remarks.
    C*
     C                   movel     cmkey         qqkey
     C                   move      cmseq#        qqseq#
     C                   movel     q1pgm         qqpnam
    C*
     C                   if        scarmk = 'Y'
     C                   call      'SC014R'
     C                   parm                    qqkey
     C                   parm                    qqseq#
     C                   parm                    qqpnam
     C                   endif
    C*
    C* Execute the user index exit here only for diary types.
    C*
     C                   if        scditp = 'Y'
    C*
     C                   move      cmseq#        v@seq#
     C                   call      'ISE002C'                            99
     C                   parm      qqqid         fq
     C                   parm      scdusr        tu
     C                   parm      q1user        uid
     C                   parm      cmdcid        dcid
     C                   parm      cmkey         v@key
     C                   parm                    v@seq#
     C                   parm      *blanks       rtrn
    C*
     C                   endif
    C*
     C                   endsr
    C*====================================================================*
**  CK1 - Command Keys - SCR01
F3=Exit   F5=Policy search   F7=Notes   F8=Reject   F24=More keys
F9=Reassign   F10=Search   F12=Previous   F13=Chg User Parms   F24=More keys
F14=Split   F16=New Index Type   F23=Secure Remarks   F24=More keys
F14=Split   F16=New Index Type   F23=Unsecure Remarks   F24=More keys
F3=Exit   F5=Policy search   F6=Bypass   F7=Notes   F24=More keys
**  QCMDEXC TABLE
OVRDBF FILE(WXERP1) TOFILE(QTEMP/WXERP1 )
