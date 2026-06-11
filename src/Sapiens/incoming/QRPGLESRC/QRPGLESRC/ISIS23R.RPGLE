     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
      *==================================================================*
     H/TITLE ISII23R - Image Document Split - MC Medical Case Key
      *==================================================================*
      * Description: This program will create a new database record      *
      *  that will be processed by a batch process running in PDSFAX.    *
      *  The database record will contain all info needed to send a      *
      *  split request to the split server and to create an DART index   *
      *  if requested.  This program also allows queing to a user.  It   *
      *  will write doc notes and keep error and completion information. *
      *                                                                  *
      * Called from: IS215R3 - Split a Queued Document                   *
      * Calls to   :                                                     *
      *                                                                  *
      * Created    : 03/02/2000                                          *
      * Programmer : C:113                                               *
      * Project    : DR095                                               *
      *==================================================================*
      * REVSIONS:                                                        *
      *                                                                  *
      * 01/28/10  C:XXX  Project: SY215                               *
      *           C:189  Retrieve brand Name
      *                                                                  *
      * ---------------------------------------------------------------- *
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.              *
      * ---------------------------------------------------------------- *
      *==================================================================*
     FISIS23F   CF   E             Workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
    
      ** Split Request File
     FIASRP     O    E             DISK

      ** Image Batch Administration Master File
     FIMBML     IF   E           K disk
    
      ** Image Document Type Master File
     FIMDTL     IF   E           K disk
    
      ** Image Document Notes
     FIMDNL2    IF   E           K disk
    
      ** Image Queue File
     FIMIQL     IF   E           K DISK

      ** DARTS Security Master File
     FSMSLL     IF   E           K disk
    
      ** User Paramters File
     FSMUPL     IF   E           K disk
    
      ** DARTS Remark Type Table File
     FSTCTL1    IF   E           K disk
    
      ** System & Application Control File
     FSSYLF     IF   E           K disk
    
      ** Work File
     FWXERP1    IF   E             disk    usropn
    
     *** Managed Care Files ***
     F** Managed Care Subscriber Master File
     FFMSML     IF   E           K DISK
     FFMSMP     IF   E             DISK    Rename(PSM01:PSM0P)

      ** Managed Care Group Master File
     FFMFNL     IF   E           K DISK
     FFMFNP     IF   E             DISK    Rename(PFN01:PFN0P)

     F** Managed Care Medical Cases Master File
     FFMCML1    IF   E           K DISK
     FFMCMP     IF   E             DISK    Rename(PCM01:PCM0P)

      *==================================================================*
      * Array, Table & Data Structure Definitions
      *==================================================================*
     D ck1             s             80    dim(5) ctdata perrcd(1)
     D dn              s             75    dim(4)
     D qca             s             55    dim(1) ctdata perrcd(1)
    
      ** Date & Time structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

      ** File Information data structure
     D fids01        e ds                  extname(szq2p)
    
      ** Program status data structure
     D psds          esds                  extname(szq1p)

     *==================================================================*
     * Standalone fields
     *==================================================================*
     D pgmnam          c                   const('Medical Cases Index')
     D comapi          C                   const('SA005R')
     D cvtapi          c                   const('FA703R')
     I*** BEGIN CHG *********** 01/28/10 ************************************
     D*** pdsnam          c                const('Progressive Data So-
     D***                                  lutions, Inc.')
     D    pdsnam       S             40    INZ
     I*** END CHG   ********* 01/28/10 **************************************
    
     D mdoc            c                   const('For Pages:')
     D ddat            c                   const('Date:')
     D mhas            c                   const('For Key: ')
     D dtim            c                   const('Time:')
     D dtoq            c                   const('To Queue:')
     D dre1            c                   const('** SPLIT Request **')

     D YES             C                   Const('Y')
     D NO              C                   Const('N')
    
     D scpnam          s             10a   inz('F0269R    ')
    
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

      ** MC Key structure
     D ##CO#           S              3
     D ##SUB#          S              5
     D ##GRP           S              3
     D ##FYR           S              9
     D ##CASE          S              7
     D #TMPKY          S             40
     D WCIN            S              1
     D WDCK            S              1
     d QQSUB#          s              5  0
     d QQGRP           s              3  0
     d QQFYR           S              3  0
     d QQCASE          S              7  0
     d QQSS#           S              9
     d QQACDT          S              8  0

     D ##conm          s             40
     D ##data          s             80
     D ##leng          s              3  0
     D ##kc            s              1
     D ##key           s             40
     D ##kl            s              1
     D ##mode          s              1
     D ##pnam          s             10
     D ##rrn           s              9  0
     D ##rtn           s              1
     D ##sbyl          s              1
     D ##secl          s              3  0
     D ##seq#          s              5  0
     D #diary          s              1
     D @#ddsc          s             30
     D @#flr           s              8
     D @#pth           s             63
     D @#rtc1          s              1
     D @#sflr          s             12
     D @@time          s              6  0
     D i               s              5  0
     D dcid            s             12
     D decom1          s            256
     D decom2          s             19
     D dekey           s                   like(##key)
     D dertcd          s              1
     D errfle          s              1
     D fq              s             10
     D g               s              3  0
     D hhdt            s              7  0
     D hhtime          s              6  0
     D hlrecf          s                   like(q2recf)
     D length          s             15  5
     D ofcpgm          s                   like(dral02)
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
     D rtrn            s              6
     D scbdsc          s             40
     D sccode          s              5
     D scdflr          s             80
     D scdusr          s             10
     D scqdsc          s             40
     D scqid           s             10
     D dtDateIn        s              8  0
     D dtDateOut       s             10
     D dtError         s              1
     D srffmt          s              8
     D srtfmt          s              8
     D svscrn          s                   like(q2recf)
     D tu              s             10
     D uid             s             10
     D v@key           s             40
     D v@seq#          s              5
     D xxcdat          s              7  0
     D xxco#           s              3  0
     D xxd             s              2
     D xxdate          s              7  0
     D xxdcid          s             12
     D xxdefl          s              1
     D xxdepg          s              1
     D xxdewr          s              1
     D xxdsdt          s              7  0
     D xxfold          s             80
     D xxh             s              2
     D xxhms           s              8
     D xxm             s              2
     D xxmdyy          s             10
     D xxpgm           s             10
     D xxpnam          s             10
     D xxs             s              2
     D xxseq#          s              5  0
     D xxsyk           s              2
     D xxtime          s              6  0
     D xxy             s              4
     D xx3a            s              3
     D xx5a            s              5
     D xx6a            s              6
     D xx75            s             75
     D zzrtcd          s              1
     D***  Begin Add  ***  01/28/10  **************************************
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************

     *==================================================================*
     * KLIST Definitions
     *==================================================================*
      ** Image Queue file
     C     IQKY          KLIST
     C                   KFLD                    SCTOQ
    
      ** Document Type file
     C     DTKYA         KLIST
     C                   KFLD                    SCTDCT

     ** Image Batch Master file
     C     bmky1         klist
     C                   kfld                    qqbchi
    
     ** Image Document Notes file
     C     dnky2         klist
     C                   kfld                    qqdcid
    
     ** Image Document Type Table file
     C     dtky          klist
     C                   kfld                    qqdoct

      ** Remark type file
     C     CTKY1         KLIST
     C                   KFLD                    SCCODE
    
     ** DARTS Security file
     C     slky          klist
     C                   kfld                    $$user
    
     ** System Control file
     C     syky          klist
     C                   kfld                    xxsyk
    
     ** User Parameter file
     C     upkey         klist
     C                   kfld                    q1user
     *****************
     *  MC file keys
     *****************
     ** MC Subscriber Master file
     C     smkey         klist
     C                   kfld                    scco#
     C                   kfld                    scsub#

     ** MC Group Master file
     C     fnkey         klist
     C                   kfld                    scco#
     C                   kfld                    scsub#
     C                   kfld                    scgrp

     C     CMKEY         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCGRP
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE

     *==================================================================*
     * PLIST Definitions
     *==================================================================*
     ** Entry parameters
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
    
     ** Date check/conversion parameters
     C     datchk        plist
     C                   parm                    dtDateIn
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    dtDateOut
     C                   parm                    dtError
    
     C     dtcymd        plist
     C                   parm                    dtDateIn
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    dtDateOut
     C                   parm                    dtError
    
     C     dtmdyy        plist
     C                   parm                    dtDateIn
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    dtDateOut
     C                   parm                    dtError
    
     ** MC Group Search
     C     acdpl1        plist
     C                   parm                    ##mode
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm                    ##rrn

     C** MC Employer Search
     C     ACDPL2        PLIST
     C                   PARM                    ##MODE
     C                   PARM                    QQSUB#
     C                   PARM                    QQGRP
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQSS#
     C                   PARM                    QQACDT
     C                   PARM                    QQRTCD
     C*
     C** MC/WC Key conversion
     C     CVTPL         PLIST
     C                   PARM                    ##RTN
     C                   PARM                    WCIN
     C                   PARM                    WDCK
     C                   PARM                    ##CO#
     C                   PARM                    ##SUB#
     C                   PARM                    ##GRP
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    #TMPKY
     ** Company file retrieve program parameter list
     C     compl         PList
     C                   Parm                    xxco#
     C                   Parm                    ##conm
     *==================================================================*
     * Main
     *==================================================================*
     C                   exsr      pgmstr
    
     C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or *in90 = *off
    
     C                   exsr      predsp
     C                   exfmt     scr01
    
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
    
     C                   if        *inkc = *off
     C                             and *inkl = *off
     C                   exsr      othfks
    
     C                   if        *in97 = *off
     C                   exsr      errck1
     C                   endif
    
     C                   if        *in90 = *off
     C                   exsr      finish
     C                   endif
    
     c                   endif
    
     C                   enddo
    
     C                   exsr      pgmend
    
     C                   move      *on           *inlr

     *====================================================================*
     *  CHKKEY  SUBROUTINE - EDIT MC KEY FIELDS
     *====================================================================*
     C     chkkey        begsr

     ** Subscriber
     C     SMkey         chain     PSM01                              60
     C  n90              if        *in60 = *on
     C                   seton                                        86  90
     C                   endif

    C** Group
     C     fnkey         chain     pfn01                              60
     C  n90              if        *in60 = *on
     C                   seton                                        87  90
     C                   endif
    
    C** Case
     C     CMkey         chain     PCM01                              60
     C  n90              if        *in60 = *on
     C                   seton                                        88  90
     C                   endif
    
     C                   endsr

     *====================================================================*
     *  DNOT DOC NOTES  - CREATE DOC NOTES AND CPY NOTES TO SMAR
     *====================================================================*
     C     dnot          begsr
    
     ** Fill all doc note fields.
     C                   time                    dsdttm
     C                   move      dsdate        #$nowd
     C                   move      dstime        #$nowt
    
     C                   clear                   xx75
     C                   movel     #$nowd        xxm
     C                   move      #$nowd        xxy
     C     2             subst     #$nowd:3      xxd
     C     xxm           cat(p)    '/'           xx3a
     C     xx3a          cat(p)    xxd           xx5a
     C     xx5a          cat(p)    '/'           xx6a
     C     xx6a          cat(p)    xxy           xxmdyy
    
     C                   movel     #$nowt        xxh
     C                   move      #$nowt        xxs
     C     2             subst     #$nowt:3      xxm
     C     xxh           cat(p)    ':'           xx3a
     C     xx3a          cat(p)    xxm           xx5a
     C     xx5a          cat(p)    ':'           xx6a
     C     xx6a          cat(p)    xxs           xxhms
    
     C     ddat          cat(p)    xxmdyy:1      xx75
     C                   cat(p)    dtim:1        xx75
     C                   cat(p)    xxhms:1       xx75
     C                   cat(p)    dre1:5        xx75
    
     C                   movel     xx75          dn(1)
    
     C     mdoc          cat(p)    srfrpg:2      xx75
     C                   cat(p)    '- ':1        xx75
     C                   cat(p)    srtopg:2      xx75
    
     C                   movel     xx75          dn(2)
    
     C     dtoq          cat(p)    srtqid:1      xx75
     C                   cat(p)    mhas:2        xx75
     C                   cat(p)    srkey:2       xx75
     C                   movel     xx75          dn(3)
     C                   clear                   dn(4)
    
     C                   do        4             g
    
     C                   movel     qqdcid        $#dcid
     C                   movel     dn(g)         qqcm75
    
     C                   if        g >= 4
     C                   move      '1'           q$rtc
     C                   else
     C                   move      ' '           q$rtc
     C                   endif
    
     C                   call      'ISDOCNR'
     C                   parm                    qqcm75
     C                   parm                    qqdcid
     C                   parm                    #$nowd
     C                   parm      q1pgm         qqpnam
     C                   parm      'H'           qqprfl
     C                   parm                    q$rtc
    
     C                   enddo
    
     C                   endsr
     *====================================================================*
     *  ERRCK1  SUBROUTINE - EDIT SCREEN 1 FIELDS
     *====================================================================*
     C     errck1        begsr
    
      ** validate the from/to page #'s
     C                   If        SCFRPG <= *zero
     C                   Seton                                        61  90
     C                   EndIf

     C  n90              If        SCTOPG <= *zero
     C                   Seton                                        62  90
     C                   EndIf

      ** from and to page cannot be > total pages
     C  n90              If        SCFRPG > qq#pgs
     C                             OR SCTOPG > qq#pgs
     C                   Seton                                        63  90
     C                   EndIf

      ** to page cannot be < from page
     C  n90              If        SCTOPG < SCFRPG
     C                   Seton                                        64  90
     C                   EndIf

      ** doc type must be valid
     C     DTKYA         CHAIN     PDT01                              60
     C  n90              If        *in60 = *on
     C                   Seton                                        65  90
     C                   EndIf

      ** to queue must be valid, if present
     C  n90              If        SCTOQ <> *blanks

     C     IQKY          CHAIN     PIQ01                              60
     C                   If        *in60 = *on
     C                   Seton                                        66  90
     C                   EndIf

     C                   ENDIF

      ** index flag must be 'Y' or 'N'
     C  n90              If        SCINDX <> YES
     C                             AND SCINDX <> NO
     C                   Seton                                        67  90
     C                   EndIf

      ** either a queue ID and/or an index flag = 'Y' must be entered
     C  n90              If        SCTOQ = *blanks
     C                             AND SCINDX <> YES
     C                   Seton                                        68  90
     C                   ENDIF

      ** we don't do any editing below unless index is chosen
     C                   If        SCINDX = YES

      ** check the key fields for validity
     C                   exsr      chkkey
    
     ** date entered must be valid
     C                   Eval      dtDateIn = SCCDAT
     C                   Call      'SRCVTDAT'    datchk
     C                   MoveL     dtDateOut     SCCDAT
    
     C  n90              if        dtError = '1'
     C                   Seton                                        69  90
     C                   endif
    
     C                   move      ' '           #diary
    
     C                   movel     scdept        sccode
     C                   move      scrmty        sccode
    
     ** If remark is secured, the secuity level displayed on screen
     **  is moved into the field to be stored when record is made
     **  or updated.
    
     C                   if        *in56 = *on
     C                             and scsecl <> ##secl
     C                   movel     scsecl        ##secl
     C                   else
     C                   if        *in56 = *off
     C                   clear                   scsecl
     C                   movel     scsecl        ##secl
     C                   endif
     C                   endif
    
     ** remark type must be valid
     C  n90              if        sccode <> *blanks
     C     sccode        chain     stctl1                             60
     C                   if        *in60 = *on
     C                   Seton                                        70  90
     C                   endif
    
     C                   else
     C                   Seton                                        71  90
     C                   endif
    
     ** determine if remark is diary or not
     C  n90              if        scditp = 'Y'
    
     ** User name, if diary type, must be valid
     C                   movel     scdusr        $$user
     C     slky          chain     psl01                              60
     C                   if        *in60 = *on
     C                   Seton                                        72  90
     C                   endif
     C                   endif
    
     C  n90              if        scditp <> ' '
     C                             and scditp <> 'Y'
     C                             and scditp <> 'N'
     C                   Seton                                        73  90
     C                   else
     C                   move      ctfl01        scditp
     C                   endif
    
     ** check Data Entry Flag to see if Data Entry Program has been called once already
     C                   if        xxdefl = 'N'
    
     ** if DRFL03 = 'Y' in file SDRPF, Remark Code is checked to see
     ** if it has an associated Data Entry Program ID.
     C                   move      'N'           xxdepg
     C     syky          chain     sdr01                              60
     C                   if        *in60 = *off
     C                   if        drfl03 = 'Y'
     C                   if        ctdepg <> *blanks
     C                   move      'Y'           xxdepg
    
     ** Doc ID not allowed if the remark type has an associated data
     **  entry program.
     C  n90              if        scdcid <> *blanks
     C                   Seton                                        74  90
     C                   endif
    
     ** Check to see if a recursive call was made.  If so... error msg
     C                   clear                   xxpgm
     C                   movel     ctdepg        xxpgm
     C                   clear                   scdcid
     C                   movel     ctdepg        scdcid
    
     C                   eval      dtDateIn = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     dtDateOut     xxdsdt
     C                   eval      dtDateIn = sccdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     dtDateOut     xxcdat
    
     C  n90              if        xxcdat <= xxdsdt
    
     C                   if        errfle = ' '
     C                   call      'ERROR1C'
    
     C                   eval      length = 55
    
     C                   call      'QCMDEXC'                              60
     C                   parm                    qca(1)
     C                   parm                    length
    
     C                   open      wxerp1
     C                   move      'Y'           errfle
     C                   endif
    
     C     7             chain     per01                              60
     C                   dow       *in60 = *off
    
     ** Check for Recursive call to program
     C                   if        erpgm = xxpgm
     C                   Seton                                        75  90
     C                   endif
    
     C                   read      per01                                  60
     C                   enddo
    
     ** ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
     ** (NOTE: It was necessary to break up the comment field of size
     **  275 into DECOM1 and DECOM2 because the largest alpha-numeric
     **  value allowed for a parameter is 256.
    
     C                   if        *in90 = *off
    
     C                   clear                   dekey
     C                   clear                   decom1
     C                   clear                   decom2
     C                   clear                   dertcd
     C                   movel     qqkey         dekey
    
     C                   call      ctdepg                               60
     C                   parm                    dekey
     C                   parm                    decom1
     C                   parm                    decom2
     C                   parm                    dertcd
    
     C                   if        *in60 = *on
     C                   seton                                        76  90
     C                   endif
    
     C                   endif
    
     C                   move      'Y'           xxdefl
    
     C                   if        dertcd = 'A'
     C                             or dertcd = 'B'
     C                   move      *on           *in90
     C                   endif
    
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
    
     C                   clear                   scdcid
     C                   movel     ctdepg        scdcid
     C                   eval      dtDateIn = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     dtDateOut     xxdsdt
     C                   eval      dtDateIn = sccdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     dtDateOut     xxcdat
     C                   if        xxcdat <= xxdsdt
    
     C                   Seton                                        77  90
     C                   endif
     C                   endif
    
     C                   endif
    
     C                   endif
     C                   endif
     C                   endif
     C                   endif
    
     C                   if        *in99 = *off
     C                             and xxdefl = 'N'
     C                             and *in90 = *off
    
     ** Doc ID must be valid.
     C                   movel     scdcid        xxdcid
     C                   movel     scdflr        xxfold
     C                   clear                   zzrtcd
    
     C                   if        scdcid <> *blank
    
     ** Must have a Default Folder entered for this function
     C  n90              if        xxfold = *blanks
     C                   Seton                                        79  90
     C                   endif
    
     C                   if        *in90 = *off
     C                   call      'SVALDOC'
     C                   parm                    xxdcid
     C                   parm                    xxfold
     C                   parm                    zzrtcd
    
     C                   if        zzrtcd = '1'
     C                   Seton                                        80  90
     C                   endif
     C                   endif
    
     C                   endif
    
     C                   endif
    
     ** Comments cannot be blank.
     C  n90              if        sccomm = *blanks
     C                   Seton                                        81  90
     C                   endif
    
     C                   ENDIF
    
     C                   endsr
     *===============================================================*
     *  FINISH
     *===============================================================*
     C     finish        begsr
    
     C                   if        *inkc = *on
     C                   move      '3'           qqrtcd
     C                   endif
    
     C                   if        *inkl = *on
     C                   move      'C'           qqrtcd
     C                   endif
    
     C                   if        qqrtcd <> '3'
     C                             and qqrtcd <> 'K'
     C                   exsr      prsctl
     C                   endif
    
     C                   endsr
     *===============================================================*
     *  KGDO - F7=Document Notes
     *===============================================================*
     C     kgdo          begsr
    
     C                   clear                   qqrtcd
     C                   call      'IS016R'
     C                   parm                    qqdcid
     C                   parm      q1pgm         xxpnam
    
     C     dnky2         chain     pdn01                              60
     C                   if        *in60 = *off
     C                   move      *on           *in59
     C                   else
     C                   move      *off          *in59
     C                   endif

     C                   Seton                                        97
    
     C                   endsr
     *===============================================================*
     *  KJDO - F10=Search
     *===============================================================*
     C     kjdo          begsr
     C                   if        slf10 = 'Y'
    
     C                   select
    
      ** MC Subscriber search
     C                   when      fld = 'SCSUB#'
     C                   move      '2'           ##mode
     C                   call      'F0055R'      acdpl1
     C     ##rrn         chain     PSM0p                              60
     C                   if        *in60 = *off
     C                   move      SMsub#        scsub#
     C                   Else
     C                   move      *zero         scsub#
     C                   endif
    
     C                   if        *inkc = *on
     C                   move      *off          *in90
     C                   endif
    
     C                   if        *inkl = *on
     C                             or SCSUB# = *zero
     C                   move      *off          *inkl
     C                   endif

     C                   seton                                        97

      ** group search
     C                   when      fld = 'SCGRP'
     C                   move      '2'           ##mode
     C                   call      'F0060R'      acdpl1
     C     ##rrn         chain     pfn0p                              60
     C                   if        *in60 = *off
     C                   move      fnsub#        scsub#
     C                   move      fnfnd         scgrp
     C                   else
     C                   move      *zero         scsub#
     C                   move      *zero         scgrp
     C                   endif
    
     C                   if        *inkc = *on
     C                   move      *off          *in90
     C                   endif
    
     C                   if        *inkl = *on
     C                             or scgrp = *zero
     C                   move      *off          *inkl
     C                   endif

     C                   seton                                        97

      ** MC Case Search
     C                   when      fld = 'SCCASE'
     C                   move      '2'           ##mode
     C                   call      SCPNAM        acdpl2

     C                   If        QQCASE <> *ZEROS
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQGRP         SCGRP
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   ENDIF

     C                   seton                                        97

      ** user search
     C                   when      fld = 'SCDUSR  '
    
     C                   call      'SC530R'
     C                   parm                    scdusr
     C                   parm                    qqrtcd
    
      ** Department search
     C                   when      fld = 'SCDEPT  '
     C                   clear                   sccode
     C                   clear                   zzrtcd
    
     C                   call      'SC580R'
     C                   parm                    sccode
     C                   parm                    zzrtcd
    
     C                   movel     sccode        scdept
     C                   move      sccode        scrmty
     C     ctky1         chain     pct01                              60
    
     C                   if        *in60 = *off
     C                   movel     ctfl01        scditp
     C                   endif
    
      ** Remark type search
     C                   when      fld = 'SCRMTY '
     C                   clear                   sccode
     C                   movel     scdept        sccode
     C                   clear                   zzrtcd
    
     C                   call      'SC580R'
     C                   parm                    sccode
     C                   parm                    zzrtcd
    
     C                   movel     sccode        scdept
     C                   move      sccode        scrmty
     C     ctky1         chain     pct01                              60
     C                   if        *in60 = *off
     C                   movel     ctfl01        scditp
     C                   endif
    
     C                   endsl
    
     C                   endif

     C                   Seton                                        97
    
     C                   endsr
     *===============================================================*
     * KMDO - F13=Change User Parms
     *===============================================================*
     C     kmdo          begsr
    
     C                   call      'F0006R'
    
     ** get the user parameter record
     C     upkey         chain     pup01                              60
     C                   Seton                                        97
    
     C                   endsr
     *===============================================================*
     *  KQDO - F16=New Index
     *===============================================================*
     C     kqdo          begsr
    
      ** Move value into return code and set flags to bypass errorcheck
      ** and end-program processing.
     C                   movel     'K'           qqrtcd
     C                   Setoff                                       90
     C                   Seton                                        97
    
     C                   endsr
     *===============================================================*
     *  KXDO - F23=Secure/Unsecure remarks
     *===============================================================*
     C     kxdo          begsr
    
     C                   if        slf23 = 'Y'
    
     ** Remark UNSECURED to SECURED
     C                   if        *in56 = *off
     C                   clear                   sccode
     C                   movel     scdept        sccode
     C                   move      scrmty        sccode
     C     ctky1         chain     pct01                              60
    
     C                   if        *in60 = *on
     C                   move      slsecl        scsecl
     C                   endif
    
     C                   move      *on           *in56
     C                   move      scsecl        ##secl
     C                   move      'Y'           ##sbyl
    
     ** Display "UNSECURE REMARKS" if "SECURE REMARKS" was displayed.
     C                   if        i  = 3
     C                   eval      i  = 4
     C                   endif
    
     C                   else
    
     ** Remark SECURED to UNSECURED
     C                   move      *off          *in56
     C                   clear                   scsecl
     C                   move      scsecl        ##secl
     C                   move      'N'           ##sbyl
    
     ** Display "SECURE REMARKS" if "UNSECURE REMARKS" was displayed.
     C                   if        i = 4
     C                   eval      i = 3
     C                   endif
    
     C                   endif
    
     C                   movel     ck1(i)        sccml1
    
     C                   endif
     C                   Seton                                        97
    
     C                   endsr
     *===============================================================*
     *  KYDO - F24=More Keys
     *===============================================================*
     C     kydo          begsr
    
     C                   if        slf24 = 'Y'
    
     C                   select
    
     C                   when      i  = 1
     C                   eval      i  = 2
    
     ** Remark Secured
     C                   when      *in56 = *on
     C                             and i  = 2
     C                   eval      i  = 4
    
     ** Remark NOT Secured
     C                   when      *in56 = *off
     C                             and i  = 2
     C                   eval      i  = 3
    
     C                   when      i  = 3
     C                             or i  = 4
     C                   eval      i  = 1
    
     C                   endsl
    
     C                   movel     ck1(i)        sccml1
    
     C                   endif

     C                   Seton                                        97
    
     C                   endsr
     *===============================================================
     * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     *===============================================================
     C     othfks        begsr
    
      ** reset the errorcheck bypass flag
     C                   Setoff                                       97

     C     *inkg         caseq     *on           kgdo                     90
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkm         caseq     *on           kmdo                     90
     C     *inkq         caseq     *on           kqdo                     90
     C     *inkx         caseq     *on           kxdo                     90
     C     *inky         caseq     *on           kydo                     90
     C                   endcs
    
     C                   endsr
     *===============================================================
     * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     *===============================================================
     C     pgmend        begsr
    
     C                   if        *inkc = *on
     C                   move      '3'           qqrtcd
     C                   else

     C                   if        *inkl = *on
     C                   move      'C'           qqrtcd
     C                   else

     C                   if        qqrtcd <> 'K'
     C                   move      '*'           qqrtcd
     C                   EndIf

     C                   endif

     C                   endif
    
     C                   endsr
     *====================================================================*
     * PGMSTR
     *====================================================================*
     C     pgmstr        begsr
    
      ** initialize fields
     C                   movel     pgmnam        sctitl
     C                   movel     'SC'          xxsyk
     C                   move      'N'           xxdewr
     C                   clear                   dertcd
     C                   clear                   decom1
     C                   clear                   decom2
    
     C                   clear                   dekey
    
     ** Save the RTCD (F=fax, I=image, " "= scanned image)
     C                   movel     qqrtcd        qqdct
     C                   clear                   qqrtcd
    
     C                   time                    dsdttm
     C                   move      *hival        @@time
     C                   eval      xxtime = 0
     C                   eval      xxdate = 0
     C                   eval      xxseq# = 0
     C                   eval      ##seq# = *hival
     C                   eval      ##secl = 0
     C                   move      'N'           ##sbyl

     ** get user parm information
     C     upkey         chain     pup01                              60
     C                   move      upco#         xxco#
     C                   if        *in60 = *on
     C                   clear                   xxco#
     C                   endif
    
      ** get DARTS security level information
     C                   movel     q1user        $$user
     C     slky          chain     psl01                              60
     C                   if        *in60 = *on
     C                   clear                   $$user
     C     slky          chain     psl01                              60
     C                   endif

     C                   movel     slfold        scdflr

      ** put company name in SCPRDS
     C                   call      comapi        compl                  20
B01  C                   if        *in20 = *on
     C***  Begin Chg  ***  01/28/10  **************************************
     C**********         movel(p)  pdsnam        ##data
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval       ##data = brandname
     C***  End   Chg  ***  01/28/10  **************************************
     C                   call      'SRCENTER'                           20
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
E01  C                   endif
     C                   movel     ##conm        scconm
    
      ** ???
     C                   if        slccif <> 'Y'
     C                   move      *on           *in98
     C                   else
     C                   move      *off          *in98
     C                   endif
    
     ** Look at DARTS System Control File to determine what's installed.
     C     syky          chain     sdr01                              60
     C                   if        *in60 = *off
    
     ** If Image is installed, set on *IN99.
     C                   if        drfl01 = 'Y'
     C                   move      *on           *in99
     C                   endif
    
      ** ???
     C                   if        drfl03 = 'Y'
     C                   move      *on           *in34
     C                   endif
    
     ** If Summary Code Option is installed, set on *IN36.
     C                   if        drfl02 = 'Y'
     C                   move      *on           *in36
     C                   endif
    
     C                   endif
    
      ** get the remark type information
     C     dtky          chain     pdt01                              60
     C                   if        *in60 = *off
     C                   movel     dtrmcd        scdept
     C                   move      dtrmcd        scrmty
     C                   else
     C                   clear                   scdept
     C                   clear                   scrmty
     C                   endif

     C                   movel     scdept        sccode
     C                   move      scrmty        sccode
     C     ctky1         chain     pct01                              60
     C                   if        *in60 = *off
     C                   movel     ctfl01        scditp
     C                   endif
    
      ** get the batch information
     C     bmky1         chain     pbm01                              60
     C                   if        *in60 = *off
     C                   movel     bmbdsc        scbdsc
     C                   else
     C                   clear                   scbdsc
     C                   endif
    
     C                   movel     scbdsc        sccomm

     C                   Eval      SCINDX = YES
    
     ** Any Non-System Doc notes exist?
     C     dnky2         chain     pdn01                              60
     C                   if        *in60 = *off
     C                   move      *on           *in59
     C                   else
     C                   move      *off          *in59
     C                   endif

      ** load the function keys
     C                   eval      i  = 1
     C                   movel     ck1(i )       sccml1
    
     ** load the screen fields for the first time
     C                   move      xxco#         scco#
     C                   movel     qqdcid        scdoci
     C                   movel     qqpth         scpth
     C                   movel     qqdoct        SCTDCT
     C                   movel     qqddsc        scddsc
     C                   movel     qqqid         scqid
     C                   movel     qqqdsc        scqdsc
     C                   movel     qqbchi        scbchi
     C                   move      qq#pgs        sc#pgs
     C                   move      qqsdat        scsdat
     C                   move      qqstim        scstim
     C                   movel     qqsrep        scsrep
    
     C                   movel     scddsc        sccomm
     C                   movel     'N'           scarmk
     C                   time                    dsdttm
     C                   move      dsdate        sccdat
    
     C                   endsr
     *====================================================================*
     * PREDSP
     *====================================================================*
     C     predsp        begsr
     C                   endsr
     *====================================================================*
     * PRSCTL
     *====================================================================*
     C     prsctl        begsr
    
     ** we have passed the edits, move screen key into CM record.
     C                   move      SCCO#         ##CO#
     C                   move      SCSUB#        ##SUB#
     C                   move      SCGRP         ##GRP
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE

     C                   eval      wcin = 'Y'
     C                   eval      wdck = 'Y'
     c                   eval      ##rtn = 'K'

     C                   CALL      CVTAPI        CVTPL

     C                   MOVEL     #TMPKY        ##key
    
     C                   movel     SCPNAM        ##pnam
    
      ** write the split request transaction record
     C                   exsr      wcom
    
     ** write the doc notes and also copy all doc notes to the SMARP for this new remark
     C                   exsr      dnot
    
     C                   endsr

    C*====================================================================*
    C*  WCOM  SUBROUTINE - WRITE REMARK RECORD
    C*====================================================================*
     C     wcom          begsr
    C*
     C                   time                    dsdttm
     C                   Eval      dtDateIn = dsDate
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MoveL     dtDateOut     hhdt
     C                   Eval      hhtime = dstime

      ** FILL THE TRANSACTION RCD FIELDS TO REQUEST A DOC SPLIT * *  *
     C                   MOVEL     QQDCID        SRFDCI
     C*
     C                   MOVEL     QQPTH         SRFPTH
     C     8             SUBST     SRFPTH:1      SRFFLR
     C     12            SUBST     SRFPTH:10     SRFSFL
     C*
     C                   MOVEL     QQQID         SRFQID
     C                   MOVEL     QQ#PGS        SRTOTP
     C*
     C                   MOVEL     SCTOQ         SRTQID
     C*
     C                   MOVE      SCFRPG        SRFRPG
     C                   MOVE      SCTOPG        SRTOPG
     C                   MOVE      SCINDX        SRINDX
     C                   MOVEL     SCTDCT        SRDCTP
     C*
     C                   MOVEL     *BLANKS       SRSPLC
     C                   MOVE      *ZEROS        SRSCDT
     C                   MOVE      *ZEROS        SRSCTM
     C*
     C                   MOVEL     Q1JOB         SRSJOB
     C                   MOVE      Q1JOBN        SRSJB#
     C                   MOVEL     Q1USER        SRSUSR
     C*
     C                   MOVEL     *BLANKS       SRJOBN
     C                   MOVEL     *BLANKS       SRJOB#
     C                   MOVEL     *BLANKS       SRUSRN
     C*
     C                   MOVEL     *BLANKS       SRSTAT
     C*
     C                   Z-ADD     DSDATE        dtDateIn
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     dtDateOut     SRDT
     C                   Z-ADD     DSTIME        SRTIME
     C                   MOVEL     Q1PGM         SRPGM
     C                   MOVEL     Q1USER        SRUSER
     C*
     C** THE "TO" INFO FOR THE NEW DOC WILL BE GENEREATED WHEN PROCESSED.
     C*
     C                   If        SCINDX = YES
     C** INDEX(DART REMARK) INFORMATION  HERE ***
     C                   MOVEL     ##KEY         SRKEY
     C                   MOVE      *ZEROS        SRSEQ#
     C                   MOVEL     ##PNAM        SRPNAM
     C                   MOVEL     SCCODE        SRRMCD
     C                   MOVEL     SCCOMM        SRCOMM
     C                   Z-ADD     SCCDAT        dtDateIn
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     dtDateOut     SRCDAT
     C*
     C                   If        qqdct = 'F'
     C                   Eval      SRFTYP = 'F'
     C                   Else
     C                   Eval      SRFTYP = 'I'
     C                   ENDIF
     C*
     C                   If        SCDITP = YES
     C                   MOVEL     SCDUSR        SRDUSR
     C                   ELSE
     C                   MOVE      *BLANKS       SRDUSR
     C                   END
     C*
     C                   ENDIF
     C*
     C                   WRITE     PSR01
     C*
     C**** SEND DATA QUEUE RQST TO "WAKE UP " SPLIT PROGRAM
     C*
     C     SRFDCI        CAT       SRSJB#        XXDAT1           18
     C     XXDAT1        CAT       SRSUSR        XXDAT2           28
     C     XXDAT2        CAT       SRSJOB        XXDAT3           38
     C     XXDAT3        CAT       'SS'          XXDATA          128
     C*
     C                   CALL      'QSNDDTAQ'
     C                   PARM      'ISSRSS  '    XXQNM            10
     C                   PARM      '*LIBL'       XXLIB            10
     C                   PARM      128           XXFLDL            5 0
     C                   PARM                    XXDATA          128
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   endsr
     C*====================================================================*
**  CK1 - Command Keys - SCR01
F3=Exit   F7=Notes   F10=Search   F12=Cancel   F24=More keys
F13=Chg user parms   F16=New index type   F24=More keys
F23=Secure Remarks   F24=More keys
F23=Unsecure Remarks   F24=More keys
**  QCMDEXC TABLE
OVRDBF FILE(WXERP1) TOFILE(QTEMP/WXERP1 )
