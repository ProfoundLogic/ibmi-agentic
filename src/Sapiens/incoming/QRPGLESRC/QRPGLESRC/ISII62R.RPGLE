     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     *==================================================================*
    H/TITLE ISII62R - SIS Claim Index Program
     *==================================================================*
      ***** NOTE: MUST HAVE SISFIL library IN THE LIBL when compiling
     *==================================================================*
     * Description: This program indexes incoming images by SIS's       *
     *              Claim key.                                          *
     *                                                                  *
     *==================================================================*
      * Created on 4/06/2009 by c:62 for Project 115IAK
     *==================================================================*
     * Revisions:                                                       *
      *                                                               *
      * 10/17/11 C:279 Project: SY195                                 *
      *                  - Modify program to pass/return UNC path     *
      *                    field and IFS flag to called/calling       *
      *                    programs.                                  *
      *                                                               *
     *==================================================================*
     * Indicator Usage:                                                 *
     *                                                                  *
     * GLOBAL---------------------------------------------------------- *
     *  03    Office NOT installed.                                     *
     * N03    Office IS installed.                                      *
     *  34    Data entry program option is installed.                   *
     *  37    Used on call to Data Entry Program (DEPGM).               *
     *  56    Display "UNSECURE REMARK", remark is secured.             *
     * N56    Display "SECURE REMARK", remark is unsecured.             *
     *  59    Validate chain command match for default security level.  *
     *  60    General chain.                                            *
     *  97    Do not process error check.                               *
     * N98    Either no security or meets security level.               *
     *  98    Protect remark type because of security level.            *
     *  99    Image product is installed.                               *
     *                                                                  *
     * Error----------------------------------------------------------- *
     *  26    SCM0609-Document ID is not allowed with a remark type     *
     *                that has an associated data entry program ID.     *
     *  31    SCM0550-AS/400 Officevision is not installed.             *
     *  32    SCM0532-Default Folder required if Document ID indicated. *
     *  35    SCM0502-Value must be 'Y' or 'N'.                         *
     *  42    SCM0603-Value must be 'Y', 'N' or blank.                  *
     *  43    SCM0604-This remark type is non-purgeable.                *
     *  44    SCM0605-Diary type must be 'Y' in order for this remark   *
     *                to be purgeable.                                  *
     *  46    SCM0502-Value must be 'Y' or 'N'.                         *
     *  47    SCM0619-Must find alternative access to this data entry   *
     *                program.                                          *
     *  51    SCM0501-Value entered is not defined in table file.       *
     *  52    SCM0500-Invalid date entered.                             *
     *  53    SCM0503-Value cannot be equal to blanks - remark required.*
     *  54    SCM0516-Document not found in specified folder.           *
     *  58    SCM0612-User is not enrolled in DARTS. See your           *
     *                System Administrator.                             *
     *  62    SCM0557-Invalid Company entered.                          *
     *  63    SCM0558-Invalid Prefix entered.                           *
     *  64    SCM0559-Invalid Policy number entered.                    *
     *  65    SCM0559-Invalid Branch entered.                           *
     *  66    SCM0559-Invalid Claim number entered.                     *
     *==================================================================*
     Fisii62f   cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     infds(fids01)
    
     ** Image Batch Administration Master File
     Fimbml     if   e           k disk
    
     ** Image Document Type Master File
     Fimdtl     if   e           k disk
    
     ** Image Document Notes
     Fimdnl2    if   e           k disk
    
     ** DARTS Remark Detail File
     Fsdcml     o    e             disk
    
     Fsdcmli    if   e           k disk
     F                                     rename(pcm01:pcm0i)
    
     ** DARTS Security Master File
     Fsmsll     if   e           k disk
    
     ** DARTS Remark Type Table File
     Fstctl1    if   e           k disk
    
     ** System & Application Control File
     Fssylf     if   e           k disk
    
     ** User Paramters File
     Fwmupl     if   e           k disk
    
     ** Work File
     Fwxerp1    if   e             disk    usropn
    
     *==================================================================
     * Array & Table Definitions
     *==================================================================
     D ck1             s             80    dim(5) ctdata perrcd(1)
     D dn              s             75    dim(4)
     D qca             s             55    dim(1) ctdata perrcd(1)
    
     D pgmnam          c                   const('Claim Index')
    
     D mdoc            c                   const('To Darts:')
     D msen            c                   const('User :')
     D ddat            c                   const('Date:')
     D dtim            c                   const('Time:')
     D drea            c                   const('** INDEXED **')
    
     ** 'SIS Claims aaarch (could use a better search pgm)
     D search          c                   const('CI00600R')
    
     ** DARTS PNAM
     D scpnam          c                   const('SISCL2')
    
     ** Date & Time structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     ** PMSC Key structure
     D                 ds
     D siskey                        10
     D  sisclaim                     10    overlay(siskey)
    
     ** File Information data structure
     D fids01        e ds                  extname(szq2p)
    
     ** Program status data structure
     D psds          esds                  extname(szq1p)

     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
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
     D ##rtcd          s              1
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
     D xPolicy         s              9  0
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

      *** 02/01/06 ***** add external ds for the SIS claims file ** made this internal, pgm
      *** returns data, but not matching external claims file record.
     D                 ds
     D clds                         256
     D  occonr                        2A   overlay(clds)
     D  ocbrnc                        2A   overlay(clds:3)
     D  ocbtch                        3A   overlay(clds:5)
     D  ocbch#                        3A   overlay(clds:8)
     D  occmnr                       10A   overlay(clds:11)
     D  ocprfx                        4A   overlay(clds:21)
     D  ocplnr                        9  0 overlay(clds:25)
     D  xblanks                     223    overlay(clds:34)

     *==================================================================*
     * 'I' specs
     *==================================================================*
     Ipcm0i
     I              cmkey                       pikey
     I              cmseq#                      piseq#

     *==================================================================*
     * KLIST Definitions
     *==================================================================*
    
     ** DARTS Additional Remarks file
     C     arky          klist
     C                   kfld                    cmkey
     C                   kfld                    cmseq#
    
     ** Image Batch Master file
     C     bmky1         klist
     C                   kfld                    qqbchi
    
     ** DARTS Remarks Detail file
     C     cmkya         klist
     C                   kfld                    qqkey
     C                   kfld                    ##seq#
    
     ** DART Remark Type file
     C     ctky          klist
     C                   kfld                    cmrmcd
    
     C     ctky1         klist
     C                   kfld                    sccode
    
     ** Image Document Notes file
     C     dnky2         klist
     C                   kfld                    qqdcid
    
     ** Image Document Type Table file
     C     dtky          klist
     C                   kfld                    scdoct
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
     *==================================================================*
     * PLIST Definitions
     ** SIS  Search program parameters
     C     sisclm        plist
     C                   parm      'CMOCUR03'    sisflnm           8
     C                   parm      001           sisrow            3 0
     C                   parm      001           sisclmn           3 0
     C                   parm                    sisYYDTA        256
     C*
     *==================================================================*
    
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
      ***** BEG ADD ***** 10/17/11 ***********************************
     c                   parm                    qquncn
     c                   parm                    qqifsp
      ***** END ADD ***** 10/17/11 ***********************************
    
     ** Date check/conversion parameters
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
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
     C                   move      *off          *in97
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
     C                   endif
    
     C                   enddo
    
     C                   exsr      pgmend
    
     C                   move      *on           *inlr
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
     C                   cat(p)    drea:5        xx75
    
     C                   movel     xx75          dn(1)
    
     C                   move      qqseq#        $xseq
     C     mdoc          cat(p)    cmkey:1       xx75
     C                   cat(p)    $xseq:5       xx75
    
     C                   movel     xx75          dn(2)
    
     C     msen          cat(p)    q1user:1      xx75
     C                   cat(p)    '.'           xx75
     C                   movel     xx75          dn(3)
     C                   clear                   dn(4)
    
     ** Call the program to write doc notes.
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
     ** Now we need to create the Additional Remarks for this
     **  remark by read/writing all Notes to the SMARP file.
     **  We will also BACK DATE the Remarks, so they are NOT modifiable.
    
     C                   call      'ISNOTAR'
     C                   parm                    qqdcid
     C                   parm      cmkey         q@key
     C                   parm      cmseq#        q@seq#
     C                   parm      *blanks       q@rcd
    
     C                   endsr

     *====================================================================*
     *  ERRCK1  SUBROUTINE - EDIT SCREEN 1 FIELDS
     *====================================================================*
     C     errck1        begsr
    
     ** Claim must be valid.
     C                   call      'ISIV62R'
     C                   parm      scclaim       ##key
     C                   parm      *blanks       ##rtcd

     C                   if        ##rtcd <> ' '
     C                   move      *on           *in66
     C                   move      *on           *in90
     C                   endif
    
     ** Date entered must be valid.
     C                   eval      srdti = sccdat
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         sccdat
    
     C  n90              if        srerr = '1'
     C                   move      *on           *in52
     C                   move      *on           *in90
     C                   endif
    
     C                   move      ' '           #diary
    
     ** Remark type must be entered and valid.
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
     C  n90              if        sccode <> *blanks
     C     sccode        chain     stctl1                             60
     C                   if        *in60 = *on
     C                   move      *on           *in51
     C                   move      *on           *in90
     C                   endif
    
     C                   else
     C                   move      *on           *in41
     C                   move      *on           *in90
     C                   endif
    
     ** Determine if remark type is diary or not.
     C  n90              if        scditp = 'Y'
    
     ** User name, if diary type, must be valid.
     C                   movel     scdusr        $$user
     C     slky          chain     psl01                              60
     C                   if        *in60 = *on
     C                   move      *on           *in58
     C                   move      *on           *in90
     C                   endif
     C                   endif
    
     C  n90              if        scditp <> ' '
     C                   if        scditp <> 'Y'
     C                             and scditp <> 'N'
     C                   move      *on           *in35
     C                   move      *on           *in90
     c                   endif
     C                   else
     C                   move      ctfl01        scditp                          DITP is blank so do
     C                   endif
    
     ** Check Data Entry Flag to see if Data Entry Program has been
     **  called once already.
     C                   if        xxdefl = 'N'
    
     ** If DRFL03 = 'Y' in file SDRPF, Remark Code is checked to see
     **  if it has an associated Data Entry Program ID.
     C                   move      'N'           xxdepg
     C     syky          chain     sdr01                              40
     C                   if        *in40 = *off
     C                   if        drfl03 = 'Y'
     C                   if        ctdepg <> *blanks
     C                   move      'Y'           xxdepg
    
     ** Doc ID not allowed if the remark type has an associated data
     **  entry program.
     C  n90              if        scdcid <> *blanks
     C                   move      *on           *in26
     C                   move      *on           *in90
     C                   endif
    
     ** Check to see if a recursive call was made.  If so... error msg
     C                   clear                   xxpgm
     C                   movel     ctdepg        xxpgm
     C                   clear                   scdcid
     C                   movel     ctdepg        scdcid
    
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdsdt
     C                   eval      srdti = sccdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxcdat
    
     C  n90              if        xxcdat <= xxdsdt
    
     C                   if        errfle = ' '
     C                   call      'ERROR1C'
    
     C                   eval      length = 55
    
     C                   call      'QCMDEXC'                              37
     C                   parm                    qca(1)
     C                   parm                    length
    
     C                   open      wxerp1
     C                   move      'Y'           errfle
     C                   endif
    
     C     7             chain     per01                              37
     C                   dow       *in37 = *off
    
     ** Check for Recursive call to program
     C                   if        erpgm = xxpgm
     C                   move      *on           *in47
     C                   move      *on           *in90
     C                   endif
    
     C                   read      per01                                  37
     C                   enddo
    
     ** ALL EDITS PASSED - CALL DATA ENTRY PROGRAM
     ** (NOTE: It was necessary to break up the comment field of size
     **  275 into DECOM1 and DECOM2 because the largest alpha-numeric
     **  value allowed for a parameter is 256.

     C                   if        *in90 = *off
    
     C                   clear                   dekey
     C                   clear                   decom1
     C                   clear                   decom2
     C                   clear                   dertcd
     C                   movel     qqkey         dekey
    
     C                   call      ctdepg                               37
     C                   parm                    dekey
     C                   parm                    decom1
     C                   parm                    decom2
     C                   parm                    dertcd
    
     C                   if        *in37 = *on
     C                   move      *on           *in90
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
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdsdt
     C                   eval      srdti = sccdat
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxcdat
     C                   if        xxcdat <= xxdsdt
    
     C                   move      *on           *in20
     C                   move      *on           *in90
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
     ** If AS/400 Office NOT Installed - Don't allow this function
     C                   if        xxofif = 'N'
     C                   move      *on           *in31
     C                   move      *on           *in90
     C                   endif
    
     ** Must have a Default Folder entered for this function
     C  n90              if        xxfold = *blanks
     C                   move      *on           *in32
     C                   move      *on           *in90
     C                   endif
    
     C                   if        *in90 = *off
     C                   call      'SVALDOC'
     C                   parm                    xxdcid
     C                   parm                    xxfold
     C                   parm                    zzrtcd
    
     C                   endif
     C                   if        qqrtcd = '1'
     C                   move      *on           *in54
     C                   move      *on           *in90
     C                   endif
    
     C                   endif
    
     C                   endif
    
     ** Comments cannot be blank.
     C  n90              if        sccomm = *blanks
     C                   move      *on           *in53
     C                   move      *on           *in90
     C                   endif
    
     ** Additional Remarks flag is required.
     C                   if        scarmk <> 'Y'
     C                             and scarmk <> 'N'
     C                   move      *on           *in46
     C                   move      *on           *in90
     C                   endif
    
     ** Purgeable item flag must be 'Y', 'N' or blank.
     C  n90              if        scpgfl <> 'Y'
     C                             and scpgfl <> 'N'
     C                             and scpgfl <> *blank
     C                   move      *on           *in42
     C                   move      *on           *in90
     C                   endif
    
     C                   select
    
     ** If purgeable item flag is = 'Y'
     C                   when      scpgfl = 'Y'
    
     C  n90              if        ctdprg = *zeros
     C                   move      *on           *in43
     C                   move      *on           *in90
     C                   endif
    
     C  n90              if        scditp <> 'Y'
     C                   move      *on           *in44
     C                   move      *on           *in90
     C                   endif
    
     ** If purgeable item flag is = *BLANKS
     C                   when      scpgfl = *blanks
    
     C                   if        ctdprg <> *zeros
     C                             and scditp = 'Y'
     C                   move      'Y'           scpgfl
     C                   else
     C                   move      'N'           scpgfl
     C                   endif
    
     C                   endsl
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
     C                             and qqrtcd <> '8'
     C                             and qqrtcd <> '9'
     C                             and qqrtcd <> 'A'
     C                             and qqrtcd <> 'C'
     C                             and qqrtcd <> 'K'
     c                             and qqrtcd <> 'P'
     C                   exsr      prsctl
     C                   endif
    
     C                   endsr

     *===============================================================*
     *  KEDO - F5=Claims name search
     *===============================================================*
     C     kedo          begsr
    **** have to use the call to get rcd data, split up(use `mocur file format) to put in the sc fl
     ****  fields ... Use CMOCUR as external DS  TOO
     C                   move      *blanks       sisyydta
     C                   movel     scclaim       sisyydta
     c*
    
      ** Call the search program
     C                   call      search        sisclm
    
     c                   movel     sisyydta      clds
      ** Break out the key fields
     C                   move      occmnr        scclaim
    
     C                   endsr
     *===============================================================*
     *  KFDO - F6=Bypass (ONLY FOR USER QUEUE)
     *===============================================================*
     C     kfdo          begsr

     C                   if        *in73 = *on
    
     ** Bypass by putting a "P" in the return code and ending.
     C                   move      'P'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    
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
     C                   move      *on           *in69
     C                   else
     C                   move      *off          *in69
     C                   endif
    
     C                   endsr

     *===============================================================*
     *  KHDO - F8=Reject Document
     *===============================================================*
     C     khdo          begsr
    
     C                   if        *in73 = *off
    
     * Reject the Document being Indexed.  This will
     *  call the Reject Document program  to allow
     *  the entry of reason for the reject, Then update
     *  the IE record, and the DO record with RJ   status.
     *  Code will be RJ. (later a program will be available
     *  to see the "rejects" and perform some processing on them.
     *  The user will be required to fill in a reason for the
     *  Reject in the program, which will be written to the
     *  Doc Notes.
    
     C                   call      'IS157R'
     C                   parm                    qqdcid
     C                   parm      'RJ'          qqdsts
     C                   parm      'ISII01R '    xxpnam
     C                   parm      ' '           qqrtc
    
     C                   if        qqrtc = 'A'
     C                   move      '3'           qqrtcd
     C                   movel     *on           *inlr
     C                   move      *off          *in90
     C                   move      *on           *in97
     C                   else
    
     ** If valid return from reject, bypass the current record.
     C                   movel     '8'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    
     C                   endif
    
     C                   endif
    
     C                   endsr

     *===============================================================*
     *  KIDO - F9=Reassign document
     *===============================================================*
     C     kido          begsr
    
     C                   move      qqsdat        #$sdt
     C                   move      qqstim        #$stm
    
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
    
     C                   select
    
     C                   when      @#rtc1 = 'A'
     C                   move      'A'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    
     C                   when      @#rtc1 = 'B'
     C                   move      'C'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
    
     C                   endsl
    
     C                   if        qqrtcd <> 'A'
     C                             and qqrtcd <> 'C'
     C                   clear                   qqkey
     C                   movel     '9'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
     C                   endif
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
      ** User search
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
     C                   move      *blanks       sccomm
     C                   movel     ctdesc        sccomm
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
     C                   move      *blanks       sccomm
     C                   movel     ctdesc        sccomm
     C                   endif
    
     C                   endsl

     C                   Eval      *in97 = *on
    
     C                   endif
    
     C                   endsr
     *===============================================================*
     * KMDO - F13=Change User Parms
     *===============================================================*
     C     kmdo          begsr
    
     C                   call      'W0006R'
    
     ** GET THE USER PARAMETER FILE
    
     C     upkey         chain     pup01                              60
    
     C                   endsr

     *===============================================================*
     * KNDO - F14=Split
     *===============================================================*
     C     kndo          begsr
    
     C                   move      qqsdat        #$sdt
     C                   move      qqstim        #$stm
    
     C                   if        qqdct = 'F'
     C                   movel     'F'           q@rtc
     C                   else
     C                   movel     'I'           q@rtc
     C                   endif
    
     C                   movel     'I'           q@rtx
    
     C                   call      'IS215R3'
     C                   parm      qqqid         $$qid
     C                   parm      qqdcid        $$dcid
     C                   parm                    #$sdt
     C                   parm                    #$stm
     C                   parm      qqdoct        $$doct
     C                   parm                    q@rtx
    
     C                   if        q@rtx = '3'
     C                   movel     q@rtx         qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
     C                   endif
    
     C                   endsr

     *===============================================================*
     *  KQDO - F16=New Index
     *===============================================================*
     C     kqdo          begsr
     *
     C                   movel     'K'           qqrtcd
     C                   move      *off          *in90
     C                   move      *on           *in97
     *
     C                   endsr
     *===============================================================*
     *  KRDO - F17=User profile search
     *===============================================================*
     C     krdo          begsr
    
     C                   if        slf09 = 'Y'
    
     C                   call      'SC530R'
     C                   parm                    scdusr
     C                   parm                    zzrtcd
    
     C                   endif
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
     C     ctky1         chain     pct01                              59
    
     C                   if        *in59 = *on
     C                   move      slsecl        scsecl
     C                   endif
    
     C                   move      *on           *in56
     C                   move      scsecl        ##secl
     C                   move      'Y'           ##sbyl
    
     ** Display "UNSECURE REMARKS" if "SECURE REMARKS" was displayed.
     C                   if        c1 = 3
     C                   eval      c1 = 4
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
     C                   if        c1 = 4
     C                   eval      c1 = 3
     C                   endif
    
     C                   endif
    
     C                   movel     ck1(c1)       sccml1
    
     C                   endif
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
     C                   when      c1 = 1
     C                             or c1 = 5
     C                   eval      c1 = 2
    
     ** Remark Secured
     C                   when      *in56 = *on
     C                             and c1 = 2
     C                   eval      c1 = 4
    
     ** Remark NOT Secured
     C                   when      *in56 = *off
     C                             and c1 = 2
     C                   eval      c1 = 3
    
     C                   when      c1 = 3
     C                             or c1 = 4
    
     C                   if        *in73 = *on
     C                   eval      c1 = 5
     C                   else
     C                   eval      c1 = 1
     C                   endif
    
     C                   endsl
    
     C                   movel     ck1(c1)       sccml1
    
     C                   endif
    
     C                   endsr
     *===============================================================
     * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     *===============================================================
     C     othfks        begsr
    
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
    
     C                   endsr

     *===============================================================
     * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     *===============================================================
     C     pgmend        begsr
    
     C                   if        *inkc = *on
     C                   move      '3'           qqrtcd
     C                   endif
    
     C                   if        *inkl = *on
     C                   move      'C'           qqrtcd
     C                   endif
    
     C                   endsr

     *====================================================================*
     * PGMSTR
     *====================================================================*
     C     pgmstr        begsr
    
     C                   movel     pgmnam        sctitl
     C                   movel     'SC'          xxsyk
     C                   eval      c = 0
     C                   eval      c1 = 0
     C                   move      'N'           xxdewr
     C                   clear                   dertcd
     C                   clear                   decom1
     C                   clear                   decom2
    
     C                   clear                   dekey
    
     ** Save the RTCD (F=fax, I=image, " "= scanned image)
     C                   movel     qqrtcd        qqdct
    
     C                   if        qqdct <> 'F'
     C                   move      *off          *in73
     C                   else
     C                   move      *on           *in73
     C                   endif
    
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
    
     C                   movel     q1user        $$user
     C     slky          chain     psl01                              60
     C                   if        *in60 = *on
     C                   clear                   $$user
     C     slky          chain     psl01                              60
     C                   endif
    
     C                   movel     slfold        scdflr
    
     C                   if        slccif <> 'Y'
     C                   move      *on           *in98
     C                   else
     C                   move      *off          *in98
     C                   endif
    
     ** Check System Control File Install Flags for this Client:
     C     'SY'          chain     ssy01                              60
     C                   if        *in60 = *off
    
     ** Check for AS/400 Office Installed
     C                   if        syofif = 'Y'
     C                   move      'Y'           xxofif
     C                   move      *off          *in03
     C                   else
     C                   move      'N'           xxofif
     C                   move      *on           *in03
     C                   endif
    
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
     C                   if        *in73 = *on
     C                   eval      c1 = 5
     C                   else
     C                   eval      c1 = 1
     C                   endif
    
     C                   movel     ck1(c1)       sccml1
    
     ** Load the record format info for the first time.
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
    
     C                   movel     scddsc        sccomm
     C                   movel     'N'           scarmk
     C                   time                    dsdttm
     C                   move      dsdate        sccdat
    
     C     dtky          chain     pdt01                              60
     C                   if        *in60 = *off
     C                   movel     dtrmcd        scdept
     C                   move      dtrmcd        scrmty
     C                   else
     C                   clear                   scdept
     C                   clear                   scrmty
     C                   endif
     c*
     c*** fill in the default Diary type here *****
     c*
     C                   movel     scdept        sccode
     C                   move      scrmty        sccode
     c     ctky1         chain     pct01                              60
     c                   if        *in60 = *off
     C                   movel     ctfl01        scditp
     c                   else
     c                   movel     *blanks       scditp
     c                   endif
    
     C     bmky1         chain     pbm01                              60
     C                   if        *in60 = *off
     C                   movel     bmbdsc        scbdsc
     C                   else
     C                   clear                   scbdsc
     C                   endif
    
     C                   movel     scbdsc        sccomm
    
     ** Any Non-System Doc notes exist?
     C     dnky2         chain     pdn01                              60
     C                   if        *in60 = *off
     C                   move      *on           *in69
     C                   else
     C                   move      *off          *in69
     C                   endif
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
     ** We have passed the edits, move screen key into CM record.
     C                   move      scclaim       sisclaim
    
     C                   movel     siskey        ##key
    
     C                   movel     scpnam        ##pnam
    
     C                   exsr      wcom
    
     ** EXSR here to write doc notes, and also to copy all doc notes
     ** to the SMARP file for this new DART remark.
     C                   exsr      dnot
    
     ** Retain the item on the users queue?
     C                   if        scretn = 'Y'
     C                   movel     'R'           qqrtcd
     C                   endif

     C                   endsr
     *====================================================================*
     *  WCOM  SUBROUTINE - WRITE REMARK RECORD
     *====================================================================*
     C     wcom          begsr
    
     C                   time                    dsdttm
    
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         hhdt
     C                   eval      hhtime = dstime
    
     C                   move      ##secl        cmsecl
     C                   move      ##sbyl        cmsbyl
    
     C                   movel     ##key         cmkey
     C                   clear                   cmcsid
     C                   movel     ##pnam        cmpnam
     C                   clear                   cmcode
     C                   movel     sccode        cmrmcd
     C                   movel     '***IMAGE'    cmdcid
     C                   move      '*** '        cmdcid
     C                   movel     sccomm        cmcomm
     C                   move      ' '           cmfl01
    
     ** Load fax folder.
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
    
     ** Fill hidden fields.
     C                   movel     scique        cmique
     C                   movel     scipth        cmipth
     C                   move      scpidx        cmpidx
     C                   movel     scapid        cmapid
     C                   movel     qqdcid        cmidoc
     C                   movel     scqpty        cmqpty
     C                   movel     scdctp        cmdctp
    
     ** Check fax flag.
     C                   if        qqdct <> 'F'
     C                   movel     'Y'           cmimgf
     C                   else
     C                   movel     'F'           cmimgf
     C                   movel     qqdcid        cmdcid
     C                   clear                   cmidoc
     C                   endif
    
     C                   clear                   cmstat
     C                   movel     q1pgm         cmpgm
    
     ** If Diary user entered, write to CMUSER, else default to current user.
     C                   if        scdusr <> *blanks
     C                   movel     scdusr        cmuser
     C                   else
     C                   movel     q1user        cmuser
     C                   endif
    
     C                   movel     q1user        cmiusr
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmdt
     C                   eval      cmtime = dstime
    
     C                   if        scditp = 'Y'
     C                             or scditp = 'N'
     C                   move      scditp        cmdifl
     C                   else
     C                   move      ' '           cmdifl
     C                   endif
    
     C                   movel     'D'           cmfl05
    
     C                   move      scpgfl        cmfl02
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmidt
    
     C                   eval      cmdcdt = *zero
     C                   movel     q1pgm         cmipgm
    
     C                   clear                   cmsmcd
     C                   clear                   cmfl04
     C                   clear                   cmfl05
    
     ** Get the last SEQ# used.
     C                   movel     ##key         qqkey
    
     C     cmkya         setgt     pcm0i
     C                   readp     pcm0i                                  60
     C                   if        *in60 = *off
     C                             and qqkey = pikey
     C                   eval      cmseq# = piseq# + 1
     C                   else
     C                   eval      cmseq# = 0
     C                   endif
    
     C                   write     pcm01
     C                   feod      sdcml
    
     C                   clear                   qqrtcd
    
     ** If additional remarks are requestd, call program to add the remarks.
     C                   movel     cmkey         qqkey
     C                   move      cmseq#        qqseq#
     C                   movel     q1pgm         qqpnam
    
     C                   if        scarmk = 'Y'
     C                   call      'SC014R'
     C                   parm                    qqkey
     C                   parm                    qqseq#
     C                   parm                    qqpnam
     C                   endif
    
     ** Execute the user index exit here only for diary types.
     C                   if        scditp = 'Y'
    
     C                   move      cmseq#        v@seq#
     C                   call      'ISE002C'                            99
     C                   parm      qqqid         fq
     C                   parm      scdusr        tu
     C                   parm      q1user        uid
     C                   parm      cmdcid        dcid
     C                   parm      cmkey         v@key
     C                   parm                    v@seq#
     C                   parm      *blanks       rtrn
    
     C                   endif
    
     C                   endsr

    C*====================================================================*
**  CK1 - Command Keys - SCR01
F3=Exit   F5=Claims search   F7=Notes   F8=Reject   F24=More keys
F9=Reassign   F10=Search   F12=Previous   F13=Chg User Parms   F24=More keys
F14=Split   F16=New Index Type   F23=Secure Remarks   F24=More keys
F14=Split   F16=New Index Type   F23=Unsecure Remarks   F24=More keys
F3=Exit   F5=Claims search   F6=Bypass   F7=Notes   F24=More keys
**  QCMDEXC TABLE
OVRDBF FILE(WXERP1) TOFILE(QTEMP/WXERP1 )
