     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
    H/TITLE W8070RPA - Pennsylvania NOI Print Program
    H*****************************************************************
    H* Program Description:  This program prints Pennsylvania NOI    *
    H*                       forms, FRONT and BACK (two Pages)       *
    H*                                                               *
    H* Called from:  W8070CPA                                        *
    H* Calls to:  None                                               *
    H*                                                               *
    H*****************************************************************
    H* Created 01/10/2006  C: 132  PROJECT: 114WKO
    H*****************************************************************
    H* Revisions:                                                    *
     H*                                                               *
     H*   10/08/21 C:079 Support: 114/96555                           *
     H*                  Use NItim2 field to report start work time   *
     H*                                                               *
     H*   06/09/20 C:318 Project: SY420                               *
     H*                Mask SSN                                       *
     H*                                                               *
     H*                                                               *
     H*   10/17/11 C:279 Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
    H*                                                               *
     H* 02/15/11 C:246  Support : 75579  CLT:114                      *
     H*                 Display  company name and address in PA FROI  *
     H*                 form based on the self insured flag setup in  *
     H*                 Work With Groups.                             *
    H*                                                               *
     H* 03/24/10 C:232  Project: 139WNN                               *
     H*                 Update programs to use the new EDI Control API*
     H*                 WA076R instead of using WMIXP.                *
     H*                                                               *
    H* 06/20/06 c:53 Support 53204                                   *
    H*               Check for data decimal error.                   *
    H*                                                               *
    H* 06/01/06 c:53 Support 52982                                   *
    H*               Correct invalid open errors.                    *
    H*                                                               *
    H* 04/25/06 c:62 Project 114DJN                                  *
    H*               Change field mappings.                          *
    H*                                                               *
    H*****************************************************************
     Fsdcmld    o    e           k disk
     F                                     rename(pcm01:pcm02)
     Fsdcmla    if   e           k disk
     F                                     rename(pcm01:pcm03)
     Fstctl1    if   e           k disk
     Fsmorl     if   e           k disk
    F*
     Fwdf2l     if   e           k disk
     Fwdell     if   e           k disk
    F*
     Fwmcdl     if   e           k disk
     Fwmcml1    if   e           k disk
     Fwmcxl     if   e           k disk
     Fwmc1l     if   e           k disk
     Fwmeml     if   e           k disk
     Fwmfnl     if   e           k disk
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***wmixl     if   e           k disk
     F*** END DLT ***** 03/24/10 **************************************
     Fwmlgl1    if   e           k disk
     Fwmlgl     uf   e           k disk
     F                                     rename(plg01:plg02)
    F*
     Fwmpdl     if   e           k disk
    F**************WMRPL   IF  E           K        DISK
     Fwmtpl1    if   e           k disk
     Fwmnil     if   e           k disk
     F                                     rename(pni01:pni04)
    F*
    F***************WTGTL   IF  E           K        DISK
     Fwtcnl1    if   e           k disk
     Fwtabl     if   e           k disk
     F                                     infds(dbfds3)
     F                                     ignore(pat01)
     F                                     ignore(pba01)
     F                                     ignore(pbc01)
     F                                     ignore(pb201)
     F                                     ignore(pb701)
    F**********  PCA01                             KIGNORE
     F                                     ignore(pcc01)
     F                                     ignore(pcn01)
     F                                     ignore(phy01)
     F                                     ignore(pid01)
     F                                     ignore(pis01)
     F                                     ignore(plr01)
    F**********  PNA01                             KIGNORE
     F                                     ignore(pnc01)
    F**********  PPA01                             KIGNORE
     F                                     ignore(ppc01)
     F                                     ignore(prc01)
     F                                     ignore(prs01)
     F                                     ignore(prt01)
     F                                     ignore(psp01)
     F                                     ignore(psy01)
     F                                     ignore(ptr01)
     F                                     ignore(ptt01)
     F                                     ignore(pvs01)
    F*
     Fwxnil     if   e           k disk
     Fwxnil1    if   e           k disk
     F                                     rename(pni01:pni02)
     F***** Beg Add 02/15/11 ******************************************
     Fsmcol     if   e           k disk
     F***** End Add 02/15/11 ******************************************
     F*
     Fqsysprt   o    f  127        printer oflind(*inoa)
     F                                     usropn
     Fqsysprt1  o    f   85        printer oflind(*inof)
     F                                     usropn
     Fqsysprt2  o    f  127        printer oflind(*inog)
     F                                     usropn
    F*
    D*================================================================
    D* Arrays and Tables
    D*================================================================
     D pr1             s              1    dim(75) ascend
     D cr1             s              1    dim(75) ascend
     D cnb             s              1    dim(40) ascend
     D cn              s              1    dim(40)
     D sz              s              1    dim(13)
     D st              s              2    dim(1)
     D cpy             s             21    dim(3) ctdata perrcd(1)
     D wd              s              1    dim(7)
     D ovr             s              1    dim(33)
    D*================================================================
    D* Constants
    D*================================================================
    D*
     D xxovr           c                   const('OVRPRTF QSYSPRT  OUT-
     D                                     Q(          )')
     D xxovr1          c                   const('OVRPRTF QSYSPRT1 OUT-
     D                                     Q(          )')
     D xxovr2          c                   const('OVRPRTF QSYSPRT2 OUT-
     D                                     Q(          )')
    D*================================================================
    D* Data Structures
    D*================================================================
     D psds          esds                  extname(szq1p)
     D dbfds3          ds
     D dbrrn3                397    400b 0
    D*  Print Line Array
     D                 ds
    D***************                               PT1
     D ptchar                        75
    D*  Character Array to be spread to Print Line Array
     D                 ds
    D************                                  CR1
     D xxchar                        75
    D*================================================================
    D* User Define Data Structures
    D*================================================================
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D qckey                         40
     D  qcco#                         3  0 overlay(qckey)
     D  qcfnd                         3  0 overlay(qckey:4)
     D  qcfyr                         3  0 overlay(qckey:7)
     D  qccase                        7  0 overlay(qckey:10)
     D                 ds
     D xxzip                          9  0
     D  xxzpm1                        5  0 overlay(xxzip)
     D  xxzpm2                        4  0 overlay(xxzip:6)
    D* Separat Phone Number into Area, phone #
     D                 ds
     D xxphn#                        10  0
     D  xxwara                        3  0 overlay(xxphn#)
     D  xxwphn                        7  0 overlay(xxphn#:4)
     D                 ds
     D xcphn#                        10  0
     D  xxcara                        3  0 overlay(xcphn#)
     D  xxcphn                        7  0 overlay(xcphn#:4)
    D*
     D                 ds
     D xx60ax                        60
     D  xx48a                        48    overlay(xx60ax)
     D   xx24a                       24    overlay(xx48a)
     D    xx12a                      12    overlay(xx24a)
     D  xx48b                        48    overlay(xx60ax:13)
     D   xx36b                       36    overlay(xx48b:13)
    D*
     D                 ds
     D xx80ax                        80
     D  xx72a                        72    overlay(xx80ax)
     D  xx16a                        16    overlay(xx80ax:65)
     D   xx08a                        8    overlay(xx16a:9)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##dcid          s                   like(cmdcid)
     D ##doc           s             12
     D ##fldr          s                   like(cmfold)
     D ##fnd           s              8
     D ##fold          s              8
     D ##fpth          s             40
     D ##rt            s              2
     D ##ser           s              8
     D ##sub           s              8
     D ##term          s              1
     D @@case          s              7
     D @@co#           s              3
     D @@date          s              8
     D @@fnd           s              3
     D @@froi          s              2
     D @@fyr           s              3
     D @@jrst          s              2
     D kkcase          s                   like(cmcase)
     D kkco#           s                   like(nico#)
     D kkdate          s                   like(nidt)
     D kkfein          s                   like(pdfein)
     D kkfnd           s                   like(nifnd)
     D kkfyr           s                   like(cmfyr)
     D kkhcpy          s                   like(lghcpy)
     d*** BEG DEL ****** 03/24/10 *************************************
     D*** kkjrst          s                   like(ixjrst)
     d*** END DEL ****** 03/24/10 *************************************
     D kkkey           s                   like(cnkey)
     D kkmod           s                   like(pdmod)
     d*** BEG DEL ****** 03/24/10 *************************************
     D*** kktset          s                   like(ixtrid)
     d*** END DEL ****** 03/24/10 *************************************
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D svfnd           s                   like(nifnd)
     D svtset          s                   like(lgtset)
     D test1           s              2  0
     D tm6             s              6  0
     D pos1            s              2  0
     D pos2            s              2  0
     D length          s              2  0
     D phone           s             10
     D x               s              5  0
     D xxaccy          s             20
     D xxacdt          s              8  0
     D xxactm          s              4  0
     D xxbucd          s             10
     D xxcaus          s             75
     D xxccsz          s             40
     D xxcfnm          s             36
     D xxcfsn          s             35
     D xxclas          s              5  0
     D xxclnm          s             75
     D xxclsn          s             35
     D xxcnam          s             50
     D xxcnty          s             35
     D xxcopy          s              1  0
     D xxcpy           s             21
     D xxcrbc          s             12
     D xxcsz           s             40
     D xxcty           s             25
     D xxdadr          s             72
     D xxdara          s              3  0
     D xxdbdt          s              8  0
     D xxdcty          s             35
     D xxdfsn          s             40
     D xxdlsn          s             72
     D xxdod           s              8  0
     D xxdphn          s              7  0
     D xxdst           s              2
     D xxdtlw          s              8  0
     D xxdtob          s              8  0
     D xxdtrp          s              8  0
     D xxdzip          s              9  0
     D xxeara          s              3  0
     D xxecny          s             35
     D xxenam          s             75
     D xxephn          s              7  0
     D xxests          s              2
     D xxfad1          s             40
     D xxfad2          s             40
     D xxfcty          s             21
     D xxfnme          s             40
     D xxfnst          s              2
     D xxfnzp          s              5
     D xxfrst          s             15
     D xxf2fn          s              9  0
     D xxhadr          s             50
     D xxhcty          s             21
     D xxhidt          s              8  0
     D xxhosn          s             40
     D xxhst           s              2
     D xxhzip          s             10
     D xxkey           s             10
     D xxlast          s             56
     D xxmadr          s             75
     D xxmc            s              2
     D xxmcty          s             40
     D xxmst           s              4
     D xxnaic          s             10
     D xxnatr          s             75
     D xxocds          s             75
     D xxopen          s              1
     D xxopn1          s              1
     D xxopn2          s              1
     D xxoutq          s             10
     D xxpart          s             75
     D xxpdzp          s              9
     D xxpfdt          s              8  0
     D xxphyn          s             40
     D xxpnam          s             30
     D xxpphn          s             10  0
     D xxprdt          s              8  0
     D xxprt           s              1
     D xxptdt          s              8  0
     D xxpttl          s             30
     D xxrmcd          s                   like(cmrmcd)
     D xxrtw           s              8  0
     D xxsctc          s                   like(f2sctc)
     D xxsic           s              6
     D***  Begin Chg  ***  06/09/20  *******************************
     D*xxss#           s             10  0
     D xxss#           s             21
     D***  End Chg  ***  06/09/20  *******************************
     D xxst            s             13
     D xxstat          s              2
     D xxsz            s             13
     D xxtadr          s             40
     D xxtcty          s             21
     D xxterm          s              1
     D xxtim1          s              4  0
     D xxtst           s              2
     D xxtzip          s              9
     D xxwfsn          s             35
     D xxwlsn          s             35
     D xxzipa          s              5
     D xxzipb          s              4
     D xxzp1           s              5
     D xxzp2           s              4
     D xx72a1          s             72
     D xx72a2          s             72
     D xx72a3          s             72
     D xx72b1          s             72
     D xx72b2          s             72
     D xx72b3          s             72
     D x1outq          s             10
     D x2outq          s             10
     D y               s              3  0
     D z               s              3  0
      ***** BEG ADD ***** 10/17/11 ***********************************
     d root            s             10
     d env             s              1
      ***** END ADD ***** 10/17/11 ***********************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    I*================================================================
    I* File Field Renames
    I*================================================================
     Ipcm03
     I              cmkey                       c3key
     I              cmcsid                      c3csid
     I              cmcomm                      c3comm
     I              cmcdat                      c3cdat
     I              cmcode                      c3code
     I              cmdcid                      c3dcid
     I              cmpnam                      c3pnam
     I              cmtrdt                      c3trdt
     I              cmtrtm                      c3trtm
     I              cmseq#                      c3seq#
     I              cmsecl                      c3secl
     I              cmsbyl                      c3sbyl
     I              cmstat                      c3stat
     I              cmuser                      c3user
     I              cmpgm                       c3pgm
     I              cmtime                      c3time
     I              cmdt                        c3dt
     I              cmiusr                      c3iusr
     I              cmfold                      c3fold
     I              cmdcdt                      c3dcdt
     I              cmdifl                      c3difl
     I              cmapid                      c3apid
     I              cmidoc                      c3idoc
     I              cmique                      c3ique
     I              cmimgf                      c3imgf
     I              cmipth                      c3ipth
     I              cmpidx                      c3pidx
     I              cmqpty                      c3qpty
     I              cmdctp                      c3dctp
     I              cmipgm                      c3ipgm
     I              cmsmcd                      c3smcd
     I              cmfl01                      c3fl01
     I              cmfl02                      c3fl02
     I              cmfl03                      c3fl03
     I              cmfl04                      c3fl04
     I              cmfl05                      c3fl05
     I              cmrmcd                      c3rmcd
     I              cmidt                       c3idt
      ***** BEG ADD ***** 10/17/11 ***********************************
     I              cmuncn                      c3uncn
     I              cmshrn                      c3shrn
     I              cmifsp                      c3ifsp
      ***** END ADD ***** 10/17/11 ***********************************
    I*
    C*================================================================
    C* Parameter lists
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@froi
     C                   parm                    @@jrst
     C                   parm                    @@date
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
    C*
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
    C*  TO SIX DIGIT YEAR
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    kkco#
     C                   PARM                    kkjrst            2
     C                   PARM                    kktset            3
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
      *
    C*
    C*================================================================
    C* Key lists
    C*================================================================
    C*
    C*  KEY LIST FOR THE ADDITIONAL LOCATION FILE
    C*
     C     alky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmctg1
    C*
    C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
    C*
     C     cdky          klist
     C                   kfld                    cmss#
    C*
    C*  KEY LIST FOR THE CLAIM MASTER FILE
    C*
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
    C*  KEY LIST FOR THE COUNTY DESCRIPTION FILE
    C*
     C     cnky          klist
     C                   kfld                    kkco#
     C                   kfld                    xxkey
    C*
    C*  KEY LIST FOR THE COMPANY FILE
    C*
     C     coky          klist
     C                   kfld                    kkco#
    C*
    C*  KEY LIST FOR THE DARTS REMARK CODE FILE
    C*
     C     ctky          klist
     C                   kfld                    ixhcpr
    C*
    C*  KEY LIST FOR THE DARTS FILE
    C*
     C     c3ky          klist
     C                   kfld                    qckey
    C*
    C*  KEY LIST FOR THE EMPLOYER MASTER FILE
    C*
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
    C*
    C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
    C*
     C     elky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    kkfyr
    C*
    C*  KEY LIST FOR THE FUND MASTER FILE
    C*
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
    C*
    C*  KEY LIST FOR THE FUND AUXILIARY FILE
    C*
     C     f2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     *
    C*
    C*  KEY LIST FOR THE EDI CLAIM CONTROL FILE
    C*
     C     ixky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkjrst
     C                   kfld                    kktset
    C*
    C*  KEY LIST FOR THE LOG HISTORY FILE
    C*
     C     lgky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkhcpy
     C                   kfld                    kkjrst
     C                   kfld                    kkdate
    C*
     C     lgky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    lgidt
     C                   kfld                    lgitm
    C*
    C*  KEY LIST FOR THE NOI WORKFILE
    C*
     C     niky          klist
     C                   kfld                    kkco#
    C*
    C*   KEY LIST FOR THE OVERRIDE FILE
    C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt
    C*
    C*  KEY LIST FOR THE PAYEE MASTER FILE
    C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
    C*  KEY LIST FOR THE CARRIER/STATE REPORTING FILE
    C*
     C     rpky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C                   kfld                    cmfyr
     *
     *  TABLE FILE
     *
     C     tbky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
    C*
    C*  TREATING PHYSICIAN
    C*
     C     tpky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     *
     *  Key for User Profile file
     *
     C     upky          klist
     C                   kfld                    cmuser
    C*
    C*================================================================
    C* Like Defines
    C*================================================================
    C*
    C*
    C*
    C*
    C/EJECT
    C*================================================================
    C* Main Processing
    C*================================================================
      ***** BEG ADD ***** 10/17/11 ***********************************
      * Get Root
     c                   call      'RTVDOCROOT'
     c                   parm                    root
      * Find IFS
     c                   call      'RTVDOCENV'
     c                   parm                    env
      ***** END ADD ***** 10/17/11 ***********************************
     C                   clear                   xxcpy
     C                   eval      x = 0
     C                   time                    dsdttm
    C*
     C                   move      @@co#         kkco#
     C                   move      @@date        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         kkdate
     C                   movel     @@froi        kkhcpy
     C                   move      @@jrst        kkjrst
     C*** beg add *** 06/01/06 ********************************
     C                   move      *off          xxopn2
     C*** end add *** 06/01/06 ********************************
    C*
     C                   if        q1prms > 4
     C                   move      @@fnd         kkfnd
     C                   move      @@fyr         kkfyr
     C                   move      @@case        kkcase
     C                   else
     C                   clear                   kkfnd
     C                   clear                   kkfyr
     C                   clear                   kkcase
     C                   endif
    C*
    C*   GET THE DISTRIBUTION INFORMATION
    C*
     C                   clear                   xxoutq
     C                   move      '0'           xxprt
     C     orky          chain     por01                              99
     C                   if        oroutq = *blank
     C                   movel     'QPRINT'      xxoutq
     C                   endif
    C*
     C                   clear                   oroutq
     C                   move      '1'           xxprt
     C     orky          chain     por01                              99
     C                   movel     oroutq        x1outq
     C                   if        x1outq = *blank
     C                   movel     'QPRINT'      x1outq
     C                   endif
    C*
     C                   clear                   oroutq
     C                   move      '2'           xxprt
     C     orky          chain     por01                              99
     C                   movel     oroutq        x2outq
     C                   if        x2outq = *blank
     C                   movel     'QPRINT'      x2outq
     C                   endif
    C*
    C*  IF THIS PGM WAS CALLED FROM THE DAILY BATCH, WXNIP WILL BE READ
    C*  3 TIMES TO PRODUCE 3 COPIES.  IF CALLED FROM THE EXTRACT
    C*  OPTION, 1 READ WILL OCCUR.
    C*
     C                   if        kkcase = *zeros
     C                   if        @@froi <> '  '
     C                   eval      xxcopy = 3
     C                   else
     C                   eval      xxcopy = 1
     C                   endif
     C                   endif
    C*
     C     1             do        2
    C*
     C                   if        kkcase = *zeros
     C                   eval      svfnd = 0
     C                   eval      x = xxcopy
     C                   move      cpy(x)        xxcpy
     C                   eval      *in71 = x = 1
     C                   eval      *in72 = x = 2
     C                   eval      *in73 = x = 3
     C                   endif
    C*
     C                   eval      *in75 = *off
     C                   if        kkcase <> *zeros
     C                   eval      *in75 = *on
     C                   endif
    C*
    C*   OPEN THE PRINT FILE
    C*
     C                   if        *in73 = *off
     C                   if        xxcopy = 1
     C                   open      qsysprt
     C                   move      *on           xxopen
     C                   endif
     C                   if        xxcopy = 2
     C                   open      qsysprt1
     C                   move      *on           xxopn1
     C*** beg add *** 06/01/06 ********************************
     C                   if        xxopn2 = *off
     C*** end add *** 06/01/06 ********************************
     C                   open      qsysprt2
     C                   move      *on           xxopn2
     C*** beg add *** 06/01/06 ********************************
     C                   endif
     C*** end add *** 06/01/06 ********************************
     C                   endif
     C                   endif
    C*
     C                   if        *in75 = *on
     C*** beg add *** 06/01/06 ********************************
     C                             and xxopn2 = *off
     C*** end add *** 06/01/06 ********************************
     C                   open      qsysprt2
     C                   move      *on           xxopn2
     C                   eval      xxcopy = 0
     C                   clear                   xxcpy
     C                   endif
    C*
     C                   select
     C                   when      *in71 = *on
     C     niky          chain     pni01                              99
     C                   when      *in72 = *on
     C     niky          chain     pni02                              99
     C                   when      *in73 = *on
     C     lgky          chain     plg01                              99
     C                   when      *in75 = *on
     C     cmky          chain     pcm01                              99
     C                   endsl
    C*
     C                   dow       *in99 = *off
    C*
     C                   do
    C*
     C                   if        *in71 = *on
     C                             and niidt <> kkdate
     C                             or *in72 = *on
     C                             and niidt <> kkdate
     C                   iter
     C                   endif
     C                   if        *in73 = *on
     C                   if        lgprsq <> 0
     C                             or lgschp <> lgexdt
     C                   iter
     C                   endif
     C                   endif
    C*
    C*   OPEN THE PRINT FILE
    C*
     C                   if        *in73 = *on
     C*** beg add *** 06/01/06 ********************************
     C                             and xxopn2 = *off
     C*** end add *** 06/01/06 ********************************
     C                   open      qsysprt2
     C                   move      *on           xxopn2
     C                   endif
    C*
     C                   exsr      srinit
    C*
    C*  IF THE FUND CHANGED, GET THE NEW FUND INFORMATION.
    C*
     C                   if        kkfnd <> svfnd
     C                   exsr      fndbrk
     C                   eval      svfnd = kkfnd
     C                   endif
    C*
    C*  LOAD CLAIMANT INFORMATION
    C*
     C     cmky          chain     pcm01                              99
     C                   if        *in99 = *on
     C                   leave
     C                   endif
    C*
     C     cdky          chain     pcd01                              99
     C                   if        *in99 = *on
     C                   leave
     C                   endif
    C*
     C     cmky          chain     pni04                              99
     C                   if        *in99 = *on
     C                   clear                   pni04
     C                   endif
    C*
     C     cmky          chain     pc101                              99
     C                   if        *in99 = *on
     C                   clear                   pc101
     C                   endif
    C*
    C* GET EMPLOYER INFO
    C*
     C     emky          chain     pem01                              99
     C                   if        *in99 = *on
     C                   leave
     C                   endif
    C*
    C* GET EMPLOYER DETAIL POLICY DATES
    C*
     C     elky          chain     pel01                              99
    C*
     C                   eval      xxpfdt = *zeros
     C                   eval      xxptdt = *zeros
     C                   if        *in99 = *off
    C*
    C*  - Convert Policy dates
    C*
     C                   eval      srdti = elpfdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxpfdt
    C*
     C                   eval      srdti = elptdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxptdt
    C*
     C                   endif
    C*
     C                   movea     '00000'       *in(21)
     C                   movea     '00000'       *in(26)
     C                   movea     '00000'       *in(31)
     C                   movea     '000000'      *in(36)
     C                   movea     '0000000'     *in(80)
    C*
    C*================================================================
    C* Employer Info
    C*================================================================
    C*  Policy number, Carrier Case number, WCB Case number
    C*
    C*  Line 11 - Employer Name
    C*
    C* EMNAME
     C                   movel(p)  emfdnm        xxenam
     C                   movel(p)  emnaic        xxnaic
     C                   eval      xxeara = emarea
     C                   eval      xxephn = emphn#
    C*
     C                   move(p)   emsic         xxsic
    C*
    C*  Line 5 - Mailing Address of Emplyer
    C*
     C                   clear                   xxmadr
    C*
     C                   if        emmad1 <> *blanks
     C                   movel(p)  emmad1        xxmadr
     C                   if        emmad2 <> *blanks
     C                   cat       emmad2:2      xxmadr
     C                   endif
     C                   endif
    C*
     C                   movel(p)  emmcty        xxmcty
     C                   movel(p)  emmst         xxmst
     C                   movel     emmzip        xxzipa
     C                   move      emmzip        xxzipb
    C*
     C                   movel(p)  emmst         xxkey
     C     xxkey         cat       emcnty:0      xxkey
     C     cnky          chain     pcn01                              99
     C                   if        *in99 = *off
     C                   movel     cndesc        xxecny
     C                   else
     C                   movel(p)  empst         xxkey
     C     xxkey         cat       emcnty:0      xxkey
     C     cnky          chain     pcn01                              99
     C                   if        *in99 = *off
     C                   movel     cndesc        xxecny
     C                   endif
     C                   endif
    C*
     C                   if        ni40a1 <> *blanks
     C                   endif
    C*
    C*================================================================
    C* Employee Info
    C*================================================================
    C*
    C*  Line 4 - Employee Name
    C*
     C                   movel(p)  cmfsn         xxcfnm
     C                   if        cmmid <> *blanks
     C                   cat       cmmid:1       xxcfnm
     C                   cat       '.':0         xxcfnm
     C                   endif
     C                   cat(p)    cmlsn:1       xxclnm
    C*
    C* CMSS#
     C                   move      cmss#         xxss#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'xxss#'       qqname           25
     C     xxss#         parm      xxss#         qqval           100
     C                   eval      xxss#=%subst(xxss#:1:1)+' '+%subst(xxss#:2:1)
     C                             +' '+%subst(xxss#:3:1)+'   '
     C                             +%subst(xxss#:4:1)+' '+%subst(xxss#:5:1)
     C                             +'   '+%subst(xxss#:6:1)+' '
     C                             +%subst(xxss#:7:1)+' '+%subst(xxss#:8:1)
     C                             +' '+%subst(xxss#:9:1)
     C***  End Add  ***  06/09/20  *******************************
     C                   move      cmclas        xxclas
     C                   move      cmests        xxests
     C                   movel(p)  cdlsn         xxdlsn
     C                   movel(p)  cdfsn         xxdfsn
     C                   if        cdmid <> ' '
     C     xxdfsn        cat       cdmid:1       xxdfsn
     C     xxdfsn        cat       '.':0         xxdfsn
     C                   endif
    C*
    C*  LINE 5 - Address
    C*
     C                   eval      xxdzip = cdzip
     C                   clear                   xxdadr
    C*
     C                   movel(p)  cdadr1        xxdadr
     C                   if        cdadr2 <> *blanks
     C     xxdadr        cat       cdadr2:2      xxdadr
     C                   endif
    C*
     C                   movel(p)  cdcty         xxdcty
     C                   move      cdst          xxdst
     C                   eval      xxdara = cdarea
     C                   eval      xxdphn = cdphn#
     C                   movel(p)  ni30a4        xxcnty
    C*
    C*================================================================
    C* Injury or Illness Info
    C*================================================================
    C*
    C*  - Date of Injury
     C                   eval      srdti = cmacdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxacdt
    C*
    C*  LINE 44 - Employer Location
    C*
    C* CMACNM - CMZIP
    C*
    C*  Line 56 - County
    C*
     C                   movel     cmaccy        xxaccy
    C*
    C*  LINE 22 2nd Pg - On employer's premises? ST, Safeguards, Safety Equip.
    C*
     C                   eval      *in81 = *off
     C                   if        cmeadr <> 'Y'
     C                   eval      *in81 = *on
     C                   endif
    C*
     C                   eval      *in82 = *off
     C                   if        ni1a1 <> 'Y'
     C                   eval      *in82 = *on
     C                   endif
    C*
     C                   eval      *in83 = *off
     C                   if        ni1a2 <> 'Y'
     C                   eval      *in83 = *on
     C                   endif
    C*
    C*  Line 61B - Time employee began work
    C*
     C                   eval      *in28 = *off
     C                   move      nitim1        tm6
     C                   movel     tm6           xxtim1
     C                   movel     xxtim1        test1
     C                   if        test1 >= 12
     C                   eval      *in28 = *on
     C                   if        test1 > 12
     C                   eval      xxtim1 = xxtim1 - 1200
     C                   endif
     C                   else
     C                   if        test1 = 0
     C                   eval      xxtim1 = xxtim1 + 1200
     C                   endif
     C                   endif
    C*
    C*  Line 61C - Time of Injury
    C*
     C                   eval      *in27 = *off
     C                   movel     cmactm        xxactm
     C                   movel     xxactm        test1
     C                   if        test1 >= 12
     C                   eval      *in27 = *on
     C                   if        test1 > 12
     C                   eval      xxactm = xxactm - 1200
     C                   endif
     C                   else
     C                   if        test1 = 0
     C                   eval      xxactm = xxactm + 1200
     C                   endif
     C                   endif
    C*
    C*  Line 65A - Date last worked
    C*
     C                   if        cmdtlw <> *zeros
     C                   eval      srdti = cmdtlw
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdtlw
     C                   else
     C                   clear                   xxdtlw
     C                   endif
    C*
    C*  Line 65B - Date Disability Began
     C                   if        cmdbdt <> *zeros
     C                   eval      srdti = cmdbdt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdbdt
     C                   else
     C                   clear                   xxdbdt
     C                   endif
    C*
    C*  Line 61A - Paid full days wages for DOI
    C*
     C                   eval      *in30 = *off
     C                   if        cmpoi <> 'Y'
     C                   eval      *in30 = *on
     C                   endif
    C*
    C*  Line 30A - Sex
     C                   select
     C                   when      cdsex = 'M'
     C                   eval      *in36 = *off
     C                   when      cdsex = 'F'
     C                   eval      *in36 = *on
     C                   endsl
    C*
    C*  Line 30B - Marital Status
     C                   select
     C                   when      cdmsts = 'M'
     C                   eval      *in38 = *off
     C                   other
     C                   eval      *in38 = *on
     C                   endsl
    C*
    C*  Line 30C - Date of Birth
    C*
     C                   if        cddtob <> *zeros
     C                   eval      srdti = cddtob
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdtob
     C                   else
     C                   clear                   xxdtob
     C                   endif
    C*
    C*  Line 33 -  Occupation
    C*
     C                   movel(p)  cmocds        xxocds
    C*
    C*  Line 70C - Date of Hire
    C*
     C                   if        cmhidt <> *zeros
     C                   eval      srdti = cmhidt
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxhidt
     C                   else
     C                   clear                   xxhidt
     C                   endif
    C*
    C*  Lines 11,12,16 2nd Pge - Specific injury/illness and part of body
    C*                           affected, medical diagnosis
    C*
     C                   clear                   xxnatr
     C                   clear                   xxpart
     C                   clear                   xxcaus
     C                   movel(p)  cmnatr        kkkey
     C     tbky          chain     pna01                              99
     C                   if        *in99 = *off
     C                   movel(p)  nadesc        xxnatr
     C                   endif
    C*
     C                   movel(p)  cmpart        kkkey
     C     tbky          chain     ppa01                              99
     C                   if        *in99 = *off
     C                   movel(p)  padesc        xxpart
     C                   endif
    C*
     C                   movel(p)  cmcaus        kkkey
     C     tbky          chain     pca01                              99
     C                   if        *in99 = *off
     C                   movel(p)  cadesc        xxcaus
     C                   endif
    C*
    C*  Line 39A - Initial Treatment checks
    C*
     C                   select
     C                   when      cmitrt = '0'
     C                   move      *on           *in84
     C                   when      cmitrt = '1'
     C                   move      *on           *in85
     C                   when      cmitrt = '2'
     C                   move      *on           *in86
     C                   when      cmitrt = '3'
     C                   move      *on           *in87
     C                   when      cmitrt = '4'
     C                   move      *on           *in88
     C                   when      cmitrt = '6'
     C                   move      *on           *in89
     C                   when      cmitrt = '7'
     C                   move      *on           *in91
     C                   endsl
    C*
    C*  Line 61A - Full Pay for Day of Injury
    C*
     C                   eval      *in29 = *off
     C                   if        cmpoi <> 'Y'
     C                   eval      *in29 = *on
     C                   endif
    C*
    C*  Line 44 - Name and address of Physician
    C*
     C                   clear                   xxphyn
     C                   clear                   xxtadr
     C                   clear                   xxtcty
     C                   clear                   xxtst
     C                   clear                   xxtzip
    C*
     C                   move      *off          *in98
     C     tpky          chain     ptp01                              99
     C                   if        *in99 = *off
     C                   movel     tpphyz        xxzipa
     C                   move      tpphyz        xxzipb
     C                   if        tpphyf <> *zeros
     C                             and tpphyn = *blanks
     C                   move      tpphyf        kkfein
     C                   move      tpphym        kkmod
    C*
     C     pdky          chain     ppd01                              98
     C                   if        *in98 = *off
     C                   movel(p)  pdname        xxphyn
     C                   movel(p)  pdadr1        xxtadr
     C                   if        pdadr2 > *blanks
     C     xxtadr        cat       pdadr2:1      xxtadr
     C                   endif
     C                   movel(p)  pdcty         xxtcty
     C                   move      pdst          xxtst
     C                   move      pdzip         xxpdzp
     C                   movel     xxpdzp        xxzipa
     C                   move      xxpdzp        xxzipb
     C                   movel(p)  xxzipa        xxtzip
     C                   if        xxzipb <> '0000'
     C                   cat       '-':0         xxtzip
     C                   cat       xxzipb:0      xxtzip
     C                   endif
     C                   endif
    C*
     C                   else
     C                   movel(p)  tpphyn        xxphyn
     C                   movel(p)  tpphya        xxtadr
     C                   movel(p)  tpphyc        xxtcty
     C                   move      tpphys        xxtst
     C                   movel(p)  xxzipa        xxtzip
     C                   if        xxzipb <> '0000'
     C                   cat       '-':0         xxtzip
     C                   cat       xxzipb:0      xxtzip
     C                   endif
     C                   endif
     C                   endif
    C*
    C*  Line 50 - Name & Address of Hospital
    C*
     C                   if        *in86 = *on
     C                             or *in88 = *on
    C*
B01  C                   if        cmhosf <> *zeros
    C*
     C                   eval      kkfein = cmhosf
     C                   eval      kkmod = cmhosm
     C     pdky          chain     ppd01                              99
B02  C                   if        *in99 = *off
     C                   movel     pdzip         xxzipa
     C                   move      pdzip         xxzipb
B03  C                   if        pddba1 = 'Y'
     C                   movel(p)  pdadr1        xxhosn
     C                   cat(p)    pdadr2:1      xxhadr
B04  C                   if        xxzipb <> '0000'
     C                   cat       '-':0         xxhzip
     C                   cat       xxzipb:0      xxhzip
E04  C                   endif
S03  C                   else
B05  C                   if        pdfsn <> *blanks
     C     pdfsn         cat(p)    pdname:1      xxhosn
     C                   else
     C                   movel(p)  pdname        xxhosn
E05  C                   endif
     C     pdadr1        cat(p)    pdadr2:2      xxhadr
     C                   movel(p)  pdcty         xxhcty
     C                   move(p)   pdst          xxhst
     C                   movel     xxzipa        xxhzip
B06  C                   if        xxzipb <> '0000'
     C                   cat       '-':0         xxhzip
     C                   cat       xxzipb:0      xxhzip
E06  C                   endif
E03  C                   endif
S02  C                   else
     C                   clear                   xxhosn
     C                   clear                   xxhadr
E02  C                   endif
    C*
S01  C                   else
    C*
     C                   movel     cmhosz        xxzipa
     C                   move      cmhosz        xxzipb
    C*
     C                   movel     cmhosn        xxhosn
     C                   if        cmhosa <> *blanks
     C     cmhosa        cat(p)    cmhosc:2      xxhadr
     C                   cat       ',':0         xxhadr
     C                   cat       cmhoss:1      xxhadr
     C                   cat       xxzipa:2      xxhadr
     C                   if        xxzipb <> '0000'
     C                   cat       '-':0         xxhadr
     C                   cat       xxzipb:0      xxhadr
     C                   endif
     C                   else
     C                   clear                   xxhadr
     C                   endif
     C                   endif
    C*
E01  C                   endif
    C*
    C*  Line 70B - Date returned to work
    C*
     C                   if        cmrtw <> *zeros
     C                   eval      srdti = cmrtw
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxrtw
     C                   else
     C                   clear                   xxrtw
     C                   endif
    C*
    C*  LINE 26 - Specific activity employee was performing when injured
    C*
    C* NI60A3, NI60A4, NI60A5, NI60A6
    C* First Line of Activity
     C                   movel(p)  ni60a1        xx72a1
     C                   movel(p)  ni60a2        xx60ax
     C                   move      xx12a         xx72a1
    C* Second Line of Activity
     C                   movel(p)  xx48b         xx72a2
    C*
    C*  Line 32  - How injury/illness ocurred
    C*
    C* NI80A1, NI80A2
    C*
    C* First Line of Injury/Illness
     C                   movel(p)  ni80a1        xx72b1
     C                   movel(p)  ni80a1        xx80ax
    C* Second Line of Injury/Illness
     C     xx08a         cat(p)    ni80a2:0      xx72b2
     C                   movel(p)  ni80a2        xx80ax
     C                   movel(p)  xx16a         xx72b3
    C*
    C*  Box 24 - Date of Death
    C*
     C                   clear                   xxdod
     C                   if        cmdod <> *zeros
     C                   eval      srdti = cmdod
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdod
     C                   endif
    C*
    C*  Line 70A - Date of employer's knowledge/notice of injury
    C*
     C                   if        cmdtrp <> *zeros
     C                   eval      srdti = cmdtrp
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxdtrp
     C                   else
     C                   clear                   xxdtrp
     C                   endif
    C*
    C*  Line 73, 75 1st Pge - Contact Information
     C*
     C                   if        nicont <> *blanks
     C*
     C                   eval      nicont = %trim(nicont)
     c                   eval      pos1 = 1
     c                   eval      pos2 = %scan(',':nicont:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      xxcfsn = %subst(nicont:pos1:length)
     c                   endif
     C*
     c                   eval      pos1 = pos2 + 1
     c                   eval      xxclsn = %subst(nicont:pos1:27)
     C*
     C                   else
     c                   clear                   xxcfsn
     c                   clear                   xxclsn
     c                   endif
    C*
     C                   eval      xxcara = nicar
     C                   eval      xxcphn = nicphn
    C*
    C*  Line 60, 63 2nd Pge - Witness Information
    C*
     C                   if        ni80a3 <> *blanks
     C*
     C                   eval      ni80a3 = %trim(ni80a3)
     c                   eval      pos1 = 1
     c                   eval      pos2 = %scan(',':ni80a3:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      xxwfsn = %subst(ni80a3:pos1:length)
     c                   endif
     C*
     c                   eval      pos1 = pos2 + 1
     c                   eval      pos2 = %scan(',':ni80a3:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      xxwlsn = %subst(ni80a3:pos1:length)
     c                   endif
     c*
     c                   eval      pos1 = pos2 + 1
     c                   eval      phone = %subst(ni80a3:pos1:10)
     C*** beg add *** 06/20/06 ********************************
     C                   testn                   phone                97
     C                   if        *in97 = *on
     C*** end add *** 06/20/06 ********************************
     c                   move      phone         xxphn#
     C*** beg add *** 06/20/06 ********************************
     C                   else
     c                   clear                   xxphn#
     C                   endif
     C*** end add *** 06/20/06 ********************************
     C*
     C                   else
     c                   clear                   xxwfsn
     c                   clear                   xxwlsn
     c                   clear                   xxphn#
     c                   endif
    C*
    C*  Line 66, 67 2nd Pge - Person Completing Form
    C*
     C                   if        niprnm <> *blanks
     C*
     C                   eval      niprnm = %trim(niprnm)
     c                   eval      pos1 = 1
     c                   eval      pos2 = %scan(',':niprnm:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      xxpnam = %subst(niprnm:pos1:length)
     c                   endif
     C*
     c                   eval      pos1 = pos2 + 1
     c                   eval      xxpttl = %subst(niprnm:pos1:19)
     C*
     C                   else
     c                   clear                   xxpnam
     c                   clear                   xxpttl
     c                   endif
     c*
     C                   move      niprph        xxpphn
    C*
     C                   movel(p)  ni20a4        xxbucd
    C*
    C*  Line 75 2nd Pge - Date Prepared
    C*
     C                   clear                   xxprdt
     C                   if        nidt1 <> *zeros
     C                   eval      srdti = nidt1
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxprdt
     C                   else
     C                   eval      xxprdt = dsdate
     C                   endif
    C*
    C*  Line 73 2nd pge - Carrier Bureau Code
    C*
     C                   movel(p)  ni20a4        xxcrbc
    C*
     C                   if        *in71 = *on
     C                   except    noi
     C                   else
     C                   except    noi2
     C                   endif
    C*================================================================
    C*
    C*  PRINT A LABEL FOR THE EMPLOYEE COPY ONLY.
    C*
     C                   if        *in72 = *on
     C*
     C                   movel(p)  cdlsn         xxcnam
     C     xxcnam        cat       ',':0         xxcnam
     C     xxcnam        cat       cdfsn:1       xxcnam
     C     xxcnam        cat(p)    cdmid:1       xxcnam
    C*
     C                   movel(p)  cdcty         xxcty
     C                   move      cdst          xxstat
     C                   eval      xxzip = cdzip
    C*
     C                   move      cdcty         xxcty
     C                   move      cdst          xxstat
     C                   eval      xxzip = cdzip
     C                   exsr      sraddr
    C*
     C                   clear                   xxccsz
     C                   movel     xxcsz         xxccsz
     C                   eval      *in42 = cdadr2 = *blanks
    C*
     C                   clear                   xxmc
     C                   if        cmmco = 'Y'
     C                   movel     'MC'          xxmc
     C                   endif
    C*
     C                   except    label
     C                   endif
    C*
    C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
    C*
     C                   if        *in73 = *on
     C                             and xxopn2 = *on
     C                   close     qsysprt2
     C                   move      *off          xxopn2
     C                   endif
    C*
    C*  WRITE THE DARTS REMARK, THEN PRINT THE NOI FORM.
    C*
     C                   if        *in75 = *off
     C                   if        *in73 = *off
     C                   exsr      darts
     C                   else
     C                   exsr      srdart
     C                   endif
     C                   endif
    C*
     C                   enddo
    C*
    C*  GET THE NEXT RECORD
    C*
     C                   select
     C                   when      *in71 = *on
     C     niky          reade     pni01                                  99
     C                   when      *in72 = *on
     C     niky          reade     pni02                                  99
     C                   when      *in73 = *on
     C     lgky          reade     plg01                                  99
     C                   when      *in75 = *on
     C                   eval      *in99 = *on
     C                   endsl
    C*
     C                   enddo
    C*
    C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
    C*
     C   71              if        xxopen = *on
     C                   close     qsysprt
     C                   move      *off          xxopen
     C                   endif
     C   72              if        xxopn1 = *on
     C                   close     qsysprt1
     C                   move      *off          xxopn1
     C                   endif
     C*** beg chg *** 06/01/06 ********************************
     C***72******        if        xxopn2 = *on
     C   73              if        xxopn2 = *on
     C*** end chg *** 06/01/06 ********************************
     C                   close     qsysprt2
     C                   move      *off          xxopn2
     C                   endif
     C   75              if        xxopn2 = *on
     C                   close     qsysprt2
     C                   move      *off          xxopn2
     C                   endif
    C*
    C*  PROCESS THE NEXT COPY
    C*
     C                   if        xxcopy = 1
     C                   eval      xxcopy = xxcopy + 1
     C                   else
     C                   leave
     C                   endif
    C*
     C                   enddo
    C*
    C*  END OF PROGRAM
    C*
     C                   eval      *inlr = *on
    C/EJECT
    C** =========================================================== **
    C* SRARRY Subroutine to Spread Character Fields to Print         *
    C** =========================================================== **
     C     srarry        begsr
    C*
     C                   eval      z = 1
     C                   eval      y = 1
     C                   clear                   ptchar
    C*
     C                   dow       y <= 75
     C                   move      cr1(z)        pr1(y)
     C                   eval      z = z + 1
     C                   eval      y = y + 2
     C                   enddo
    C*
     C                   endsr
    C** =========================================================== **
    C* SRINIT SUBROUTINE              ** Subroutine **   S R I N I T *
    C* THIS SUBROUTINE INITIALIZES ALL WORK FIELDS                   *
    C** =========================================================== **
     C     srinit        begsr
    C*
     C                   if        *in75 = *off
     C                   if        *in73 = *off
     C                   eval      kkfnd = nifnd
     C                   eval      kkfyr = nifyr
     C                   eval      kkcase = nicase
     C                   else
     C                   eval      kkfnd = lgfnd
     C                   eval      kkfyr = lgfyr
     C                   eval      kkcase = lgcase
     C                   endif
     C                   endif
    C*
     C                   endsr
    C** =========================================================== **
    C* SRADDR SUBROUTINE              ** Subroutine **   S R A D D R *
    C* THIS SUBROUTINE PUTS TOGETHER AN ADDRESS INTO ONE STRING      *
    C* CONTAINING THE CITY, STATE AND ZIP                            *
    C** =========================================================== **
     C     sraddr        begsr
    C*
     C                   clear                   sz
     C                   clear                   st
     C                   clear                   xxzp1
     C                   clear                   xxzp2
     C                   movel     xxstat        xxst
     C                   movea     xxst          sz
     C                   move      xxzpm1        xxzp1
     C                   movea     xxzp1         sz(4)
     C                   if        xxzpm2 <> 0
     C                   move      '-'           sz(9)
     C                   move      xxzpm2        xxzp2
     C                   movea     xxzp2         sz(10)
     C                   endif
    C*
     C                   clear                   xxsz
     C                   movea     sz            xxsz
     C                   clear                   xxlast
     C                   clear                   xxfrst
     C                   movel     xxcty         xxlast
     C                   movel     xxsz          xxfrst
     C                   move      '1'           xxterm
     C                   call      'SRNAME'
     C                   parm                    xxlast
     C                   parm                    xxfrst
     C                   parm                    xxterm
     C                   movel     xxlast        xxcsz
    C*
     C                   endsr
    C** =========================================================== **
    C* FNDBRK SUBROUTINE              ** Subroutine **   F N D B R K *
    C* THIS SUBROUTINE GETS THE FUND INFORMATION.                    *
    C** =========================================================== **
     C     fndbrk        begsr
    C*
    C*   GET REMARK CODE FOR DIVISION DIARY REMARKS.
    C*
     C     fnky          chain     pcx01                              99
     C                   clear                   xxrmcd
     C                   if        *in99 = *off
     C                   move      cxrsrd        xxrmcd
     C                   endif
    C*
    C*  GET THE FUND NAME AND ADDRESS
    C*
     C     fnky          chain     pfn01                              99
     C***** Beg Add 02/15/11 ******************************************
     C*   Move the comp mailing address to the carrier if it is a
     C*   Self insured flag is set 'Y' in Group setup.
     C                   if        fnsfin = 'Y'
     C     coky          chain     smcol                              70
     C                   movel(p)  coname        xxfnme
     C                   movel(p)  comad1        xxfad1
     C                   movel(p)  comzip        xxfnzp
     C                   if        comad2 <> *blanks
     C                   movel(p)  comad2        xxfad2
     C                   endif
     C                   movel(p)  comcty        xxfcty
     C                   move(p)   comst         xxfnst
     C                   else
     C***** End Add 02/15/11 ******************************************
    C*
     C                   movel(p)  fnname        xxfnme
    C*
     C                   movel(p)  fnmad1        xxfad1
     C                   movel(p)  fnmzip        xxfnzp
     C                   if        fnmad2 <> *blanks
     C                   movel(p)  fnmad2        xxfad2
     C                   endif
     C                   movel(p)  fnmcty        xxfcty
     C                   move(p)   fnmst         xxfnst
    C*
     C***** Beg Del 02/15/11 ******************************************
    C* Use Alternate Name & Address if exists
     C***                if        fnanam <> *blanks
     C***                movel(p)  fnanam        xxfnme
     C***                endif
     C***                if        fnaad1 <> *blanks
     C***                movel(p)  fnazip        xxfnzp
     C***                movel(p)  fnaad1        xxfad1
     C***                if        fnaad2 <> *blanks
     C***                movel(p)  fnaad2        xxfad2
     C***                endif
     C***                movel(p)  fnacty        xxfcty
     C***                move(p)   fnast         xxfnst
     C***                endif
     C***** End Del 02/15/11 ******************************************
     C***** Beg Add 02/15/11 ******************************************
     C                   endif
     C***** End Add 02/15/11 ******************************************
    C*
    C*  GET THE CARRIER CODE NUMBER FROM FUND AUXILIARY FILE
    C*
     C     fnky          chain     pf201                              99
    C*
     C                   if        *in99 = *off
     C                   movel(p)  f2fein        xxf2fn
     C                   movel(p)  f2sctc        xxsctc
     C                   else
     C                   clear                   xxsctc
     C                   clear                   xxf2fn
     C                   endif
    C*
     C                   endsr
    C** =========================================================== **
    C* DARTS SUBROUTINE               ** Subroutine **   D A R T S   *
    C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH NOI FORM       *
    C* PRINTED.                                                      *
    C** =========================================================== **
     C     darts         begsr
    C*
    C*  ESTABLISH KEY
     C                   clear                   cmkey
     C                   clear                   qckey
     C                   move      kkco#         qcco#
     C                   move      kkfnd         qcfnd
     C                   move      kkfyr         qcfyr
     C                   move      kkcase        qccase
    C*
    C*  DO NOT WRITE A DARTS RECORD IF THE REMARK CODE WAS NOT SETUP.
    C*
     C                   if        xxrmcd <> *blanks
    C*
     C                   clear                   pcm02
     C                   movel     qckey         cmkey
     C                   clear                   cmcomm
     C                   if        *in71 = *on
     C                   movel     cpy(1)        cmcomm
     C                   endif
     C                   if        *in72 = *on
     C                   movel     cpy(2)        cmcomm
     C                   endif
     C                   clear                   cmcsid
     *
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmcdat
     C                   clear                   cmcode
     C                   clear                   cmdcid
     C                   movel     'W0070R'      cmpnam
     *
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmtrdt
     *
     C                   eval      cmtrtm = dstime
     C                   clear                   cmsecl
     C                   move      'N'           cmsbyl
     C                   clear                   cmstat
     C                   movel     q1user        cmuser
     C                   movel     'W8070RFL'    cmpgm
     C                   eval      cmtime = dstime
     *
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmdt
     *
     C                   movel     q1user        cmiusr
     C                   clear                   cmfold
     C                   clear                   cmdcdt
     C                   clear                   cmdifl
     C                   move      'WC'          cmapid
     C                   clear                   cmidoc
     C                   clear                   cmique
     C                   clear                   cmimgf
     C                   clear                   cmipth
     C                   clear                   cmpidx
     C                   clear                   cmqpty
     C                   clear                   cmdctp
     C                   movel     'W8070RFL'    cmipgm
     C                   clear                   cmsmcd
     C                   clear                   cmfl01
     C                   clear                   cmfl02
     C                   clear                   cmfl03
     C                   clear                   cmfl04
     C                   clear                   cmfl05
     C                   move      xxrmcd        cmrmcd
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmidt
      ***** BEG ADD ***** 10/17/11 ***********************************
     c                   clear                   cmuncn
     c                   clear                   cmifsp
      ***** END ADD ***** 10/17/11 ***********************************
    C*
    C*  GET LAST SEQ# USED
    C*
     C     c3ky          setgt     pcm03
     C     c3ky          readpe    pcm03                                  99
     C                   if        *in99 = *off
     C                   eval      cmseq# = c3seq# + 1
     C                   else
     C                   eval      cmseq# = 1
     C                   endif
    C*
     C                   write     pcm02
     C                   feod      sdcmld
     C                   endif
    C*
     C                   endsr
    C** =========================================================== **
    C* SRDART SUBROUTINE              ** Subroutine **   S R D A R T *
    C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH REPORT WHICH   *
    C* IS PROCESSED.                                                 *
    C** =========================================================== **
     C     srdart        begsr
    C*
    C*  DON'T CREATE THE DARTS REMARK IF THE HARDCOPY DARTS CODE IS
    C*  BLANK OR INVALID.
    C*
     C                   move      '148'         kktset
     C*** BEG CHG ****** 03/24/10 *************************************
     C***  ixky          chain     pix01                              99
     c                   call      'WA076R'      ixapi                  60
     c                   if        @rtncd = 'Y'
     c                   move      '0'           *in99
     c                   else
     c                   move      '1'           *in99
     c                   endif
     c*** END CHG ****** 03/24/10 *************************************
     C                   if        *in99 = *off
    C*
    C*  ESTABLISH KEY
    C*
     C                   clear                   qckey
     C                   move      kkco#         qcco#
     C                   move      kkfnd         qcfnd
     C                   move      kkfyr         qcfyr
     C                   move      kkcase        qccase
    C*
    C*  ESTABLISH RECORD
    C*
     C                   clear                   pcm02
     C                   movel     qckey         cmkey
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmcdat
     C                   movel     ixhcpr        cmrmcd
    C*
     C     cmrmcd        chain     pct01                              99
     C                   if        *in99 = *off
    C*
     C                   move      ctsmcd        cmsmcd
     C                   movel     cpy(3)        cmcomm
    C*
    C*  GET FOLDER NAME
    C*
    C*
     C                   movel     kkfnd         ##fnd
     C                   movel(p)  'C'           ##fold
     C                   move      cmcase        ##fold
    C*
     C                   call      'SRFBLD'
     C                   parm                    ##fnd
     C                   parm      'CS'          ##ser
     C                   parm      cttype        ##sub
     C                   parm                    ##fold
     C                   parm      *blanks       ##fpth
     C                   parm      *blanks       ##term
    C*
     C                   movel(p)  ##fpth        cmfold
    C*
    C*  GET DOCUMENT ID
    C*
     C                   call      'SRDBLD'
     C                   parm                    ##fpth
     C                   parm      cttype        ##rt
     C                   parm      *blanks       ##doc
     C                   parm      *blanks       ##term
    C*
     C                   movel(p)  ##doc         cmdcid
      ***** BEG ADD ***** 10/17/11 ***********************************
     c                   if        cmfold <> ' ' or cmdcid <> ' '
     c                   eval      cmuncn = '/' + %trim(root) + '/' +
     c                                      %trim(cmfold) + '/' +
     c                                      %trim(cmdcid)
     c                   eval      cmifsp = env
     c                   else
     c                   eval      cmuncn = ' '
     c                   eval      cmifsp = ' '
     c                   endif
      ***** END ADD ***** 10/17/11 ***********************************
     C                   movel(p)  'W0070R'      cmpnam
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmtrdt
    C*
     C                   eval      cmtrtm = dstime
     C                   move      'N'           cmsbyl
     C                   movel     lgiusr        cmuser
     C                   movel     q1pgm         cmpgm
     C                   eval      cmtime = dstime
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmdt
    C*
     C                   movel     lgiusr        cmiusr
     C                   move      'WC'          cmapid
     C                   movel     q1pgm         cmipgm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         cmidt
    C*
    C*  GET LAST SEQ# USED
    C*
     C     c3ky          setgt     pcm03
     C     c3ky          readpe    pcm03                                  99
     C                   if        *in99 = *off
     C                   eval      cmseq# = c3seq# + 1
     C                   else
     C                   eval      cmseq# = 1
     C                   endif
    C*
     C                   write     pcm02
     C                   feod      sdcmld
    C*
    C*  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
    C*
     C     lgky1         chain     plg02                              99
     C                   if        *in99 = *off
     C                   eval      lgprsq = cmseq#
     C                   update    plg02
     C                   endif
    C*
    C*  CALL W8070C1FL TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
    C*
     C                   call      'W8070C1F'
     C                   parm                    q1job
     C                   parm                    q1user
     C                   parm                    q1jobn
     C                   parm      cmdcid        ##dcid
     C                   parm      cmfold        ##fldr
    C*
     C                   endif
     C                   endif
    C*
     C                   endsr
    C** =========================================================== **
    C/EJECT
     Oqsysprt1  e            label          1 11
     O                       xxcnam              50
     O          e            label          1
     O                       cdadr1              45
     O          e      n42   label          1
     O                       cdadr2              45
     O          e            label          1
     O                       xxccsz              45
     O                       xxmc                 2
    O* ============================================================ *
    O*
     Oqsysprt   e            noi              01
     *
     *  Claim number, Policy #, Date of Accident, Employee SS#
     O          e            noi            3  5
     C***  Begin Chg  ***  06/09/20  *******************************
     O*                      xxss#               76 '0 & & &&& & &&& & & & '
     O                       xxss#               76
     C***  End Chg  ***  06/09/20  *******************************
     O          e            noi         1
     O                       xxacdt              76 ' & &&& & &&& & & & '
     *  Employer First Name
     O          e            noi              13
     O                       xxdfsn              45
     *  Employer First Name
     O          e            noi         3
     O                       xxdlsn              77
     *  Employee Address
     O          e            noi         3
     O                       xxdadr              77
     *  Employee City State & Zip
     O          e            noi              23
     O                       xxdcty              40
     O                       xxdst               49
     O                       xxdzip              77 ' & & && & &&& & && & '
     *  Employee County and Phone Number
     O          e            noi         3
     O                       xxcnty              40
     O                       xxdara              49 ' & & '
     O                       xxdphn              70 ' & & &&&& & && & '
     *  Employee Gender, Marial Status & Date of Birth
     O          e            noi              29
     O              n36                           9 'X'
     O              n38                          15 'X'
     O          e            noi         1
     O               36                           9 'X'
     O               38                          15 'X'
     O                       cm#dep              25 ' & & '
     O                       xxdtob              54 ' & &&& & &&& & & & '
     *  Employee Occupation
     O          e            noi         3
     O                       xxocds              80
     *  Employee NCCI Class Code, Employment Status
     O          e            noi         3
     O          e            noi         1
     O                       xxclas              21 '0 & & & & '
     O                       xxests              33
     O          e            noi              41
     O                       xxenam              80
     O          e            noi         3
     O                       xxmadr              80
     *  Employer City, State, Zip - FEIN and NAIC Code
     O          e            noi              48
     O                       xxmcty              45
     O                       xxmst               52
     O                       emmzip              77 ' & & & & &&& && && & '
     O          e            noi              52
     O                       xxsic               11
     O                       emfein              38 ' & &&& & && & && & & '
     O                       xxeara              49 ' & & '
     O                       xxephn              70 ' & & &&&& & && & '
     O          e            noi              56
     O                       xxecny              40
     O                       xxnaic              52
     *  Employee Full pay day of injury, Time began work, Accident Time
     O          e            noi              60
     O              n30                           7 'X'
     O              n28                          33 'X'
     O              n27                          50 'X'
     O          e            noi         1
     O               30                           7 'X'
     O                       xxtim1              28 ' & && & '
     O               28                          33 'X'
     O                       xxactm              46 ' & && & '
     O               27                          50 'X'
     *
     *  Date last Work, Date Disability Began
     O          e            noi              65
     O                       xxdtlw              24 ' & &&& & &&& & & & '
     O                       xxdbdt              50 ' & &&& & &&& & && & '
    O*
     *  Date Employer Notified, Date Returned to Work, Hire Date
     O          e            noi              69
     O                       xxdtrp              24 ' & &&& & &&& & & & '
     O                       xxrtw               50 ' & &&& & &&& & && & '
     O                       xxhidt              75 ' & &&& & &&& & && & '
    O*
     *  Contact First Name, Contact Phone Number
     O          e            noi              73
     O                       xxcfsn              40
     O                       xxcara              49 ' & & '
     O                       xxcphn              70 ' & & &&&& & && & '
     O          e            noi              76
     O                       xxclsn              40
    O*
    O/EJECT
     ** =========================================================== **
     *  NOW CREATE 2ND or Back of First Page
     ** =========================================================== **
     *  Nature, Part and Cause Codes
     O          e            noi              08
     O                       cmnatr               7
     O                       cmpart              24
     O                       cmcaus              43
     O          e            noi              11
     O                       xxnatr              80
     O          e            noi         3
     O                       xxpart              80
     O          e            noi         3
     O                       xxcaus              80
     * Occur on Employer Premises? out of state? Safeguards provided? Used?
     O          e            noi              21
     O              n81                           7 'X'
     O              n82                          36 'X'
     O              n83                          51 'X'
     O          e            noi         1
     O                       cmast               23
     O          e            noi         1
     O               81                           7 'X'
     O               82                          36 'X'
     O               83                          51 'X'
     *
     * Equipment, Material or Chemicals employee was using at accident
     O          e            noi              26
     O                       xx72a1              77
     O          e            noi         1
     O                       xx72a2              77
     O          e            noi         1
     O                       xx72a3              77
     *
     * How injury occured - Description
     O          e            noi              32
     O                       xx72b1              77
     O          e            noi         1
     O                       xx72b2              77
     O          e            noi         1
     O                       xx72b3              77
     *
     * Injury Info - Date of Death, Initial Treatment flags
     O          e            noi              39
     O               84                          56 'X'
     O                       xxdod               25 ' & &&& & &&& & && & '
     O          e            noi         1
     O               85                          56 'X'
     O          e            noi         1
     O               86                          56 'X'
     O          e            noi         1
     O               89                          56 'X'
     * Injury Info - 19a-b  Physican and Hospital Info
     O          e            noi              44
     O                       xxphyn              48
     O               91                          56 'X'
     O          e            noi         1
     O                       xxtadr              48
     O               87                          56 'X'
     O          e            noi         1
     O                       xxtcty              29
     O                       xxtst               32
     O                       xxtzip              48
     O               88                          56 'X'
     O          e            noi              50
     O                       xxhosn              48
     O                       xxpfdt              76 ' & && & &&& & && & '
     O          e            noi         1
     O                       xxhadr              58
     O          e            noi         1
     O                       xxhcty              29
     O                       xxhst               32
     O                       xxhzip              48
     O          e            noi         2
     O                       xxptdt              76 ' & && & &&& & && & '
     * Line 60 2nd Pge - Witness Info - Name, phone number
     O          e            noi              60
     O                       xxwfsn              40
     O                       xxwara              49 ' & & '
     O                       xxwphn              70 ' & & &&&& & && & '
     O          e            noi         3
     O                       xxwlsn              40
     * Line 66 2nd Pge - Contact Name, phone#, Insurance Carrier Info
     O          e            noi              68
     O                       xxpnam              38
     O                       xxfnme              80
     O          e            noi         1
     O                       xxpttl              38
     O                       xxfad1              80
     O          e            noi         1
     O                       xxpphn              20 '   &   -    '
     O                       xxfad2              80
     O          e            noi         1
     O                       xxfcty              61
     O                       xxfnst              68
     O                       xxfnzp              77
     O          e            noi         1
     O                       xxbucd              53
     O                       xxf2fn              71 '0  -       '
     *  Line 75 2nd Pge - Date Prepared
     O          e            noi              75
     O                       xxprdt              25 ' & &&& & &&& & && & '
    O/EJECT
    O* ============================================================ *
     Oqsysprt2  e            noi2              1
     *
     *  Claim number, Policy #, Date of Accident, Employee SS#
     O          e            noi2           3  5
     C***  Begin Chg  ***  06/09/20  *******************************
     O*                      xxss#               76 '0 & & &&& & &&& & & & '
     O                       xxss#               76
     C***  End Chg  ***  06/09/20  *******************************
     O          e            noi2        1
     O                       xxacdt              76 ' & &&& & &&& & & & '
     *  Employer First Name
     O          e            noi2             13
     O                       xxdfsn              45
     *  Employer First Name
     O          e            noi2        3
     O                       xxdlsn              77
     *  Employee Address
     O          e            noi2        3
     O                       xxdadr              77
     *  Employee City State & Zip
     O          e            noi2             23
     O                       xxdcty              40
     O                       xxdst               49
     O                       xxdzip              77 ' & & && & &&& & && & '
     *  Employee County and Phone Number
     O          e            noi2        3
     O                       xxcnty              40
     O                       xxdara              49 ' & & '
     O                       xxdphn              70 ' & & &&&& & && & '
     *  Employee Gender, Marial Status & Date of Birth
     O          e            noi2             29
     O              n36                           9 'X'
     O              n38                          15 'X'
     O          e            noi2        1
     O               36                           9 'X'
     O               38                          15 'X'
     O                       cm#dep              25 ' & & '
     O                       xxdtob              54 ' & &&& & &&& & & & '
     *  Employee Occupation
     O          e            noi2        3
     O                       xxocds              80
     *  Employee NCCI Class Code, Employment Status
     O          e            noi2        3
     O          e            noi2        1
     O                       xxclas              21 '0 & & & & '
     O                       xxests              33
     O          e            noi2             41
     O                       xxenam              80
     O          e            noi2        3
     O                       xxmadr              80
     *  Employer City, State, Zip - FEIN and NAIC Code
     O          e            noi2             48
     O                       xxmcty              45
     O                       xxmst               52
     O                       emmzip              77 ' & & & & &&& && && & '
     O          e            noi2             52
     O                       xxsic               11
     O                       emfein              38 ' & &&& & && & && & & '
     O                       xxeara              49 ' & & '
     O                       xxephn              70 ' & & &&&& & && & '
     O          e            noi2             56
     O                       xxecny              40
     O                       xxnaic              52
     *  Employee Full pay day of injury, Time began work, Accident Time
     O          e            noi2             60
     O              n30                           7 'X'
     O              n28                          33 'X'
     O              n27                          50 'X'
     O          e            noi2        1
     O               30                           7 'X'
     O                       xxtim1              28 ' & && & '
     O               28                          33 'X'
     O                       xxactm              46 ' & && & '
     O               27                          50 'X'
     *
     *  Date last Work, Date Disability Began
     O          e            noi2             65
     O                       xxdtlw              24 ' & &&& & &&& & & & '
     O                       xxdbdt              50 ' & &&& & &&& & && & '
    O*
     *  Date Employer Notified, Date Returned to Work, Hire Date
     O          e            noi2             69
     O                       xxdtrp              24 ' & &&& & &&& & & & '
     O                       xxrtw               50 ' & &&& & &&& & && & '
     O                       xxhidt              75 ' & &&& & &&& & && & '
    O*
     *  Contact First Name, Contact Phone Number
     O          e            noi2             73
     O                       xxcfsn              40
     O                       xxcara              49 ' & & '
     O                       xxcphn              70 ' & & &&&& & && & '
     O          e            noi2             76
     O                       xxclsn              40
    O*
    O/EJECT
     ** =========================================================== **
     *  NOW CREATE 2ND or Back of First Page
     ** =========================================================== **
     *  Nature, Part and Cause Codes
     O          e            noi2             08
     O                       cmnatr               7
     O                       cmpart              24
     O                       cmcaus              43
     O          e            noi2             11
     O                       xxnatr              80
     O          e            noi2        3
     O                       xxpart              80
     O          e            noi2        3
     O                       xxcaus              80
     * Occur on Employer Premises? out of state? Safeguards provided? Used?
     O          e            noi2             21
     O              n81                           7 'X'
     O              n82                          36 'X'
     O              n83                          51 'X'
     O          e            noi2        1
     O                       cmast               23
     O          e            noi2        1
     O               81                           7 'X'
     O               82                          36 'X'
     O               83                          51 'X'
     *
     * Equipment, Material or Chemicals employee was using at accident
     O          e            noi2             26
     O                       xx72a1              77
     O          e            noi2        1
     O                       xx72a2              77
     O          e            noi2        1
     O                       xx72a3              77
     *
     * How injury occured - Description
     O          e            noi2             32
     O                       xx72b1              77
     O          e            noi2        1
     O                       xx72b2              77
     O          e            noi2        1
     O                       xx72b3              77
     *
     * Injury Info - Date of Death, Initial Treatment flags
     O          e            noi2             39
     O               84                          56 'X'
     O                       xxdod               25 ' & &&& & &&& & && & '
     O          e            noi2        1
     O               85                          56 'X'
     O          e            noi2        1
     O               86                          56 'X'
     O          e            noi2        1
     O               89                          56 'X'
     * Injury Info - 19a-b  Physican and Hospital Info
     O          e            noi2             44
     O                       xxphyn              48
     O               91                          56 'X'
     O          e            noi2        1
     O                       xxtadr              48
     O               87                          56 'X'
     O          e            noi2        1
     O                       xxtcty              29
     O                       xxtst               32
     O                       xxtzip              48
     O               88                          56 'X'
     O          e            noi2             50
     O                       xxhosn              48
     O                       xxpfdt              76 ' & && & &&& & && & '
     O          e            noi2        1
     O                       xxhadr              58
     O          e            noi2        1
     O                       xxhcty              29
     O                       xxhst               32
     O                       xxhzip              48
     O          e            noi2        2
     O                       xxptdt              76 ' & && & &&& & && & '
     * Line 60 2nd Pge - Witness Info - Name, phone number
     O          e            noi2             60
     O                       xxwfsn              40
     O                       xxwara              49 ' & & '
     O                       xxwphn              70 ' & & &&&& & && & '
     O          e            noi2        3
     O                       xxwlsn              40
     * Line 66 2nd Pge - Contact Name, phone#, Insurance Carrier Info
     O          e            noi2             68
     O                       xxpnam              38
     O                       xxfnme              80
     O          e            noi2        1
     O                       xxpttl              38
     O                       xxfad1              80
     O          e            noi2        1
     O                       xxpphn              20 '   &   -    '
     O                       xxfad2              80
     O          e            noi2        1
     O                       xxfcty              61
     O                       xxfnst              68
     O                       xxfnzp              77
     O          e            noi2        1
     O                       xxbucd              53
     O                       xxf2fn              71 '0  -       '
     *  Line 75 2nd Pge - Date Prepared
     O          e            noi2             75
     O                       xxprdt              25 ' & &&& & &&& & && & '
    O* ============================================================ *
**  COPY MSG
EMPLOYER COPY
EMPLOYEE COPY
DIVISION COPY
