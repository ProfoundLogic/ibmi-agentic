     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
     H/TITLE M0021R - Work with Environment Library List
     H*********************************************************************
     H* Called from:                                                      *
     H*********************************************************************
     H*********************************************************************
     H* Revisions:                                                        *
     H* 12/07/16 C:079 Project SY376                                      *
     H*                Load data in inquiry mode                          *
     H*********************************************************************
    H*
     Fm0021f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fmmllp     uf a e             disk    rename(pll01:dbrcdf)
     Fmmlll1    if   e           k disk
     Fpmclp     if   e             disk    rename(pcl01:pcl0p) usropn
     Fpmcll     if   e           k disk    rename(pcl01:pcl00) usropn
     Fstgtp     if   e             disk    rename(pgt01:pgt0p)
     Fstgtl     if   e           k disk    rename(pgt01:pgt00)
    D*
     D/copy mPrMenu
     D/copy sPrDate
     D/copy sPrRunCmd
     D/copy sPrString
    D*
     D CheckObj        pr                  extPgm('SRCHKOBJ')
     D  obj                          10    const
     D  type                         10    const
     D  error                         1
    D*
     D CheckObjwLib    pr                  extPgm('SRCHKOB2')
     D  lib                          10    const
     D  obj                          10    const
     D  type                          7    const
     D  error                         1
    D*
     D CltEnv          pr             1n
     D  inClt#                             like(scClt#)
    D*
     D CrtDesc         pr                  like(scDesc)
    D*
     D CrtLibrary      pr
    D*
     D CrtMetl         pr                  like(scMetl)
    D*
     D DeleteDtaAra    pr
     D  inDtaAra                     10    const
    D*
     D EditSCR01       pr             1n
     D EditSCR02       pr             1n
     D EditSCR03       pr             1n
    D*
     D LoadFrDtaAra    pr
     D  inDtaAra                     10    const
     D  inApp                         3    const
    D*
     D LoadToDtaAra    pr
     D  inDtaAra                     10    const
     D  inApp                         3    const
    D*
     D MaintFlds       pr
     D  inType                        1    options(*nopass) const
    D*
     D SortLibs        pr
     D*
     D SrchClt#        pr                  extPgm('P0005R')
     D  call                          1    const
     D  kc                            1n
     D  kl                            1n
     D  rrn                           9  0
     D*
     D SrchRls         pr                  extPgm('S2380R')
     D  call                          1    const
     D  kc                            1n
     D  kl                            1n
     D  rrn                           9  0
     D  tableID                       4    const
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D MODEA           C                   '-- Add --'
     D MODEC           C                   '- Change -'
     D MODED           C                   '-Display-'
     D optnm           c                   'Environment Library Lists'
     D comapi          c                   'SA005R'
     D #ofScr          c                   2
     D #ofLibs         c                   25
     D #ofOvrs         c                   5
     D firstOvr        c                   250
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
     D afterSCR01      s              1n
     D afterSCR02      s              1n
     D addMode         s              1n
     D chgMode         s              1n
     D cpyMode         s              1n
     D command         s            255
     D dtaAra          s             10
     D dtaAraInfo      s            450
     D inqMode         s              1n
     D kkKey           s                   like(GTKEY)
     D kkTbID          s                   like(GTTBID)
     D saveIn40        s                   like(*in40)
     D svApp           s                   like(scApp)  inz
     D svClt#          s                   like(scClt#) inz
     D svEnv           s                   like(scEnv)  inz
     D svRls           s                   like(scRls)  inz
     D srchRRN         s              9  0
     D x               s              3  0
     D xxClt#          s                   like(LLCLT#) inz('BASE')
     D pEnv            s                   like(LLENV)  inz('P')
     D qEnv            s                   like(LLENV)  inz('Q')
    D*---------------------------------------------------------------------
     D PSDS          ESDS                  ExtName(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D seq#s           ds
     D  scSeq1
     D  scSeq2
     D  scSeq3
     D  scSeq4
     D  scSeq5
     D  scSeq6
     D  scSeq7
     D  scSeq8
     D  scSeq9
     D  scSeq10
     D  scSeq11
     D  scSeq12
     D  scSeq13
     D  scSeq14
     D  scSeq15
     D  scSeq16
     D  scSeq17
     D  scSeq18
     D  scSeq19
     D  scSeq20
     D  scSeq21
     D  scSeq22
     D  scSeq23
     D  scSeq24
     D  scSeq25
     D seq#                           3  0 dim(#ofLibs) overlay(seq#s) inz
     D*
     D libs            ds
     D  scLib1
     D  scLib2
     D  scLib3
     D  scLib4
     D  scLib5
     D  scLib6
     D  scLib7
     D  scLib8
     D  scLib9
     D  scLib10
     D  scLib11
     D  scLib12
     D  scLib13
     D  scLib14
     D  scLib15
     D  scLib16
     D  scLib17
     D  scLib18
     D  scLib19
     D  scLib20
     D  scLib21
     D  scLib22
     D  scLib23
     D  scLib24
     D  scLib25
     D lib                           10    dim(#ofLibs) overlay(libs)
     D*
     D frFiles         ds
     D  scFrFil1
     D  scFrFil2
     D  scFrFil3
     D  scFrFil4
     D  scFrFil5
     D frFile                        10    dim(#ofOvrs) overlay(frFiles)
     D*
     D toFiles         ds
     D  scToFil1
     D  scToFil2
     D  scToFil3
     D  scToFil4
     D  scToFil5
     D toFile                        10    dim(#ofOvrs) overlay(toFiles)
     D*
     D toLibs          ds
     D  scToLib1
     D  scToLib2
     D  scToLib3
     D  scToLib4
     D  scToLib5
     D toLib                         10    dim(#ofOvrs) overlay(toLibs)
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    C*
     C/copy sCopyMT41c
    C*
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
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
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
     C     keyMMLLL1p    klist
     C                   kfld                    scRls
     C                   kfld                    xxClt#
     C                   kfld                    scApp
     C                   kfld                    pEnv
     C*
     C     keyMMLLL1q    klist
     C                   kfld                    scRls
     C                   kfld                    xxClt#
     C                   kfld                    scApp
     C                   kfld                    qEnv
     C*
     C     keySTGTL      klist
     C                   kfld                    ##co#
     C                   kfld                    kkTbID
     C                   kfld                    kkKey
    C*====================================================================
    C* ChgSts - Change Status
    C*====================================================================
     C     ChgSts        begsr
     C*
     C     @@rrn         chain     dbRcdF
     C*
     C                   if        %found
     C*
     C                   select
     C                   when      LLSTAT = 'I'
     C                   eval      LLSTAT = 'A'
     C                   other
     C                   eval      LLSTAT = 'I'
     C                   endsl
     C*
     C                   callp     MaintFlds
     C*
     C                   update    dbRcdF
     C*
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*====================================================================
     C     dltpre        begsr
     C*
     C                   callp     DeleteDtaAra(LLDTAA)
     C*
     C                   endsr
    C*
    C*====================================================================
    C* DSPLYR - Processing after displaying information
    C*====================================================================
     C     dsplyr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
     C     errck1        begsr
     C*
     C                   if        addMode or cpyMode
     C                   eval      *in90 = EditSCR01
     C                   endif
     C*
     C                   if        not *in90
     C                   eval      afterSCR01 = *on
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
     C     errck2        begsr
     C*
     C                   eval      saveIn40 = *in40
     C                   eval      *in40 = *off
     C                   eval      *in90 = EditSCR02
     C*
     C                   if        not *in90
     C                   eval      *in40 = saveIn40
     C                   eval      afterSCR02 = *on
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
     C     errck3        begsr
     C*
     C                   eval      *in90 = EditSCR03
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KFDO   - Processing for F6
    C*====================================================================
     C     KFDO          begsr
     C*
     C                   select
     C                   when      %subst(hFld:1:5) = 'SCLIB'
     C                   callp     CrtLibrary
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
     C     KJDO          begsr
     C*
     C                   select
     C                   when      hFld = 'SCCLT#' and addMode
     C                   exsr      SearchClt#
     C                   when      hFld = 'SCRLS'
     C                   exsr      SearchRls
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
     C     mvdbsc        begsr
     C*
     C                   eval      *in40 = addMode
     C*
     C                   select
     C                   when      cpyMode
     C                   eval      scApp  = LLAPP
     C                   eval      scClt# = *blanks
     C                   eval      scEnv  = LLENV
     C                   eval      scRls  = LLRLS
     C                   when      chgMode
     C                   eval      scApp  = LLAPP
     C                   eval      scClt# = LLCLT#
     C                   eval      scEnv  = LLENV
     C                   eval      scRls  = LLRLS
     C*** Beg Add *** 12/07/16 ****************************************
     C                   when      inqMode
     C                   eval      scApp  = LLAPP
     C                   eval      scClt# = LLCLT#
     C                   eval      scEnv  = LLENV
     C                   eval      scRls  = LLRLS
     C*** End Add *** 12/07/16 ****************************************
     C                   other
     C                   eval      scApp  = svApp
     C                   eval      scClt# = svClt#
     C                   eval      scEnv  = svEnv
     C                   eval      scRls  = svRls
     C                   endsl
     C*
     C                   select
     C                   when      addMode or cpyMode
     C                   eval      scDtaA = CrtDtaAraNm(scClt#:
     C                                                  scApp:
     C                                                  scEnv:
     C                                                  scRls
     C                                                 )
     C                   other
     C                   eval      scDtaA = LLDTAA
     C                   endsl
     C*
     C*** Beg Chg *** 12/07/16 ****************************************
     C*****************  if        afterSCR01
     C                   if        afterSCR01 or inqMode
     C*** End Chg *** 12/07/16 ****************************************
     C                   select
     C                   when      inqMode or chgMode
     C                   callp     LoadFrDtaAra(scDtaA:scApp)
     C                   when      cpyMode
     C                   callp     LoadFrDtaAra(LLDTAA:scApp)
     C                   endsl
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*====================================================================
     C     mvscdb        begsr
     C*
     C                   eval      LLAPP  = scApp
     C                   eval      LLCLT# = scClt#
     C                   eval      LLENV  = scEnv
     C                   eval      LLRLS  = scRls
     C                   eval      LLDTAA = scDtaA
     C*
     C                   if        afterSCR02
     C                   callp     LoadToDtaAra(scDtaA:scApp)
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHERR - Processing for other modes
    C*====================================================================
     C     otherr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* OthFks - Processing for other functions keys
    C*====================================================================
     C     OthFks        begsr
    C*
     C     *inkf         caseq     *on           kfdo                     90
     C     *inkj         caseq     *on           kjdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PgmEnd - End of program processing
    C*====================================================================
     C     PgmEnd        begsr
    C*
     C                   If        %open(PMCLP)
     C                   close     pmclp
     C                   close     pmcll
     C                   Endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C                   eval      *in40   = @@mode = 'A'
     C                   eval      *in41   = @@mode = 'P'
     C                   eval      addMode = @@mode = 'A'
     C                   eval      chgMode = @@mode = 'C'
     C                   eval      inqMode = @@mode = 'D'
     C                   eval      cpyMode = @@mode = 'P'
     C*
     C                   eval      afterSCR01 = *off
     C                   eval      afterSCR02 = *off
     C*
     C                   eval      command = 'ADDLIBLE LIB(CLT099) ' +
     C                                       'POSITION(*LAST)'
     C                   callp(e)  RunCmd(command:%len(command))
     C                   If        not %open(PMCLP)
     C                   open      pmclp
     C                   open      pmcll
     C                   Endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PreOth - Pre-processing for other modes
    C*====================================================================
     C     PreOth        begsr
     C*
     C                   select
     C                   when      addMode or cpyMode
     C                   eval      scDtaA = CrtDtaAraNm(scClt#:
     C                                                  scApp:
     C                                                  scEnv:
     C                                                  scRls
     C                                                 )
     C                   endsl
     C*
     C*** Beg Chg *** 12/07/16 ****************************************
     C*****************  if        afterSCR01
     C                   if        afterSCR01 or inqMode
     C*** End Chg *** 12/07/16 ****************************************
     C                   callp     LoadFrDtaAra(scDtaA:scApp)
     C                   endif
     C*
     C                   select
     C                   when      @@mode = 'S'
     C                   exsr      ChgSts
     C                   return
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* SearchClt# - Search for client #
    C*====================================================================
     C     SearchClt#    begsr
     C*
     C                   callp     SrchClt#('2':*inKC:*inKL:srchRRN)
     C*
     C     srchRRN       chain     PMCLP
     C*
     C                   select
     C                   when      not %found or *inKL
     C                   eval      *in81 = *on
     C                   when      %found
     C                   eval      scClt# = %editc(CLCLT#:'X')
     C                   eval      *in82 = *on
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* SearchRls - Search for release
    C*====================================================================
     C     SearchRls     begsr
     C*
     C                   callp     SrchRls('2':*inKC:*inKL:srchRRN:'CDDT')
     C*
     C     srchRRN       chain     stgtp
     C*
     C                   select
     C                   when      not %found or *inKL
     C                   eval      *in84 = *on
     C                   when      %found
     C                   eval      scRls  = GTKEY
     C                   eval      *in81 = *on
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* UPDATR - Update data base record and after processing
    C*====================================================================
     C     updatr        begsr
    C*
     C     @@rrn         chain     dbrcdf                             99
    C*
B01  C                   if        %found
     C                   exsr      MvScDb
     C                   callp     MaintFlds
     C                   update    dbrcdf
E01  C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* WRITER - Write data base record and after processing
    C*====================================================================
     C     writer        begsr
    C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   callp     MaintFlds('I')
     C                   write     dbrcdf
     C                   feod      mmllp
     C*
     C** **              eval      outFlg = *on
     C*
     C                   eval      svApp  = scApp
     C                   eval      svClt# = *blanks
     C                   eval      svEnv  = scEnv
     C                   eval      svRls  = scRls
    C*
     C                   endsr





    C*====================================================================
    C* CltEnv - Client Environment
    C*====================================================================
     PCltEnv           b
     D CltEnv          pi             1n
     D  inClt#                             like(scClt#)

     D sgl             s              1
     D x               s              1  0
     D y               s              1  0

     C                   eval      x = %len(inClt#)
     C                   do        x             y
     C                   eval      sgl = %subst(inClt#:y:1)
     C                   testn                   sgl                  61
     C                   if        *in61 and sgl <> ' '
     C                   return    *off
     C                   endif
     C                   enddo

     C                   return    *on

     PCltEnv           e





    C*====================================================================
    C* CrtDesc - Create Description
    C*====================================================================
     PCrtDesc          b
     D CrtDesc         pi                  like(scDesc)

     D desc            s                   like(scDesc)

     C                   select
     C                   when      scClt# = 'BASE'
     C                   eval      desc = 'Base Application - ' + scRls
     C                   when      CltEnv(scClt#)
     C                   eval      desc = 'Client ' + %trim(scClt#) +
     C                              ' Application - ' + scRls
     C                   other
     C                   eval      desc = %trim(scClt#) + ' - ' + scRls
     C                   endsl
     C*
     C                   select
     C                   when      scEnv = 'D'
     C                   eval      desc = %trim(desc) + ' - DEV'
     C                   when      scEnv = 'I'
     C                   eval      desc = %trim(desc) + ' - INT'
     C                   when      scEnv = 'P'
     C                   eval      desc = %trim(desc) + ' - PDN'
     C                   when      scEnv = 'Q'
     C                   eval      desc = %trim(desc) + ' - QA'
     C                   endsl

     C                   return    desc

     PCrtDesc          e





    C*====================================================================
    C* CrtDtaAraNm - Create the DTAARA name from the prompts
    C*====================================================================
     PCrtDtaAraNm      b
     D CrtDtaAraNm     pi            10
     D  inClt#                        4    const
     D  inApp                         3    const
     D  inEnv                         1    const
     D  inRls                         4    const

     D dtaAraNm        s             10

     C                   eval      dtaAraNm = inEnv +
     C                                        %subst(inApp:1:1) +
     C                                        %subst(inClt#:1:3) +
     C                                        inRls

     C                   return    dtaAraNm

     PCrtDtaAraNm      e





    C*====================================================================
    C* CrtLibrary - Create library
    C*====================================================================
     PCrtLibrary       b
     D CrtLibrary      pi

     D dtaAraNm        s             10

     C                   eval      command = 'CRTLIB LIB('
     C*
     C                   select
     C                   when      hfld = 'SCLIB1'
     C                   eval      command = %trim(command) + scLib1
     C                   when      hfld = 'SCLIB2'
     C                   eval      command = %trim(command) + scLib2
     C                   when      hfld = 'SCLIB3'
     C                   eval      command = %trim(command) + scLib3
     C                   when      hfld = 'SCLIB4'
     C                   eval      command = %trim(command) + scLib4
     C                   when      hfld = 'SCLIB5'
     C                   eval      command = %trim(command) + scLib5
     C                   when      hfld = 'SCLIB6'
     C                   eval      command = %trim(command) + scLib6
     C                   when      hfld = 'SCLIB7'
     C                   eval      command = %trim(command) + scLib7
     C                   when      hfld = 'SCLIB8'
     C                   eval      command = %trim(command) + scLib8
     C                   when      hfld = 'SCLIB9'
     C                   eval      command = %trim(command) + scLib9
     C                   when      hfld = 'SCLIB10'
     C                   eval      command = %trim(command) + scLib10
     C                   when      hfld = 'SCLIB11'
     C                   eval      command = %trim(command) + scLib11
     C                   when      hfld = 'SCLIB12'
     C                   eval      command = %trim(command) + scLib12
     C                   when      hfld = 'SCLIB13'
     C                   eval      command = %trim(command) + scLib13
     C                   when      hfld = 'SCLIB14'
     C                   eval      command = %trim(command) + scLib14
     C                   when      hfld = 'SCLIB15'
     C                   eval      command = %trim(command) + scLib15
     C                   when      hfld = 'SCLIB16'
     C                   eval      command = %trim(command) + scLib16
     C                   when      hfld = 'SCLIB17'
     C                   eval      command = %trim(command) + scLib17
     C                   when      hfld = 'SCLIB18'
     C                   eval      command = %trim(command) + scLib18
     C                   when      hfld = 'SCLIB19'
     C                   eval      command = %trim(command) + scLib19
     C                   when      hfld = 'SCLIB20'
     C                   eval      command = %trim(command) + scLib20
     C                   when      hfld = 'SCLIB21'
     C                   eval      command = %trim(command) + scLib21
     C                   when      hfld = 'SCLIB22'
     C                   eval      command = %trim(command) + scLib22
     C                   when      hfld = 'SCLIB23'
     C                   eval      command = %trim(command) + scLib23
     C                   when      hfld = 'SCLIB24'
     C                   eval      command = %trim(command) + scLib24
     C                   when      hfld = 'SCLIB25'
     C                   eval      command = %trim(command) + scLib25
     C                   endsl
     C*
     C                   eval      command = %trim(command) + ')'
     C*
     C                   callp(e)  RunCmd(command:%len(command))

     C                   return

     PCrtLibrary       e





    C*====================================================================
    C* CrtMetl - Menu Environment Name
    C*====================================================================
     PCrtMetl          b
     D CrtMetl         pi                  like(scMetl)

     D metl            s                   like(scMetl)

     C                   select
     C                   when      scClt# = 'BASE'
     C                   eval      metl = scRls
     C                   when      CltEnv(scClt#)
     C                   eval      metl = %trim(scClt#) + '/' + scRls
     C                   other
     C                   eval      metl = %trim(scRls) + '/' +
     C                                    %trim(scClt#) + 'D'
     C                   endsl
     C*
     C                   if        %len(%trim(metl)) < 9
     C                   select
     C                   when      scEnv = 'D'
     C                   eval      metl = %trim(metl) + '/DEV'
     C                   when      scEnv = 'I'
     C                   eval      metl = %trim(metl) + '/INT'
     C                   when      scEnv = 'P'
     C                   eval      metl = %trim(metl) + '/PDN'
     C                   when      scEnv = 'Q'
     C                   eval      metl = %trim(metl) + '/QA'
     C                   endsl
     C                   endif

     C                   return    metl

     PCrtMetl          e





    C*====================================================================
    C* DeleteDtaAra - Delete the DTAARA
    C*====================================================================
     PDeleteDtaAra     b
     D DeleteDtaAra    pi
     D  inDtaAra                     10    const

     D rtnCde          s              4

     C                   callp     LoadDtaAra('*DLT':inDtaAra:dtaAraInfo:rtnCde)

     PDeleteDtaAra     e





    C*====================================================================
    C* EditSCR01 - Edit SCR01 information
    C*====================================================================
     PEditSCR01        b
     D EditSCR01       pi             1n

     C                   if        scEnv <> 'P' and
     C                             scEnv <> 'Q' and
     C                             scEnv <> 'D'
     C                   eval      *in71 = *on
     C                   return    *on
     C                   endif
     C*
     C                   eval      kkTbID = 'CDDT'
     C                   eval      kkKey  = scRls
     C     keySTGTL      chain     pgt00
     C                   if        not %found
     C                   eval      *in72 = *on
     C                   return    *on
     C                   endif
     C*
     C                   if        scClt# <> 'BASE'
     C                   select
     C                   when      CltEnv(scClt#)
     C     keyMMLLL1p    chain     pll01
     C                   if        not %found
     C                   eval      *in73 = *on
     C                   return    *on
     C                   endif
     C                   other
     C                   if        scEnv <> 'D'
     C                   eval      *in74 = *on
     C                   return    *on
     C                   endif
     C     keyMMLLL1q    chain     pll01
     C                   if        not %found
     C                   eval      *in75 = *on
     C                   return    *on
     C                   endif
     C                   endsl
     C                   endif
     C*
     C                   return    *off

     PEditSCR01        e





    C*====================================================================
    C* EditSCR02 - Edit SCR02 information
    C*====================================================================
     PEditSCR02        b
     D EditSCR02       pi             1n

     D error           s              1
     D errorInd        s              1n

     C                   if        scDesc = *blanks
     C                   eval      *in81 = *on
     C                   return    *on
     C                   endif
     C*
     C                   if        scMetl = *blanks
     C                   eval      *in82 = *on
     C                   return    *on
     C                   endif
     C*
     C                   if        addMode
     C*
     C                   if        scDtaA = *blanks
     C                   eval      *in83 = *on
     C                   return    *on
     C                   endif
     C*
     C                   callp     CheckObj(scDtaA:'*DTAARA':errorInd)
     C                   if        not errorInd
     C                   eval      *in84 = *on
     C                   return    *on
     C                   endif
     C*
     C                   endif
     C*
     C* Validate LIBL libraries
     C                   do        #ofLibs       x
     C                   if        lib(x) <> *blanks
     C                   callp     CheckObj(lib(x):'*LIB':*in71)
     C                   if        *in71
     C                   eval      *in(x+30) = *on
     C                   return    *on
     C                   endif
     C                   endif
     C                   enddo
     C*
     C                   do        #ofOvrs       x
     C* Validate FROM files
     C                   if        frFile(x) <> *blanks
     C                   callp     CheckObj(frFile(x):'*FILE':*in72)
     C                   if        *in72
     C                   eval      *in(x+30) = *on
     C                   return    *on
     C                   endif
     C* Validate TO files w/Library
     C                   callp     CheckObjwLib(toLib(x):toFile(x):
     C                                          '*FILE':error)
     C                   select
     C                   when      error = '1'
     C                   eval      *in74 = *on
     C                   eval      *in(x+30) = *on
     C                   return    *on
     C                   when      error = '2'
     C                   eval      *in73 = *on
     C                   eval      *in(x+30) = *on
     C                   return    *on
     C                   endsl
     C                   else
     C                   eval      toFile(x) = *blanks
     C                   eval      toLib(x) = *blanks
     C                   endif
     C                   enddo
     C*
     C                   return    *off

     PEditSCR02        e





    C*====================================================================
    C* EditSCR03 - Edit SCR03 information
    C*====================================================================
     PEditSCR03        b
     D EditSCR03       pi             1n

     C                   return    *off

     PEditSCR03        e





    C*====================================================================
    C* LoadFrDtaAra - Load SCR02 from DTAARA
    C*====================================================================
     PLoadFrDtaAra     b
     D LoadFrDtaAra    pi
     D  inDtaAra                     10    const
     D  inApp                         3    const

     D dtaAra          s                   like(inDtaAra)
     D libPrefix       s              4
     D rtnCode         s              4
     D start           s              3  0
     D xxEnv           s                   like(scEnv)
     D x               s              3  0
     D y               s              3  0

     C                   do        #ofLibs       y
     C                   eval      seq#(y) = y * 10
     C                   enddo
     C*
     C                   if        inDtaAra = *blanks
     C                   return
     C                   endif
     C*
     C                   select
     C                   when      inApp = 'QTE'
     C                   eval      dtaAra = %trim(inDtaAra) + 'Q'
     C                   when      inApp = 'LIV'
     C                   eval      dtaARa = inDtaAra
     C                   endsl
     C*
     C                   callp     LoadDtaAra('*IN':dtaAra:dtaAraInfo:rtnCode)
     C*
     C                   select
     C                   when      rtnCode = '*CRT'
     C                   select
     C                   when      scClt# = 'BASE'
     C                   eval      lib(1)         = 'QTEMP'
     C                   eval      lib(#ofLibs-4) = 'ACMSLIB'
     C                   eval      lib(#ofLibs-3) = 'PDSTOOLS'
     C                   eval      lib(#ofLibs-2) = 'QGPLFISERV'
     C                   eval      lib(#ofLibs-1) = 'QGPL'
     C                   eval      lib(#ofLibs)   = 'CLT099'
     C                   exsr      BaseLibs
     C                   when      not CltEnv(scClt#)
     C                   callp     LoadDtaAra('*IN':CrtDtaAraNm('BASE':
     C                                                          scApp:
     C                                                          'Q':
     C                                                          scRls
     C                                                         ):
     C                                        dtaAraInfo:
     C                                        rtnCode
     C                                       )
     C                   exsr      LoadData
     C                   clear                   scDesc
     C                   clear                   scMetl
     C                   exsr      DvpLibs
     C                   other
     C                   callp     LoadDtaAra('*IN':CrtDtaAraNm('BASE':
     C                                                          scApp:
     C                                                          'P':
     C                                                          scRls
     C                                                         ):
     C                                        dtaAraInfo:
     C                                        rtnCode
     C                                       )
     C                   exsr      LoadData
     C                   clear                   scDesc
     C                   exsr      CltLibs
     C                   endsl
     C                   other
     C                   exsr      LoadData
     C                   endsl
     C*
     C                   if        scDesc = *blanks
     C                   eval      scDesc = CrtDesc
     C                   endif
     C*
     C                   if        scMetl = *blanks
     C                   eval      scMetl = CrtMetl
     C                   endif
     C*
    C*====================================================================
     C     BaseLibs      begsr
     C*
     C                   eval      kkTbID = 'CDDT'
     C                   eval      kkKey  = scRls
     C     keySTGTL      chain     pgt00
     C*
     C                   eval      x = 1
     C                   eval      xxEnv = scEnv
     C*
     C                   eval      libPrefix = %trim(GTCF01)
     C*
     C                   if        xxEnv = 'Q'
     C                   if        inApp = 'QTE'
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'QADTAQ'
     C                   endif
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'QADTA'
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'QA'
     C                   eval      xxEnv = 'P'
     C                   endif
     C*
     C                   if        xxEnv = 'P'
     C                   if        inApp = 'QTE'
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'PNDTAQ'
     C                   endif
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'PNDTA'
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'PNOBJ'
     C                   endif
     C*
     C                   if        GTCF01 <> 'BASE'
     C                   eval      libPrefix = 'BASE'
     C                   if        inApp = 'QTE'
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'PNDTAQ'
     C                   endif
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'PNDTA'
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'PNOBJ'
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
     C     CltLibs       begsr
     C*
     C                   eval      kkTbID = 'CDDT'
     C                   eval      kkKey  = scRls
     C     keySTGTL      chain     pgt00
     C*
     C                   eval      x = 1
     C     'QTEMP     '  lookup    lib(x)                                 99
     C                   if        %found
     C                   eval      seq#(x) = 1
     C                   endif
     C*
     C                   eval      x = 1
     C     *blanks       lookup    lib(x)                                 99
     C                   eval      xxEnv = scEnv
     C*
     C                   eval      libPrefix = 'C' + %trim(scClt#)
     C*
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + '.DATA'
     C                   eval      seq#(x) = x
     C*
     C                   if        xxEnv = 'D'
     C                              and (x + 1) <= #ofLibs
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'DVP'
     C                   eval      seq#(x) = x
     C                   eval      xxEnv = 'Q'
     C                   endif
     C*
     C                   if        xxEnv = 'Q'
     C                              and (x + 1) <= #ofLibs
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'QA'
     C                   eval      seq#(x) = x
     C                   eval      xxEnv = 'P'
     C                   endif
     C*
     C                   if        xxEnv = 'P'
     C                              and (x + 2) <= #ofLibs
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'PNDTA'
     C                   eval      seq#(x) = x
     C                   eval      x = x + 1
     C                   eval      lib(x) = libPrefix + 'PNOBJ'
     C                   eval      seq#(x) = x
     C                   endif
     C*
     C                   callp     SortLibs
     C*
     C                   do        #ofLibs       y
     C                   eval      seq#(y) = y * 10
     C                   enddo
     C*
     C                   endsr
     C*
    C*====================================================================
     C     DvpLibs       begsr
     C*
     C                   eval      kkTbID = 'CDDT'
     C                   eval      kkKey  = scRls
     C     keySTGTL      chain     pgt00
     C*
     C                   eval      x = 1
     C     'QTEMP     '  lookup    lib(x)                                 99
     C                   if        %found
     C                   eval      seq#(x) = 1
     C                   endif
     C*
     C                   eval      x = 1
     C     *blanks       lookup    lib(x)                                 99
     C                   eval      xxEnv = scEnv
     C*
     C                   eval      x = x + 1
     C                   eval      lib(x) = %trim(scClt#) + 'D'
     C                   eval      seq#(x) = x
     C*
     C                   callp     SortLibs
     C*
     C                   do        #ofLibs       y
     C                   eval      seq#(y) = y * 10
     C                   enddo
     C*
     C                   endsr
     C*
    C*====================================================================
     C     LoadData      begsr
     C*
     C                   do        #ofLibs       y
     C                   eval      start = (y * 10) - 9
     C                   eval      lib(x + y) = %subst(dtaAraInfo:start:10)
     C                   enddo
     C*
     C                   do        #ofOvrs       y
     C                   eval      start = (y * 10) - 9 + firstOvr
     C                   eval      frFile(y) = %subst(dtaAraInfo:start   :10)
     C                   eval      toFile(y) = %subst(dtaAraInfo:start+10:10)
     C                   eval      toLib(y) = %subst(dtaAraInfo:start+20:10)
     C                   enddo
     C*
     C                   eval      scDesc = %subst(dtaAraInfo:401:40)
     C                   eval      scMetl = %subst(dtaAraInfo:441:10)
     C*
     C                   endsr

     PLoadFrDtaAra     e





    C*====================================================================
    C* LoadToDtaAra - Load DTAARA from SCR02
    C*====================================================================
     PLoadToDtaAra     b
     D LoadToDtaAra    pi
     D  inDtaAra                     10    const
     D  inApp                         3    const

     D dtaAra          s                   like(inDtaAra)
     D rtnCode         s              4
     D start           s              3  0
     D x               s              3  0

     C                   callp     SortLibs
     C*
     C                   do        #ofLibs       x
     C                   eval      start = (x * 10) - 9
     C                   eval      %subst(dtaAraInfo:start:10) = lib(x)
     C                   enddo
     C*
     C                   do        #ofOvrs       x
     C                   eval      start = (x * 10) - 9 + firstOvr
     C                   eval      %subst(dtaAraInfo:start   :10) = frFile(x)
     C                   eval      %subst(dtaAraInfo:start+10:10) = toFile(x)
     C                   eval      %subst(dtaAraInfo:start+20:10) = toLib(x)
     C                   enddo
     C*
     C                   eval      %subst(dtaAraInfo:401:40) = scDesc
     C                   eval      %subst(dtaAraInfo:441:10) = scMetl
     C*
     C                   select
     C                   when      inApp = 'QTE'
     C                   eval      dtaAra = %trim(inDtaAra) + 'Q'
     C                   when      inApp = 'LIV'
     C                   eval      dtaARa = inDtaAra
     C                   endsl
     C*
     C                   callp     LoadDtaAra('*OUT':dtaAra:dtaAraInfo:rtnCode)

     PLoadToDtaAra     e





    C*====================================================================
    C* MaintFlds - Load maintenance fields
    C*====================================================================
     PMaintFlds        b
     D MaintFlds       pi
     D  inType                        1    options(*nopass) const

     D type            s                   like(inType) inz

     C                   if        %parms >= 1
     C                   eval      type = inType
     C                   endif
     C*
     C                   eval      LLUSER = q1User
     C                   eval      LLPGM  = q1Pgm
     C                   eval      LLDT   = DateTo7(SystemDate)
     C                   eval      LLTIME = TimeTo6(SystemTime)
     C*
     C                   if        type = 'I'
     C                   eval      LLSTAT = 'A'
     C                   eval      LLIUSR = LLUSER
     C                   eval      LLIPGM = LLPGM
     C                   eval      LLIDT  = LLDT
     C                   eval      LLITIM = LLTIME
     C                   endif

     PMaintFlds        e





    C*====================================================================
    C* SortLibs - Sort the library arrays
    C*====================================================================
     PSortLibs         b
     D SortLibs        pi

     D change          s              1n
     D holdSeq#        s                   like(scSeq1)
     D holdLib         s                   like(scLib1)
     D x               s              3  0
     D y               s              3  0
     D z               s              3  0

     C                   dou       not change
     C                   eval      change = *off
     C                   do        #ofLibs       x
     C                   if        x < #ofLibs and
     C                              seq#(x) > seq#(x+1)
     C                   eval      holdSeq# = seq#(x)
     C                   eval      holdLib = lib(x)
     C                   eval      seq#(x) = seq#(x+1)
     C                   eval      lib(x)  = lib(x+1)
     C                   eval      seq#(x+1) = holdSeq#
     C                   eval      lib(x+1)  = holdLib
     C                   eval      change = *on
     C                   endif
     C                   enddo
     C                   enddo

     C                   do        #ofLibs       x
     C                   if        lib(x) = *blanks
     C                   eval      z = x
     C     x             do        #ofLibs       y
     C                   if        lib(y) <> *blanks
     C                   eval      lib(z) = lib(y)
     C                   eval      lib(y) = *blanks
     C                   eval      z = z + 1
     C                   endif
     C                   enddo
     C                   endif
     C                   enddo

     PSortLibs         e
