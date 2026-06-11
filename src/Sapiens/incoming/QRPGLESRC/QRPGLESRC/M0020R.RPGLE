     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
     H/TITLE M0020R - Create Library List Dataarea - Prmpt
     H*********************************************************************
     H*                                                                   *
     H*********************************************************************
    H*
     Fm0020f    cf   e             workstn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
    F*
    F*  Physical file definition (for maintaining record)
    D*
     D*copy mPrMenu
     D/copy sPrDate
     D/copy sPrRunCmd
     D/copy sPrString
     D*
     D LoadDtaAra      pr                  extPgm('M0020C1')
     D  type                          4    const
     D  dtaAra                       10    const
     D  dtaAraInfo                  450
     D  rtnCde                        4
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
     D RtvSuffix       pr                  extPgm('RTVSUFFIX')
     D  suffix                        3
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
    D*
     D*entry
     DM0020R           pr                  extPgm('M0020R')
     D @@Mode                         1    const
     D @@PrDs                        40    const
     DM0020R           pi
     D @@Mode                         1    const
     D @@PrDs                        40    const
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D modeA           C                   '-- Add --'
     D modeC           C                   '- Change -'
     D modeD           C                   '-Display-'
     D optnm           s             40    inz('Maintain Environments')
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
     D afterSCR01      s              1n
     D afterSCR02      s              1n
     D addMode         s              1n
     D chgMode         s              1n
     D cpyMode         s              1n
     D dspMode         s              1n
     D command         s            255
     D dtaAra          s             10
     D dtaAraInfo      s            450
     D inqMode         s              1n
     D pdsClt          s             10
     D pdsFil          s             10
     D pdsMFil         s             10
     D pdsMObj         s             10
     D pdsMQte         s             10
     D pdsMth          s             10
     D pdsMTst         s             10
     D pdsObj          s             10
     D pdsQte          s             10
     D pdsTst          s             10
     D saveIn40        s                   like(*in40)
     D svApp           s                   like(scApp)  inz
     D srchRRN         s              9  0
     D suffix          s              3
     D x               s              3  0
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
     D  scFFl1
     D  scFFl2
     D  scFFl3
     D  scFFl4
     D  scFFl5
     D frFile                        10    dim(#ofOvrs) overlay(frFiles)
     D*
     D toFiles         ds
     D  scTFl1
     D  scTFl2
     D  scTFl3
     D  scTFl4
     D  scTFl5
     D toFile                        10    dim(#ofOvrs) overlay(toFiles)
     D*
     D toLibs          ds
     D  scTLb1
     D  scTLb2
     D  scTLb3
     D  scTLb4
     D  scTLb5
     D toLib                         10    dim(#ofOvrs) overlay(toLibs)
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    C*
     D psds          esds                  extname(szq1p)
    D*
     D pdsnam          c                   const('Stoneriver, Inc.')
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##data          s             80
     D ##leng          s              3  0
     D cpyflg          s              1
     D dsptyp          s              1
     D outflg          s              1
     D savedt          s              7  0
     D savetm          s              6  0
     D scuser          s             10
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     DbrandName        s             80    inz
     D*
     DRtvBrnd          pr                  extPgm('RTVBRND')
     D outType                        1    const
     D inLine1                       80
     D inLine2                       80    options(*nopass)
     D outCo#                         3    options(*nopass)
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D FL4             S             78    DIM(14)                              Function lines
     D FL5             S             78    DIM(14)                              Function lines
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##conm
    C*
      /Free
       RtvSuffix(suffix);
       pdsClt  = 'PDSCLT'  + suffix;
       pdsFil  = 'PDSFIL'  + suffix;
       pdsMFil = 'PDSMFIL' + suffix;
       pdsMObj = 'PDSMOBJ' + suffix;
       pdsMQte = 'PDSMQTE' + suffix;
       pdsMth  = 'PDSMTH'  + suffix;
       pdsMTst = 'PDSMTST' + suffix;
       pdsObj  = 'PDSOBJ'  + suffix;
       pdsQte  = 'PDSQTE'  + suffix;
       pdsTst  = 'PDSTST'  + suffix;
      /End-free
     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     C                   movel     q1user        scuser
     C                   move      *off          *inkl
     C                   move      *on           outflg
     C                   move      *off          dsptyp
     C                   exsr      getcof
     C                   exsr      getpgt
     C                   exsr      pgmstr
    C*
B01  C                   dou       *inkc = *on
     C                             or *inkl = *on
     C                             or outflg = *on
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
B02  C                   dou       *in90 = *off
     C                   exfmt     scr01
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B04  C                   if        *inkc = *off
     C                             and *inkl = *off
B04  C                   if        *in90 = *off
     C                   exsr      errck1
E04  C                   endif
B04  C                   if        *in90 = *off
B05  C                   if        #ofscr = 1
     C                   exsr      finish
X05  C                   else
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C                   cas                     otherr
     C                   endcs
     C                   exsr      xscr02
E05  C                   endif
E04  C                   endif
E03  C                   endif
E03  C                   endif
E02  C                   enddo
E01  C                   enddo
     C                   exsr      pgmend
    C*
     C                   return
    C*================================================================
    C*  FINISH - Final processing for file update/display
    C*================================================================
     C     finish        begsr
     C     @@mode        caseq     'A'           writer
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C     @@mode        caseq     'P'           writer
     C                   cas                     otherr
     C                   endcs
     C                   endsr
    C*================================================================
    C*  GETCOF - Gets company and fund names if possible; otherwise
    C*           get PDS name for company name
    C*================================================================
     C     getcof        begsr
     C                   call      comapi        compl                  99
B01  C                   if        *in99 = *on
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval      ##data = brandname
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        ##conm
E01  C                   endif
     C* * *              movel     ##conm        scconm
     C                   endsr
      /Free
      //==============================================================
      // Get date/time before display screen and saves
      //==============================================================
       Begsr GetDtTm;
        saveTm = TimeTo6(SystemTime);
        saveDt = DateTo7(SystemDate);
       Endsr;
      /End-free
    C*================================================================
    C*  GETPGT - Get program title for display heading
    C*================================================================
     C     getpgt        begsr
     C                   movel(p)  optnm         ##data
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      40            ##leng
     C                   movel     ##data        scprds
     C                   endsr
    C*================================================================
    C*  KEDO   - Process F5; refresh
    C*================================================================
     C     kedo          begsr
     C* * **             exsr      refrsh
     C                   move      *on           *in90
     C                   endsr
    C*================================================================
    C*  REFRS2 - Refresh screen values and date/time (on screen chg)
    C*================================================================
     C     refrs2        begsr
     C                   exsr      GetDtTm
     C                   endsr
    C*================================================================
    C*  XSCR02 - Display second screen and edit check
    C*================================================================
     C     xscr02        begsr
     C                   exsr      refrs2
     C                   Eval      AV_KEYS_DS.ARRAY = fl2
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
B01  C                   dou       *in90 = *off
     C                   Exsr      MvDbSc
     C                   exfmt     scr02
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C     *inke         caseq     *on           kedo
     C                   cas                     othfks
     C                   endcs
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
B03  C                   if        *in90 = *off
     C                             and @@mode <> 'D'
     C                   exsr      errck2
E03  C                   endif
B03  C                   if        *in90 = *off
B04  C                   if        #ofscr = 2
     C                   exsr      finish
X04  C                   else
     C     @@mode        caseq     'C'           updatr
     C     @@mode        caseq     'D'           dsplyr
     C                   cas                     otherr
     C                   endcs
     C                   exsr      xscr03
E04  C                   endif
E03  C                   endif
E02  C                   endif
E02  C                   endif
E01  C                   enddo
B01  C                   if        *inkl = *on
     C                   move      *on           *in90
     C                   exsr      refrs2
E01  C                   endif
     C                   endsr
    C*================================================================
    C*  XSCR03 - Display third screen and edit check
    C*================================================================
     C     xscr03        begsr
     C                   exsr      refrs2
     C                   Eval      AV_KEYS_DS.ARRAY = fl3
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
B01  C                   dou       *in90 = *off
     C                   exfmt     scr03
     C                   movea     '00000'       *in(71)
     C                   movea     '00000'       *in(76)
     C                   movea     '00000'       *in(81)
     C                   movea     '00000'       *in(86)
B02  C                   if        *inkc = *off
     C                             and *inkl = *off
     C     *inke         caseq     *on           kedo
     C                   cas                     othfks
     C                   endcs
     C                   if        *inkl = *on
     C                   eval      *in90 = *on
     C                   endif
B03  C                   if        *inkc = *off
     C                             and *inkl = *off
B03  C                   if        *in90 = *off
     C                             and @@mode <> 'D'
     C                   exsr      errck3
E03  C                   endif
B03  C                   if        *in90 = *off
     C                   exsr      finish
E03  C                   endif
E02  C                   endif
E02  C                   endif
E01  C                   enddo
B01  C                   if        *inkl = *on
     C                   move      *on           *in90
     C                   exsr      refrs2
E01  C                   endif
     C                   endsr
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
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
     C                   eval      *in90 = EditSCR01
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
     C*
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
     C     mvdbsc        begsr
     C*
     C                   if        afterSCR01
     C                   callp     LoadFrDtaAra(scDtaA:scApp)
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*====================================================================
     C     mvscdb        begsr
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
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PgmStr - Start of program processing
    C*====================================================================
     C     PgmStr        begsr
     C*
     C                   eval      afterSCR01 = *off
     C                   eval      afterSCR02 = *off
     C*
      /Free
       If @@prds <> *blanks;
          optnm = @@prds;
       Endif;
       dspMode = @@mode = 'D';
       chgMode = @@mode = 'C';
       addMode = @@mode = 'A';
       *in09 = dspMode;
       scMode = moded;
      /End-free
     C*
     C                   endsr
    C*
    C*====================================================================
    C* PreOth - Pre-processing for other modes
    C*====================================================================
     C     PreOth        begsr
     C*
     C                   if        afterSCR01
     C                   callp     LoadFrDtaAra(scDtaA:scApp)
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* UPDATR - Update data base record and after processing
    C*====================================================================
     C     updatr        begsr
    C*
     C                   exsr      MvScDb
    C*
     C                   endsr
    C*
    C*====================================================================
    C* WRITER - Write data base record and after processing
    C*====================================================================
     C     writer        begsr
    C*
     C                   exsr      mvscdb
    C*
     C                   endsr










    C*====================================================================
    C* CrtDesc - Create Description
    C*====================================================================
     PCrtDesc          b
     D CrtDesc         pi                  like(scDesc)

     D desc            s                   like(scDesc)

      /Free
       desc = 'CompSuite Interface';
       Return desc;
      /End-free

     PCrtDesc          e





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

      /Free
       Select;
       When scApp = 'LIV';
          metl = 'LIVE';
       When scApp = 'MTH';
          metl = 'MTHEND';
       When scApp = 'QTE';
          metl = 'QUOTE';
       When scApp = 'TST';
          metl = 'TEST';
       Other;
          metl = scApp;
       Endsl;

       metl = %trim(metl) + '/' + cltId;

       Return metl;
      /End-free

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

     D error           s              1
     D errorInd        s              1n

      /Free
       If scApp = *blank;
          *in71 = *on;
          Return *on;
       Endif;

       scDtaA = 'M' + scApp + 'XA';
       CheckObj(scDtaA:'*DTAARA':errorInd);
       Select;
       When dspMode and errorInd;
          *in72 = *on;
          Return *on;
       Endsl;

       Return *off;
      /End-free

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
     C* * *              if        scMetl = *blanks
     C* * *              eval      *in82 = *on
     C* * *              return    *on
     C* * *              endif
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

     D libPrefix       s              4
     D rtnCode         s              4
     D start           s              3  0
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
     C                   callp     LoadDtaAra('*IN':inDtaAra:dtaAraInfo:rtnCode)
     C*
      /Free
       Select;
       When rtnCode = '*CRT';
          Clear scDesc;
          Clear scMetl;
          Clear lib;
          lib(1)         = 'QTEMP';
          lib(3)         = pdsClt;
          lib(5)         = pdsMFil;
          lib(7)         = pdsMObj;
          lib(9)         = pdsFil;
          lib(11)        = pdsObj;
          lib(#ofLibs-2) = 'PDSTOOLS';
          lib(#ofLibs-1) = 'QGPL';
          lib(#ofLibs)   = 'QSYS38';
          Select;
          When scApp = 'QTE';
             lib(4)         = pdsMQte;
             lib(8)         = pdsQte;
          When scApp = 'MTH';
             lib(4)         = pdsMth;
          When scApp = 'TST';
             lib(4)         = pdsMTst;
             lib(8)         = pdsTst;
          Endsl;
          addMode = *on;
          scMode = modea;
       Other;
          Exsr LoadData;
          If not dspMode;
             chgMode = *on;
             scMode = modec;
          Endif;
       Endsl;

       If scDesc = *blanks;
          scDesc = CrtDesc;
       Endif;
       If scMetl = *blanks;
          scMetl = CrtMetl;
       Endif;

      /End-free
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

     D rtnCode         s              4
     D start           s              3  0
     D x               s              3  0

      /Free
       SortLibs();

       For x = 1 to #ofLibs;
          start = (x * 10) - 9;
          %subst(dtaAraInfo:start:10) = lib(x);
       Endfor;

       For x = 1 to #ofOvrs;
          start = (x * 10) - 9 + firstOvr;
          %subst(dtaAraInfo:start   :10) = frFile(x);
          %subst(dtaAraInfo:start+10:10) = toFile(x);
          %subst(dtaAraInfo:start+20:10) = toLib(x);
       Endfor;

       %subst(dtaAraInfo:401:40) = scDesc;
       %subst(dtaAraInfo:441:10) = scMetl;


       LoadDtaAra('*OUT':inDtaAra:dtaAraInfo:rtnCode);

      /End-free
     PLoadToDtaAra     e





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
