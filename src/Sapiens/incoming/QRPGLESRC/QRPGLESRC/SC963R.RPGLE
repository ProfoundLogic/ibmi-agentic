      *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2003
      *========================================================================
    H/TITLE SC963R - Print all remareks Image print request
     H*========================================================================
     H* Description: This program prints the image documents attached to dart
     H*              remarks for a given case.
     H*
     H*=====================================================================
     H* Created on 06/30/2003 by c:62 for Project 129WAO
     H*========================================================================
    H* Revisions:
    H*
    H* 10/19/17 c:62 Support 50443(45)
    H*               Extract the folder
    H*
    H* 12/01/16 c:62 Support 44459(45)
    H*               Extract the document id from the unc path instead of CMdcid.
    H*
    H* 08/01/14 c:62 Support IM045 86020
    H*               Expand document parm to handle SMMDP/MDOID (40A)
    H*
    H* 06/22/07 c:02 project FIX b4 base tape goes out
    H*               -Change call to PrintDoc (sc964c) to use the actual
    H*                CMFL05 OPTICAL FLAG instead of cmDCTP (not sure why
      *                this was being used). SO optical IMAGES will work with
      *                this process
    H*
    H* 03/14/05 c:62 project 129IJS
    H*               -Add process to print remark comment.  This will
    H*                make sure that the comments are printed only once.
      *               -Change the query selection of remark code and
      *                summary code to an OR
    H*
    H* 01/31/05 c:62 project DR129 45568
    H*               Process Additional Documents if attached to an image
    H*
     H*========================================================================
     Fsdcml     if   e           k disk
     Fwmcml     if   e           k disk    rename(pcm01:pcl01)
     F                                     prefix(cl)
     Fwmcml1    if   e           k disk    rename(pcm01:pcl02)
     F                                     prefix(c2)
     Fimpsl3    uf a e           k disk    rename(pps01:pps02)
     Fimpsl4    uf a e           k disk    rename(pps01:pps03)
     Fsmmdl1    if   e           k disk
     *=========================================================================
      * Prototypes
     *=========================================================================
      /copy SC963RPR

     *=========================================================================
      * Global Variables
     *=========================================================================
      * External program information structure
     D psds          esds                  extname(szq1p)
     D                 ds
     D subfolder                     12
     D  dfdat                         6    Overlay(subfolder:1)
     D  dtdat                         6    Overlay(subfolder:7)
     D                 ds
     D claim#                        16
     D  xxco#                         3  0 Overlay(claim#:1)
     D  xxfnd                         3  0 Overlay(claim#:4)
     D  xxfyr                         3  0 Overlay(claim#:7)
     D  xxcase                        7  0 Overlay(claim#:10)

     D                 ds
     DRmkArr                          5    dim(42)
     Dxxrm01                   1      5
     Dxxrm02                   6     10
     Dxxrm03                  11     15
     Dxxrm04                  16     20
     Dxxrm05                  21     25
     Dxxrm06                  26     30
     Dxxrm07                  31     35
     Dxxrm08                  36     40
     Dxxrm09                  41     45
     Dxxrm10                  46     50
     Dxxrm11                  51     55
     Dxxrm12                  56     60
     Dxxrm13                  61     65
     Dxxrm14                  66     70
     Dxxrm15                  71     75
     Dxxrm16                  76     80
     Dxxrm17                  81     85
     Dxxrm18                  86     90
     Dxxrm19                  91     95
     Dxxrm20                  96    100
     Dxxrm21                 101    105
     Dxxrm22                 106    110
     Dxxrm23                 111    115
     Dxxrm24                 116    120
     Dxxrm25                 121    125
     Dxxrm26                 126    130
     Dxxrm27                 131    135
     Dxxrm28                 136    140
     Dxxrm29                 141    145
     Dxxrm30                 146    150
     Dxxrm31                 151    155
     Dxxrm32                 156    160
     Dxxrm33                 161    165
     Dxxrm34                 166    170
     Dxxrm35                 171    175
     Dxxrm36                 176    180
     Dxxrm37                 181    185
     Dxxrm38                 186    190
     Dxxrm39                 191    195
     Dxxrm40                 196    200
     Dxxrm41                 201    205
     Dxxrm42                 206    210

     D                 ds
     DSumArr                          4    dim(14)
     Dxxsc01                   1      4
     Dxxsc02                   5      8
     Dxxsc03                   9     12
     Dxxsc04                  13     16
     Dxxsc05                  17     20
     Dxxsc06                  21     24
     Dxxsc07                  25     28
     Dxxsc08                  29     32
     Dxxsc09                  33     36
     Dxxsc10                  37     40
     Dxxsc11                  41     44
     Dxxsc12                  45     48
     Dxxsc13                  49     52
     Dxxsc14                  53     56

     D eKey            s             40a
     D eYear           s              3a
     D eCase           s              7a
     D efdat           s              7a
     D etdat           s              7a
     D eotyp           s              1a

     D folder          s             16
     D pSeq#           s              5a
     D rtnCode         s              1a
     D rtnCode6        s              6a
     D eCompany        s              3a
     D eGroup          s              3a
     D eEmployer       s              9a
     D eOutq           s             10a
     D esmcds          s             56a
     D ermcds          s            210a
     D outq            s             10a
     D pPolYear        s              3a
     D pmcGroup        s              3a
     D pSubs           s              5a
     D pCase           s              7a
     D pType           s              1a
     D pVerb           s             10a   inz('*COVER    ')
     D mcKey           s             40a
     D pQueue          s             10a   inz(*blanks)
     D pMode           s              1a   inz('O')
     D pPrintFlag      s              1a   inz('N')

     D company         s              3  0
     D group           s              3  0
     D employer        s              9  0
     D division        s              5  0
     D polYear         s              3  0
     D subs            s              5  0
     D case            s              7  0
     D returnCode      s              1a
     D wcin            s              1a
     D wdck            s              1a
     D line1           s             78a
     D line2           s             78a
     D line3           s             78a
     D*** Begin Chg *** 08/01/14 ************************
     D*document*       s             12a
     D document        s            100a
     D*** End   Chg *** 08/01/14 ************************
     D*** Begin Add *** 10/19/17 ******************************************
     D extFolder       s                   Like(CMfold)
     D hldCMuncn       s                   Like(CMuncn)
     D slashCnt        s              5  0
     D lastSlash       s              5  0
     D*** End   Add *** 10/19/17 ******************************************
     D ppath           s            256a
     D hpath           s            256a
     D dseq#           s              5a
     D*** Begin Add *** 12/01/16 ********************************************
     D slshLoc         s              3s 0
     D*** End   Add *** 12/01/16 ********************************************
     D*** begin chg *** 03/14/05 *******************************************
     D*pdocsuf**       s             11a
     D                 ds
     D pdocsuf                       24a
     D  seq#                          5a   overlay(pdocsuf:1)
     D  type                          2a   overlay(pdocsuf:6)
     D  remarkCode                    5a   overlay(pdocsuf:9)
     D  remarkMonth                   2a   overlay(pdocsuf:15)
     D  remarkDash1                   1a   overlay(pdocsuf:17)
     D  remarkDay                     2a   overlay(pdocsuf:18)
     D  remarkDash2                   1a   overlay(pdocsuf:20)
     D  remarkYear                    4a   overlay(pdocsuf:21)
     D  remarkDate                   10a   overlay(pdocsuf:15)
     D*** end   chg *** 03/14/05 *******************************************
     D*** begin add *** 03/14/05 *******************************************
     D datcvt                          d   datfmt(*usa)
     D*** end   add *** 03/14/05 *******************************************
     D y               s              3s 0
     D z               s              3s 0
     D cfdat           s              7s 0
     D ctdat           s              7s 0

      *** Local Variables ***
     D firstDoc        s              1a   inz('1')
     D nextFlag        s              1a   inz('1')
     D svpsid          s              2a
     D imagprt         s              1a

     D image           c                    const('***IMAGE***')
     D rootFolder      c                   const('/CLAIMS OUTPUT')
    
     *=========================================================================
      * KList Definitions
     *=========================================================================
      * DARTS Remark file key
     C     cmKey1        klist
     C                   kfld                    mckey

      * Managed Care Case file key
     C     cmkey2        klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    employer

     C     cmkey3        klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    polYear
     C                   kfld                    case

     C     cmkey4        klist
     C                   kfld                    company
     C                   kfld                    group

     C     mdKey1        klist
     C                   kfld                    cmkey
     C                   kfld                    cmseq#

     C     pskey2        klist
     C                   kfld                    nextFlag

     C     pskey3        klist
     C                   kfld                    epsid


     *=========================================================================
      * PLIST Definitions
     *=========================================================================
     C     *entry        plist
     C                   parm                    eCompany
     C                   parm                    eGroup
     C                   parm                    eEmployer
     C                   parm                    eYear
     C                   parm                    eCase
     C                   parm                    ePsid             2
     C                   parm                    efdat
     C                   parm                    etdat
     C                   parm                    eotyp
     C                   parm                    esmcds
     C                   parm                    ermcds

     *=========================================================================
      * Main
     *=========================================================================

     C                   move      eGroup        pmcGroup
     C                   move      eCompany      company
     C                   move      pmcGroup      group
     C                   move      eEmployer     employer
     C                   move      eYear         polYear
     C                   move      eCase         case
     C                   movea     esmcds        sumarr
     C                   movea     ermcds        rmkarr
     C                   move      efdat         cfdat
     C                   move      etdat         ctdat

      * call employer or claim subroutine based on entry parms
     C                   select
     C                   when      employer <> 0
     C                   exsr      procEmp
     C                   when      polYear <> 0
     C                   exsr      procClm
     C                   other
     C                   exsr      procGrp
     C                   endsl


     C                   move      *on           *inlr

      *=========================================================================
      * DocOne - If this is the first image or other document produce cover
      *=========================================================================
     C     DocOne        begsr

     C                   eval      firstDoc = ' '

     C*
     C* If a specific print server id was passed in, use it instead of
     C* load balancing
     C                   if        ePsid <> *blank
     C                   eval       svpsid = ePsid
     C     pskey3        chain     pps03                              60
     C                   else
     C*
      * get the next dtaq
     C     pskey2        chain     pps02                              60
     C                   if        NOT *in60
     C                   eval      svpsid = pspsid
     C                   eval      psfl01 = ' '
     C                   update    pps02

     C                   else
     C                   eval      nextFlag = ' '
     C     pskey2        chain     pps02                              60
     C                   if        NOT *in60
     C                   eval      svpsid = pspsid
     C                   update    pps02
     C                   endif
     C                   endif
     C                   endif

      * write the request to the dataq for a cover page
     C                   eval      rtnCode = *blanks
     C                   eval      line1 = *blanks
     C                   eval      line2 = *blanks
     C                   eval      line3 = *blanks
     C                   callp     InfoAPI(rtnCode:eCompany:pmcGroup:
     C                                     pPolYear:pCase:line1:line2:line3)

      * print the cover page for the documents
     C                   callp     PrintCover(pspque:pspqlb:pVerb:
     C                                        line1:line2:line3:eotyp)

      * write entries to dataq with the docids
     C                   callp     DocidList(mckey:pspque:pspqlb:efdat:
     C                                        etdat:eotyp:esmcds:ermcds)

     C                   endsr
     c*** Begin Add *** 12/01/16 **********************************
      *=========================================================================
      * GetDocID - Extract Document ID from UNC field.
      *=========================================================================
     C     GetDocID      begsr

      /free

       //*** Begin Add *** 10/19/17 ****************************************
       hldCMuncn = CMuncn;
       //*** End   Add *** 10/19/17 ****************************************
       // Lets find slashes
       Dow CMuncn <> *Blanks;
          slshLoc = %Scan('/':CMuncn);
          If slshLoc <> 0;
             CMuncn = %Subst(CMuncn:slshLoc+1);
          Else;
             document = CMuncn;
             //*** Begin Add *** 10/19/17 **********************************
             CMuncn = hldCMuncn;
             //*** End   Add *** 10/19/17 **********************************
             Leave;
          EndIf;
       EndDo;

      /end-free

     C                   endsr
     c*** End   Add *** 12/01/16 **********************************
     c*** Begin Add *** 10/19/17 *******************************************
      *=========================================================================
      * GetFolder - Extract Folder from UNC field.
      *=========================================================================
     C     GetFolder     begsr

      /free

       hldCMuncn = CMuncn;
       extFolder = ' ';
       slashCnt  = 0;
       lastSlash = 0;
       // Lets find slashes
       Dow CMuncn <> *Blanks;
          slshLoc = %Scan('/':CMuncn:lastSlash+1);
          If slshLoc <> 0;
             slashCnt = slashCnt + 1;
             Select;
             When slashCnt <= 2;
                CMuncn = %Subst(CMuncn:slshLoc+1);
             Other;
                lastSlash = slshLoc;
             EndSl;
          Else;
             extFolder = %Subst(CMuncn:1:lastSlash-1);
             CMuncn    = hldCMuncn;
             Leave;
          EndIf;
       EndDo;

      /end-free

     C                   endsr
     c*** End   Add *** 10/19/17 *******************************************
      *=========================================================================
      * procEmp - processes for an employer passed in
      *=========================================================================
     C     procEmp       begsr

      * get each claim for the employer
     C     cmkey2        chain     pcl01                              60
      *
     C                   dow       NOT *in60
     C                   eval      polYear = clcmfyr
     C                   eval      division = clcmdiv
     C                   eval      case = clcmcase
     C                   move      clcmcase      pCase
     C                   move      clcmfyr       pPolYear
      *
      * call api to get key
     C                   eval      returnCode = 'K'
     C                   eval      mcKey = *blanks
     C                   callp     CaseKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       pPolYear:pCase:mcKey)

     C                   eval      xxco# = clcmco#
     C                   eval      xxfnd = clcmfnd
     C                   eval      xxfyr = clcmfyr
     C                   eval      xxcase = clcmcase
      *
      * process all darts remarks for that key
     C                   exsr      ProcDarts
      *
     C     cmkey2        reade     pcl01                                  60
     C                   enddo

     C                   endsr
      *=========================================================================
      * procClm - processes for a claim passed in
      *=========================================================================
     C     procClm       begsr

      * get each claim for the employer
     C     cmkey3        chain     pcl02                              60
      *
     C                   dow       NOT *in60
     C                   move      c2cmcase      pCase
     C                   move      c2cmfyr       pPolYear
      *
      * call api to get key
      *
     C                   eval      returnCode = 'K'
     C                   eval      mcKey = *blanks
     C                   callp     CaseKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       pPolYear:pCase:mcKey)

     C                   eval      xxco# = c2cmco#
     C                   eval      xxfnd = c2cmfnd
     C                   eval      xxfyr = c2cmfyr
     C                   eval      xxcase = c2cmcase
      *
      * process all darts remarks for that key
      *
     C                   exsr      ProcDarts
      *
     C     cmkey3        reade     pcl02                                  60
     C                   enddo

     C                   endsr
      *=========================================================================
      * procGrp - processes for all Claims for this group
      *=========================================================================
     C     procGrp       begsr
      *
      * get each claim for the employer
      *
     C     cmkey4        chain     pcl02                              60
      *
     C                   dow       NOT *in60
     C                   move      c2cmcase      pCase
     C                   move      c2cmfyr       pPolYear
      *
      * call api to get key
      *
     C                   eval      returnCode = 'K'
     C                   eval      mcKey = *blanks
     C                   callp     CaseKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       pPolYear:pCase:mcKey)

     C                   eval      xxco# = c2cmco#
     C                   eval      xxfnd = c2cmfnd
     C                   eval      xxfyr = c2cmfyr
     C                   eval      xxcase = c2cmcase
      *
      * process all darts remarks for that key
      *
     C                   exsr      ProcDarts
      *
     C     cmkey4        reade     pcl02                                  60
     C                   enddo

     C                   endsr
      *=========================================================================
      * ProcDarts - Process all the darts records for image documents
      *=========================================================================
     C     ProcDarts     begsr
     C*
     C* Created Path Statement
     C*
     C                   movel(p)  claim#        folder
     C                   move      efdat         dfdat
     C                   move      etdat         dtdat
     C*
     C     rootFolder    cat(p)    '/':0         ppath
     C                   cat       folder:0      ppath
     C                   cat       '/':0         ppath
     C                   cat       subfolder:0   ppath
     C                   cat       '/':0         ppath

     C     cmkey1        chain     pcm01                              60
     C                   dow       NOT *in60
     C*** begin chg *** 03/14/05 *******************************************
     C                   eval      pPrintFlag = 'N'
     C*** end   chg *** 03/14/05 *******************************************

     C                   if        cmcdat >= cfdat or cfdat = 0
     C                   if        cmcdat <= ctdat or ctdat = 0

     C                   setoff                                       1617

     C*** begin chg *** 03/14/05 *******************************************
     C**********         if        esmcds = *blanks
     C**********         seton                                        16
     C**********         else
     C                   if        esmcds <> *blanks
     C*** end   chg *** 03/14/05 *******************************************
     C                   if        cmsmcd <> *blanks
     C                   eval      y=1
     C     cmsmcd        lookup    sumarr(y)                              16
     C                   endif
     C                   endif

     C*** begin chg *** 03/14/05 *******************************************
     C**********         if        ermcds = *blanks
     C**********         seton                                        17
     C**********         else
     C                   if        ermcds <> *blanks
     C*** end   chg *** 03/14/05 *******************************************
     C                   if        cmrmcd <> *blanks
     C                   eval      z=1
     C     cmrmcd        lookup    rmkarr(z)                              17
     C                   endif
     C                   endif
     C*** begin add *** 03/14/05 *******************************************
     C                   if        ermcds = *blanks and esmcds = *blanks
     C                   seton                                        16
     C                   endif
     C*** begin add *** 03/14/05 *******************************************

     C*** begin chg *** 03/14/05 *******************************************
     C**********         if        *in16 = *on and *in17 = *on
     C                   if        *in16 or *in17
     C*** end   chg *** 03/14/05 *******************************************

     C                   move      cmseq#        pSeq#

     C*** begin chg *** 03/14/05 *******************************************
     C**********         move      cmseq#        dseq#
     C**********         movel(p)  dseq#         pdocsuf
     C**********         cat       cmrmcd:1      pdocsuf
     C                   move      cmseq#        seq#
     C                   clear                   type
     C                   movel(p)  cmrmcd        remarkCode
     C     *cymd         move      cmidt         datcvt
     C                   move      datcvt        remarkDate
     C                   eval      remarkDash1 = '-'
     C                   eval      remarkDash2 = '-'
     C*** end   chg *** 03/14/05 *******************************************

      * only process if it's an image or fax document

     C                   if        (cmdcid <> *blanks) AND
     C                             ((cmimgf = 'Y') OR
     C                             (cmimgf = 'F') OR
     C                             (cmimgf = 'I'))

      * if this is the first document of this claim...
     C                   if        firstDoc = '1' and (eotyp = 'B' or
     C                             eotyp = 'P')
     C                   exsr      DocOne
     C                   endif

     c*** Begin Add *** 12/01/16 **********************************
     C                   If        CMuncn <> ' '
     C                   ExSr      GetDocID
     C*** Begin Add *** 10/19/17 *******************************************
     C                   ExSr      GetFolder
     C*** End   Add *** 10/19/17 *******************************************
     C                   Else
     c*** End   Add *** 12/01/16 **********************************
     C                   if        ((cmimgf = 'Y') OR
     C                             (cmimgf = 'F') OR
     C                             (cmimgf = 'I'))
     C                   eval      document = cmidoc
     C                   else
     C                   eval      document = cmdcid
     C                   endif
     C*** Begin Add *** 10/19/17 *******************************************
     C                   eval      extFolder = CMfold
     C*** End   Add *** 10/19/17 *******************************************
     c*** Begin Add *** 12/01/16 **********************************
     C                   endif
     c*** End   Add *** 12/01/16 **********************************

      * print the document

     C                   if        document <> ' '
     C                   eval      imagprt = ' '
     C*** begin chg *** 03/14/05 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     C**********                   eotyp:ppath:pdocsuf)
     C                   eval      type = '-M'
     **** Start change *** 06/22/07 **********************************************
     C****               callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C***                          cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     C***                          eotyp:ppath:pdocsuf:pPrintFlag)
     C*** end   chg *** 03/14/05 *******************************************
     C*** Begin Chg *** 10/19/17 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C**********                   eotyp:ppath:pdocsuf:pPrintFlag)
     C                   callp     PrintDoc(document:extfolder:mckey:pSeq#:
     C                             cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C                             eotyp:ppath:pdocsuf:pPrintFlag)
     C*** End   Chg *** 10/19/17 *******************************************
     **** end change *** 06/22/07 **********************************************
     C                   endif

      * handle errors
     C                   select
     C                   when      rtnCode = '1'
     C                   when      rtnCode = '2'
     C                   when      rtnCode = '3'
     C                   when      rtnCode = '4'
     C                   when      rtnCode = '5'
     C                   endsl

     C                   endif

      * Get all attached documents

     C*** begin chg *** 01/31/05 *******************************************
     C**********         if        cmimgf <> 'Y'
     C**********                   AND (cmimgf <> 'F'
     C**********                   AND cmimgf <> 'I')
     C                   if        (cmimgf <> 'Y'
     C                              and cmimgf <> 'F'
     C                              and cmimgf <> 'I') or
     C                             (cmqpty = 'Y')
     C*** end   chg *** 01/31/05 *******************************************

      * if this is the first document of this claim...
     C                   if        firstDoc = '1' and (eotyp = 'B' or
     C                             eotyp = 'P')
     C                   exsr      DocOne
     C                   endif

     C                   if        cmqpty = 'Y'
     C     mdkey1        chain     pmd01                              61

     C                   dow       NOT *in61

      * print the document
     C                   eval      imagprt = 'N'
     C                   eval      document = mdoid
     C*** Begin Add *** 10/19/17 *******************************************
     C                   eval      extFolder = MDoFlr
     C*** End   Add *** 10/19/17 *******************************************
     C*** begin chg *** 03/14/05 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     C**********                   eotyp:ppath:pdocsuf)
     C                   eval      type = '-A'
     **** Start change *** 06/22/07 **********************************************
     C****               callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C***                          cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     C***                          eotyp:ppath:pdocsuf:pPrintFlag)
     C*** end   chg *** 03/14/05 *******************************************
     C*** Begin Chg *** 10/19/17 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C**********                   eotyp:ppath:pdocsuf:pPrintFlag)
     C                   callp     PrintDoc(document:extFolder:mckey:pSeq#:
     C                             cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C                             eotyp:ppath:pdocsuf:pPrintFlag)
     C*** End   Chg *** 10/19/17 *******************************************
     **** end change *** 06/22/07 **********************************************

     C     mdkey1        reade     pmd01                                  61
     C                   enddo

     C                   endif

     C                   if        cmimgf = 'O'
     C                             OR (cmimgf = 'W')
     C                   if        cmdcid <> ' '

      * print the document
     C                   eval      imagprt = 'O'
     c*** Begin Add *** 12/01/16 **********************************
     C                   If        CMuncn <> ' '
     C                   ExSr      GetDocID
     C*** Begin Add *** 10/19/17 *******************************************
     C                   ExSr      GetFolder
     C*** End   Add *** 10/19/17 *******************************************
     C                   Else
     c*** End   Add *** 12/01/16 **********************************
     C                   eval      document = cmdcid
     C*** Begin Add *** 10/19/17 *******************************************
     C                   eval      extFolder = CMfold
     C*** End   Add *** 10/19/17 *******************************************
     c*** Begin Add *** 12/01/16 **********************************
     C                   EndIf
     c*** End   Add *** 12/01/16 **********************************
     C*** begin chg *** 03/14/05 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     C**********                   eotyp:ppath:pdocsuf)
     C                   eval      type = '-M'
     **** Start change *** 06/22/07 **********************************************
     C****               callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C***                          cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     C***                          eotyp:ppath:pdocsuf:pPrintFlag)
     C*** end   chg *** 03/14/05 *******************************************
     C*** Begin Chg *** 10/19/17 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C**********                   eotyp:ppath:pdocsuf:pPrintFlag)
     C                   callp     PrintDoc(document:extFolder:mckey:pSeq#:
     C                             cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C                             eotyp:ppath:pdocsuf:pPrintFlag)
     C*** End   Chg *** 10/19/17 *******************************************
     **** end change *** 06/22/07 **********************************************


     C                   endif
     C                   endif
     C                   endif

     C                   endif
     C                   endif
     C                   endif

     C     cmkey1        reade     pcm01                                  60
     C                   enddo

     C* If a specific print server id was not passed in - load balance

     C                   if        ePsid = *blank
     C                   if        firstDoc = ' '
      * set the records to balance the dataq's
     C                   callp     loadBal(svpsid)
     C                   endif
     C                   endif
     C                   eval      firstDoc = '1'

     C                   endsr
      *=========================================================================
      * LoadBal - Balance the print load to different dataqueues
      *=========================================================================
     P LoadBal         B

      *** Procedure Interface ***
     D LoadBal         PI
     D  currentID                     2a

      *** Local Variables ***
     C     pskey3        klist
     C                   kfld                    currentID

      *** Procedure Definition ***
      * SetGT on the current ID to find out if there is a valid record after it.
      * If there is, set it's flag to active. If not, get the first valid record
      * in the file and set it's flag to active.

     C     pskey3        setgt     pps03                              60

     C                   if        NOT *in60
     C                   read      pps03                                  60
     C                   if        NOT *in60
     C                   eval      psfl01 = '1'
     C                   update    pps03
     C                   endif

     C                   else
     C                   eval      currentID = *loval
     C     pskey3        setll     pps03                              60
     C                   read      pps03                                  60
     C                   if        NOT *in60
     C                   eval      psfl01 = '1'
     C                   update    pps03
     C                   endif
     C                   endif

     P LoadBal         E
