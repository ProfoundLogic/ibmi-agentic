      *========================================================================
     * (C) Copyright Fiserv, Inc. 2005 - 2007
      *========================================================================
    H/TITLE SC983R - Print all remarks Image print request
      *========================================================================
      * Description: This program prints the image documents attached to dart
      *              remarks for a given case.
      *
      * Created    : 04/03/00
      * Programmer : C:113
      * Project    : 87MIW
      *========================================================================
     * Revisions:
    H*
    H* 06/22/07 c:02 project FIX b4 base tape goes out
    H*               -Change call to PrintDoc (sc964c) to use the actual
    H*                CMFL05 OPTICAL FLAG instead of cmDCTP (not sure why
      *                this was being used). SO optical IMAGES will work with
      *                this process
    H*
     *
     * 05/25/07 C:191 Proj: WC1515
     *                Check claims authority
     *
     * 07/12/05 C:062 Proj: DR193
     *                Add output type processing                           d
     *
     * 04/07/03 C:128 Proj: 129WAO
     *                Modify to include all documents, OV, attached objectsd
     *                as well as "imported" documents.
     *
     * 11/27/02 C:87  Proj: 114IEC
     *                If 'Print Server ID' is passed in, then use it instead
     *                of the load balancing process.
     *
     * 07/24/02 C:87  Proj: 114IEC
     *                Added code to process for an employer when the entry parm
     *                eEmployer has a value, and to process for a claim when
     *                eYear/eCase have values.  Some of the code was reorganized
     *                in order to accomplish this.
     *
     * ----------------------------------------------------------------------
     *  NOTE: Please add comments to the top of revisions.
     * ----------------------------------------------------------------------
      *========================================================================
      *========================================================================
      * File Definitions
      *========================================================================
     Fsdcml     if   e           k disk
     Fwmcml     if   e           k disk    rename(pcm01:pcl01)
     F                                     prefix(cl)
     F***  begin add  ***  07/24/02  *******************************************
     Fwmcml1    if   e           k disk    rename(pcm01:pcl02)
     F                                     prefix(c2)
     F***  end   add  ***  07/24/02  *******************************************
     Fimpsl3    uf a e           k disk    rename(pps01:pps02)
     Fimpsl4    uf a e           k disk    rename(pps01:pps03)
      *** Begin Change 04/07/03 ********************************************
     Fsmmdl1    if   e           k disk
      *** End   Change 04/07/03 ********************************************
     *=========================================================================
      * Prototypes
     *=========================================================================
      /copy SC983RPR

     *=========================================================================
      * Global Variables
     *=========================================================================
      * External program information structure
     D psds          esds                  extname(szq1p)
     d*** begin add *** 07/12/05 *******************************************
     D                 ds
     D subfolder                     12
     D  dfrdt                         6    Overlay(subfolder:1)
     D  dtodt                         6    Overlay(subfolder:7)
     D                 ds
     D claim#                        16
     D  xxco#                         3  0 Overlay(claim#:1)
     D  xxfnd                         3  0 Overlay(claim#:4)
     D  xxfyr                         3  0 Overlay(claim#:7)
     D  xxcase                        7  0 Overlay(claim#:10)
     d*** end   add *** 07/12/05 *******************************************

     D eKey            s             40a
     D***  begin chg  ***  07/24/02  *******************************************
     D****eRtnCode        s              1a
     D eYear           s              3a
     D eCase           s              7a
     D***  end   chg  ***  07/24/02  *******************************************

     D pSeq#           s              5a
     D rtnCode         s              1a
     D rtnCode6        s              6a
     D eCompany        s              3a
     D eGroup          s              3a
     D eEmployer       s              9a
     D eOutq           s             10a
     d*** begin add *** 07/12/05 *******************************************
     D efrdt           s              7a
     D etodt           s              7a
     D eoutt           s              1a
     D folder          s             16
     d*** end   add *** 07/12/05 *******************************************
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
     D document        s             12a
     d*** begin add *** 07/12/05 *******************************************
     D ppath           s            100a
     D hpath           s            100a
     D dseq#           s              5a
     D pdocsuf         s             11a
     D y               s              3s 0
     D z               s              3s 0
     D cfrdt           s              7s 0
     D ctodt           s              7s 0
     D suffix          s              3a
     D pflr            s             80a
     d*** end   add *** 07/12/05 *******************************************

      *** Local Variables ***
     D firstDoc        s              1a   inz('1')
     D nextFlag        s              1a   inz('1')
     D svpsid          s              2a
     D imagprt         s              1a

     D image           c                    const('***IMAGE***')
     d*** begin add *** 07/12/05 *******************************************
     D rootFolder      c                   const('/CLAIMS OUTPUT')
     d*** end   add *** 07/12/05 *******************************************
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
     C**********         kfld                    subs
     C                   kfld                    group
     C                   kfld                    employer

     F***  begin add  ***  07/24/02  *******************************************
     C     cmkey3        klist
     C                   kfld                    company
     C                   kfld                    group
     C                   kfld                    polYear
     C                   kfld                    case
     c***  end   add  ***  07/24/02  *******************************************
     c*** begin add *** 07/12/05 *******************************************
     C     cmkey4        klist
     C                   kfld                    company
     C                   kfld                    group
     c*** end   add *** 07/12/05 *******************************************

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
     C***  begin chg  ***  07/24/02  *******************************************
     C****************** parm                    eRtnCode
     C                   parm                    eYear
     C                   parm                    eCase
     C***  end   chg  ***  07/24/02  *******************************************
     C***  begin add  ***  11/27/02  *******************************************
     C                   parm                    ePsid             2
     C***  end   add  ***  11/27/02  *******************************************
     c*** begin add *** 07/12/05 *******************************************
     C                   parm                    eoutt
     C                   parm                    efrdt
     C                   parm                    etodt
     c*** end   add *** 07/12/05 *******************************************
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** parameter list for claims secuirty API
     C     cuspl         plist
     C                   parm                    ##co#             3
     C                   parm                    ##fnd             3
     C                   parm                    ##fyr             3
     C                   parm                    ##case            7
     C                   parm      q1user        ##usr            10
     C                   parm                    ##rtcd            1
     C*** END ADD ****** 05/25/07 ********************************************

     *=========================================================================
      * Main
     *=========================================================================
      * get the managed care subscriber number
     C*****************  CALLP     GetSubAPI(returnCode:eCompany:eGroup:
     C*******************************        pSubs:pmcGroup)

     C                   move      eGroup        pmcGroup
     C                   move      eCompany      company
     C                   move      pmcGroup      group
     C*****************  move      pSubs         subs
     C                   move      eEmployer     employer
     C***  begin add  ***  07/24/02  *******************************************
     C                   move      eYear         polYear
     C                   move      eCase         case
     c*** begin add *** 07/12/05 *******************************************
     C                   move      efrdt         cfrdt
     C                   move      etodt         ctodt
     c*** end   add *** 07/12/05 *******************************************

      * call employer or claim subroutine based on entry parms
     C                   select
     C                   when      employer <> 0
     C                   exsr      procEmp
     C                   when      polYear <> 0
     C                   exsr      procClm
     c*** begin add *** 07/12/05 *******************************************
     C                   other
     C                   exsr      procGrp
     c*** end   add *** 07/12/05 *******************************************
     C                   endsl
     C***  end   add  ***  07/24/02  *******************************************

     C***  begin mov  ***  07/24/02  *******************************************
     C***  moving this code to subroutine procEmp
      ***
      **get each claim for the employer
     C***  cmkey2        chain     pcl01                              60
      ***
     C***                dow       NOT *in60
     C***                eval      polYear = clcmfyr
     C***                eval      division = clcmdiv
     C***                eval      case = clcmcase
     C***                move      clcmcase      pCase
     C***                move      clcmfyr       pPolYear
      ***
      **call api to get key
     C***                eval      returnCode = 'K'
     C***                eval      mcKey = *blanks
     C***                callp     CaseKeyAPI(returnCode:
     C***                                    eCompany:pmcGroup:
     C***                                    pPolYear:pCase:mcKey)
      ***
      **process all darts remarks for that key
     C***                callp     ProcDarts
      ***
     C***  cmkey2        reade     pcl01                                  60
     C***                enddo
     C***  end   mov  ***  07/24/02  *******************************************

     C                   move      *on           *inlr



      *=========================================================================
      * DocOne - If this is the first image or other document produce cover
      *=========================================================================
     C     DocOne        begsr

     C                   eval      firstDoc = ' '

     C***  begin add  ***  11/27/02  ***************************************
     C*
     C* If a specific print server id was passed in, use it instead of
     C* load balancing
     C                   if        ePsid <> *blank
     C                   eval       svpsid = ePsid
     C     pskey3        chain     pps03                              60
     C                   else
     C***  end   add  ***  11/27/02  ***************************************
      * get the next dtaq
     C     pskey2        chain     pps02                              60
     C                   if        NOT *in60
     C                   eval      svpsid = pspsid
     C                   eval      psfl01 = ' '
     C                   update    pps02

     C                   else
     C                   eval      nextFlag = ' '
     C     pskey2        chain     pps02                              60
     c*** begin add *** 07/12/05 *******************************************
     C                   if        NOT *in60
     c*** end   add *** 07/12/05 *******************************************
     C                   eval      svpsid = pspsid
     C                   update    pps02
     c*** begin add *** 07/12/05 *******************************************
     c                   endif
     c*** end   add *** 07/12/05 *******************************************
     C                   endif
     C***  begin add  ***  11/27/02  ***************************************
     C                   endif                                                  ePsid<>' '
     C***  end   add  ***  11/27/02  ***************************************

      * write the request to the dataq for a cover page
     C                   eval      rtnCode = *blanks
     C                   eval      line1 = *blanks
     C                   eval      line2 = *blanks
     C                   eval      line3 = *blanks
     C                   callp     InfoAPI(rtnCode:eCompany:pmcGroup:
     C                                     pPolYear:pCase:line1:line2:line3)

      * print the cover page for the documents
     c*** begin chg *** 07/12/05 *******************************************
     C**********         callp     PrintCover(pspque:pspqlb:pVerb:
     C**********                              line1:line2:line3)
     C                   callp     PrintCover(pspque:pspqlb:pVerb:
     C                                        line1:line2:line3:eoutt)
     c*** end   chg *** 07/12/05 *******************************************

      * write entries to dataq with the docids
     c*** begin chg *** 07/12/05 *******************************************
     C**********         callp     DocidList(mckey:pspque:pspqlb)
     C                   callp     DocidList(mckey:pspque:pspqlb:efrdt:
     C                                        etodt:eoutt)
     c*** end   chg *** 07/12/05 *******************************************


     C                   endsr
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
     C*** BEG ADD ****** 05/25/07 *************************************************
     C*** Check claim authority
     C                   move      clcmco#       ##co#
     C                   move      clcmfnd       ##fnd
     C                   move      clcmfyr       ##fyr
     C                   move      clcmcase      ##case
     C                   move      *blanks       ##rtcd
     C*
     C                   call      'WA067R'      cuspl
     C* authorised when rtcd <>'2'
     C                   if        ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************
      * call api to get key
     C                   eval      returnCode = 'K'
     C                   eval      mcKey = *blanks
     C                   callp     CaseKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       pPolYear:pCase:mcKey)
     c*** begin add *** 07/12/05 *******************************************
     C                   eval      xxco# = clcmco#
     C                   eval      xxfnd = clcmfnd
     C                   eval      xxfyr = clcmfyr
     C                   eval      xxcase = clcmcase
     c*** end   add *** 07/12/05 *******************************************
      *
      * process all darts remarks for that key
     C******             callp     ProcDarts
     C                   exsr      ProcDarts
      *
     C*** BEG ADD ****** 05/25/07 *************************************************
     C                   endif                                                  ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************
     C     cmkey2        reade     pcl01                                  60
     C                   enddo

     C                   endsr                                                  procEmp



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
     C*** BEG ADD ****** 05/25/07 *************************************************
     C*** Check claim authority
     C                   move      c2cmco#       ##co#
     C                   move      c2cmfnd       ##fnd
     C                   move      c2cmfyr       ##fyr
     C                   move      c2cmcase      ##case
     C                   move      *blanks       ##rtcd
     C*
     C                   call      'WA067R'      cuspl
     C* authorised when rtcd <>'2'
     C                   if        ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************
      * call api to get key
     C                   eval      returnCode = 'K'
     C                   eval      mcKey = *blanks
     C                   callp     CaseKeyAPI(returnCode:
     C                                       eCompany:pmcGroup:
     C                                       pPolYear:pCase:mcKey)
     c*** begin add *** 07/12/05 *******************************************
     C                   eval      xxco# = c2cmco#
     C                   eval      xxfnd = c2cmfnd
     C                   eval      xxfyr = c2cmfyr
     C                   eval      xxcase = c2cmcase
     c*** end   add *** 07/12/05 *******************************************
      *
      * process all darts remarks for that key
     C******             callp     ProcDarts
     C                   exsr      ProcDarts
      *
     C*** BEG ADD ****** 05/25/07 *************************************************
     C                   endif                                                  ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************
     C     cmkey3        reade     pcl02                                  60
     C                   enddo

     C                   endsr                                                  procClm
     c*** begin add *** 07/12/05 *******************************************
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
     C*** BEG ADD ****** 05/25/07 *************************************************
     C*** Check claim authority
     C                   move      c2cmco#       ##co#
     C                   move      c2cmfnd       ##fnd
     C                   move      c2cmfyr       ##fyr
     C                   move      c2cmcase      ##case
     C                   move      *blanks       ##rtcd
     C*
     C                   call      'WA067R'      cuspl
     C* authorised when rtcd <>'2'
     C                   if        ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************
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
     C*** BEG ADD ****** 05/25/07 *************************************************
     C                   endif                                                  ##rtcd <> '2'
     C*** END ADD ****** 05/25/07 *************************************************
     C     cmkey4        reade     pcl02                                  60
     C                   enddo

     C                   endsr                                                  procClm
     c*** end   add *** 07/12/05 *******************************************



      *=========================================================================
      * ProcDarts - Process all the darts records for image documents
      *=========================================================================
     C     ProcDarts     begsr
     P*******ProcDarts       B

      *** Procedure Interface ***
     D****** ProcDarts       PI
      *** Procedure Definition ***
     c*** begin add *** 07/12/05 *******************************************
     C*
     C* Created Path Statement
     C*
     C                   movel(p)  claim#        folder
     C                   move      efrdt         dfrdt
     C                   move      etodt         dtodt
     C*
     C                   call      'SA030R'
     C                   parm      *blanks       suffix
     C                   parm      *blanks       pflr
     C*
     c                   if        pflr <> *blanks
     c     pflr          cat(p)    rootfolder:0  ppath
     c***                cat       rootfolder:0  ppath
     c                   cat       '/':0         ppath
     c                   else
     C     rootFolder    cat(p)    '/':0         ppath
     C                   endif
     C                   cat       folder:0      ppath
     C                   cat       '/':0         ppath
     C                   cat       subfolder:0   ppath
     C                   cat       '/':0         ppath
     c*** end   add *** 07/12/05 *******************************************
     C     cmkey1        chain     pcm01                              60
     C                   dow       NOT *in60
     c*** begin add *** 07/12/05 *******************************************
     C                   if        cmcdat >= cfrdt or cfrdt = 0
     C                   if        cmcdat <= ctodt or ctodt = 0
     c*** end   add *** 07/12/05 *******************************************

     C                   move      cmseq#        pSeq#

      * only process if it's an image or fax document
      *** Begin Change 04/07/03 ********************************************
     C******             if        cmdcid <> *blanks
     C******                       AND
     C******                       (cmimgf = 'Y' OR cmimgf = 'F')
     C                   if        (cmdcid <> *blanks) AND
     C                             ((cmimgf = 'Y') OR
     C                             (cmimgf = 'F') OR
     C                             (cmimgf = 'I'))
      *** End   Change 04/07/03 ********************************************

      * if this is the first document of this claim...
     c*** begin chg *** 07/12/05 *******************************************
     C**********         if        firstDoc = '1'
     C                   if        firstDoc = '1' and (eoutt = 'B' or
     C                             eoutt = 'P')
     c*** end   chg *** 07/12/05 *******************************************
     C                   exsr      DocOne
     C                   endif

     C****************** if        cmdcid = image  ***** 5/12/00 MG ****
      *** Begin Change 04/07/03 ********************************************
     C******             if        cmimgf = 'Y'
     C                   if        ((cmimgf = 'Y') OR
     C                             (cmimgf = 'F') OR
     C                             (cmimgf = 'I'))
      *** End   Change 04/07/03 ********************************************
     C                   eval      document = cmidoc
     C                   else
     C                   eval      document = cmdcid
     C                   endif

      * print the document
     c*** begin add *** 07/12/05 *******************************************
     C                   if        document <> ' '
     c*** end   add *** 07/12/05 *******************************************
     C                   eval      imagprt = ' '
     c*** begin chg *** 07/12/05 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmdctp:cmimgf:svpsid:rtnCode6:imagprt)
     **** change 6/22/07 ****************************************************
     *****               callp     PrintDoc(document:cmfold:mckey:pSeq#:
     *****                         cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     ******                        eoutt:ppath:pdocsuf)
     C                   callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C                             cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C                             eoutt:ppath:pdocsuf)
     **** END change 6/22/07 ****************************************************
     c*** end   chg *** 07/12/05 *******************************************
     c*** begin add *** 07/12/05 *******************************************
     c                   endif
     c*** end   add *** 07/12/05 *******************************************

      * handle errors
     C                   select
     C                   when      rtnCode = '1'
     C                   when      rtnCode = '2'
     C                   when      rtnCode = '3'
     C                   when      rtnCode = '4'
     C                   when      rtnCode = '5'
     C                   endsl

     C                   endif

      *** Begin Change 04/07/03 ********************************************

      * Get all attached documents

     C                   if        cmimgf <> 'Y'
     C                             AND (cmimgf <> 'F'
     C                             AND cmimgf <> 'I')

      * if this is the first document of this claim...
     c*** begin chg *** 07/12/05 *******************************************
     C**********         if        firstDoc = '1'
     C                   if        firstDoc = '1' and (eoutt = 'B' or
     C                             eoutt = 'P')
     c*** end   chg *** 07/12/05 *******************************************
     C                   exsr      DocOne
     C                   endif

     C                   if        cmqpty = 'Y'
     C     cmkey1        chain     pmd01                              61

     C                   dow       NOT *in61

      * print the document
     C                   eval      imagprt = 'N'
     C                   eval      document = mdoid
     c*** begin chg *** 07/12/05 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmdctp:cmimgf:svpsid:rtnCode6:imagprt)
     **** change 6/22/07 ****************************************************
     *****               callp     PrintDoc(document:cmfold:mckey:pSeq#:
     *****                         cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     ******                        eoutt:ppath:pdocsuf)
     C                   callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C                             cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C                             eoutt:ppath:pdocsuf)
     **** END change 6/22/07 ****************************************************
     c*** end   chg *** 07/12/05 *******************************************

     C     cmkey1        reade     pmd01                                  61
     C                   enddo

     C                   endif

     C                   if        cmimgf = 'O'
     C                             OR (cmimgf = 'W')
     c*** begin add *** 07/12/05 *******************************************
     C                   if        cmdcid <> ' '
     c*** end   add *** 07/12/05 *******************************************

      * print the document
     C                   eval      imagprt = 'O'
     C                   eval      document = cmdcid
     c*** begin add *** 07/12/05 *******************************************
     C**********         callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C**********                   cmdctp:cmimgf:svpsid:rtnCode6:imagprt)
     **** change 6/22/07 ****************************************************
     *****               callp     PrintDoc(document:cmfold:mckey:pSeq#:
     *****                         cmdctp:cmimgf:svpsid:rtnCode6:imagprt:
     ******                        eoutt:ppath:pdocsuf)
     C                   callp     PrintDoc(document:cmfold:mckey:pSeq#:
     C                             cmfl05:cmimgf:svpsid:rtnCode6:imagprt:
     C                             eoutt:ppath:pdocsuf)
     **** END change 6/22/07 ****************************************************
     C                   endif
     C                   endif
     C                   endif
     c*** end   add *** 07/12/05 *******************************************
     C                   endif
     C                   endif
      *** Begin Change 04/07/03 ********************************************

     C     cmkey1        reade     pcm01                                  60
     C                   enddo

     C***  begin add  ***  11/27/02  ***************************************
     C* If a specific print server id was not passed in - load balance
     C                   if        ePsid = *blank
     C***  end   add  ***  11/27/02  ***************************************
     C                   if        firstDoc = ' '
      * set the records to balance the dataq's
     C                   callp     loadBal(svpsid)
     C                   endif
     C***  begin add  ***  11/27/02  ***************************************
     C                   endif                                                  ePsid=' '
     C***  end   add  ***  11/27/02  ***************************************
     C                   eval      firstDoc = '1'

     P****** ProcDarts       E

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
     c*** begin add *** 07/12/05 ******************************************
     C                   if        NOT *in60
     c*** end   add *** 07/12/05 ******************************************
     C                   eval      psfl01 = '1'
     C                   update    pps03
     c*** begin add *** 07/12/05 ******************************************
     c                   endif
     c*** end   add *** 07/12/05 ******************************************

     C                   else
     C                   eval      currentID = *loval
     C     pskey3        setll     pps03                              60
     C                   read      pps03                                  60
     c*** begin add *** 07/12/05 ******************************************
     C                   if        NOT *in60
     c*** end   add *** 07/12/05 ******************************************
     C                   eval      psfl01 = '1'
     C                   update    pps03
     c*** begin add *** 07/12/05 ******************************************
     c                   endif
     c*** end   add *** 07/12/05 ******************************************
     C                   endif

     P LoadBal         E
