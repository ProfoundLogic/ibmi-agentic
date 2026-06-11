      *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2005
      *========================================================================
    H/TITLE SC982R - Print All Remarks - OV documents
      *========================================================================
      * Description: This program prints the OV documents attached to dart
      *              remarks for a given case.
      *
      * Created    : 04/03/00
      * Programmer : C:113
      * Project    : 87MIW
      *========================================================================
     * Revisions:
     *
     * 07/12/05 C:062 Proj: DR193
     *                Output type processing                               d
     *
     * 04/07/03 C:128 Proj: 129WAO
     *                Modify to include all documents, OV, attached objectsd
     *                as well as "imported" documents.
     *
     * ----------------------------------------------------------------------
     *  NOTE: Please add comments to the top of revisions.
     * ----------------------------------------------------------------------
      *========================================================================
      *========================================================================
      * File Definitions
      *========================================================================
     Fqsysprt   o    f   95        printer oflind(*inoa)
     F                                     usropn
     Fsdcml     if   e           k disk

     *=========================================================================
      * Prototypes
     *=========================================================================
      /copy SC982RPR

     *=========================================================================
      * Variable Definitions
     *=========================================================================
      * External program information structure
     D psds          esds                  extname(szq1p)

     d*** begin add *** 07/12/05 *******************************************
     D                 ds
     D subfolder                     12
     D  dfrdt                         6    Overlay(subfolder:1)
     D  dtodt                         6    Overlay(subfolder:7)

     D                 ds
     D document1                     29
     D  claim#1                      16    Overlay(document1:1)
     D  report1                      12    Overlay(document1:18)

     D                 ds
     D document2                     27
     D  claim#2                      16    Overlay(document2:1)
     D  report2                      10    Overlay(document2:18)
     D                                     inz('Cover2.PDF')
     d*** end   add *** 07/12/05 *******************************************
     D eKey            s             40a
     D eOutq           s             10a
     D eRtnCode        s              1a
     d*** begin add *** 07/12/05 *******************************************
     D efrdt           s              7a
     D etodt           s              7a
     D eoutt           s              1a
     d*** end   add *** 07/12/05 *******************************************

     D pSeq#           s              5a
     D rtnCode         s              1a
     d*** begin add *** 07/12/05 *******************************************
     D documenta       s              1a   dim(12)
     D x               s              3s 0
     D y               s              3s 0
     D z               s              3s 0
     D cfrdt           s              7s 0
     D ctodt           s              7s 0
     d*** end   add *** 07/12/05 *******************************************

     D pDocID          s                   like(cmdcid)
     D pFolder         s                   like(cmfold)

     D pDataq          s             10a   inz('TEMPQ')
     D pDataqLib       s             10a   inz('QTEMP')
     D pMaxLength      s              5  0 inz(512)
     D pEntLength      s              5  0 inz(512)
     D pText           s             20a   inz('TEMP DTAQ FOR SC982R')
     d*** begin add *** 07/12/05 *******************************************
     D ppath           s            100a
     D hpath           s            100a
     D folder          s             16
     d*** end   add *** 07/12/05 *******************************************

     D                 ds
     D rData                        512a
     D  rVerb                        10a   overlay(rData:1)
     D  rDocid                       12a   overlay(rData:11)
     D  rFolder                      80a   overlay(rData:23)

     D pData           s            512a
     D pRtnCode        s              1a
     D documents       s              1a   inz('0')

     D printVerb       s             10a   inz('*PRINT')
     D endVerb         s             10a   inz('*END')

     D CreateDtaq      c                   const('SCCRTDTQ')
     D DeleteDtaq      c                   const('SCDLTDTQ')
     D RtvDtaq         c                   const('SCRTVDTQ')
     D SendToDtaq      c                   const('SCSNDDTQ')
     D ValidDoc        c                   const('SCVALDOC')
     D title           c                   const('Print All Remarks Process')
     D caseCover       c                   const('Document Cover Page')
     d*** begin chg *** 07/12/05 *******************************************
     D*line1*****      s             11a   inz('Document ID')
     D rootFolder      c                   const('/CLAIMS OUTPUT')
     D line1           s             78a
     D line2           s             78a
     D line3           s             78a
     D ReturnCode      s              1a
    
     D                 ds
     D claimkey                      40
     D  Company                       3    overlay(claimkey:1)
     D  Group                         3    overlay(claimkey:4)
     D  PolYear                       3    overlay(claimkey:7)
     D  Case                          7    overlay(claimkey:10)

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     d*** end   chg *** 07/12/05 *******************************************
    
     *=========================================================================
      * KList Definitions
     *=========================================================================
     C     cmKey1        klist
     C                   kfld                    eKey


     *=========================================================================
      * PLIST Definitions
     *=========================================================================
     C     *entry        plist
     C                   parm                    eKey
     C                   parm                    eOutq
     C                   parm                    eRtnCode
     d*** begin chg *** 07/12/05 *******************************************
     C                   parm                    eoutt
     C                   parm                    efrdt
     C                   parm                    etodt
     d*** end   chg *** 07/12/05 *******************************************

     C     valdocpl      plist
     C                   parm                    pDocId
     C                   parm                    pFolder
     C                   parm                    pRtnCode

     C     crtdtqpl      plist
     C                   parm                    pDataq
     C                   parm                    pDataqLib
     C                   parm                    pMaxLength
     C                   parm                    pText
     C                   parm                    pRtnCode

     C     dltdtqpl      plist
     C                   parm                    pDataq
     C                   parm                    pDataqLib
     C                   parm                    pRtnCode

     C     snddtqpl      plist
     C                   parm                    pDataq
     C                   parm                    pDataqLib
     C                   parm                    pEntLength
     C                   parm                    pData
     C                   parm                    pRtnCode

     C     rtvdtqpl      plist
     C                   parm                    pDataq
     C                   parm                    pDataqLib
     C                   parm                    rData
     C                   parm                    pRtnCode

     *=========================================================================
      * Main
     *=========================================================================
      * create a temp dataq
     C                   callb     CreateDtaq    crtdtqpl

     C                   open      qsysprt
     c*** begin add *** 07/12/05 ******************************************
     C                   movel(p)  ekey          claimkey

     C                   call      'SA702RR'
     C                   parm      *blanks       returnCode
     C                   parm                    Company
     C                   parm                    Group
     C                   parm                    PolYear
     C                   parm                    Case
     C                   parm      *blanks       line1
     C                   parm      *blanks       line2
     C                   parm      *blanks       line3

     C                   time                    dsdttm
     c*** end   add *** 07/12/05 ******************************************
     C                   except    head
     c*** begin add *** 07/12/05 ******************************************
     C                   move      efrdt         cfrdt
     C                   move      etodt         ctodt
     c*** end   add *** 07/12/05 ******************************************

     C     cmkey1        chain     pcm01                              60
     C                   dow       *in60 = *off
     c*** begin add *** 07/12/05 ******************************************
     C                   if        cmcdat >= cfrdt or cfrdt = 0
     C                   if        cmcdat <= ctodt or ctodt = 0
     c*** end   add *** 07/12/05 ******************************************
     C                   if        cmdcid <> *blanks
      *** Begin Change 04/07/03 ********************************************
     C******             if        cmimgf <> 'Y'
     C******                       AND cmimgf <> 'F'
     C                   if        cmimgf = *blank
     C                             AND cmfl01 <> 'Y'
      *** End   Change 04/07/03 ********************************************

     C                   move      cmseq#        pSeq#
     C                   eval      pDocID = cmdcid
     C                   eval      pFolder = cmfold
     C                   clear                   rtnCode

      * get the document if on optical
     C                   if        cmfl05 = 'O'

     C                   callp     GetOptical(eKey:pSeq#:pDocID:pFolder:rtnCode)
     C                   endif

      * validate document
     C                   callb     ValidDoc      valdocpl

     C                   if        pRtnCode = *on
      * write the docid and folder to a dataq
     C                   clear                   pData
     C                   eval      pData = printVerb + pDocID + pFolder
     C                   callb     SendToDtaq    snddtqpl
     C   oa              except    head
     C   oa              setoff                                       oa
     C                   except    detail
     C                   eval      documents = *on
     C                   endif

     C                   endif

     C                   endif
     c*** begin add *** 07/12/05 ******************************************
     C                   endif
     C                   endif
     c*** end   add *** 07/12/05 ******************************************

     C     cmkey1        reade     pcm01                                  60
     C                   enddo

     C                   if        documents = *off
     C                   except    error
     C                   endif

     C                   close     qsysprt

     C                   clear                   pData
     C                   eval      pData = endVerb
     C                   callb     SendToDtaq    snddtqpl
     c*** begin add *** 07/12/05 ******************************************
    C*
     C* Let's generate the path statement
     C*

     C                   movel(p)  ekey          folder
     C                   move      efrdt         dfrdt
     C                   move      etodt         dtodt
     C                   eval      claim#1 = folder
     C                   eval      claim#2 = folder
     C                   eval      rtncode = *blanks

     C     rootFolder    cat(p)    '/':0         hpath
     C                   cat       folder:0      hpath
     C                   cat       '/':0         hpath
     C                   cat       subfolder:0   hpath
     C                   cat       '/':0         hpath

     C*
     C* Call program that converts the report into a PDF
     C*
     C                   if        eoutt = 'F' or eoutt = 'B'
     C     hpath         cat(p)    document2:0   ppath
     C                   callp     Output_Proc(ppath:eoutt)
     C                   endif
     c*** end   add *** 07/12/05 ******************************************

      * print the documents
     C                   callb     RtvDtaq       rtvdtqpl
     C                   dow       rVerb <> endVerb
     c*** begin add *** 07/12/05 ******************************************
     C                   movea     rdocid        documenta
     C                   eval      x = 1
     C     '.'           lookup    documenta(x)                           99
     C                   if        *in99 = *on
     C                   move      ' '           documenta(x)
     C                   endif
     C                   movea     documenta     report1
     C     hpath         cat(p)    document1:0   ppath
     C                   cat       '.PDF':0      ppath
     c*** end   add *** 07/12/05 ******************************************
     c*** begin chg *** 07/12/05 ******************************************
     C**********         callp     PrintDoc(rDocID:rFolder:eOutq:rtnCode)
     C                   callp     PrintDoc(rDocID:rFolder:eOutq:ppath:
     C                                      eoutt:rtnCode)
     c*** end   chg *** 07/12/05 ******************************************
     C                   callb     RtvDtaq       rtvdtqpl
     C******             if        pRtnCode = *on
     C******             leave
     C******             endif
     C                   enddo

      * delete the temp dataq
     C                   callb     DeleteDtaq    dltdtqpl

     C                   return

     *===============================================================
      * Output specs
     *===============================================================
      * Case cover page
     Oqsysprt   e            head              2
     o*** begin add *** 07/12/05 ******************************************
     O                       dsdate              14 '  /  /    '
     o*** end   add *** 07/12/05 ******************************************
     O                       title               58
     o*** begin add *** 07/12/05 ******************************************
     O                       dstime              95 '  :  :  '
     o*** end   add *** 07/12/05 ******************************************
     O          e            head        1
     o*** begin add *** 07/12/05 ******************************************
     O                       q1pgm               14
     o*** end   add *** 07/12/05 ******************************************
     O                       caseCover           56
     o*** begin add *** 07/12/05 ******************************************
     O                                           90 'PAGE:'
     O                       page                95
     o*** end   add *** 07/12/05 ******************************************
     o*** begin chg *** 07/12/05 ******************************************
     O**********e            head        2  2
     O***********            line1               15
     O          e            head        2
     O                       line1               82
     O          e            head        1
     O                       line2               82
     O          e            head        1
     O                       line3               82
     O          e            head        2  2
     O                                           15 'Document ID'
     o*** end   chg *** 07/12/05 ******************************************

      * Employer cover page detail
     O          e            detail
     O                       cmdcid              16
      * ERROR
     O          e            error               42
     O                                              'No documents found for th+
     O                                              e specified case'
