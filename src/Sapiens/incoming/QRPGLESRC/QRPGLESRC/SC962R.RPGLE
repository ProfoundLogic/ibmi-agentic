     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     *
      *========================================================================
    H/TITLE SC962R - Print All Remarks - OV documents
      *========================================================================
      * Description: This program prints the OV documents attached to dart
      *              remarks for a given case.
      *
      *=====================================================================
      * Created on 06/30/2003 by c:62 for Project 129WAO
      *========================================================================
     * Revisions:
     *
      *  03/15/13 C:33  Redmine: 3556
      *                 Add/Retrieve UNCN/IFSP to data queue
      *
      *  10/17/11 C:277 Project: SY195
      *                 QDLS migration to IFS
      *                 Add two parameters with command
      *
      * 12/15/08 C: 220 SUPPORT: 63991
      *                -THE DARTS PRINT REPORT INS NOT PRINTINNG R/H
      *                 MARGIN ON REPORT.
      *
     * 03/14/05 c:62 project 129IJS
      *               Change the query selection of remark code and
      *               summary code to an OR
     *
      *========================================================================
     Fsdcml     if   e           k disk
      ******* BEG CHG ******* 12/15/08 ********************************
     F******* qsysprt   o    f   95        printer oflind(*inoa)
     Fqsysprt   o    f  108        printer oflind(*inoa)
      ******* END CHG ******* 12/15/08 ********************************
     F                                     usropn

     *=========================================================================
      * Prototypes
     *=========================================================================
      /copy SC962RPR

     *=========================================================================
      * Variable Definitions
     *=========================================================================
      * External program information structure
     D psds          esds                  extname(szq1p)

     D                 ds
     D subfolder                     12
     D  dfdat                         6    Overlay(subfolder:1)
     D  dtdat                         6    Overlay(subfolder:7)

     D                 ds
     D document1                     29
     D  claim#1                      16    Overlay(document1:1)
     D  report1                      12    Overlay(document1:18)

     D                 ds
     D document2                     27
     D  claim#2                      16    Overlay(document2:1)
     D  report2                      10    Overlay(document2:18)
     D                                     inz('Cover2.PDF')

     D eKey            s             40a
     D eOutq           s             10a
     D eRtnCode        s              1a
     D efdat           s              7a
     D etdat           s              7a
     D eotyp           s              1a
     D esmcds          s             56a
     D ermcds          s            210a

     D pSeq#           s              5a
     D rtnCode         s              1a
     D documenta       s              1a   dim(12)
     D x               s              3s 0
     D y               s              3s 0
     D z               s              3s 0
     D cfdat           s              7s 0
     D ctdat           s              7s 0

     D pDocID          s                   like(cmdcid)
     D pFolder         s                   like(cmfold)
     D***  Begin Add  ***  03/15/13  ************************************
     D pUncN           s                   like(CMuncN)
     D pIfsP           s                   like(CMifsP)
     D***  End   Add  ***  03/15/13  ************************************

     D pDataq          s             10a   inz('TEMPQ')
     D pDataqLib       s             10a   inz('QTEMP')
     D pMaxLength      s              5  0 inz(512)
     D pEntLength      s              5  0 inz(512)
     D pText           s             20a   inz('TEMP DTAQ FOR SC982R')
     D ppath           s            256a
     D hpath           s            256a
     D*** Begin Add *** 10/17/11 **************************************
     D  uncnpath       S            255a
     D  fsf            S              1a
     D*** End   Add *** 10/17/11 **************************************

     D                 ds
     D rData                        512a
     D  rVerb                        10a   overlay(rData:1)
     D  rDocid                       12a   overlay(rData:11)
     D  rFolder                      80a   overlay(rData:23)
     D***  Begin Add  ***  03/15/13  ************************************
     D  rUncN                       255a   overlay(rData:103)
     D  rIfsP                         1a   overlay(rData:358)
     D***  End   Add  ***  03/15/13  ************************************

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

     D pData           s            512a
     D pRtnCode        s              1a
     D documents       s              1a   inz('0')

     D printVerb       s             10a   inz('*PRINT')
     D endVerb         s             10a   inz('*END')
     D folder          s             16

     D CreateDtaq      c                   const('SCCRTDTQ')
     D DeleteDtaq      c                   const('SCDLTDTQ')
     D RtvDtaq         c                   const('SCRTVDTQ')
     D SendToDtaq      c                   const('SCSNDDTQ')
     D ValidDoc        c                   const('SCVALDOC')
     D title           c                   const('Print All Remarks Process')
     D caseCover       c                   const('Document Cover Page')
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
     C                   parm                    efdat
     C                   parm                    etdat
     C                   parm                    eotyp
     C                   parm                    esmcds
     C                   parm                    ermcds

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
     C                   movea     esmcds        sumarr
     C                   movea     ermcds        rmkarr

     C                   open      qsysprt

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

     C                   except    head
     C                   move      efdat         cfdat
     C                   move      etdat         ctdat

     C     cmkey1        chain     pcm01                              60
     C                   dow       *in60 = *off

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
     C*** end   add *** 03/14/05 *******************************************

     C*** begin chg *** 03/14/05 *******************************************
     C**********         if        *in16 = *on and *in17 = *on
     C                   if        *in16 or *in17
     C*** end   chg *** 03/14/05 *******************************************

     C                   if        cmdcid <> *blanks
     C                   if        cmimgf = *blank
     C                             AND cmfl01 <> 'Y'

     C                   move      cmseq#        pSeq#
     C                   eval      pDocID = cmdcid
     C                   eval      pFolder = cmfold
      /Free
       //***  Begin Add  ***  03/15/13  ************************************
       pUncN = CMuncN;
       pIfsP = CMifsP;
       //***  Begin Add  ***  03/15/13  ************************************
      /End-free
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
     C***  Begin Chg  ***  03/15/13  ***************************************
     C**********         eval      pData = printVerb + pDocID + pFolder
      /Free
       pData = printVerb + pDocID + pFolder + pUncN + pIfsP;
      /End-free
     C***  End   Chg  ***  03/15/13  ***************************************
     C                   callb     SendToDtaq    snddtqpl
     C   oa              except    head
     C   oa              setoff                                       oa
     C                   except    detail
     C                   eval      documents = *on
     C                   endif

     C                   endif
     C                   endif

     C                   endif
     C                   endif
     C                   endif

     C     cmkey1        reade     pcm01                                  60
     C                   enddo

     C                   if        documents = *off
     C                   except    error
     C                   endif

     C                   close     qsysprt

     C                   clear                   pData
     C                   eval      pData = endVerb
     C                   callb     SendToDtaq    snddtqpl
    C*
     C* Let's generate the path statement
     C*

     C                   movel(p)  ekey          folder
     C                   move      efdat         dfdat
     C                   move      etdat         dtdat
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
     C                   if        eotyp = 'F' or eotyp = 'B'
     C     hpath         cat(p)    document2:0   ppath
     C                   callp     Output_Proc(ppath:eotyp)
     C                   endif

      * print the documents
     C                   callb     RtvDtaq       rtvdtqpl
     C                   dow       rVerb <> endVerb
     C                   movea     rdocid        documenta
     C                   eval      x = 1
     C     '.'           lookup    documenta(x)                           99
     C                   if        *in99 = *on
     C                   move      ' '           documenta(x)
     C                   endif
     C                   movea     documenta     report1
     C     hpath         cat(p)    document1:0   ppath
     C                   cat       '.PDF':0      ppath
     C***  Begin Chg  ***  03/15/13  ***************************************
     C****Begin*Add *** 10/17/11 **************************************
     C**********         MOVEL     CMUNCN        uncnpath
     C**********         MOVE      CMIFSP        fsf
     C****End***Add *** 10/17/11 **************************************
      /Free
       uncNPath = rUncN;
       fsf = rIfsP;
      /End-free
     C***  Begin Chg  ***  03/15/13  ***************************************
     C*** Begin Chg *** 10/17/11 **************************************
     C*****              callp     PrintDoc(rDocID:rFolder:eOutq:ppath:
     C*****                                 eotyp:rtnCode)
     C                   callp     PrintDoc(rDocID: rFolder: eOutq: ppath:
     C                                      eotyp: rtnCode: uncnpath: fsf)
     C*** End   Chg *** 10/17/11 **************************************
     C                   callb     RtvDtaq       rtvdtqpl
     C                   enddo

      * delete the temp dataq
     C                   callb     DeleteDtaq    dltdtqpl

     C                   return

     *===============================================================
      * Output specs
     *===============================================================
      * Case cover page
     Oqsysprt   e            head              2
     O                       dsdate              14 '  /  /    '
     O                       title               58
     O                       dstime              95 '  :  :  '
     O          e            head        1
     O                       q1pgm               14
     O                       caseCover           56
     O                                           90 'PAGE:'
     O                       page                95
     O          e            head        2
     O                       line1               82
     O          e            head        1
     O                       line2               82
     O          e            head        1
     O                       line3               82
     O          e            head        2  2
     O                                           15 'Document ID'

      * Employer cover page detail
     O          e            detail
     O                       cmdcid              16
      * ERROR
     O          e            error               42
     O*** Beg   chg *** 12/15/008 ******************************************
     O*******                                       'No documents found for th+
     O*******                                       e specified case'
     O                                              '    No documents found fo+
     O                                              r the specified case'
     O*** End   chg *** 12/15/008 ******************************************
